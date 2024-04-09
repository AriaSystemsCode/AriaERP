*:************************************************************************
*: Program file  : MFALOCT.PRG
*: Program desc. : 1- Cutting Ticket Allocation
*: For screen    : SOALOCT.SCX (0,1,2,3)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Manufactering Module
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *E300777,1 Rewrite the program to work in the 2.7 version
*:************************************************************************
*: Calls         : 
*:         Programs   : SO\SOALOCT.FXP
*:         Screens    : None
*:         Procedures : None
*:         Functions  : None
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Example            :
*:  DO gpDoProg WITH 'AWRMFALOCT',.F.,'MF','C'
*:*************************************************************
*:  Modifications :
*:************************************************************************
*
*-- If the sales orders module not installed , do not allow running the program.
IF !("SO" $ gcCmpModules)
  *** The sales orders module is not installed for ***
  *** this company.  You have to install the sales ***
  *** orders module first.
  *** <  Ok  > ***
  =gfModalGen("TRM32053B00000" , "DIALOG")
  glQuitting = .T.
  RETURN
ENDIF

*-- Call the program with "C" parameter for Cutting Ticket Allocation.
DO (gcAppHome+'SO\SOALOCT') WITH 'C'
