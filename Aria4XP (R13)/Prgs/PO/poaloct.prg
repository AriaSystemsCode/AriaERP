*:************************************************************************
*: Program file  : POALOCT.PRG
*: Program desc. : 1- Style Purchase Orders Allocation
*: For screen    : SOALOCT.SCX
*: System        : ARIA BUSINESS SYSTEM
*: Module        : Style Purchase orders Modules
*: Developer     : Wael M. Abo-Shawareb (WSH)
*: Issue NO.     : 037584,1                                                 :*
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
*:  DO gpDoProg WITH 'AWRPOALOCT',.F.,'PO','C'
*:*************************************************************
*:  Modifications :
*:************************************************************************

*-- If the sales orders module not installed , do not allow running the program.
IF !("SO" $ oAriaApplication.CompanyInstalledModules)
  *** The sales orders module is not installed for ***
  *** this company.  You have to install the sales ***
  *** orders module first.
  *** <  Ok  > ***
  =gfModalGen("TRM32053B00000" , "DIALOG")
  RETURN
ENDIF

*-- Call the program with "P" parameter for Style Purchase Orders Allocation.
DO (oAriaApplication.ProgramHome + "SO\SOALOCT.FXP") WITH 'P'
