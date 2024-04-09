*:***************************************************************************
*: Program file  : lfPreGrd
*: Program desc. : Pre_Option Grid dummy Function
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Ahmed Mohamed Ibrahim (AMM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Note That     : This is a dummy function called from ICREPORT program before calling of
*:               : gfOpGrid to do nothing if programmer does not include this function in its 
*:               : program code, there for OG is still done because I adjust the calling of OG
*:               : gfOpGrid function to be under condition which does not affected with dummy 
*:               : function, but if programmer want to do OG under condition like me 
*:               : in Style Ranking report (ICSRANK) all to do is to include lfPreGrd function
*:               : in its program and then write the code for its condition in it and if the 
*:               : result of this condition is to skip doing OG he must put variable <llDoOpGrid = .F.> 
*:               : before return, see W:\ARIA27\REPORTS\IC\ICSRANK.PRG and ICREPORT for more detail.
*:***************************************************************************
*: Example       : =lfPreGrd()
*:***************************************************************************
*:

RETURN .T.