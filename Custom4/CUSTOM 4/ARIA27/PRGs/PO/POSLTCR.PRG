*:************************************************************************
*: Program file  : POSLTCR.PRG
*: Program desc. : Letter Of Credit (From Style Purchase Order Module)
*: For screen    : POLETCR.SCX (0,1,2,3,4,4A,4B)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Style Purchase orders.
*:      Developer: Reham Al-Allamy
*:           Date: 02/11/1998
*:************************************************************************
*: Documented *E300801,1 Rewrite the program to work in the 2.7 version
*:************************************************************************
*: Calls         : 
*:         Programs   : POLETCR.Fxp (Main Letter of Credit Program)
*:         Screens    : None
*:         Procedures : None
*:         Functions  : None
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example            :
*:   DO gpDoProg WITH 'AWRPOSLTCR' , .F. , 'PO'
*:************************************************************************
*:  Modifications :
*:************************************************************************
*
*-- Call the program with "S" parameter for Letter of Credit from 
*-- Style Purchase Order Module.
DO (gcAppHome+'\POLETCR') WITH 'S'

