*:************************************************************************
*: Program file  : MAMLTCR.PRG
*: Program desc. : Letter Of Credit (From Material Module)
*: For screen    : POLETCR.SCX (0,1,2,3,4,4A,4B)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: MATERIAL MODULE.
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
*:   DO gpDoProg WITH 'AWRMAMLTCR' , .F. , 'MA'
*:************************************************************************
*:  Modifications :
*:************************************************************************
*
*-- Call the program with "M" parameter for Letter of Credit from 
*-- Material Module.
DO (gcAppHome+'\POLETCR') WITH 'M'

