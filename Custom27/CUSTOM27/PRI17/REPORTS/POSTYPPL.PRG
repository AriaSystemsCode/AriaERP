****************************************************************************
*: Program file      : POSTYPPL  (C#101883)
*: Program desc.     : PRINT STYLE PURCHASE ORDER FOR P.Lablels.
*: System            : Aria Apparel System (A27).
*: Module            : Style Purchase Order (PO)
*: Developer         : Ahmed Mohamed El_Anwar(AME)
*: Date              : 06/20/2000
*:**************************************************************************


=GfOpenFile (gcDataDir+'Fabric',gcDataDir+'Fabric','SH')
SELECT STYLE
SET RELATION TO Fabric INTO Fabric ADDITIVE
SELECT POSHDR

*!*************************************************************
*! Name      : lfGetFab
*! Developer : AHMED MOHAMMED El_Anwar
*! Date      : 06/22/2000
*! Purpose   : Valid function of the print cost sheet setting on the option
*!             grid to enable or disable the setting of print cost on cost sheet.
*! Reference : *C101883,1 AME
*!*************************************************************
*! Called from : Report.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetFab()
*!*************************************************************
*C101883
FUNCTION  lfGetFab
PARAMETERS lcDummy
lcFab = Fabric.fABRIC
lcCont = Fabric.CONTENT
lcDesc = Fabric.Desc

RETURN ''
