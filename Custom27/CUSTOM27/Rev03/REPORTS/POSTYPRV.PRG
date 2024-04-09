*:***************************************************************************
*: Program file  : POSTYPRV
*: Program desc. : Print PO for Revue
*: For Report    : POSTYPRV.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO
*: Developer     : AHMED MAHER (AMH)
*: Reference     : C102428,1
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************

*!*************************************************************
*! Name      : lfGetRlFld
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/16/2001
*! Purpose   : to get the Related fields of HTS code.
*!*************************************************************
*! Called from : POSTYPRV.FRX
*!*************************************************************
*! Calls       : gfRltFld()
*!*************************************************************
FUNCTION lfGetRlFld
PARAMETERS lcReturn

DIMENSION laQuotaCat[1,2]
DIMENSION laDutyRate[1,2]

laQuotaCat[1,1] = 'CCOTA1'
laQuotaCat[1,2] = 'lcQuotaCat'
laDutyRate[1,1] = 'NDUTY1'
laDutyRate[1,2] = 'lnDutyRate'
=gfRltFld(POSHDR.CHTS, @laQuotaCat , 'CHTS')
=gfRltFld(POSHDR.CHTS, @laDutyRate , 'CHTS')
RETURN ''
*--end of lfGetRlFld.