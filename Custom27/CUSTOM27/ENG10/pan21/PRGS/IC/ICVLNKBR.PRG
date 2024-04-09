*!**********************************************************************************************************
*! Program file  : IcVlnkBr.PRG 
*! Program desc. : Save Exported File With Long names
*! System        : ARIA APPAREL SYSTEM 2.7
*! Module        : IC
*! Developer     : NADER NABIL - NNA
*! Date          : 03/14/2006
*! Purpose       : This program nominate a file with long name like (Stock20060314013025.txt)
*! REF.          : C#130448,1
*!**********************************************************************************************************
*! Passed Parameters  : None
*!**********************************************************************************************************
*! Example       : DO IcVlnkBr
*!**********************************************************************************************************
*! Modifications :
*!**********************************************************************************************************
LPARAMETERS lcMemoFile
PRIVATE lcCentury
STORE '' TO lcCentury
*--Hold the current Set Century
lcCentury = SET('CENTURY')
SET CENTURY ON

ON ERROR DO lPError WITH ERROR(), MESSAGE(), LINENO()
IF TYPE('lcMemoFile')#'C'
	RETURN
ENDIF
*--Restore Variables that was used in Report IcfreStu.prg
RESTORE FROM (lcMemoFile) ADDITIVE
RENAME &lcOldFile TO &lcNewFile

ERASE (lcMemoFile)
SET CENTURY &lcCentury

*!***************************************************************************
*! Name      : lPError
*! Developer : NADER NABIL (NNA)
*! Date      : 03/14/2006
*! Purpose   : Quit if any error
*!***************************************************************************
*! Called from : ARPINVNC
*!***************************************************************************
*! Example     : =lPError()
*!***************************************************************************
PROCEDURE lPError
PARAMETERS lnError,lcMsg,lnLine
MESSAGEBOX('Error#:'+STR(lnError)+'   '+lcMsg+'   ,'+STR(lnLine))
READ EVENTS
*--End of lPError.
