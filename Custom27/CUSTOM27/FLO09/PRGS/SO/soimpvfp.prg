*!**********************************************************************************************************
*! Program file  : Soimpvfp.PRG 
*! Program desc. : Import an excel sheet to a foxpro DBF file
*! System        : ARIA APPAREL SYSTEM 2.7
*! Module        : SO
*! Developer     : NADER NABIL - NNA
*! Date          : 01/12/2005
*! Purpose       : Fox2.6 deals only with Excel4 that hold only about 16000 rows with data so we import from 
*!               : VFP6 that deal with Excel97 that hold about 65000 row with data
*! REF.          : C#128703,1
*!**********************************************************************************************************
*! Passed Parameters  : Memo file path and name that holds variables that passed from Foxpro2.6 program
*!**********************************************************************************************************
*! Example       : DO Soimpvfp.PRG
*!**********************************************************************************************************
*! Modifications :
*!**********************************************************************************************************
LPARAMETERS lcMemoFile
PRIVATE lcCentury , lcImported
SET SAFETY OFF
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
IMPORT FROM (lcPathName) TYPE XLS
lcImported = ALIAS()
COPY TO (gcWorkDir + lcXls) FOX2X
IF USED(lcImported)
  USE IN (lcImported)
  ERASE (gcWorkDir+lcImported+'.DBF')
  ERASE (gcWorkDir+lcImported+'.CDX')
ENDIF
close all
ERASE (lcMemoFile)
SET CENTURY &lcCentury
*!***************************************************************************
*! Name      : lPError
*! Developer : NADER NABIL (NNA)
*! Date      : 01/18/2005
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
