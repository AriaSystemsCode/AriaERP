*:***************************************************************************
*: Program file  : RmrAutth.PRG
*: Program desc. : Return Authorization for The nate nast
*: System        : Aria 4 XP
*: Module        : Return Merchandise (RM)
*: Developer     : Mariam Mazhar(MMT)
*: Issue #       : C201073 [T20080610.0012]
*:***************************************************************************
*: Calls : 
*:    Procedures : 
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO RmrAut
*:***************************************************************************
*: Modifications:
*:***********************************************************************************************


*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : 	Mariam Mazhar(MMT)
*! Date      :  11/23/2008
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOptMsg()
*!*************************************************************
FUNCTION lfvOptMg
PARAMETER lcParam

lcCurrAlis = ALIAS()

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 2nd. line Variable

laOptMsg[1,2] = 75                && Line length

IF EMPTY(lcRpMsg1) .AND. EMPTY(lcRpMsg2) .AND. EMPTY(lcRpMsg3)

  IF FILE(gcDataDir+lcTypInv+'.MEM')
    RESTORE FROM gcDataDir+lcTypInv+'.MEM' ADDITIVE
  ENDIF

  =gfOptMsg('laOptMsg')

ELSE
  =gfOptMsg('laOptMsg')
ENDIF

SET MEMOWIDTH TO 75              && the length of the memo field.
SAVE TO gcDataDir+lcTypInv+'.MEM' ALL LIKE lcRpMsg*

SELECT (lcCurrAlis)

lcParam = .T.

RETURN lcParam


