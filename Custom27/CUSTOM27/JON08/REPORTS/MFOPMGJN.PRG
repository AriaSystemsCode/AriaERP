*:***************************************************************************
*: Program file  : MFOPMGJN
*: Program desc. : Custom Operations Management Report for Jonna
*: For Report    : MFOPMGJN.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Modules       : MF,PO,MA
*: Developer     : AHMED MAHER (AMH)
*: Date          : 04/04/2002
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfvOMsg
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MFOPMGJN
*:***************************************************************************

*!*************************************************************
*! Name      : lfvOMsg
*! Developer : AHMED MAHER
*! Date      : 04/07/2002
*! Purpose   : To Open the Optional message screen
*!*************************************************************
*! Called from : MFOPMGIN.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOMsg()
*!*************************************************************
FUNCTION lfvOMsg
PARAMETERS lcDumm

lcDumm = .T.
PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcOMsg1'   && 1st. line Variable
laOptMsg[1,2] = 75          && Line length
laOptMsg[2,1] = 'lcOMsg2'   && 1st. line Variable
laOptMsg[2,2] = 75          && Line length
laOptMsg[3,1] = 'lcOMsg3'   && 1st. line Variable
laOptMsg[3,2] = 75          && Line length

= gfOptMsg('laOptMsg')      && Call Function to write optional message.
RETURN .T.