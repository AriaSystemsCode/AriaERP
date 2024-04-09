*!**************************************************************************
*! Name      : FPCMAIN.PRG
*! Developer : TAK / RICK
*! Date      : 12/13/1999
*! Purpose   : Fresh Produce Custom Process Functions Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
****************************************************************************




*!***************************************************************
*! Name      : lfWHN_STR                            
*! Purpose   : When function for start date.
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'WHN_STR' -> When function for start date.
*!***************************************************************
*
FUNCTION lfWHN_STR

*-- Write your code.

RETURN



*!***************************************************************
*! Name      : lfVLD_STR                           
*! Purpose   : Valid function for start date.
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'VLD_STR' -> Valid function for start date.
*!***************************************************************
FUNCTION lfVLD_STR

*-- Write your code.

RETURN



*!***************************************************************
*! Name      : lfWHN_CMP                           
*! Purpose   : When function for complete date.
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'WHN_CMP' -> When function for complete date.
*!***************************************************************
*
FUNCTION lfWHN_CMP

*-- Write your code.

RETURN


*!***************************************************************
*! Name      : lfVLD_CMP                           
*! Purpose   : Valid function for complete date.
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'VLD_CMP' -> Valid function for complete date.
*!***************************************************************
*
FUNCTION lfVLD_CMP

*-- Write your code.

RETURN


*!***************************************************************
*! Name      : lfORD_SAV                           
*! Purpose   : Function to be executed upon saving
*!***************************************************************
*! Program   : 'SOORD'   -> Sales Orders screen.
*! Event     : 'ORD_SAV' -> Function to be executed upon saving.
*!***************************************************************
*
FUNCTION lfORD_SAV

*-- Write your code.

RETURN
