*!**************************************************************************
*! Name      : INTSGMAN.PRG
*! Developer : BASSEM RAFAAT
*! SuperVisor: ASHRAF SHERIF
*! Date      : 08/10/2000
*! Purpose   : International Sourcing Group Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
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

*!*************************************************************
*! Name      : lfStylCost
*! Developer : BASSEM RAFAAT
*! SuperVisor: ASHRAF SHERIF
*! Date      : 08/10/2000
*! Purpose   : Flage for International Sourcing Group.
*!*************************************************************
*! Called from  : INTSGMAN.PRG
*! Program name : ARGROSSP.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfStylCost
*!*************************************************************
FUNCTION lfStylCost

llIntrSG = .T.
                   *-- End function lfStylCost --*
                  
