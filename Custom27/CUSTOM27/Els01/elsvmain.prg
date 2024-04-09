*:****************************************************************************************
*: Program desc. : Main Program.
*:****************************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:****************************************************************************************
*! Name      : lfPRTACK
*! Developer : Hassan Ali (HIA)
*! Date      : 07/25/2002
*! Purpose   : Just to disable the message of acknoledgement.
*:****************************************************************************************
*! Called from : Class "ProcessPo"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfPRTACK
*Just to disable the message of acknoledgement.
EndFunc
*:****************************************************************************************