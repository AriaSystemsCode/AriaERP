PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

FUNCTION lfDspCmpNm
lcCompName = "NORTH BAY APPAREL"
IF UPPER(ALLTRIM(SYCASNHD.Crep_name)) ="EBUCCME2"
   lcReport=oAriaApplication.ReportHome+"AS\"+"EBUCCME3.FRX"
ENDIF
Return