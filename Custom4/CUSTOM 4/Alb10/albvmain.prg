*:****************************************************************************************
*: Name      : Main Program for Visual Foxpro modules
*: Developer : Waleed Hamed Zekr Allah
*: Date      : 08/20/2008
*: Purpose   : Main program for EDI Triggers
******************************************************************************************
*: Return      : None
******************************************************************************************
*: Program desc. : Main Program.
*:****************************************************************************************
*C201045,1 WLD Update 864 while processing in case of Text Message
*:****************************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:****************************************************************************************
*! Name      : lfUpdTXT
*! Developer : Walid Hamed  (WLD)
*! Date      : 08/20/2008
*! Purpose   : Update 864 while processing in case of Text Message
*:****************************************************************************************
*! Called from : Class "EdiProcessTX"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfUpdTXT

  if ALLTRIM(EDILIBDT.cEDIRef) = 'Text Message'
      =oObj.viewtransaction() 
    REPLACE cStatus WITH 'U' IN EDILIBDT
    REPLACE nProcTran WITH nProcTran + 1 IN 'EDILIBHD'
  ENDIF 

ENDFUNC  
