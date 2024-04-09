*:****************************************************************************************
*: Name      : Main Program for Visual Foxpro modules
*: Developer : Waleed Hamed 
*: Date      : 12/10/2008
*: Purpose   : Main program for EDI Triggers
******************************************************************************************
*: Return      : None
******************************************************************************************
*: Program desc. : Main Program.
*:****************************************************************************************
*C201085,1 WLD 12/10/2008 REPLACE THE ORDHDR.cOrderCat WITH THE MCUST_DIV
*:****************************************************************************************
Parameter lcEvntFun,lcFunPars
lcFunPars  = Iif(Type('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+Alltrim(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = Eval(lcFunToRun)

Return llRetValue
*-- End of Program.

*:****************************************************************************************
*! Name      : LFCSTDIV
*! Developer : Walid Hamed (WLD)
*! Date      : 12/10/2008
*! Purpose   : REPLACE THE ORDHDR.cOrderCat WITH THE MCUST_DIV
*:****************************************************************************************
Function lfCstDiv
  Replace cOrderCat With MCUST_DIV
Endfunc
