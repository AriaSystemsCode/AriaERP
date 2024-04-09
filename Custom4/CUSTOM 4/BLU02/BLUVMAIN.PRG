*:****************************************************************************************
*: Program desc. : Main Program.
*:****************************************************************************************
*C200972,1 HIA 01-APR-2008 RETREIVE user defind fileds "CPRODDESC1", "CPRODDESC2" 
*:****************************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.
*:****************************************************************************************
*! Name      : lfADJFILE
*! Developer : Hassan Ibrahim Ali (HIA)
*! Date      : 01-APR-2008
*! Purpose   : RETREIVE user defind fileds "CPRODDESC1", "CPRODDESC2" 
*:****************************************************************************************
*! Called from : Print method of screen EBTKTPRC
*:****************************************************************************************
Function lfADJFILE

 SELECT (lccTempName) 
 ALTER table (lccTempName) add COLUMN CPRODDESC1 C(35)
 ALTER table (lccTempName) add COLUMN CPRODDESC2 C(35)

ENDFUNC
*:****************************************************************************************
*! Name      : lfFillFile
*! Developer : Hassan Ibrahim Ali (HIA)
*! Date      : 01-APR-2008
*! Purpose   : RETREIVE user defind fileds "CPRODDESC1", "CPRODDESC2" 
*:****************************************************************************************
*! Called from : Print method of screen EBTKTPRC
*:****************************************************************************************
Function lfFillFile

 SELECT (lccTempName) 
 Replace cProdDesc1 WITH ALLTRIM(Style.cProdDesc1)
 Replace cProdDesc2 WITH ALLTRIM(Style.cProdDesc2)

ENDFUNC