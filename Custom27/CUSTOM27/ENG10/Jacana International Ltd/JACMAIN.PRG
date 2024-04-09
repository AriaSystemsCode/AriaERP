*:****************************************************************************************
*: Program file  : JACMAIN.PRG
*: Program desc. : Main Program.
*: System        : Aria Apparel System (A27).
*: Developer     : BASSEM RAFAAT ERNEST-(BWA) Due to C#200428
*: Date          : 11/12/2002
*: Purpose       : JAC Custom Process Program .
*:****************************************************************************************
*: Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*:             lcFunPars -> Process function parameters, sent as a string.
*:****************************************************************************************
*: Returns   : Logical value.       
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.
*:**************************************************************************
*: Name          : lfSENORD
*: Developer     : BASSEM RAFAAT ERNEST-(BWA)
*: Date          : 02/20/2001
*: Purpose       : 
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : =lfSENORD()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#200428
*:**************************************************************************
FUNCTION lfSENORD

IF lcRpSnd # "A"
  lcRpExp = lcRpExp + [ AND ] + IIF(lcRpSnd = "Y" , [SENDORD = "Y"] , [SENDORD = "N"])
ENDIF

*-- End OF lfSENORD
