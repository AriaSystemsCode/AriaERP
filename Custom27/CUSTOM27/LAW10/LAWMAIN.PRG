*:****************************************************************************************
*: Program file  : LAWMAIN.PRG
*: Program desc. : Main Program.
*: System        : Aria Apparel System (A27).
*: Developer     : Khalid Mohi El-Din
*: Date          : 07/08/2003
*: Purpose       : Lawrence Custom Process Program .
*:****************************************************************************************
*: Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*:             lcFunPars -> Process function parameters, sent as a string.
*:****************************************************************************************
*: Returns   : Logical value.       
*:****************************************************************************************
*C#200577,1
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************
*
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:*************************************************************
*: Name      : lfPRNTRUN
*: Developer : Khalid Mohi El-Din
*: Date      : 07/08/2003
*: Purpose   : To do not display the printer screen
*:*************************************************************
*: Example   : = lfPRNTRUN()
*:*************************************************************
FUNCTION lfPRNTRUN

IF TYPE("lcFormName") = "C" AND lcFormName $ "ALPKTKLS|ARPINVLS"
  RETURN .T.
ELSE
  RETURN .F.
ENDIF  