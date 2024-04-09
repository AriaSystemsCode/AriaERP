*!**************************************************************************
*! Name      : JetMAIN.PRG
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/06/2012
*! Purpose   : Jet10 Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C201304.122,C201308.exe,C201309.exe{T20100205.0005}
*!**************************************************************************
*! Modifications
*! B609908,1 MMT Add trgger to Carrier integration class to use Customer.DBA instead of Warhouse[T20120425.0017]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*!**************************************************************************
*! Name      : lfUPDSHPFRM
*! Developer : Mariam Mazhar[MMT]
*! Date      : 05/06/2012
*! Purpose   : Update Ship from Name
*!**************************************************************************
FUNCTION lfUPDSHPFRM
IF !EMPTY(Customer.DBA)
  m.SHIP_FROM_NAME = Customer.DBA
ELSE
  m.SHIP_FROM_NAME = Customer.BtName
ENDIF  
