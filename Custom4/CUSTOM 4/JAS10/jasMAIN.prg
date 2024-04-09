*!**************************************************************************
*! Name      : JASMAIN.PRG
*! Developer : Mariam Mazhar[MMT]
*! Date      : 02/14/2011
*! Purpose   : JASCO Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C201304.122,C201308.exe,C201309.exe{T20100205.0005}
*!**************************************************************************
*! Modifications
*! C201459,1 MMT 02/01/2012 Add Triggers to Carrier Integration screen to change Ship From Name[T20100811.0006]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*!**************************************************************************
*! Name      : LFSUMORDCH
*! Developer : Mariam Mazhar[MMT]
*! Date      : 02/14/2011
*! Purpose   : Sum Order charges
*!**************************************************************************
FUNCTION LFSUMORDCH
lnSelAlias = SELECT()
IF !USED('ORDERCHG')
  =gfOpenTable('ORDERCHG','ORDERCHG')
ELSE
  SELECT ORDERCHG
  gfSetOrder('ORDERCHG')  
ENDIF
SELECT ORDERCHG
lnTotalChrge = 0
lcInvHead = loFormSet.ariaForm1.ariapageframe1.page4.cntInvoicesummary.InvoiceHeaderFile
lcOrderNum = EVALUATE(lcInvHead +'.Order')
IF gfSeek('O'+lcOrderNum)
  SELECT ORDERCHG
  SUM nchrgamnt TO lnTotalChrge  REST WHILE CORDTYPE+ORDER+STR(LINENO,6)+CCHRGCODE= 'O'+lcOrderNum
ENDIF
REPLACE TotalChg  WITH TotalChg +lnTotalChrge  ,;
				Freight   WITH Freight+lnTotalChrge  ,;
				Cod_Amt   WITH Cod_Amt+lnTotalChrge IN (lcInvHead)  
SELECT(lnSelAlias)				
*! C201459,1 MMT 02/01/2012 Add Triggers to Carrier Integration screen to change Ship From Name[T20100811.0006][Start]
*!**************************************************************************
*! Name      : lfUPDSHPFRM
*! Developer : Mariam Mazhar[MMT]
*! Date      : 02/02/2012
*! Purpose   : Update Ship from Name
*!**************************************************************************
FUNCTION lfUPDSHPFRM
m.SHIP_FROM_NAME = Customer.DBA
*! C201459,1 MMT 02/01/2012 Add Triggers to Carrier Integration screen to change Ship From Name[T20100811.0006][End]