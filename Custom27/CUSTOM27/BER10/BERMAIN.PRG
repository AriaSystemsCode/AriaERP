*!**************************************************************************
*! Name      : BERMAIN.PRG
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/24/2000
*! Purpose   : Berkley Shirts Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C101796,1
*!**************************************************************************
*
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!**************************************************************************
*! Name      : lfCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/25/2000
*! Purpose   : Create the modified Label file for BERKLEY SHIRTS
*!**************************************************************************
*
FUNCTION lfCreatFil
IF RIGHT(lcFormName,2) = "BR"

  *-- Creating temp file which is used to save data from Label Info screen
  CREATE TABLE (gcWorkDir+lclblTbl) (Invoice C(6),Labels N(6),nFrom N(4),;
                                     nTo N(4),Style C(19),TotQty N(6),;
                                     Note1 C(30),Note2 C(30),;
                                     Vendor C(10),Sku C(10) )             
  INDEX ON Invoice TAG (lclblTbl)

  CREATE CURSOR (lclblInfo) (Invoice C(6),Labels N(6),nLblsNo N(4),nFrom N(4),;
                             nTo N(4),Style C(19),TotQty N(06),;
                             Note1 C(30),Note2 C(30),Vendor C(10),Sku C(10) )             
  INDEX ON Invoice + STR(nLblsNo,4) TAG (lclblInfo)
  ZAP
ENDIF  
*-- End of lfCreatFil.

*!**************************************************************************
*! Name      : lfShowLbls
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/23/2000
*! Purpose   : Display Custom Label Screen
*!**************************************************************************
*
FUNCTION lfShowLbls
IF RIGHT(lcFormName,2) = "BR"
  lnCAlias = SELECT(0)
  lnFrom = 1                
  SELECT (lclblTbl)
  REPLACE nFrom WITH lnFrom 

  lnTo   = lnlabels
  lnQty  = 0
  STORE '' TO lcStyle,lcNote1,lcNote2,lcVendor,lcSku
  SELECT (lnCAlias)
  DO (gcAppHome + 'SOSHLBB0')
ENDIF
*-- End of lfShowLbls.
