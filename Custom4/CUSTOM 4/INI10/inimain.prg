*!**************************************************************************
*! Name      : INIMAIN.PRG
*! Developer : Tarek Mohammed Ibraheem
*! Date      : 05/29/2011
*! Purpose   : Intimelli Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       *C201350,1 TMI 05/29/2011
*!**************************************************************************
*! Modifications
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

************************************************************************************
* Name       : lfALWDIVEDIT
* Developer  : Tarek Mohammed Ibrahim
* Date       : 05/29/2011
* Tracking # : *C201350,1 TMI 
* Purpose    : Allow Division edit in the Temp sales order screen
************************************************************************************
FUNCTION lfALWDIVEDIT
loFormSet.ARIAFORM1.Ariapageframe1.Page1.cboDivision.Enabled = .T.
*- end of FUNCTION lfALWDIVEDIT

************************************************************************************
* Name       : lfALWDIVEDIT
* Developer  : Tarek Mohammed Ibrahim
* Date       : 05/29/2011
* Tracking # : *C201350,1 TMI 
* Purpose    : Validate in the saving process that the header's divison meets the detials' 
************************************************************************************
FUNCTION lfSVCHCKDIV

LOCAL lnSlct,lcDiv,lcKey,llRet,lcSty
llRet = .F.
IF loFormSet.lcOrdType = 'T'
  lnSlct = SELECT(0)
  lcDiv = loFormSet.ARIAFORM1.Ariapageframe1.Page1.cboDivision.Value
  lcOrdLine = loFormSet.oFormEnvironment.lcOrdLine
  SELECT &lcOrdLine
  lcKey = EVAL(KEY())
  SET RELATION TO STYLE INTO STYLE
  LOCATE
  LOCATE FOR lcDiv <> STYLE.CDIVISION 
  llRet = FOUND()
  lcSty = STYLE
  SET RELATION OFF INTO STYLE
  IF llRet
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Some styles have different division than the header, starting with the style &lcSty, cannot proceed')
  ENDIF
  SELECT (lnSlct)
ENDIF  
RETURN llRet
*- end of FUNCTION lfSVCHCKDIV