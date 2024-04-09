*!**************************************************************************
*! Name      : RAMMAIN.PRG
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 10/17/2012
*! Purpose   : ENG\Rustin Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C201523,1
*!**************************************************************************
*! Modifications
*! C201523,1 HIA 10/17/2012 Add Contract field to MFG Order screen [T20110914.0019]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : lfMFORDC
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 10/17/2012
*! Purpose   : Add Contract Option to MFG Order screen [T20110914.0019]
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfMFORDC

  lnNewBarNo = CNTBAR('_OPTIONPOP') + 1
  DEFINE BAR lnNewBarNo OF _OPTIONPOP PROMPT "View Contractor"  SKIP FOR _Screen.ActiveForm.Parent.ActiveMode $ 'S'
  ON SELECTION BAR lnNewBarNo  OF _OPTIONPOP DO lfContr IN RAMMAIN.PRG 
   
ENDFUNC 

*-- end of lfMFORDC.

*!**************************************************************************
*! Name      : lfContr
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 10/17/2012
*! Purpose   : Add Contract Option to MFG Order screen [T20110914.0019]
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfContr

DO FORM (oAriaApplication.ScreenHome + 'MA\MAMFORDC.SCX') WITH _Screen.ActiveForm.Parent
ENDFUNC 

*-- end of lfContr.

*!**************************************************************************