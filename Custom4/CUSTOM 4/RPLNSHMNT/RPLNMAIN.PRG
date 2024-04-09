*!**************************************************************************
*! Name      : RPLNMAIN.PRG
*! Developer : SAB (Saber A.Razek)
*! Date      : 09/21/2011
*! Purpose   : Replenishment Custom Process Program.
*!  C201389 ==> for Aria4  attachments
*!  C201390 ==> for Aria27 attachments
*! Ticket id T20110621.0044
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       
*!**************************************************************************
* Modifications
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*:********************************************************************************
*:* Name        : lfOPNCNCSSN
*:* Developer   : SAB - Saber A.Razek
*:* Date        : 09/21/2011
*:* Purpose     : Define a bar in customer screen to open concession setup screen
*:********************************************************************************
FUNCTION lfOPNCNCSSN  
*SET STEP ON 
  LOCAL lcHostFormName,lnBarNo
  lnBarNo = CNTBAR('_INQURYPOP') + 1
  lcHostFormName = '[' + loFormSet.cHostFormName + ']'
  DEFINE BAR lnBarNo OF _INQURYPOP PROMPT "Concession Setup" ;
                SKIP FOR gfFormIsActive(&lcHostFormName) AND ;
                         (_Screen.ActiveForm.Parent.ActiveMode = 'S' .OR. ;
                         EMPTY(_Screen.ActiveForm.Parent.AriaForm1.keyStore.keytextbox.Value))
  ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfDspCnsSetupScr
ENDFUNC

*:********************************************************************************
*:* Name        : lfDspCnsSetupScr
*:* Developer   : SAB - Saber A.Razek
*:* Date        : 09/21/2011
*:* Purpose     : Call Concession Setup Screen
*:********************************************************************************
FUNCTION lfDspCnsSetupScr
  lcExeKeyValue = "'"+_Screen.ActiveForm.Parent.AriaForm1.keyAccount.keytextbox.Value+"','"+;
                      _Screen.ActiveForm.Parent.AriaForm1.keyStore.keytextbox.Value+"'"
  oAriaApplication.DoProgram("AWRCNCSSNS",lcExeKeyValue,.F.,'IC')
ENDFUNC