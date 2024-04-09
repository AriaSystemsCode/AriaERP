*!**************************************************************************
*! Name      : UKMAIN.PRG               *C101683,1 
*! Developer : WAB Walid Abd El Wahab 
*! Date      : 12/02/1999
*! Purpose   : United Knitware Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
PARAMETER lcEvntFun

lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'()'
*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*!*************************************************************************
*! Name      : lfAddOption 
*! Developer : WAB  - WAlid Abd Wahab
*! Date      : 12/02/1999
*! Purpose   : add option to call screen recive by style
*!*************************************************************************
*! Program   : 'POSTREC'  -> Customer screen.
*! Event     : 'AddOption'-> add option to call screen recive by style.
*!*************************************************************************
*! Returns   : None.
*!*************************************************************************
FUNCTION lfAddOption
*--check if the option pad is already defined on the sysmenu
IF !lfFoundPad('Options')
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF
lnBarNo = CntBar('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Receive by Style' SKIP FOR (lcPType <>'M' OR (lcPType='M' AND (lnType<>1 OR EMPTY(laType[1]))))
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO gfDoTriger WITH "MFRCVCT",PADR("RECBYSTYL",10)
RETURN
*!*************************************************************
*! Name      : lfFoundPad
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/02/1999
*! Purpose   : check if any pad menu is exit in _sysmenu
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
FUNCTION lfFoundPad
PARAMETER lcPadName
PRIVATE llFound
llFound = .F.
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
	IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = lcPadName
        llfound = .T.
		EXIT
	ENDIF
ENDFOR
RETURN(llFOund)
*!*************************************************************************
*! Name      : lfRECBYSTYL
*! Developer : WAB  - WAlid Abd Wahab
*! Date      : 12/02/1999
*! Purpose   : call screen recive by style
*!*************************************************************************
*! Program   : 'POSTREC'  -> Customer screen.
*! Event     : 'RECBYSTYL'-> call screen recive by style.
*!*************************************************************************
*! Returns   : None.
*!*************************************************************************
FUNCTION lfRECBYSTYL
PRIVATE lnCurrAls
lnCurrAls = ALIAS()
*--logig field determin if the user recived styles
llFoundRec = .F.
DO (gcAppHome+gcWinAppl+'\MFRECST.FXP')
SELECT (lnCurrAls)
laScrMode=.F.
IF llFoundRec
  laScrMode[4]=.T.
ELSE
  laScrMode[1]=.T.
ENDIF
SHOW GETS
RETURN
