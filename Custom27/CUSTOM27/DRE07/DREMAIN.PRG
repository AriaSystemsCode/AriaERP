*!**************************************************************************
*! Name      : DREMAIN.PRG
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/09/2003
*! Purpose   : Dream Apparel Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C102684,1
*!**************************************************************************

PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!**************************************************************************
*! Name      : lfvCstAcct
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/09/2003
*! Purpose   : Validate the customer field in the PO Revision screen of MAPRCAM
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! 102780
FUNCTION lfvCstAcct
PARAMETERS lcRetVal

lcRetVal = .T.
PRIVATE llOpenCust,lcMatAcct,llObjRet,lnAlias
STORE .F. TO llOpenCust
lcVar = SYS(18)
IF TYPE('LAUSRFIELD') = "U"
  RETURN
ENDIF
lnCustPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CMATACCT'),1)
IF !(lcVar == "LAOGFXFLT(" + ALLTRIM(STR(lnCustPos)) + ",6)")
  RETURN
ENDIF
lcCustAcct = EVALUATE(lcVar)
lnAlias = SELECT(0)
IF !USED('CUSTOMER')
  = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
  llOpenCust = .T.
ENDIF
IF !EMPTY(lcCustAcct) .AND. !SEEK('M' + lcCustAcct, 'CUSTOMER')
  lcMatAcct  = lcCustAcct
  llObjRet   = CusBrowM(@lcMatAcct , '' , 'M')
  lcMatAcct  = IIF(llObjRet , lcMatAcct , '')
  lcCustAcct = lcMatAcct
  laUsrFields[lnCustPos,6] = lcCustAcct
  laOGFxFlt[lnCustPos,6] = lcCustAcct
ENDIF
IF llOpenCust
  USE IN CUSTOMER
ENDIF
SELECT (lnAlias)
RETURN lcRetVal

*!**************************************************************************
*! Name      : lfMatAcct
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/02/2003
*! Purpose   : Add customer field to the generate M.PO. screen in MAMATRQ
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102819
FUNCTION lfMatAcct

lcFields = lcFields + ",cMatAcct :P='@! XXXXX' :H='Customer' "+;
                      ":V=gfDoTriger('MAMATRQ',PADR('VMATACCT',10))"

*!**************************************************************************
*! Name      : lfvMatAcct
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/02/2003
*! Purpose   : Validate the customer field in the generate M.PO. screen of MAMATRQ
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102819
FUNCTION lfvMatAcct

PRIVATE llOpenCust,lcMatAcct,llObjRet
STORE .F. TO llOpenCust
IF !USED('CUSTOMER')
  = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
  llOpenCust = .T.
ENDIF
SELECT (lcPOTmpHD)
IF '?' $ CMATACCT .OR. (!EMPTY(CMATACCT) .AND. !SEEK('M' + CMATACCT , 'CUSTOMER'))
  lcMatAcct = CMATACCT
  llObjRet  = CusBrowM(@lcMatAcct , '' , 'M')
  SELECT (lcPOTmpHD)
  lcMatAcct = IIF(llObjRet , lcMatAcct , '')
  REPLACE CMATACCT WITH lcMatAcct
ENDIF
IF llOpenCust
  USE IN CUSTOMER
ENDIF