*!**************************************************************************
*! Name      : NEVMAIN.PRG
*! Developer : TAREK MOHAMED IBRAHIM
*! Date      : 09/04/2007
*! Purpose   : Nevica Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*! C200846 - t20070726.0003
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue


*:**************************************************************************
*:* Name        : lfvChkNam
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/04/2007
*:* Purpose     : Valid function to check that the field entered is one of the list CDESC01,...,CDESC20
*:***************************************************************************
*:* Called from : CUSTOMER.SCX , the custom field valid function of CUSTOMER.CSTYDESC
*:***************************************************************************
FUNCTION lfvChkNam
PARAMETER llReturn
llReturn = .T.

PRIVATE lnFldPos,lnLine,lcFldList
lcFldList='CDESC01|CDESC02|CDESC03|CDESC04|CDESC05|CDESC06|CDESC07|CDESC08|CDESC09|CDESC10|'+;
          'CDESC11|CDESC12|CDESC13|CDESC14|CDESC15|CDESC16|CDESC17|CDESC18|CDESC19|CDESC20|'
IF TYPE('laOgFxFlt') <> 'U'          
  lnFldPos = ASCAN(laOgFxFlt,'CSTYDESC')
  IF lnFldPos>0
    lnLine = ASUBSCRIPT(laOgFxFlt,lnFldPos,1)
    IF !EMPTY(laOgFxFlt[lnLine,6]) .AND. !UPPER(PADR(ALLT(laOgFxFlt[lnLine,6]),7)) $ lcFldList
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Pls specify a field name from the list CDESC01,...,CDESC20')
      laOgFxFlt[lnLine,6] = ''
    ENDIF
  ENDIF
ENDIF

RETURN llReturn

*-- end of lfvChkNam.