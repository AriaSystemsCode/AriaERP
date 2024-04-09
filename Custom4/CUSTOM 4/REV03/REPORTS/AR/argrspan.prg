*:***************************************************************************
*: Program file  : ARGRSPAN.PRG
*: Program desc. : CUSTOMIZED GROSS PROFIT FOR REVUE.
*: Date          : 08/12/2008
*: System        : Aria Advantage Series.4XP
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Mariam Mazhar[MMT]
*: Tracking Job Number: C201040[T20080422.0026]
*:***************************************************************************
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Report When Function 
*!*************************************************************
FUNCTION lfwRepWhen
lnAcctPos  = lfItmPos('CUSTOMER.ACCOUNT') && get Account Fixed filter Position.
lnInvHdPos = lfItmPos('INVHDR.INVDATE')   && get Invoice date filter Position.
lnOrdHdPos = lfItmPos('ORDHDR.COMPLETE') && get Complete date filter Position.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : To get the position of the fixed filter in OG.
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************

FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(loogscroll.laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(loogscroll.laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.
*!*************************************************************
*! Name      : lfsAccount
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Set and Rest functions for account filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsAccount()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfsAccount
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SELECT Customer
  GO TOP
ENDIF

*!*************************************************************
*! Name      : lfsrSty
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Set and Rest functions for style filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRSty
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  GO TOP IN STYLE
ELSE  && Reset code
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDIF
*!*************************************************************
*! Name      : lfClrRead
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/12/2008
*! Purpose   : Function used to suppressing the field in the grid.
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfClrRead()
*!*************************************************************
*--This function called from lcRPSortBy , lcRPBasdOn.
FUNCTION lfClrRead
CLEARREAD()

*-- End of lfCreatTmp.
