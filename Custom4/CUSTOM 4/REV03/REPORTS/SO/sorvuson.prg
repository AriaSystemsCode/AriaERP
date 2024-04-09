*:***************************************************************************
*: Program file  : sorvuson.PRG
*: Program desc. : Custom Season to Season CustomerAnalysis 
*: Date          : 10/27/2008
*: System        : Aria Advantage Series.4XP
*: Module        : Sales order (SO)
*: Developer     : Mariam Mazhar[MMT]
*: Tracking Job Number: C201060[T20080422.0034]
*:***************************************************************************
*: C201060,1 Convert Report to Aria4 and use request Handler[T20080422.0034]
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[T20090727.0031]
*:***************************************************************************

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF oAriaApplication.MULTIINST 
  SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\SO\sorevson.FXP ADDITIVE
  DO X:\ARIA4XP\SRVRPTS\SO\sorevson.FXP  
  
ELSE
  lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
  DO lcSrvRpt+"SO\sorevson.FXP" WITH .F.,.F.
ENDIF   
RETURN 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

*!**************************************************************************
*! Name      : lfwRepWhen
*: Developer : Mariam Mazhar (MMT)
*: Date      : 10/27/2001
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnAcctPos  = lfItmPos('CUSTOMER.ACCOUNT') && get Account Fixed filter Position.
lnSeasnPos = lfItmPos('ORDHDR.SEASON')    && get Class filter Position.
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*: Developer : Mohamed Shokry (MHM)
*: Date      : 10/27/2001
*! Purpose   : To get the position of the fixed filter in OG.
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lfsrAcc
*: Developer : Mohamed Shokry (MHM)
*: Date      : 10/27/2001
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
*
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
ENDCASE
*-- End of lfsrAcc.
