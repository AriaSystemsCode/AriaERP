*:***************************************************************************
*: Program file  : SORVURG.PRG 
*: Program desc. : Daily Order Register (for Revue)
*: For Report    : (SOREVRG.FRX)
*: System        : Aria Advantage Series.4XP
*: Module        : Sales Order (SO)
*: Developer     : Mariam Mazhar[MMT]
*: Date          : 10/21/2008
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: ..C201021,1
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[T20090727.0031]
*:***************************************************************************

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]

IF oAriaApplication.MULTIINST 
  SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\SO\sorevrg.FXP ADDITIVE
  DO X:\ARIA4XP\SRVRPTS\SO\sorevrg.FXP  
  
ELSE
  lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
  DO lcSrvRpt+"SO\sorevrg.FXP" WITH .F.,.F.
ENDIF   
RETURN 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

loogScroll.cCROrientation = 'L'
lcStyTitle = gfItemMask('HI')
*!*************************************************************
*! Name      : lfvCompany
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/21/2008
*! Purpose   : - Call Companies mover function 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover,lfCmpExpr
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvCompany()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************

FUNCTION lfvCompany
= lfOGMover(@laRpSorCmp,@laRpTarCmp,'Select Company',.T.,'')
loogscroll.llOGFltCh = .T.
= lfCmpExpr()
*-- end of lfvCompany.

*!**************************************************************************
*! Name      : lfwRepWhen
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/21/2008
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen

*-- Define companies array that be used in company mover
IF EMPTY(laRpCmpCod)
  DECLARE laRpCmpCod[1,3]
  STORE '' TO lcRpCmpExp
  *-- Collect all companies
  SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mModlSet ;
    FROM SYCCOMP                            ;
    INTO ARRAY laRpCmpCod                   ;
    ORDER BY 1
  DECLARE laRpSorCmp[ALEN(laRpCmpCod,1),1],laRpTarCmp[ALEN(laRpCmpCod,1),1]
  FOR lnI = 1 TO ALEN(laRpCmpCod,1)
    STORE laRpCmpCod[lnI,1] TO laRpSorCmp[lnI,1],laRpTarCmp[lnI,1]
  ENDFOR
  =lfCmpExpr()
  *--
  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ENTERED'),1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(DATE())+'|'+DTOC(DATE())
  ENDIF
ELSE
  IF llNComp .AND. ALEN(laRpTarCmp,1) = 1
    llNComp = .F.
    CLEARREAD()
  ENDIF
ENDIF

=gfOpenFile(oAriaApplication.SysPath+'SYUUSER', 'CUSER_ID','SH')
*
*-- End of lfwRepWhen.


*!**************************************************************************
*! Name      : lfwOldVal
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/21/2008
*! Purpose   : To save the old value.
*!**************************************************************************
*! Example   : = lfwOldVal()
*!**************************************************************************
*
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))


*!***************************************************************************
*! Name      : lfCmpExpr
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/21/2008
*! Purpose   : - Evaluate Company expression.
*!***************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!***************************************************************************
*! Called from : lfvCategory(),lfvCompany(),lfwRepWhen()
*!***************************************************************************
*! Passed Parameters  : ....
*!***************************************************************************
*! Returns            : ....
*!***************************************************************************
*! Example   : = lfCmpExpr()
*!***************************************************************************
*E301294,1  AKA 08/23/99  
*!***************************************************************************

FUNCTION lfCmpExpr
PRIVATE laTarget

IF EMPTY(laRpTarCmp)
  = ACOPY(laRpSorCmp,laTarget)
ELSE
  = ACOPY(laRpTarCmp,laTarget)
ENDIF
= ASORT(laTarget)
lcOldComp = lcRpCmpExp
lcRpCmpExp = ''
FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpCmpExp = IIF(EMPTY(lcRpCmpExp),PADR(laTarget[lnI],2),;
                    lcRpCmpExp + ','+PADR(laTarget[lnI],2))
ENDFOR

IF LEN(lcRpCmpExp) > 2
  llNComp = .T.
ELSE
  llNComp = .F.
ENDIF
CLEARREAD()
*-- end of lfCmpExpr.



*!************************************************************
*! Name      : RefreshStatus
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/21/2008
*! Purpose   : function to refresh the status option
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
FUNCTION RefreshStatus
LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarCmp)
    FOR lnTarget = 1 TO ALEN(laRpTarCmp,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarCmp[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 