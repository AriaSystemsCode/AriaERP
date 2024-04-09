*:**************************************************************************
*: Program file  : SOREVU10
*: Program desc. : Custom Bookings Analysis Report for Revue.
*: System        : ARIA 4XP.
*: Module        : Sales Order (SO)
*: Developer     : Mostafa Eid (MOS)
*: Date          : 07/13/2008
*: Reference     : C201039
*:**************************************************************************
*: C201039,2 MMT 11/30/2008 Convert report tp work from request builder[T20080422.0025]
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[T20090727.0031]
*!**************************************************************************

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF oAriaApplication.MULTIINST 
  SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\SO\sorev10.FXP ADDITIVE
  DO X:\ARIA4XP\SRVRPTS\SO\sorev10.FXP  
  
ELSE
  lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
  DO lcSrvRpt+"SO\sorev10.FXP" WITH .F.,.F.
ENDIF   
RETURN 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
FUNCTION lfwRepWhen
lnAcctPos  = lfItmPos('CUSTOMER.ACCOUNT') && get Account Fixed filter Position.
lnStylePos = lfItmPos('STYLE.CSTYMAJOR')  && get Style Group Fixed filter Position.
lnClassPos = lfItmPos('CUSTOMER.CLASS')   && get Class filter Position.
lnDivsnPos = lfItmPos('ORDHDR.CDIVISION') && get Season filter Position.
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : To get the position of the fixed filter in OG.
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
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
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
ENDCASE
*-- End of lfsrAcc.



*-- End of lpCreatFil.
*!**************************************************************************
*! Name      : lfEvalSegs
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Evaluate NonMajor Type and variables.
*!**************************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!**************************************************************************
*! Example     : = lfEvalSegs()
*!**************************************************************************
*
FUNCTION lfEvalSegs

*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.

*-- Compute Free/Color Items in Style Structure. [Begin]
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

*-- Loop Around Non Major elements.
FOR lnI = 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnI,1] = "C"            && Color
    lnClrPo    = laMajSeg[lnI,4]
    lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
    lnColorLen = LEN(laMajSeg[lnI,3])
  ENDIF
ENDFOR    
RETURN ''
*-- End of lfEvalSegs.

*!**************************************************************************
*! Name      : lfsrAcc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
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

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrvSty()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT Style
    SET ORDER TO TAG Cstyle
    SET RELATION TO Style.Style INTO STYLE_X
    GO TOP IN Style
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT Style
    SET ORDER TO TAG Style
ENDCASE
*-- End of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Sum a specific field for the current style in style file
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Returns   : Calculated field value.
*!**************************************************************************
*! Example   : =lfStySum()
*!**************************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)

*-- End of lfStySum.
