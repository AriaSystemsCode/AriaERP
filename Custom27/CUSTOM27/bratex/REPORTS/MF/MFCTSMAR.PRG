*:***************************************************************************
*: Program file  : MFCTSMAR (C102717,1)
*: Program desc. : Report for cutting tickes information from Simtex to Aria
*: For Report    : MFCTSMAR.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Manufactoring
*: Developer     : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MFCTSMAR
*:***************************************************************************

SELECT CTKTRECV
SET RELATION TO CUTTKT INTO CUTTKTH ADDITIVE
SET RELATION TO STYLE  INTO STYLE   ADDITIVE
SET FILTER TO &lcRpExp.
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET FILTER TO
  SET RELATION TO
  RETURN
ENDIF

DO gfDispRe WITH 'MFCTSMAR'

SET FILTER TO
SET RELATION TO

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AMH-AHMED MAHER
*! Date      : 10/03/2002
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : AMH-AHMED MAHER
*! Date      : 10/03/2002
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AMH-AHMED MAHER
*! Date      : 10/21/2002
*! Purpose   : When function of O.G.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

IF !USED(lcTmpName)
  SELECT DISTINCT CFILE_NAM,DAUTSTGEN FROM CTKTRECV ORDER BY CFILE_NAM INTO CURSOR (lcTmpName)
ENDIF