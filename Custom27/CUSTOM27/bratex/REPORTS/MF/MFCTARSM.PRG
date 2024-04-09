*:***************************************************************************
*: Program file  : MFCTARSM (C102699,1)
*: Program desc. : Report for cutting tickes information from Aria to Simtex
*: For Report    : MFCTARSM.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Manufactoring
*: Developer     : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MFCTARSM
*:***************************************************************************

SELECT SIMTEX
SET RELATION TO CUTTKT INTO CUTTKTH ADDITIVE
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
SET FILTER TO &lcRpExp.
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET FILTER TO
  SET RELATION TO
  RETURN
ENDIF

DO gfDispRe WITH 'MFCTARSM'

SET FILTER TO
SET RELATION TO

*!*************************************************************
*! Name      : lfSROrder
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/22/2002
*! Purpose   : Rise change order flag, in range browse screen.
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
*! Example   : =lfSROrder()
*!*************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm

*-- Set Relation with customer file.
DO CASE
  CASE lcParm = 'S'
    GO TOP IN ORDHDR
  
  CASE lcParm = 'R'
ENDCASE
*-- end of lfsChOrder.