*:***************************************************************************
*: Program file  : MAMATLBL CP# 102737
*: Program desc. : Fabric Color Label
*: For Report    : MAMATLBL.FRX
*: System        : Aria Advantage Series.
*: Module        : Material (MA)
*: Developer     : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MAMATLBL
*:***************************************************************************

lcTempFile = gfTempName()
CREATE CURSOR (lcTempFile) (CFABRIC0 C(7),CCOLOR0 C(6),CFABDESC0 C(20),CCLRDESC0 C(30),;
                            CFABRIC1 C(7),CCOLOR1 C(6),CFABDESC1 C(20),CCLRDESC1 C(30),;
                            CFABRIC2 C(7),CCOLOR2 C(6),CFABDESC2 C(20),CCLRDESC2 C(30),;
                            CCNTLBL C(1))

SELECT FABRIC
SET RELATION TO FABRIC+COLOR INTO FABDYE

*-- To get the selected Fabrics if any.
lcFabFile = ''
llUseFab  = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'FABRIC.FABRIC'),1)
IF lnPosition > 0
  lcFabFile = laOGFxFlt[lnPosition,6]
  llUseFab  = IIF(!EMPTY(lcFabFile) .AND. USED(lcFabFile) .AND. RECCOUNT(lcFabFile)>0,.T.,.F.)
ENDIF

lnCntLbl = -1
IF llUseFab
  lcScanCond = 'REST WHILE FABRIC+COLOR = '+lcFabFile+'.FABRIC FOR '+lcRpExp
  SELECT (lcFabFile)
  SCAN
    IF SEEK(FABRIC,'FABRIC')
      SELECT FABRIC
      =lfUpdTemp(lcScanCond)
    ENDIF
  ENDSCAN
ELSE
  lcScanCond = 'FOR '+lcRpExp
  =lfUpdTemp(lcScanCond)
ENDIF

SELECT (lcTempFile)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
 SET DEVICE TO SCREEN
  RETURN
ENDIF

DO gfDispRe WITH EVAL('lcFormName')
USE IN (lcTempFile)
SELECT FABRIC
SET RELATION TO
SET DEVICE TO SCREEN
*-- end of Report Code.

*!*************************************************************
*! Name      : lfSRVFab
*: Developer : AHMED MAHER (AMH)
*! Date      : 01/06/2003
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm

PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*: Developer : AHMED MAHER (AMH)
*! Date      : 01/06/2003
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp

PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/06/2003
*! Purpose   : Generic code to get the old value of the field.
*!*************************************************************
*! Called from : Option Grid (Option that we respect to use its old value)
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvWare
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/06/2003
*! Purpose   : Valid function of the Warehouse field.
*!*************************************************************
*! Called from : Option Grid [Warehous field option]
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWare()
*!*************************************************************
FUNCTION lfvWare

PRIVATE lcVar, lcObj
lcVar = SYS(18)                      && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(EVALUATE(lcVar))     && Varible to hold the current field value
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'WAREHOUS'))
  lcObj = gfBrowWare( .T. )
  IF !EMPTY(lcObj)
    &lcVar = lcObj      && Update the field
  ELSE
    &lcVar = laOldVal
  ENDIF
ENDIF
*-- end of lfvWare.

*!*************************************************************
*! Name      : lfUpdTemp
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/06/2003
*! Purpose   : Update the temp. file
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : Scan condation
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUpdTemp()
*!*************************************************************
FUNCTION lfUpdTemp
PARAMETERS lcScanCond

SCAN &lcScanCond.
  lnCntLbl = MOD(lnCntLbl+1,3)
  lcCntLbl = STR(lnCntLbl,1)
  SELECT (lcTempFile)
  IF lnCntLbl = 0
    APPEND BLANK
  ENDIF
  REPLACE CCNTLBL               WITH lcCntLbl     ,;
          ('CFABRIC' +lcCntLbl) WITH FABRIC.FABRIC,;
          ('CCOLOR'  +lcCntLbl) WITH FABRIC.COLOR ,;
          ('CFABDESC'+lcCntLbl) WITH FABRIC.DESC  ,;
          ('CCLRDESC'+lcCntLbl) WITH gfCodDes(FABRIC.COLOR,'COLOR')
ENDSCAN
*-- end of lfUpdTemp.