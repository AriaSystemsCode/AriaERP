*:**************************************************************************
*: Program file  : ARRegion
*: Program desc. : CP to add Region mover in AR Aging and Customer Statement.
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 10/28/2001
*: Reference     : C200240 , C200241
*:**************************************************************************
*: Functions     : lfFillMovr , lfvRegion
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*

*!**************************************************************************
*! Name      : lfFillMovr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/25/2001
*! Purpose   : To collect data for the region mover.
*!**************************************************************************
*! Example   : =lfFillMovr()
*!**************************************************************************
*
FUNCTION lfFillMovr
PARAMETER lfDummy
lfDummy = .T.

DIMENSION laRpSRegn[1,1],laRpTRegn[1,1]
STORE '' TO laRpSRegn,laRpTRegn

*--The Region.
SELECT CODES
LOCATE
SELECT DISTINCT cCode_NO + ' - ' + cDiscRep ;
FROM CODES ;
WHERE cDefCode + cFld_Name + cCode_No + cDiscRep + cRltd_Nam = "NREGION" ;
AND crltField = 'N';
INTO ARRAY laRpSRegn

RETURN lfDummy
*-- End of lfFiLLMovr.

*!**************************************************************************
*! Name      : lfvRegion
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/25/2001
*! Purpose   : To open the region mover.
*!**************************************************************************
*! Example   : =lfvRegion()
*!**************************************************************************
*
FUNCTION lfvRegion
PARAMETER lfDummy
lfDummy = .T.

= gfMover(@laRpSRegn,@laRpTRegn,'Region',.T.,'')
RETURN lfDummy
*-- End of lfvRegion.
