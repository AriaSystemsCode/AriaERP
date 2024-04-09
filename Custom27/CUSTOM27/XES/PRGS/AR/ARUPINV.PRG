*:************************************************************************
*: Program file  : ARUPINV.PRG
*: Program desc. : UPDATE INVLINE FILE COST FIELD IF EMPTY BY VALUE IN STYLE FILE
*:         System: ARIA APPAREL SERIES
*:           Date: 11/30/2000.
*:      Developer: Ahmed Abdel Naby (AAN)
*:************************************************************************
*: C102069,1.
*:************************************************************************

lcAvrgCost = gfGetMemVar('M_COST_METH')
llAvrgCost = IIF(lcAvrgCost = 'A',.T.,.F.)

=gfOpenFile(gcDataDir+'Style',gcDataDir+'Style','SH')
=gfOpenFile(gcDataDir+'InvLine',gcDataDir+'InvLineS','SH')

SET RELATION TO Style INTO Style

GO TOP

SCAN FOR Cost=0
  WAIT WINDOW ALLTRIM(Style)+'/'+ALLTRIM(STR(IIF(llAvrgCost,Style.Ave_Cost,Style.TotCost))) NOWAIT
  REPLACE NEXT 1 Cost WITH IIF(llAvrgCost,Style.Ave_Cost,Style.TotCost) 
ENDSCAN
WAIT WINDOW "Update Complete"
RETURN
