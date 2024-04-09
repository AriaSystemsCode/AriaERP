**************************************************************************
*: Program file  : ICStkOnH.PRG (C# )
*: Program desc. : Stock On Hand Report For Arrow Misr
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY - (SSH)
*:************************************************************************
*: Calls : FUNCTIONS  : None
*:
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************

SELECT Style
SET FILTER TO (TOTSTK<>0) .OR. (nStkVal<>0)
DO gfDispRe WITH EVAL('lcRpForm')
RETURN