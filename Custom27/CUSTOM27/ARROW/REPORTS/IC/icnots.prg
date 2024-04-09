**************************************************************************
*: Program file  : ICNOTS.PRG (C# )
*: Program desc. : Negative open to cell report
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY - (SSH)
*:************************************************************************
*: Calls : FUNCTIONS  : None
*:
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************

SELECT Style
SET FILTER TO (TOTSTK -TOTORD)<0
DO gfDispRe WITH EVAL('lcRpForm')
RETURN