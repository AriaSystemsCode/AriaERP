*:***************************************************************************
*: Program file     : MAFXPRC.PRG
*: Program desc.    : FIX PROGRAM TO FIX THE Price Field.
*: Date             : 05/15/2004
*: System           : Aria Advantage Series.
*: Module           : MATERIAL  (MA)
*: Developer        : NADER NABIL (NNA)
*: Tracking Job NO# : B122673
*:***************************************************************************
*: Calls            : lfvDbf
*:***************************************************************************
*: Passed Parameters: None
*:***************************************************************************
*: Example          : DO MAFXPRC
*:***************************************************************************
PARAMETER lcDataDir
PRIVATE lnRecCount , lnRecNo
STORE 0 TO lnRecCount , lnRecNo
IF FILE(lcDataDir+'VENDMATL.DBF')
  IF USED('VENDMATL')
    USE IN VENDMATL
  ENDIF
  USE lcDataDir+'VENDMATL.DBF' IN 0 ORDER  VENMAT
ELSE
  RETURN(.F.)  
ENDIF

SELECT VENDMATL
lnRecCount = RECCOUNT()

SCAN
  lnRecNo = lnRecNo + 1
  =gfThermo(lnRecCount , lnRecNo , "Copying the price field " , '')
  REPLACE NfabCost WITH Price
ENDSCAN
RETURN

