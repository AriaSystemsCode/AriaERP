PARAMETER lnDiff
IF TYPE("lnDiff") # "N"
  lnDiff = 1
ENDIF  
SELECT POSLN
SET ORDER TO 1
GO TOP
SELECT POSHDR
SCAN
  lcPO = PO
  STORE 0 TO ln1,ln2,ln3,ln4
  SELECT POSLN
  =SEEK("P"+lcPO)
  SCAN REST WHILE cstytype+po = "P"+lcPO FOR TRANCD='2'
    ln1 = ln1 + nAct_Cst1*TotQty
    ln2 = ln2 + nAct_Cst2*TotQty
    ln3 = ln3 + nAct_Cst3*TotQty
    ln4 = ln4 + nAct_Cst4*TotQty
  ENDSCAN
  IF ABS(POSHDR.nAct_Cost1 - ln1)>lnDiff
    SELECT lsttes2
    APPEND BLANK
    REPLACE   diff   WITH POSHDR.nAct_Cost1 - ln1,;
              csttyp WITH '1',;
              po WITH POSHDR.PO
  ENDIF

  IF ABS(POSHDR.nAct_Cost2 - ln2)>lnDiff
    SELECT lsttes2
    APPEND BLANK
    REPLACE   diff   WITH POSHDR.nAct_Cost2 - ln2,;
              csttyp WITH '2',;
              po WITH POSHDR.PO
  ENDIF
  IF ABS(POSHDR.nAct_Cost3 - ln3)>lnDiff
    SELECT lsttes2
    APPEND BLANK
    REPLACE   diff   WITH POSHDR.nAct_Cost3 - ln3,;
              csttyp WITH '3',;
              po WITH POSHDR.PO
  ENDIF
  IF ABS(POSHDR.nAct_Cost4 - ln4)>lnDiff
    SELECT lsttes2
    APPEND BLANK
    REPLACE   diff   WITH POSHDR.nAct_Cost4 - ln4,;
              csttyp WITH '4',;
              po WITH POSHDR.PO
  ENDIF

ENDSCAN