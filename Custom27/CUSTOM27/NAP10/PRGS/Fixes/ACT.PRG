PARAMETER lnFrom ,lnTO
IF lnFrom =0
lnFrom =1
ENDIF
IF lnTO = 0
  lnTO = 4
ENDIF
FOR lnInd = lnFrom TO lnTO
  lcInd = STR(lnInd,1)
  SELECT POSLN27
  REPLACE nAct_Cst&lcInd  WITH POSLN26.nAct_Cost&lcInd
ENDFOR

