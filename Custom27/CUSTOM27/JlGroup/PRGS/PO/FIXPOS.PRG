lcDataDir = GETDIR(SET('DEFAULT'),'Select Data Directory')

IF EMPTY(lcDataDir)
  RETURN
ENDIF

IF FILE(lcDataDir+'POSHDR.DBF')
  USE (lcDataDir+'POSHDR') IN 0 SHARED ORDER TAG POSHDR
ELSE
  WAIT WINDOW 'POSHDR file does not exist, Cannot procced'
  RETURN
ENDIF

IF FILE(lcDataDir+'POSLN.DBF')
  USE (lcDataDir+'POSLN') IN 0 SHARED
ELSE
  WAIT WINDOW 'POSLN file does not exist, Cannot procced'
  RETURN
ENDIF

SELECT POSLN
SCAN FOR CSTYTYPE = 'A'
  WAIT WINDOW 'Fix PO # : ' + PO + ' Style : ' + STYLE NOWAIT
  IF !SEEK(CSTYTYPE+PO,'POSHDR')
    IF SEEK('P'+PO,'POSHDR')
      REPLACE CSTYTYPE WITH 'P'
    ENDIF
  ENDIF
ENDSCAN
WAIT WINDOW 'Fixing completed successfully'