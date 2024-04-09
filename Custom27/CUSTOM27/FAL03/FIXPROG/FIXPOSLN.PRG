lcDataDir = GETDIR(SET('DEFAULT'),'Select Data Directory')

IF EMPTY(lcDataDir)
  RETURN
ENDIF

IF FILE(lcDataDir+'POSLN.DBF')
  USE (lcDataDir+'POSLN') IN 0 SHARED ORDER TAG POSLN
  USE (lcDataDir+'POSLN') IN 0 SHARED ORDER TAG POSLN AGAIN ALIAS POSLN2
ELSE
  WAIT WINDOW 'POSLN file does not exist, Cannot procced'
  RETURN
ENDIF

IF FILE(lcDataDir+'POSHDR.DBF')
  USE (lcDataDir+'POSHDR') IN 0 SHARED ORDER TAG POSHDR
ELSE
  WAIT WINDOW 'POSHDR file does not exist, Cannot procced'
  RETURN
ENDIF

IF FILE(lcDataDir+'STYLE.DBF')
  USE (lcDataDir+'STYLE') IN 0 SHARED ORDER TAG STYLE
ELSE
  WAIT WINDOW 'STYLE file does not exist, Cannot procced'
  RETURN
ENDIF

SELECT POSLN
SCAN
  WAIT WINDOW 'Fix PO # : ' + PO + ' Style : ' + STYLE NOWAIT
  IF !SEEK(STYLE,'STYLE')
    lcPoKey = CSTYTYPE + PO
    lnTotQty = TOTQTY
    IF SEEK(lcPoKey,'POSHDR')
      IF POSHDR.ENTERED < {01/01/2002}
        DELETE
        SELECT POSHDR
        IF SEEK(lcPoKey,'POSLN2')
          REPLACE NSTYORDER WITH MAX(NSTYORDER - lnTotQty,0),;
                  OPEN      WITH MAX(OPEN - lnTotQty,0)
        ELSE
          DELETE
        ENDIF
        SELECT POSLN2
        LOCATE
      ENDIF
    ELSE
      DELETE
    ENDIF
  ENDIF
ENDSCAN

WAIT WINDOW 'Fixing completed successfully'