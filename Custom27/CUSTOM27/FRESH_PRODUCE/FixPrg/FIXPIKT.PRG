LCDATA=GETDIR('','Select data directory')
IF EMPTY(LCDATA)
  WAIT WINDOW ' NO DATA DIRECRORY WAS SELECTED!'
  RETURN
ENDIF  

CLOSE DATA
SELECT 0
USE (LCDATA+'posln.dbf') SHARED
SET ORDER TO TAG Posln

SELECT 0
USE (LCDATA+'poshdr.dbf') SHARED
SET ORDER TO TAG poshdr

SELECT 0
USE (LCDATA+'ordline.dbf') SHARED
SET ORDER TO TAG Ordline

SELECT 0
USE (LCDATA+'ordhdr.dbf') SHARED
SET ORDER TO TAG Ordhdr

SELECT 0
USE (LCDATA+'cutpick.dbf') SHARED
SET ORDER TO TAG Cutpkord

SELECT 0
USE (LCDATA+'bomvar.dbf') SHARED
SET ORDER TO TAG Bomvar


SELECT POSLN
SEEK 'A'
SCAN REST WHILE cStyType='A' FOR ShipNo = '*' OR TOTQTY=0
  WAIT WINDOW 'Check/update Adorn. P/O :'+POSLN.PO NOWAIT

  SELECT POSLN
  =RLOCK()
  REPLACE COWNER WITH 'DELETE'
  UNLOCK

  IF SEEK('AD'+POSLN.PO+STR(POSLN.lineno,6),'BOMVAR')
    SELECT BOMVAR
    DELETE REST WHILE cidtype+ccost_id+STR(lineno,6) = 'AD'+POSLN.PO+STR(POSLN.lineno,6)
  ENDIF  

  SELECT POSLN
  IF !SEEK('2'+POSLN.Po+STR(POSLN.Lineno,6),'CUTPICK')
    LOOP
  ENDIF  

  lcOrdKey = 'O'+CUTPICK.Order+CUTPICK.cOrdLine 
  SELECT CUTPICK
  =RLOCK()
  DELETE
  UNLOCK

  IF SEEK(lcOrdKey,'ORDLINE')
    =SEEK('O'+ORDLINE.Order,'ORDHDR')
    SELECT ORDHDR
    =RLOCK()
    REPLACE ORDHDR.TOTCUT WITH ORDHDR.TOTCUT - ORDLINE.TOTCUT
    UNLOCK
    SELECT ORDLINE
    =RLOCK()
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      REPLACE ORDLINE.CUT&Z WITH 0
    ENDFOR 
    REPLACE ORDLINE.TOTCUT WITH 0
    UNLOCK
  ENDIF  

ENDSCAN

SELECT POSLN
DELETE ALL FOR COWNER = 'DELETE'


SELECT POSHDR
SEEK 'A'
SCAN REST WHILE cStyType='A'
  WAIT WINDOW 'Check/update Adorn. P/O :'+POSHDR.PO NOWAIT

  IF POSHDR.Status='X'
    IF SEEK('AD'+POSHDR.PO,'BOMVAR')
      SELECT BOMVAR
      DELETE REST WHILE cidtype+ccost_id = 'AD'+POSLN.PO
    ENDIF  

    IF SEEK('2'+POSHDR.Po,'CUTPICK')
      SELECT CUTPICK
      SCAN WHILE trancd+ctktno = '2'+POSHDR.Po
        lcOrdKey = 'O'+CUTPICK.Order+CUTPICK.cOrdLine 
        IF SEEK(lcOrdKey,'ORDLINE')
          =SEEK('O'+ORDLINE.Order,'ORDHDR')
          SELECT ORDHDR
          =RLOCK()
          REPLACE ORDHDR.TOTCUT WITH ORDHDR.TOTCUT - ORDLINE.TOTCUT
          UNLOCK
          SELECT ORDLINE
          =RLOCK()
          FOR lnX=1 TO 8
            Z=STR(lnX,1)
            REPLACE ORDLINE.CUT&Z WITH 0
          ENDFOR 
          REPLACE ORDLINE.TOTCUT WITH 0
          UNLOCK
        ENDIF  
      ENDSCAN

      SELECT CUTPICK
      =SEEK('2'+POSHDR.Po)
      DELETE REST WHILE trancd+ctktno = '2'+POSHDR.Po
    ENDIF  
  ENDIF
  
  SELECT POSHDR
  =RLOCK()
  REPLACE NSTYORDER WITH 0 ,;
          NICOST1   WITH 0 ,;
          NICOST2   WITH 0 
  UNLOCK

  SELECT POSLN
  SEEK 'A'+POSHDR.PO
  SCAN REST WHILE cStyType+PO = 'A'+POSHDR.PO FOR TranCd='1'
    SELECT POSHDR
    =RLOCK()
    REPLACE NSTYORDER WITH NSTYORDER + POSLN.TOTQTY          ,;
            OPEN      WITH (NSTYORDER-RECEIVE-DAMAGE-CANCEL) ,;
            NICOST1  WITH NICOST1+(POSLN.NCOST1*POSLN.TOTQTY),;
            NFCOST1  WITH NICOST1,;  
            NICOST2  WITH NICOST2+(POSLN.NCOST2*POSLN.TOTQTY),;
            NFCOST2  WITH NICOST2,;
            POTOTAL  WITH NICOST1+NICOST2
    UNLOCK
    SELECT POSLN
  ENDSCAN

  SELECT POSHDR
ENDSCAN

WAIT WINDOW 'FINISHED!' NOWAIT
CLOS DATA
RETURN
