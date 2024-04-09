lcData=GETDIR('','Select data directory')
IF EMPTY(lcData)
  WAIT WINDOW ' NO DATA DIRECRORY WAS SELECTED!'
  RETURN
ENDIF  

CLOSE DATA

*--Open needed files.
SELECT 0
USE (lcData+'ORDHDR.dbf')

SELECT 0
USE (lcData+'ORDLINE.dbf') SHARED
SET ORDER TO TAG ORDLINE

SELECT 0
USE (lcData+'STYDYE.dbf') SHARED
SET ORDER TO TAG STYDYE


*-- Check all open order .
SELECT ORDHDR
SCAN FOR ORDHDR.Status = 'O' AND ORDHDR.Open <> 0
  WAIT WINDOW Order NOWAIT


  SELECT ORDLINE
  =SEEK(ORDHDR.cOrdType+ORDHDR.Order)
  *-- Check all lines for that open order which has no piktkt
  *-- asigned and warehouse was different that original.
  
  SCAN REST WHILE cOrdType+Order=ORDHDR.cOrdType+ORDHDR.Order ;
        FOR TotQty <> 0 AND cWareCode <> ORDHDR.cWareCode AND ;
            TotPik = 0  AND EMPTY(Piktkt)

 *--Move the ordered qty at style warehouse level and switch the warehouse.
    SELECT STYDYE
    IF SEEK(ORDLINE.Style+ORDLINE.cWareCode+SPACE(10))
      =RLOCK()
      FOR lnX= 1 TO 8
        Z=STR(lnX,1)
        REPLACE ORD&Z WITH ORD&Z - ORDLINE.QTY&Z
      ENDFOR
      REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
    ENDIF
    IF SEEK(ORDLINE.Style+ORDHDR.cWareCode+SPACE(10))
      =RLOCK()
      FOR lnX= 1 TO 8
        Z=STR(lnX,1)
        REPLACE ORD&Z WITH ORD&Z + ORDLINE.QTY&Z
      ENDFOR
      REPLACE TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
    ENDIF

    *--Update ordline warehouse.
    SELECT ORDLINE
    =RLOCK()
    REPLACE cWareCode WITH ORDHDR.cWareCode
    UNLOCK

  ENDSCAN

ENDSCAN

CLOSE DATA
