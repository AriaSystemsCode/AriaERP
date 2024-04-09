CLOSE DATA

lcDataDir = GETDIR('','Select 2.7 data directory')
IF EMPTY(lcDataDir)
  WAIT 'Nothing selected'
  RETURN
ENDIF

SELECT 0
USE (lcDataDir+'ORDLINE.DBF')
SET ORDER TO TAG ORDLINE

SELECT 0
USE (lcDataDir+'PIKTKT.DBF')
SET ORDER TO TAG PIKTKT

SELECT 0
USE (lcDataDir+'ORDHDR.DBF')
SET ORDER TO TAG ORDHDR

SELECT 0
USE (lcDataDir+'STYDYE.DBF')
SET ORDER TO TAG STYDYE


*--Select only adorned pick tickets and only open not pulled or released or complete.
SELECT PIKTKT
SCAN FOR CPICKTYPE='A' AND STATUS $ 'O,H'
  WAIT WINDOW PIKTKT NOWAIT

  IF !SEEK('O'+PIKTKT.Order,'ORDHDR')
    LOOP
  ENDIF

  SELECT PIKTKT
  =RLOCK()
  REPLACE cWareCode WITH ORDHDR.cWareCode
  UNLOCK

  SELECT ORDLINE
  SEEK 'O'+PIKTKT.Order
  SCAN REST WHILE cOrdType+Order = 'O'+PIKTKT.Order FOR Piktkt = PIKTKT.Piktkt
    IF ORDLINE.cWareCode = ORDHDR.cWareCode
      LOOP
    ENDIF
    
    *--Update style/warehous order and alocated qty.
    SELECT STYDYE
    IF SEEK(ORDLINE.Style+ORDLINE.cWareCode+SPACE(10),'STYDYE')
      =RLOCK()
      FOR lnX= 1 TO 8
        lcZ=STR(lnX,1)
        REPLACE ALO&lcZ WITH ALO&lcZ - ORDLINE.PIK&lcZ,;
                ORD&lcZ WITH ORD&lcZ - ORDLINE.PIK&lcZ
      ENDFOR
      REPLACE TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
    ENDIF

    IF SEEK(ORDLINE.Style+ORDHDR.cWareCode+SPACE(10),'STYDYE')
      =RLOCK()
      FOR lnX= 1 TO 8
        lcZ=STR(lnX,1)
        REPLACE ALO&lcZ WITH ALO&lcZ + ORDLINE.PIK&lcZ,;
                ORD&lcZ WITH ORD&lcZ + ORDLINE.PIK&lcZ
      ENDFOR
      REPLACE TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
    ENDIF

    *--Update order line warehouse
    SELECT ORDLINE
    =RLOCK()
    REPLACE cWareCode WITH ORDHDR.cWareCode
    UNLOCK
  
  ENDSCAN

ENDSCAN

CLOSE DATA
RETURN

