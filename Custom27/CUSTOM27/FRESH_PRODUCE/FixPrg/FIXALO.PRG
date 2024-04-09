LCDATA=GETDIR('','Select data directory')
IF EMPTY(LCDATA)
  WAIT WINDOW ' NO DATA DIRECRORY WAS SELECTED!'
  RETURN
ENDIF  

CLOSE DATA
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

SELECT 0
USE (LCDATA+'posln.dbf') SHARED
SET ORDER TO TAG Posln

SELECT 0
USE (LCDATA+'poshdr.dbf') SHARED
SET ORDER TO TAG poshdr

SELECT 0
USE (LCDATA+'STYDYE.dbf') SHARED
SET ORDER TO TAG STYDYE


SELECT (lcTmpLine)
SET ORDER TO TAG POSLN
USE (gcWorkDir+lcTmpLine) IN 0 AGAIN ALIAS PTMPLINE ORDER TAG POSLN
SELECT (lcTmpLine)
GO TOP
SCAN FOR TranCd = '1'
  lcTKey = cStyType+Po+Style+STR(LineNo,6)  
  DIME laUQty[9]
  laUQty = 0
  SELECT PTMPLINE
  =SEEK(lcTKey)
  SCAN WHILE cStyType+Po+Style+STR(LineNo,6) =lcTKey FOR TranCd<>'5'
    SCATTER MEMVAR
    FOR lnI=1 TO 8
      lcI = STR(lnI,1)
      laUQty[lnI] = laUQty[lnI] + IIF(TranCd='1',Qty&lcI,-Qty&lcI)
    ENDFOR
    laUQty[9] = laUQty[9] + IIF(TranCd='1',TotQty,-TotQty)
  ENDSCAN
  *--If there is something to cancel.
  IF laUQty[9]<>0
    SELECT (lcTmpLine)
    IF !SEEK(lcTKey+'5',lcTmpLine)
      APPEND BLANK
      GATHER MEMVAR
      REPLACE TranCd WITH '5'
    ENDIF
    FOR lnI=1 TO 8
      lcI = STR(lnI,1)
      IF laUQty[lnI]<>0
        REPLACE Qty&lcI WITH laUQty[lnI]
      ENDIF
    ENDFOR
    REPLACE TotQty WITH laUQty[9]
    *--Put the pointer back.
    =SEEK(lcTKey)
    LOCATE REST WHILE cStyType+Po+Style+STR(LineNo,6)=lcTKey FOR TranCd='1'
  ENDIF
ENDSCAN
USE IN PTMPLINE

SELECT (lcTmpLine)
SCAN FOR TRANCD='5'
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laQtyCanl

  * 1- Update CutPick
  SELECT CUTPICK
  =SEEK('2'+&lcTmpLine..Po+STR(&lcTmpLine..LineNo,6),'CUTPICK')
  =RLOCK()
  FOR lnI=1 TO 8
    lcZ=STR(lnI,1)
    REPLACE Qty&lcZ WITH MAX(Qty&lcZ-laQtyCanl[lnI],0)
  ENDFOR
  REPLACE TotQty WITH MAX(TotQty-laQtyCanl[9],0)
  UNLOCK

  * 2- Update Order files.
  SELECT ORDHDR
  =SEEK('O'+CUTPICK.Order,'ORDHDR')
  =RLOCK()
  REPLACE TotCut WITH MAX(TotCut-laQtyCanl[9],0)
  UNLOCK
  SELECT ORDLINE
  =SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')    
  =RLOCK()
  REPLACE TotCut WITH MAX(TotCut - laQtyCanl[9],0),;
          TotPik WITH MAX(TotPik - laQtyCanl[9],0)
  FOR lnI = 1 TO 8
    lcZ = STR(lnI,1)
    REPLACE Cut&lcZ WITH MAX(Cut&lcZ-laQtyCanl[lnI],0),;
            Pik&lcZ WITH MAX(Pik&lcZ-laQtyCanl[lnI],0)
  ENDFOR
  UNLOCK

  * 3- Update style.
  =SEEK(&lcTmpLine..Style,'STYLE')
  SELECT STYLE
  =RLOCK()
  FOR lnI = 1 TO 8
    lcSz = STR(lnI,1)
    REPLACE Alo&lcSz WITH Alo&lcSz - MAX(Alo&lcSz-laQtyCanl[lnI],0)
  ENDFOR
  REPLACE TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
  UNLOCK 


  * 7- Update styDye.
  =SEEK(&lcTmpLine..Style+PADR(&lcTmpLine..cWareCode,6)+SPACE(10))
  =RLOCK()
  FOR lnI = 1 TO 8
    lcSz = STR(lnI,1)
    REPLACE Alo&lcSz WITH Alo&lcSz - MAX(Alo&lcSz-laQtyCanl[lnI],0)
  ENDFOR
  REPLACE TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
  UNLOCK 

ENDSCAN
SELECT (lcTmpLine)
RETURN
