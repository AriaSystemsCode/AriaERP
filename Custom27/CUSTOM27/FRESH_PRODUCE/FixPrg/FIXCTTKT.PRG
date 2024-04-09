lcDataDir=GETDIR('','Select data directory')
IF EMPTY(lcDataDir)
  WAIT WINDOW 'No data directory was selected. Cannot Proceed.'
  RETURN
ENDIF  

Close Data

*-- Open the necessary files.

SELECT 0
USE (lcDataDir+'CutTktH.dbf') SHARED
SET ORDER TO TAG CutTktH
SELECT 0
USE (lcDataDir+'CutTktL.dbf')  SHARED
SET ORDER TO TAG CutTktL

SELECT 0
USE (lcDataDir+'PosHdr.dbf') SHARED
SET ORDER TO TAG PosHdr

SELECT 0
USE (lcDataDir+'PosLn.dbf') SHARED
SET ORDER TO TAG PosLn

SELECT 0
USE (lcDataDir+'MfgOprDt.dbf') SHARED
SET ORDER TO TAG TktOpTrn

SELECT CutTktH
SCAN FOR Status = "C"
  
  *-- The following cuttkts have a record with trancd = "4"
  IF CutTkt = "040003" OR CutTkt = "040072" OR CutTkt = "040254"
    LOOP
  ENDIF
  
  SELECT CutTktL
  SEEK CutTktH.Cuttkt
  SCAN REST WHILE CutTkt+Style+Dyelot+Trancd = CutTktH.Cuttkt FOR TranCd $ '234'
    WAIT WINDOW "Style/Color : " + Style NOWAIT

    SELECT MfgOprDt
    IF SEEK("M"+CutTktL.Cuttkt+"CMT   "+"1")
      SCATTER MEMVAR MEMO
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE TranCd WITH CutTktL.TranCd
      FOR lnCounter = 1 TO 8
        lcCounter = STR(lnCounter,1)
        REPLACE nLotQty&lcCounter WITH CutTktL.Qty&lcCounter
      ENDFOR
      REPLACE nLotTotQty WITH CutTktL.TotQty
    ENDIF  
  ENDSCAN
ENDSCAN
WAIT CLEAR

SELECT PosHdr
SCAN FOR cStyType = "D" AND Status = "C"
  
  SELECT PosLn
  SEEK "D"+PosHdr.Po
  SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+Trancd = "D"+PosHdr.Po FOR TranCd $ '245'
    WAIT WINDOW "Style/Color : " + Style NOWAIT

    SELECT MfgOprDt
    IF SEEK("I"+PosLn.Po+"DYE   "+"1")
      SCATTER MEMVAR MEMO
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE TranCd WITH PosLn.TranCd
      FOR lnCounter = 1 TO 8
        lcCounter = STR(lnCounter,1)
        REPLACE nLotQty&lcCounter WITH PosLn.Qty&lcCounter
      ENDFOR
      REPLACE nLotTotQty WITH PosLn.TotQty
    ENDIF  
  ENDSCAN
ENDSCAN
WAIT CLEAR

Close Data
