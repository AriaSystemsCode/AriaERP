*:***************************************************************************
*: Program file  : MAFXMAT
*: Program desc. : Fixing POFHDR file 
*: For screen    : 
*:        System : Aria Advantage Series.
*:        Module : (MA)
*:     Developer : Mohamed Shokry (MHM)
*:***************************************************************************
*: Calls : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:
PARAMETER lcDataDir
IF TYPE('lcDataDir') <> 'C'
  lcDataDir = GETDIR('','Select data directory')
ENDIF

IF EMPTY(lcDataDir)
  RETURN
ENDIF  
IF !FILE(lcDataDir+'POFHDR.DBF')
  RETURN
ENDIF
IF !FILE(lcDataDir+'POFLN.DBF')
  RETURN
ENDIF

IF !USED('POFHDR')
  =gfOpenFile(lcDataDir+'POFHDR','POFHDR','SH')
ENDIF

IF !USED('POFLN')
  =gfOpenFile(lcDataDir+'POFLN','POFLN','SH')
ENDIF

SELECT POFHDR
SET RELATION TO cMatType + pomat INTO POFLN

SELECT POFHDR
LOCATE
SCAN FOR cMatType+PoMat = 'P'
  IF Status = "L"
    SELECT POFLN
    SCAN REST WHILE cMatType+PoMat+Fabric+Color = PofHdr.cMatType + PofHdr.PoMat FOR TranCd='4'
      WAIT WINDOW 'MA PO : ' + poMat NOWAIT
      IF EMPTY(cWareCode)
        REPLACE cWareCode WITH PofHdr.cWareCode 
      ENDIF
    ENDSCAN  
  ENDIF
ENDSCAN

IF USED('POFHDR')
  USE IN POFHDR
ENDIF
IF USED('POFLN')
  USE IN POFLN
ENDIF
