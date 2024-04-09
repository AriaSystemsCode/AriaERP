
PRIVATE lcDataDir
lcDataDir = GETDIR('','Select Data Company')

IF EMPTY(lcDataDir)
  WAIT WINDOW "User Pressed Cancel"
  RETURN
ENDIF

IF !FILE(lcDataDir + 'INVHDR.DBF')
  WAIT WINDOW "Wrong Data Company"
  RETURN
ENDIF

PRIVATE llInvHdr , llGLDist , llGLBatch , llGLTrnsHd , llGLTrnsDt , llGLPTrnHd , ;
        llGLPTrnDt , lcTmpBatch
STORE .F. TO llInvHdr , llGLDist , llGLBatch , llGLTrnsHd , llGLTrnsDt , llGLPTrnHd , ;
             llGLPTrnDt
lcTmpBatch = 'TmpBatch'

*-- Opening necessary files.
IF !USED('InvHdr')
  USE (lcDataDir + 'InvHdr') IN 0
  llInvHdr = .T.
ENDIF

IF !USED('GLDist')
  USE (lcDataDir + 'GLDist') IN 0 ORDER TAG GLDistNo
  llGLDist = .T.
ENDIF

IF !USED('GLBatch')
  USE (lcDataDir + 'GLBatch') IN 0 ORDER TAG BatchNo
  llGLBatch = .T.
ENDIF

IF !USED('GLTrnsHd')
  USE (lcDataDir + 'GLTrnsHd') IN 0 ORDER TAG BatchTrn
  llGLTrnsHd = .T.
ENDIF

IF !USED('GLTrnsDt')
  USE (lcDataDir + 'GLTrnsDt') IN 0 ORDER TAG BatchTrn
  llGLTrnsDt = .T.
ENDIF

IF !USED('GLPTrnHd')
  USE (lcDataDir + 'GLPTrnHd') IN 0 ORDER TAG BatchTrn
  llGLPTrnHd = .T.
ENDIF

IF !USED('GLPTrnDt')
  USE (lcDataDir + 'GLPTrnDt') IN 0 ORDER TAG BatchTrn
  llGLPTrnDt = .T.
ENDIF

*-- Fixing first part which is the differences between nGLAmount and nEqvAmnt in GLDist file.
CREATE TABLE (lcDataDir + lcTmpBatch) (cBatchNo C(6))
INDEX ON cBatchNo TAG (lcTmpBatch)

SELECT GLDist
SCAN FOR nGLAmount <> nEqvAmnt
  WAIT WINDOW 'Fixing Batch # ' + GLBatch NOWAIT
  IF !EMPTY(GLBatch) AND !SEEK(GLBatch,lcTmpBatch)
    m.cBatchNo = GLBatch
    INSERT INTO (lcTmpBatch) FROM MEMVAR

    IF SEEK(GLBatch,'GLBatch')
      SELECT GLBatch
      DELETE REST WHILE cBatchNo = GLDist.GLBatch
    
      IF SEEK(GLDist.GLBatch,'GLTrnsHd')
        SELECT GLTrnsHd
        DELETE REST WHILE cBatchNo + cTranNo = GLDist.GLBatch
      ENDIF

      IF SEEK(GLDist.GLBatch,'GLTrnsDt')
        SELECT GLTrnsDt
        DELETE REST WHILE cBatchNo + cTranNo = GLDist.GLBatch
      ENDIF

      IF SEEK(GLDist.GLBatch,'GLPTrnHd')
        SELECT GLPTrnHd
        DELETE REST WHILE cBatchNo + cTranNo = GLDist.GLBatch
      ENDIF
    
      IF SEEK(GLDist.GLBatch,'GLPTrnDt')
        SELECT GLPTrnDt
        DELETE REST WHILE cBatchNo + cTranNo = GLDist.GLBatch
      ENDIF
    ENDIF
    
  ENDIF
  SELECT GLDist
  REPLACE nEqvAmnt WITH nGLAmount  
ENDSCAN

SELECT GLDist
SCAN FOR !EMPTY(GLBatch)
  IF SEEK(GLBatch,lcTmpBatch)
    WAIT WINDOW 'Fixing Batch # ' + GLBatch NOWAIT
    REPLACE Posted    WITH '' , ;
            GLBatch   WITH '' , ;
            cTrnsledn WITH '' , ;
            nEntryNo  WITH 0  , ;
            GLComp    WITH ''
  ENDIF
ENDSCAN

WAIT CLEAR

*-- Closing all files.
IF llInvHdr
  USE IN InvHdr
ENDIF

IF llGLDist
  USE IN GLDist
ENDIF

IF llGLBatch
  USE IN GLBatch
ENDIF

IF llGLTrnsHd
  USE IN GLTrnsHd
ENDIF

IF llGLTrnsDt
  USE IN GLTrnsDt
ENDIF

IF llGLPTrnHd
  USE IN GLPTrnHd
ENDIF

IF llGLPTrnDt
  USE IN GLPTrnDt
ENDIF

IF USED(lcTmpBatch)
  USE IN (lcTmpBatch)
  ERASE (lcDataDir + lcTmpBatch + '.DBF')
  ERASE (lcDataDir + lcTmpBatch + '.CDX')
ENDIF
*-- End of fix program
