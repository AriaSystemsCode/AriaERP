*:**************************************************************************
*: Program     : ARSADFIX.PRG
*: Date        : 12/04/2001
*: Description : Fix invoice and GL Dist
*: Developer   : Sameh Saiid Ezzat (SSE)
*: Refer to    : B605196,1
*:**************************************************************************
*
PRIVATE lcDataDir
lcDataDir = GETDIR('','Select Company Data.')
IF EMPTY(lcDataDir) OR !FILE(lcDataDir+'InvHdr.DBF')
  WAIT WINDOW 'Wrong company data directory!' TIMEOUT 3
  RETURN 
ENDIF

PRIVATE llInvHdr , llGLDist , llGLBatch , llGlTrnHdr , llGlTrnDet , llArCusHst , ;
        llGLPstHdr , llGLPstDet , lcPeriod , lcTempInv , laGLDist
STORE .F. TO llInvHdr , llGLDist , llGLBatch , llGlTrnHdr , llGlTrnDet , llArCusHst
lcPeriod = ''
lcTempInv = 'TmpInvNo'
SET DELETE ON

*-- Open needed files.
IF !USED('InvHdr')
  USE (lcDataDir+'InvHdr.DBF') IN 0 SHARE 
  llInvHdr = .T.
ENDIF

IF !USED('GLDist')
  USE (lcDataDir+'GLDist.DBF') IN 0 SHARE ORDER TAG GLDistNo
  llGLDist = .T.
ENDIF

IF !USED('GLBatch')
  USE (lcDataDir+'GLBatch.DBF') IN 0 SHARE ORDER TAG BatchNo
  llGLBatch = .T.
ENDIF

IF !USED('GlTrnsHd')
  USE (lcDataDir+'GLTrnsHd.DBF') IN 0 SHARE ORDER TAG BatchTrn
  llGlTrnHdr = .T.
ENDIF

IF !USED('GLTrnsDt')
  USE (lcDataDir+'GLTrnsDt.DBF') IN 0 SHARE ORDER TAG BatchTrn
  llGlTrnDet = .T.
ENDIF

IF !USED('ArCusHst')
  USE (lcDataDir+'ArCusHst.DBF') IN 0 SHARE ORDER TAG ActHst
  llArCusHst = .T.
ENDIF

IF !USED('GLPTrnHd')
  USE (lcDataDir+'GLPTrnHd.DBF') IN 0 SHARE ORDER TAG BatchTrn
  llGLPstHdr = .T.
ENDIF

IF !USED('GLPTrnDt')
  USE (lcDataDir+'GLPTrnDt.DBF') IN 0 SHARE ORDER TAG BatchTrn
  llGLPstDet = .T.
ENDIF

SELECT GLDist
=AFIELDS(laGLDist)
CREATE TABLE (lcDataDir+'TempDist') FROM ARRAY laGLDist
INDEX ON Tran_No + Tran_Type TAG TempDist

CREATE TABLE (lcDataDir+lcTempInv) (BatchNo C(6))
INDEX ON BatchNo TAG (lcTempInv)

SELECT InvHdr
SCAN FOR (Discount < 0 AND DiscPcnt = 0) OR (Discount = 0 AND DiscPcnt > 0 AND InvHdr.Status <> "V")
  WAIT WINDOW 'Fixing Invoice # ' + Invoice NOWAIT
  IF SEEK(InvHdr.Invoice + "IN",'GLDist')
    IF SEEK(InvHdr.Account+GLDist.GLFYear,'ArCusHst')
      lcPeriod = GLDist.GLPeriod
      SELECT ArCusHst
      IF (InvHdr.Discount < 0 AND InvHdr.DiscPcnt = 0)
        REPLACE nDisAmt WITH nDisAmt + InvHdr.Discount , ;
                nDisAmt&lcPeriod WITH nDisAmt&lcPeriod + InvHdr.Discount
      ELSE
        REPLACE nDisAmt WITH nDisAmt + (InvHdr.ShipAmt * InvHdr.DiscPcnt/100) , ;
                nDisAmt&lcPeriod WITH nDisAmt&lcPeriod + (InvHdr.ShipAmt * InvHdr.DiscPcnt/100)
      ENDIF          
    ENDIF
    
    IF !EMPTY(GLDist.GLBatch) AND SEEK(GLDist.GLBatch,'GLBatch')
      m.BatchNo = GLDist.GLBatch
      INSERT INTO (lcTempInv) FROM MEMVAR
    
      SELECT GLBatch
      DELETE REST WHILE cBatchNo = GLDist.GLBatch
    
      IF SEEK(GLDist.GLBatch,'GLTrnsDt')
        SELECT GLTrnsDt
        DELETE REST WHILE cBatchNo + cTranNo = GLDist.GLBatch
      ENDIF

      IF SEEK(GLDist.GLBatch,'GLTrnsHd')
        SELECT GLTrnsHd
        DELETE REST WHILE cBatchNo + cTranNo = GLDist.GLBatch        
      ENDIF

      IF SEEK(GLDist.GLBatch,'GLPTrnDt')
        SELECT GLPTrnDt
        DELETE REST WHILE cBatchNo + cTranNo = GLDist.GLBatch
      ENDIF

      IF SEEK(GLDist.GLBatch,'GLPTrnHd')
        SELECT GLPTrnHd
        DELETE REST WHILE cBatchNo + cTranNo = GLDist.GLBatch        
      ENDIF
    ENDIF  
    
    SELECT GLDist
    SCAN REST WHILE Tran_no + Tran_Type + GLSession + Catg_Key = InvHdr.Invoice + "IN"
      REPLACE Posted    WITH '' , ;
              GLBatch   WITH '' , ;
              cTrnsledn WITH '' , ;
              nEntryNo  WITH 0 , ;
              GLComp    WITH ''
      IF (InvHdr.Discount < 0 AND InvHdr.DiscPcnt = 0)
        IF Catg_Key = '005'
          DELETE
        ENDIF
      ELSE
        IF !SEEK(GLDist.tran_no+GLDist.tran_type,'TempDist')
          SCATTER MEMVAR MEMO
          m.GLAccount = "4010-000"
          m.nGLAmount = InvHdr.ShipAmt * InvHdr.DiscPcnt/100
          m.nEqvAmnt = m.nGLAmount
          m.Catg_Key = "005"
          INSERT INTO TempDist FROM MEMVAR
        ENDIF        
      ENDIF  
    ENDSCAN
    
    SELECT InvHdr
    IF (InvHdr.Discount < 0 AND InvHdr.DiscPcnt = 0)
      REPLACE Discount WITH 0
    ELSE
      REPLACE Discount WITH InvHdr.ShipAmt * InvHdr.DiscPcnt/100 * -1
    ENDIF  
  ENDIF
ENDSCAN
WAIT CLEAR

SELECT GLDist
SCAN FOR !EMPTY(GLBatch)
  WAIT WINDOW 'Batch # ' + GLBatch NOWAIT
  IF SEEK(GLBatch,(lcTempInv))
    REPLACE Posted    WITH '' , ;
            GLBatch   WITH '' , ;
            cTrnsledn WITH '' , ;
            nEntryNo  WITH 0 , ;
            GLComp    WITH ''    
  ENDIF  
ENDSCAN
WAIT CLEAR

SELECT GLDist
APPEND FROM (lcDataDir + 'TempDist')

*-- Close the files opened.
IF llInvHdr
  USE IN InvHdr
ENDIF

IF llGLDist
  USE IN GLDist
ENDIF

IF llGLBatch
  USE IN GLBatch
ENDIF

IF llGlTrnHdr
  USE IN GlTrnsHd
ENDIF

IF llGlTrnDet
  USE IN GlTrnsDt
ENDIF

IF llArCusHst
  USE IN ArCusHst
ENDIF

IF llGLPstHdr
  USE IN GLPTrnHd
ENDIF

IF llGLPstDet
  USE IN GLPTrnDt
ENDIF

USE IN TempDist
ERASE (lcDataDir + 'TEMPDIST.DBF')
ERASE (lcDataDir + 'TEMPDIST.CDX')

IF USED(lcTempInv)
  USE IN (lcTempInv)
  ERASE (lcDataDir + lcTempInv + '.DBF')
  ERASE (lcDataDir + lcTempInv + '.CDX')  
ENDIF