lcdatadir=GETDIR()

WAIT 'Fixing EDI Error Header File...' WINDOW NOWAIT
USE (lcdatadir+'EdiErorH') ORDER TAG EdiErorH EXCL
SELECT cFileCode,cIntchgSeq,cPartCode,COUNT(*) AS nCount FROM EdiErorH ;
WHERE !DELETED() GROUP BY cFileCode,cIntchgSeq,cPArtCode HAVING COUNT(*)  > 1 INTO CURSOR TempFix
SELECT TempFix
SCAN
  lcFileCode  = cFileCode
  lcIntchgSeq = cIntchgSeq
  lcPartCode = cPartCode
  WAIT 'Fixing file '+lcFileCode+'...' WINDOW NOWAIT
  SELECT EDIERORH
  =SEEK(lcFileCode)  
  LOCATE REST WHILE CFILECODE+SEGID = lcFileCode ;
  FOR cIntchgSeq = lcIntchgSeq AND cPartCode = lcPartCode
  SKIP
  DELETE REST WHILE CFILECODE+SEGID = lcFileCode ;
  FOR cIntchgSeq = lcIntchgSeq AND cPartCode = lcPartCode
  =SEEK(lcFileCode)  
  LOCATE REST WHILE CFILECODE+SEGID = lcFileCode ;
  FOR cIntchgSeq = lcIntchgSeq AND cPartCode = lcPartCode
  REPLACE cErrSeqnce WITH '1'
ENDSCAN
WAIT 'Reindex EDI Error Header File....' WINDOW NOWAIT
SELECT EdiErorH
PACK
USE
USE IN TempFix

WAIT 'Fixing EDI Error Details File...' WINDOW NOWAIT
USE (lcdatadir+'EdiErorD') ORDER TAG EDIERORD EXCL
SELECT cFileCode,cIntchgSeq,cPartCode,cGroupSeq,COUNT(*) AS nCount FROM EdiErorD ;
WHERE !DELETED() GROUP BY cFileCode,cIntchgSeq,cPArtCode,cGroupSeq HAVING COUNT(*)  > 1 INTO CURSOR TempFix
SELECT TempFix
SCAN
  lcFileCode  = cFileCode
  lcIntchgSeq = cIntchgSeq
  lcGroupSeq  = cGroupSeq
  lcPartCode = cPartCode
  WAIT 'Fixing file '+lcFileCode+'...' WINDOW NOWAIT

  SELECT EDIERORD
  =SEEK(lcFileCode+lcPartCode+lcIntchgSeq)
  LOCATE REST WHILE CFILECODE+CPARTCODE+CINTCHGSEQ = lcFileCode+lcPartCode+lcIntchgSeq ;
              FOR   cGroupSeq = lcGroupSeq
  SKIP
  DELETE REST WHILE CFILECODE+CPARTCODE+CINTCHGSEQ = lcFileCode+lcPartCode+lcIntchgSeq ;
              FOR   cGroupSeq = lcGroupSeq
  =SEEK(lcFileCode+lcPartCode+lcIntchgSeq)
  LOCATE REST WHILE CFILECODE+CPARTCODE+CINTCHGSEQ = lcFileCode+lcPartCode+lcIntchgSeq ;
              FOR   cGroupSeq = lcGroupSeq
  REPLACE cErrSeqnce WITH '1'
ENDSCAN
WAIT 'Reindex EDI Error Details File....' WINDOW NOWAIT
SELECT EdiErorD
PACK
USE
USE IN TempFix
WAIT CLEAR