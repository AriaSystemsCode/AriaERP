PARAMETERS lcDataPath
IF TYPE('lcDataPath')#'C' AND PARAM()=0
  lcDataPath = GETDIR('','Select Data Dir.')
ENDIF  
IF EMPTY(lcDataPath)
  RETURN
ENDIF
USE IN (IIF(USED('CODES'),'CODES',0))
lcError = ON('ERROR')
llError = .F.
ON ERROR llError = .T.
USE (lcDataPath+'CODES') IN 0 ALIAS CODES EXCL
ON ERROR &lcError
IF llError
  WAIT 'Can not open data file' window
  RETURN
ENDIF
lcTmpCodes = ("X"+SUBSTR(SYS(2015),4))
lcTmpCursor = ("X"+SUBSTR(SYS(2015),4))
DIME laIndex[3,2]
laIndex[1,1] = 'cdefcode+crltfield+cfld_name'
laIndex[1,2] = 'Idrltfname'
laIndex[2,1] = 'cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam'
laIndex[2,2] = 'Ccode_no'
laIndex[3,1] = 'cdefcode+ccode_no+crltfield+cfld_name'
laIndex[3,2] = 'Codes'
IF TYPE('CODES.CCOMP_ID')='C' AND TYPE('CODES.CDEFCODE')='U'
  IF TYPE('CODES.CDEFCODE')='U'
    SELECT IIF(EMPTY(CCOMP_ID) AND CFLD_NAME<>'ALL ' AND CFLD_NAME<>'N/A ','D','N') AS CDEFCODE,*;
           FROM (lcDataPath+'CODES');
           INTO DBF (lcDataPath+lcTmpCodes)
    SELECT (lcTmpCodes)
    PRIVATE lnCount
    FOR lnCount = 1 TO ALEN(laIndex,1)
       INDEX ON &laIndex[lnCount,1] TAG &laIndex[lnCount,2] ADDITIVE
    ENDFOR
    USE IN CODES
    USE IN (lcTmpCodes)
    ERASE (lcDataPath+'CODES.DBF')
    ERASE (lcDataPath+'CODES.FPT')
    ERASE (lcDataPath+'CODES.CDX')
    RENAME (lcDataPath+lcTmpCodes+'.DBF') TO (lcDataPath+"CODES.DBF")
    *RENAME (lcDataPath+lcTmpCodes+'.FPT') TO (lcDataPath+"CODES.FPT")
    RENAME (lcDataPath+lcTmpCodes+'.CDX') TO (lcDataPath+"CODES.CDX")
  ENDIF
ELSE
  SELECT   *,COUNT(CFLD_NAME+CCODE_NO+CRLTFIELD) AS NCOUNTREC;
           FROM CODES GROUP BY CFLD_NAME,CCODE_NO,CRLTFIELD ORDER BY CFLD_NAME;
           INTO CURSOR (lcTmpCursor)
  IF TYPE('CODES.CDEFCODE')='U'
  SELECT 'N' AS CDEFCODE,*;
           FROM (lcDataPath+'CODES');
           INTO DBF (lcDataPath+lcTmpCodes)
  ELSE         
    SELECT * ;
             FROM (lcDataPath+'CODES');
             INTO DBF (lcDataPath+lcTmpCodes)
  ENDIF
  SELECT (lcTmpCodes)
  REPLACE ALL CDEFCODE WITH 'N'
  PRIVATE lnCount 
  FOR lnCount = 1 TO ALEN(laIndex,1)
     INDEX ON &laIndex[lnCount,1] TAG &laIndex[lnCount,2] ADDITIVE
  ENDFOR
  SET ORDER TO TAG Ccode_no
  SELECT (lcTmpCursor)
  SCAN FOR NCOUNTREC=2 AND CRLTFIELD ='N' 
    IF SEEK('N'+cfld_name+ccode_no,lcTmpCodes)
      REPLACE &lcTmpCodes..CDEFCODE WITH 'D'
    ENDIF
  ENDSCAN
    USE IN CODES
    USE IN (lcTmpCodes)
    ERASE (lcDataPath+'CODES.DBF')
    ERASE (lcDataPath+'CODES.FPT')
    ERASE (lcDataPath+'CODES.CDX')
    RENAME (lcDataPath+lcTmpCodes+'.DBF') TO (lcDataPath+"CODES.DBF")
    *RENAME (lcDataPath+lcTmpCodes+'.FPT') TO (lcDataPath+"CODES.FPT")
    RENAME (lcDataPath+lcTmpCodes+'.CDX') TO (lcDataPath+"CODES.CDX")

ENDIF
