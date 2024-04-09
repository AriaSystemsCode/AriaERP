*Fix program to Fix the File MDINVNTL for Material Batches
CLOSE ALL 
lcA27SysFilesPath =GETDIR('','Select A27 Sysfiles Folder')
IF EMPTY(lcA27SysFilesPath) OR !DIRECTORY(lcA27SysFilesPath)
  MESSAGEBOX('Invalid SysFiles Directory')
  RETURN .F.
ENDIF 
lcA27SysFilesPath = ADDBS(lcA27SysFilesPath)
TRY  
  USE (lcA27SysFilesPath+'SyCCOmp.DBF') SHARED ORDER 1 IN 0 
CATCH 
  QUIT 
ENDTRY  
SELECT SyCCOmp
SCAN FOR lrunFromA4
  lnMajorLen  = 0
  lnNonMaj  = 0
  lcSeparate = ''
  lcDataDir = ADDBS(ALLTRIM(SyCCOmp.ccom_ddir))
  IF !DIRECTORY(lcDataDir)
    LOOP 
  ENDIF 
  USE (lcDataDir+'icistru.DBF') IN 0 ORDER 1
  =SEEK('M'+'1','icistru')
  lnMajorLen = icistru.nisegsize 
  lcSeparate = icistru.cisegsepr 
  =SEEK('M'+'2','icistru')
  lnNonMaj = icistru.nisegsize 
  USE IN 'icistru'
  IF lnMajorLen  = 0 OR  lnNonMaj  = 0
    LOOP 
  ENDIF 
  lcConnStr = "Driver={SQL Server};server="+ALLTRIM(SyCCOmp.CCONSERVER)+";DATABASE="+ALLTRIM(SyCCOmp.CCONDBNAME)+;
                ";uid="+ALLTRIM(SyCCOmp.CCONUSERID)+";pwd="+ALLTRIM(SyCCOmp.CCONPASWRD)
  lnConnHandle =SQLSTRINGCONNECT(lcConnStr) 
  IF lnConnHandle <= 0
    LOOP 
  ENDIF 
  USE (lcDataDir+'MDINVNTL.DBF') IN 0 
  SELECT MDINVNTL
  LOCATE
  SCAN FOR CBATTYPE = 'M'
    lnExec = SQLEXEC(lnConnHandle ,"Select Style From ITEM WHERE STYLE ='"+MDINVNTL.STYLE+"'",'TmpItem')
    IF lnExec > 0 
      SELECT TmpItem
      LOCATE
      IF EOF()
        SELECT MDINVNTL
        lcFullStyle =  PADR(SUBSTR(MDINVNTL.STYLE,1,lnMajorLen),lnMajorLen)+lcSeparate+PADR(MDINVNTL.COLOR,lnNonMaj)
        REPLACE STYLE WITH lcFullStyle
      ENDIF  
      USE IN TmpItem
    ENDIF 
    SELECT MDINVNTL
  ENDSCAN 
  =SQLDISCONNECT(lnConnHandle)
  SELECT SyCCOmp
ENDSCAN 
CLOSE ALL 