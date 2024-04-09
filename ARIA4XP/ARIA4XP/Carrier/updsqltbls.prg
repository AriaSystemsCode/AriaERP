** A27 installation folder
lcMsg = 'This program will all the tables '+ CHR(13)
lcMsg = lcMsg + 'CARRIER_LOGIN_INFORMATION_T'+ CHR(13)
lcMsg = lcMsg + 'CARRIER_T'+ CHR(13)
lcMsg = lcMsg + 'to the sql databases used by aria'

lcA27 = GETDIR('','A27 installation folder')
IF EMPTY(lcA27)
  RETURN
ENDIF

SET STEP ON 
lcUpdateSql = FILETOSTR('updsqltbls.sql')

CD (lcA27)
IF !FILE('CLIENTS.DBF')
  RETURN
ENDIF

SET DELETED ON

USE CLIENTS.DBF
LOCATE
SCAN
  lcPath = ADDBS(ALLTRIM(CDATAPATH))+'SYSFILES\'
  SELECT 0
  USE (lcPath+'SYCCOMP')
  SCAN FOR LRUNFROMA4
    lnConn = SQLSTRINGCONNECT('driver=sql server;server='+ALLTRIM(CCONSERVER)+';database='+ALLTRIM(CCONDBNAME)+';uid='+ALLTRIM(CCONUSERID)+';pwd='+ALLTRIM(CCONPASWRD))
    IF lnConn>0
      lcDB = ALLTRIM(CCONDBNAME)
      lcExec = lcUpdateSql
      lnExec=SQLEXEC(lnConn,lcExec)
    ENDIF 
    
  ENDSCAN
ENDSCAN