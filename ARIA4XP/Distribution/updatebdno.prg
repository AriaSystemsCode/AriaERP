*** Update Aria4XP Build#
_SCreen.Visible =.F.
SET CPDIALOG OFF 
SET DELETED ON
lcDef = ADDBS(FULLPATH(""))
IF FILE(lcDef +'ErrorLog.txt')
  ERASE (lcDef +'ErrorLog.txt')  
ENDIF
loa5ariaenvironment = CREATEOBJECT('Aria.Environment.AriaEnviromentVariables')
= SQLSETPROP(0, "DispLogin", 3)
lcsysflconnect = loa5ariaenvironment.aria50systemfilesconnectionstringodbc
lnconnecthandle = SQLSTRINGCONNECT(lcsysflconnect)
lnsqlget = SQLEXEC(lnconnecthandle, "Select 0 as lSelect,* From Clients  Order by CClientID", "CLIENTS")
IF lnsqlget > 0
  SELECT CLIENTS
  SCAN
    lfComPareMenu(CLIENTS.ARIA27SYS)
  ENDSCAN  
ENDIF
MESSAGEBOX('Unlocking process done, please check log file:'+lcDef +'ErrorLog.txt')


FUNCTION lfComPareMenu
LPARAMETERS lcSysFiles
*
SET DELETED ON 
TRY 
lcSysFiles = ADDBS(ALLTRIM(lcSysFiles))
CATCH 
  lcSysFiles = ''
ENDTRY 
IF EMPTY(lcSysFiles) OR !FILE(lcSysFiles +'SYCATRIB.DBF') 
  STRTOFILE('Client:'+CLIENTS.CClientID+"   File:"+lcSysFiles +'SYCATRIB.DBF'+"  is not found"+CHR(13)+CHR(10),lcDef +'ErrorLog.txt',1)
  RETURN 
ENDIF 
USE lcSysFiles +'SYCATRIB.DBF' SHARED IN 0
SELECT SYCATRIB
LOCATE FOR sycatrib.cproperty = 'CBUILD'
IF FOUND()
  REPLACE SYCATRIB.cpropvalue WITH "265"
ENDIF

USE IN SYCATRIB