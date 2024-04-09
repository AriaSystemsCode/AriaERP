_Screen.Visible = .F.
lcSqlDictPth = ''
lnDispLogin = SQLGetprop(0,"DispLogin")
lnCountRecord = 0
Try
  loEnvObj = Createobject('Aria.Environment.AriaEnviromentVariables')
  If Type('loEnvObj') = 'O'
    lcsqlsysfilesconnectionstring = loEnvObj.aria50SystemFilesConnectionStringOdbc
    =SQLSetprop(0,"DispLogin",3)
    If !Empty(lcsqlsysfilesconnectionstring)
      lnConnHandle =Sqlstringconnect(lcsqlsysfilesconnectionstring)
      If lnConnHandle > 0
        Try
          lnRemResult = SQLExec(lnConnHandle,"Select * from Clients ","Clients")
          If lnRemResult > 0
            lcSqlDictPth = Clients.ARIA40SYS
          Endif
        Catch
        Endtry
        =SQLDisconnect(lnConnHandle)
      Endif
    Endif
  Endif
Catch
Endtry
= SQLSetprop(0,"DispLogin",lnDispLogin)
IF !EMPTY(lcSqlDictPth)
*Return lcSqlDictPth 
  STRTOFILE(lcSqlDictPth,'SqlDictPath.txt')
ENDIF
