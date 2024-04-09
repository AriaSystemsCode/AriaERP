_SCReen.Visible =.F.
LOCAL lcReServer
lcReServer = ALLTRIM(SUBSTR(SYS(0), 1, AT('#', SYS(0))-1))
loActivator = CREATEOBJECT("Aria.Utilities.RemoteCall.AriaActivator")
loA5ariaEnvironment = loActivator.GetRemoteObject("Aria.Environment.AriaEnviromentVariables" , lcReServer, 1500)
= SQLSETPROP(0, "DispLogin", 3)
lcSysFlConnect = loA5ariaEnvironment.Aria50SystemFilesConnectionStringODBC
lnconnecthandle = SQLSTRINGCONNECT(lcsysflconnect)
lnsqlget = SQLEXEC(lnconnecthandle, "Select 0 as lSelect,* From Clients  Order by CClientID", "CLIENTS")
IF lnsqlget > 0
   SELECT CLIENTS
   LOCATE 
   SCAN
     WAIT WINDOW "Updating Client " + ALLTRIM(CLIENTS.CCLIENTNAME) TIMEOUT 2
     USE ADDBS(ALLTRIM(CLIENTS.ARIA40SYS))+"SYDFILES.DBF" SHARED IN 0
     SELECT SYDFILES
     DELETE FOR INLIST(ALLTRIM(CFILE_NAM),'POFHDR','FABRIC','FABDYE')
     USE IN SYDFILES
     SELECT CLIENTS
   ENDSCAN
   WAIT WINDOW "Update Completed Successfully" TIMEOUT 2
ENDIF     