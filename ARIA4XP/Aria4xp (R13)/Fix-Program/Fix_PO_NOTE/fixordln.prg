LOCAL lcRequestServer
lcRequestServer = ALLTRIM(SUBSTR(SYS(0), 1, AT('#', SYS(0))-1))
loActivator = CREATEOBJECT("Aria.Utilities.RemoteCall.AriaActivator")
loA5ariaEnvironment = loActivator.GetRemoteObject("Aria.Environment.AriaEnviromentVariables" , lcRequestServer, 1500)
lcclients = 'Clients'
lcOutclients = 'Out_Clients'
=SQLSETPROP(0,"DispLogin",3)
lcsysflconnect = loA5ariaEnvironment.aria50SystemFilesConnectionStringOdbc
lnconnecthandle = SQLSTRINGCONNECT(lcsysflconnect)
IF lnconnecthandle <= 0
  MESSAGEBOX("Connection Level Error",16+512,'Connection Error!')
  RETURN .F.
ELSE
  Y = SQLEXEC(lnconnecthandle,'Select * From Clients',lcclients)
  IF Y > 0
    SELECT(lcclients)
    LOCATE
    IF EOF()
      MESSAGEBOX("Clients Table is empty",16+512,'Empty Table!')
      RETURN .F.
    ENDIF

    SELECT (lcclients)
    SCAN
      lcClient = ADDBS(ALLTRIM(cClientName))
      lcAria27Sys = ADDBS(ALLTRIM(Aria27Sys))
      lcAbsDataPath = ADDBS(ALLTRIM(cDataPath))
      USE ADDBS(Aria27Sys)+"SYCCOMP.DBF" SHARED IN 0
      SELECT SYCCOMP
      SCAN FOR !DELETED()
        WAIT WINDOW "Updating Client# "+lcClient + " - Company# "+ ALLTRIM(cComp_id) NOWAIT
        *lcDBFPath = ADDBS(ALLTRIM(cCom_dDir))
        var_cconserver = SYCCOMP.cconserver
        var_ccondbname = SYCCOMP.ccondbname
        var_cconpaswrd = SYCCOMP.cconpaswrd
        var_cconuserid = SYCCOMP.cconuserid

        TRY
          * Put your code here.
          LCCONNSTR = "Driver={SQL Server};server="+ALLTRIM(var_cconserver)+";DATABASE="+ALLTRIM(var_ccondbname );
            +";uid="+ALLTRIM(var_cconuserid )+";pwd="+ALLTRIM(var_cconpaswrd )

          LNCONHAND=SQLSTRINGCONNECT(LCCONNSTR)
          IF LNCONHAND>0

            lcSqlStr0 = "Update posln set note_mem = '' where note_mem IS NULL"
            lnResult = SQLEXEC(LNCONHAND,lcSqlStr0 )

            IF lnResult > 0
              WAIT WINDOW "Compnay: "+ ALLTRIM(SYCCOMP.cComp_id)+'-'+ALLTRIM(SYCCOMP.ccom_name)+" Updated sucessfully." NOWAIT
            ELSE
              WAIT WINDOW "Cannot Update Compnay: "+ ALLTRIM(SYCCOMP.cComp_id)+'-'+ALLTRIM(SYCCOMP.ccom_name) NOWAIT
            ENDIF
          ELSE
            MESSAGEBOX("Couldn't open file PO Line for Client# "+lcClient + " - Company# "+ ALLTRIM(cComp_id),16+512,"Open Table failed!") 
          ENDIF


        CATCH
          MESSAGEBOX("Couldn't open file PO Line for Client# "+lcClient + " - Company# "+ ALLTRIM(cComp_id)+" Error:"+MESSAGE() ,16+512,"Open Table failed!")
        ENDTRY
        *IF USED('posln')
        *  USE IN posln
        *ENDIF
      ENDSCAN
      IF USED('SYCCOMP')
        USE IN SYCCOMP
      ENDIF
    ENDSCAN
    MESSAGEBOX("All clients updated sucessfully.",64+512,"Updating completed.")
  ELSE
    MESSAGEBOX("Connection Level Error",16+512,'Connection Error!')
  ENDIF
ENDIF
