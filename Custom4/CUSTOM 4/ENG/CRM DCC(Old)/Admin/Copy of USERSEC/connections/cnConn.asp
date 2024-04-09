<%
'WAL_05/17/2004 add code tp read values in setup file[start]
'get values seved in the file 
Dim objTxtFile
Set objTxtFile = Server.CreateObject("Scripting.FileSystemObject")
Dim strAppPath
Dim strFilePath

strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))

If Right(strAppPath,1) = "\" Then
	strFilePath = "admin\crmsetup\setup\setup.txt"
Else
	strFilePath = "\admin\crmsetup\setup\setup.txt"
End If
Set objTxtFile = objTxtFile.OpenTextFile(strAppPath & strFilePath,1)

strFile = objTxtFile.ReadAll
Dim strArSetups
	strArSetups = Split(strFile," AND ", -1 , 1)

	'Declare Vartiables To Hold the temporary key and values
	Dim strKey
	Dim strValue
	For intLoop = 0 To UBound(strArSetups)
		strArKeyValue = Split(strArSetups(intLoop) , "=" , -1 , 1)
		Application(strArKeyValue(0)) = strArKeyValue(1)
		Session(strArKeyValue(0)) = strArKeyValue(1)
	Next
	Application("DataConnectionString") = "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("DataPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"
	Application("SystemConnectionString") = "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("SystemPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"
objTxtFile.Close
Set objTxtFile = Nothing
'WAL_05/17/2004 add code tp read values in setup file[end]
Dim MM_cnConn_STRING
'constr = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source=" & Trim(Session("SQLServer")) & ";UID="& Trim(Session("SqlUserName"))& ";PWD="&Trim(Session("SqlPassWord"))
'MM_cnConn_STRING = constr'Application("SqlServer") '"Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=websecurity;Data Source=hossam;uid=sa;pwd=aria"
If Session("DBType") = "ORACLE" then
	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=OraOLEDB.Oracle;Data Source="&Trim(session("SQLServer"))&";user id=" & Trim(Session("SqlUserName")) & ";password=" & Trim(Session("SqlPassWord")) & ""	
Else
	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog="&Trim(session("DBName"))&";Data Source="&Trim(session("SQLServer"))&";uid=" & Trim(Session("SqlUserName")) & ";pwd=" & Trim(Session("SqlPassWord")) & ""
end if
MM_cnConn_STRING = Application("SqlServer")	
%>
