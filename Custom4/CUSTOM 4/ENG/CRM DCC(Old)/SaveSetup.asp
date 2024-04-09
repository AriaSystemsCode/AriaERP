<%@ Language=VBScript %>

<%


Dim strAppPath
Dim strFilePath

strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))

If Right(strAppPath,1) = "\" Then
	strFilePath = "admin\crmsetup\setup\setup.txt"
Else
	strFilePath = "\admin\crmsetup\setup\setup.txt"
									
End If
'Response.Write(strAppPath & strFilePath)

Dim objFileSystem
Dim objTextFile
'Response.Write(strAppPath & strFilePath)
Set objFileSystem = Server.CreateObject("Scripting.FileSystemObject")
Set objTextFile = objFileSystem.CreateTextFile(strAppPath & strFilePath , True)

strLine = ""
strLine = strLine & "CompanyID=" & Request("txtCompID")
strLine = strLine & " AND " & "DataPath=" & Request("flDataFiles")
strLine = strLine & " AND " & "SystemPath=" & Request("flSystemPath")
strLine = strLine & " AND " & "SQLServer=" & Request("txtServerName")
strLine = strLine & " AND " & "UserCanEdit=" & Request("lstEditProfile")
strLine = strLine & " AND " & "NotificationAddress=" & Request("txtNotifyAddress")
strLine = strLine & " AND " & "ConfirmationAddress=" & Request("txtConfirmAddress")
strLine = strLine & " AND " & "OTSSetup=" & Trim(Request("lstOTS"))
strLine = strLine & " AND " & "PasswordField=" & Request("lstPassField")


objTextFile.WriteLine(strLine)
objTextFile.Close
Set objFileSystem = Nothing

	Dim objFile
	Set objFile = Server.CreateObject("Scripting.FileSystemObject")



	strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))

	If Right(strAppPath,1) = "\" Then
		strFilePath = "admin\crmsetup\setup\setup.txt"
	Else
		strFilePath = "\admin\crmsetup\setup\setup.txt"
										
	End If


	Set objTxtFile = objFile.OpenTextFile(strAppPath & strFilePath,1)
	Dim strLine

	strFile = objTxtFile.ReadAll
	
	Dim strArSetups
	strArSetups = Split(strFile," AND ", -1 , 1)

	'Declare Vartiables To Hold the temporary key and values
	Dim strKey
	Dim strValue

	For intLoop = 0 To UBound(strArSetups)
		strArKeyValue = Split(strArSetups(intLoop) , "=" , -1 , 1)
		Session(strArKeyValue(0)) = strArKeyValue(1)
	Next

Response.Redirect("crmSetup.asp")
'Application("CompanyID")

'Application("DataPath")

'Application("SystemPath")

'Application("UserCanEdit")

'Application("NotificationAddress")




%>

