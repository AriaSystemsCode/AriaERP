<%@ Language=VBScript %>

<%
Response.Buffer = True
If Trim(Request.QueryString("id")) = "Data" Then
	Session("DataPath") = Request("hdnPath")
End IF

IF Trim(Request.QueryString("id")) = "System" Then
	Session("SystemPath") = Request("hdnPath")
End IF

IF Trim(Request.QueryString("id")) = "Logo" Then
	Set fso = Server.CreateObject("Scripting.FileSystemObject")

	strPath = Request.ServerVariables("APPL_PHYSICAL_PATH") & "Images\"
	Response.Write(Request("hdnPath"))
	
	fso.CopyFile Request("hdnPath"),strPath,True


	Session("LogoPath") = Request("file")
End IF

Response.Redirect("crmSetup.asp")

%>
