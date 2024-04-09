<%@ Language=VBScript %>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<title>Zoom</title>
</HEAD>
<BODY>

<%
'Make A connection to the SQL Server
Dim Conn
Set Conn = Server.CreateObject("ADODB.Connection")
'HDM [Start] use the Application variable that holds the connection string instead of this rubbish
'StrConnParam = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source=" & Trim(Session("SQLServer"))& ";UID="& Trim(Session("SqlUserName"))& ";PWD="&Trim(Session("SqlPassWord"))
StrConnParam = Application("SqlServer")
'HDM [End]
Conn.Open StrConnParam

Dim rsResponse
Set rsResponse = Server.CreateObject("ADODB.RecordSet")
strSQL = "Select * from suIssDT Where CIssueNo='"& Trim(Request.QueryString("strIssue")) &"' AND intLineNo="& CInt(Request.QueryString("intLine")) 
'Response.Write strSQL
rsResponse.Open strSQL,Conn
If rsResponse.EOF Then
	Response.Write("Error")
Else
	Response.Write(rsResponse("mRespAct"))
End If
'Release all objects
rsResponse.Close()
Set rsResponse = Nothing
Conn.Close()
Set Conn = Nothing
%>
</BODY>
</HTML>
