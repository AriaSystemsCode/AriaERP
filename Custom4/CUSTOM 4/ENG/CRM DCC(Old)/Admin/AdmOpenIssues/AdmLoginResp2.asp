<%@ Language=VBScript %>
<%Response.Buffer = true%>
<%if Trim(Session("AdmID"))="" then
	Response.Redirect "Default.asp"
end if
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
</HEAD>
<BODY>
<%
'Connection to db
set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open "dsn=TrackSys;"

'Adminstrator Recordset
set rsAdm = Server.CreateObject("ADODB.RecordSet")
'Adm usr/pwd Query
if Trim(Request.Form("txtUserID"))="" OR Trim(Request.Form("txtPwd"))="" then
	Response.Write "Enter User/Pwd"
else
			strSQL = "SELECT * FROM Syuuser WHERE Cuser_id ='" & Trim(Ucase(Request.Form("txtUserID"))) & "' AND Cusr_pass='" & Trim(Ucase(Request.Form("txtPwd"))) & "'"
			Response.Write strSQL
			rsAdm.Open strSQL , Conn
			do while not rsAdm.EOF
			'Response.Write rsAdm("Cuser_id") & "<BR>"
			rsAdm.MoveNext()
			loop
			if rsAdm.BOF and rsAdm.EOF then
				Response.Write "<BR>Invalid user name and password <BR>"
			else
				Session("AdmID") = Request.Form("txtUserID")
				'Response.Write "<BR>Exist<BR>"
				Response.Redirect "ListIssues.asp"
			end if
end if
%>
</BODY>
</HTML>
