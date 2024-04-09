<%@ Language=VBScript %>
<%
Response.Buffer = True
'session("ID") = ""
if session("ID") = "" then
	Session("ID") = Application("ID")
	Dim Conn
	Set Conn = server.CreateObject("ADODB.Connection")
	Conn.Open Application("DataConnectionString")

	Set Session("RSCust") = Server.CreateObject("ADODB.RecordSet")
	strsql = "Select * from customer where type+account+store like 'M" & Ucase(trim(Session("ID"))) & "%'"
'Response.Write strsql

	'WMA change Session("RSCust") to be updatable [Start]
	'Session("RSCust").open strsql,Conn
	Session("RSCust").open  strSql,conn , 2, 4
	
end if
'Response.Write session("ID")
'Response.End 
	'Response.Write "<font size=2>" &trim(Session("RSCust")("account"))
	'Response.End 
Response.Redirect("catalog.asp")'?ID=" & ID)

%>