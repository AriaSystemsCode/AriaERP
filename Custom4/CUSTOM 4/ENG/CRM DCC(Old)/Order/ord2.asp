<%@ Language=VBScript %>
<%

Session("ID") = Request("ID")

Dim Conn
Set Conn = server.CreateObject("ADODB.Connection")
Conn.Open Application("DataConnectionString")

Set Session("RSCust") = Server.CreateObject("ADODB.RecordSet")
strsql = "Select * from customer where type+account+store like 'M" & Ucase(trim(Session("ID"))) & "%'"

'WMA change Session("RSCust") to be updatable [Start]
'Session("RSCust").open strsql,Conn
Session("RSCust").open  strSql,conn , 2, 4
'WMA change Session("RSCust") to be updatable [end]

Response.Redirect("sorderh.asp")

%>