<%@ Language=VBScript %>
<%

Session("ID") = Request("ID")

Dim Conn
Set Conn = server.CreateObject("ADODB.Connection")
Conn.Open Application("DataConnectionString")
Set Session("RSCust") = Server.CreateObject("ADODB.RecordSet")
strsql = "Select * from customer where type+Account+store ='M"&Session("ID")&"'"
Session("RSCust").open strsql,Conn

Response.Redirect "returnadd.asp"
'Response.Redirect("")

%>