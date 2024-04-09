<%@ Language=VBScript %>
<%
Response.Buffer = True
if session("ID") = "" then
	'Session("ID") = Application("ID")
	Dim Conn
	Set Conn = server.CreateObject("ADODB.Connection")
	Conn.Open Application("DataConnectionString")

	Set Session("RSCust") = Server.CreateObject("ADODB.RecordSet")
	strsql = "Select * from customer where type+account+store like'M" & Ucase(trim(Session("ID"))) & "%'"

	'WMA change Session("RSCust") to be updatable [Start]
	'Session("RSCust").open strsql,Conn
	Session("RSCust").open  strSql,conn , 2, 4
	'Session("ID")  = trim(Session("RSCust")("account"))
	
end if
Dim ID ' as string
ID = Session("ID")
strUser = Session("userID")
strName = Session("userName")
strProfile = session("styProfile")
strPrice = session("PriceCode")
Session("OrderFlag") = ""
strUserTokens = Session("UserTokens")
'Session.Abandon 
'if isobject(Session("RSLine")) then
'	Session("RSLine").close
'	set Session("RSLine") = nothing
'end if
Session("ID") = ID
Session("userID") = strUser
Session("userName") = strName
session("styProfile") = strProfile
session("PriceCode") = strPrice

Response.Redirect("ord2.asp?ID=" & ID)

%>