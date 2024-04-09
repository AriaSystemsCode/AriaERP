<%@ Language=VBScript %>
<%
if session("ID") = "" then
	Session("ID") = Application("ID")
	Dim Conn
	Set Conn = server.CreateObject("ADODB.Connection")
	Conn.Open Application("DataConnectionString")

	Set Session("RSCust") = Server.CreateObject("ADODB.RecordSet")
	strsql = "Select * from customer where type+account+store like'M" & Ucase(trim(Session("ID"))) & "%'"

	'WMA change Session("RSCust") to be updatable [Start]
	'Session("RSCust").open strsql,Conn
	Session("RSCust").open  strSql,conn , 2, 4
	'Session("ID")  = trim(Session("RSCust")("account"))
	'Response.Write "<font size=2>" & Session("ID")

end if
if Trim(Session("ID")) = "" then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 

%>
<%
	Dim ID ' as string
	ID = Session("ID")
	Session.Abandon 

	Response.Redirect("returnInit.asp?ID=" & ID)
%>