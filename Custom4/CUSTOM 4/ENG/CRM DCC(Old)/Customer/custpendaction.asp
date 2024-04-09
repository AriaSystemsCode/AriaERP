<%@ Language=VBScript %>
<%Response.Buffer=True%>
<html>
<head>
<meta NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<TITLE>CRM - Customer Pending</TITLE>
</head>
<body topmargin="0" leftmargin="0" marginheight="0" marginwidth="0" bgcolor="#aecae6">
<%
'Response.Write(request("seqnum"))
'Response.End 
IF trim(request("seqnum"))="" And (trim(cstr(request("modify")))<>"" or trim(cstr(request("remove")))<>"") Then
		response.redirect "custpending.asp"
Else
		Set conn=server.CreateObject("ADODB.connection")
		Set rs=server.CreateObject("ADODB.recordset")
		Set rs2=server.CreateObject("ADODB.recordset")
		conn.Open Application("SystemConnectionString")
		Set conn2=server.CreateObject("ADODB.connection")
		conn2.Open Application("DataConnectionString")
		sqls="select * from contact where ccont_id='"&Session("customerid")&"'"
		RS2.open sqls,conn2,1,3
		sqlstat="select  * from syschdul where cseqnumber='"&request("seqnum")&"'"
		rs.open sqlstat,conn ,1,3
		If trim(cstr(request("modify")))<>"" then
			Response.Redirect "custschedule.asp?SEQ="&request("seqnum")
		ElseIf trim(cstr(request("remove")))<>"" Then
						sqlS1="delete from syschdul Where cseqnumber='"&request("seqnum")&"'"
						Set RSTemp=Server.CreateObject("ADODB.Recordset")
						RSTemp.Open sqls1,conn,1,3
						response.redirect "custpending.asp"
		ElseIf 	trim(cstr(request("add")))<>""	Then 
			Response.Redirect "custpendadd.asp"
		End If
		Conn.Close
		Conn2.Close
		Set Conn2=nothing
		Set Conn=nothing%>	
			
<%End If%>

</body></html>