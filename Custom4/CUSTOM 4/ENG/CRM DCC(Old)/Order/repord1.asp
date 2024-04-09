<%Response.Buffer=true%>
<%
Dim rep,customerid ' as string
rep = Session("rep")
customerid = Session("customerid")

'wma
'Session.Abandon 
Set conn = server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

Set Session("RSLine") = server.CreateObject("ADODB.recordset")
strSql = "select * from ordline where .f."
Session("RSLine").open  strSql, conn, 2, 4
'wma end 
	

Response.Redirect("repord2.asp?rep=" & rep & "&customerid=" & customerid)

%>
