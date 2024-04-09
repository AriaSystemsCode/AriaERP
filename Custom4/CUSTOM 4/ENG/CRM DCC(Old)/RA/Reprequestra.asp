<%@ Language=VBScript %>

<%Response.Buffer=true
rep = Session("rep")
id = Session("customerid")
IF len(request("RAStore")) >0 then
	store = Session("StoreValue")
else
end if
customer = Session("ID")

'wma
'Session.Abandon 
Set conn = server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

Set Session("rsReturnLine") = server.CreateObject("ADODB.recordset")
'strSql = "select * from retline where .f."
strSql = "select * from raline where .f."
Session("rsReturnLine").open  strSql, conn, 2, 4
'wma

Response.Redirect("repreturnadd.asp?customerid=" & id & "&rep=" & rep & "&store=" & Store & "&id=" & customer)
%>
