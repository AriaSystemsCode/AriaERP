<%
Session("rep") = Request("rep")
Session("customerid") = request("customerid")


Set conn = Server.CreateObject("ADODB.Connection")
conn.Open Application("DataConnectionString")

Set Session("RSCust") = server.CreateObject("ADODB.RecordSet")
strSql = "select * from customer where type+account+store like 'M"& Session("customerid") &"%'"
Session("RSCust").open  strSql,conn , 2, 4

Response.Redirect("sorderh.asp")
%>