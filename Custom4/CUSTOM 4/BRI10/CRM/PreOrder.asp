<%@ Language=VBScript %>
<%
session("customerid") = Request.QueryString("CustomerID")
session("ID") = Request.QueryString("CustomerID")
application("ID") = Request.QueryString("CustomerID")

if not isobject(Session("rsCust")) then
	set Session("rsCust") = server.CreateObject("adodb.recordset")
        custid = session("customerid")
	Session("rsCust").open "select * from Customer where Account='"&custid&"'", Application("DataConnectionString")
end if

Response.Redirect("custom/webpages/Order.aspx")
'Response.Redirect("webpages/Order.aspx")
%>