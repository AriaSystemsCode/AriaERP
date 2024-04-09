<%@ Language=VBScript %>
<%
Response.Redirect("../../PreOrder.asp?CustomerID=" + Request.QueryString("CustomerID") +  + "&WareHous=" + Session("WareHous"))
%>

