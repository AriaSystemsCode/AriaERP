<%@ Language=VBScript %>
<%

response.Redirect("Custom/WebPages/CustomerSelection.aspx?RepID=" + Session("RepID") + "&WareHous=" + Session("WareHous"))
%>

