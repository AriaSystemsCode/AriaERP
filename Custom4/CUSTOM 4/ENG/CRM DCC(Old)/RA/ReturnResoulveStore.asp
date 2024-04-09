<%@ Language=VBScript %>
<%
Response.Buffer=True
If  len(Trim(Session("rep"))) >0 Then
	Session("StoreValue")= Request("Store")
	Response.Redirect ("reprequestra.asp?RAStore=1")
End if

If  len(Trim(Session("ID"))) >0 Then
	Session("StoreValue")= Request("Store")
	Response.Redirect ("returnadd.asp")
End if

%>