<%@ Language=VBScript %>
<%
Session("StoreValue")= Request.QueryString ("Store")

	if Session("StoreValue")="" or Session("StoreValue")= "MAIN" then
		Session("Store") = ""
		Session("StoreID")="MAIN"
	else

		Session("Has_Store")="Y"
		Session("Store")   = Session("StoreValue")
		Session("StoreID") = Session("StoreValue")
	end if 
'Response.Write Session("Store")&Session("StoreID")
'Response.End 

Response.Redirect "sorderh.asp?From="& request("From")
%>
