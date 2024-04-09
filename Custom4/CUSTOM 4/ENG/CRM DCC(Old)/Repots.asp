<%@ Language=VBScript %>
<%
Response.Buffer = True
'check setup var to display OTS values or availablity
'if Session("ShowOTS") = "T" then
	Response.Redirect("ots/Repots.asp")
'else
	'Response.Redirect("ots/findstyle.asp")
'end if
%>