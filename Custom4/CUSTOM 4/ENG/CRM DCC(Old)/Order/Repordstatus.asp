<%@ Language=VBScript %>
<%Response.Expires=-1%>
<%
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
	</script>	
<%END IF 

Response.Redirect("Ordstatus.asp")
%>
