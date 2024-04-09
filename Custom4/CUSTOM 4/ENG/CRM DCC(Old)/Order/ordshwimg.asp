<%@ LANGUAGE="VBSCRIPT" %>
<%
if Trim(Session("ID")) = "" and Trim(Session("customerID")) = "" then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
	</script>	
<%END IF 

%>
<html>

<head>
<title>CRM - show Image</title>
<LINK REL=stylesheet HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</head>

<body>
<BR><BR><BR><BR>
<table border="0" width="80%" cellspacing="0" cellpadding="0" align="center">
  <tr>
		<td align="center"><p><img border="0" src="../styimg/<%=Response.Write(request("name"))%>.jpg"></p></td>
	</tr>
	<tr>
		<td align="center">
	<br><input type="button" value="Close" name="B3" onclick="window.close()">
	
	  </td>
	
	</tr>
</table>

</body>
</html>
