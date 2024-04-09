<%@ Language=VBScript %>
<%Response.buffer=true%>
<html>
<head>
<title>CRM - Contact Management</title>
<meta http-equiv="Content-Type" content="text/html;">
<LINK rel="stylesheet" type="text/css" href="images/<%=Session("THEME")%>/common.css">

</head>
<%If trim(Session("customerid"))="" then
	response.redirect "repcust.asp"
End If%>
<SCRIPT LANGUAGE=javascript>
<!--
function SubmitNext()
{
	document.FORM2.action = 'orderhsave.asp';
	document.FORM2.submit()
}
//-->
</SCRIPT>

<body>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><BR></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>

<TR>
	
<!--	
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected <%=Session("CustField")%> is <%=Session("customerid")%> - <%=session("rscust")("btname")%></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>


</table>
<br>
<br>
<div align="center">
  <center>
<TABLE width="35%" border=1 bordercolor="#000000" style="border-collapse:collapse" cellpadding="0" cellspacing="0" class="btable">
    <TR>
      <TD class="light_cell">
        <li><A href="Customer/custpending.asp" >
         Pending Activities </a></li>
      </TD></TR>
	<TR>
      <TD class="light_cell"> 
        <li><A href="Customer/custhistory.asp">
           History Activities</a></li>
      </TD></TR>
	</table>

  </center>
</div>

</html>
</body>