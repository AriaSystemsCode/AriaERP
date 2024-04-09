<%@ Language=VBScript %>
<%
Response.Buffer=true
IF Session("ID")="" And Session("rep")="" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 


IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF
set connTemp=server.createobject("ADODB.Connection")
set RSTemp=server.createobject("ADODB.Recordset")
connTemp.open Application("DataConnectionString")
sqlTemp="select * from customer where account='"&session("customerid")&"'"
RSTemp.open sqlTemp,connTemp,1,3

%>
<HTML>
<HEAD>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
<Title>CRM - Return Authorization Request - Saving R.A.</Title>

<SCRIPT LANGUAGE=javascript>
<!--
function ra_conf()
{
//	document.location.replace;
		if (confirm("Your RA has been successfully saved in the system under this number: <%=session("strseq")%>. \n \n Do you want to want to print RA request Now?"))
	{
	
		//window.location = "ordconfrpt.asp?OrderNo=<%=request("order")%>";
		window.location = "RetAuthRep.asp?RanoNo=<%=session("strseq")%>&viewer=ActiveX";
	}
}
//-->
</SCRIPT>

</head>
<body onload="ra_conf()">

<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><BR></p>
<%Else%>

<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>

	<TD colspan=13>
	<P>Your currently selected customer is <b><%=Session("customerid")%> - <%=RSTemp.fields("btname").value%></b></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	
</TR>
</table>
 <%Set RSTemp=nothing
  ConnTemp.close
  Set ConnTemp=nothing

End IF%>

<Table width=95% height=50 border=1 align=center>
<TR>
<TD class=title>Request R/A</TD>
</TR>
</Table>

	<%IF compWork = "Y" Then%>


<P>
  <table border="0" width=95% cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%">Your Return Authorization Request has been sucssefully saved in the system with this number: <strong><%=session("strseq")%></strong></td>
    </tr>
  </table>

<%End IF
'Freeing Sessions
Session("Division")=""
Session("Season")=""
%>
</BODY>
</HTML>

