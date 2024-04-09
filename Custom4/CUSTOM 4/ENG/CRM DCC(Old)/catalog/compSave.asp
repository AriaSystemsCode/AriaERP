<%@ Language=VBScript %>
<%
Response.Buffer=true
Response.ExpiresAbsolute = Now() - 1
Response.AddHeader "cache-control", "must-revalidate"
Response.AddHeader "cache-control", "Private"
Response.AddHeader "pragma", "no-cache"


IF Trim(Session("ID")) = "" And Trim(Session("rep"))= "" Then
'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%End if

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
End IF
set connTemp=server.createobject("ADODB.Connection")
set RSTemp=server.createobject("ADODB.Recordset")
connTemp.open Application("DataConnectionString")
sqlTemp="select * from customer where account='"&session("customerid")&"'"
RSTemp.open sqlTemp,connTemp,1,3

%>
<HTML>
<HEAD>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/Catalog.css" TYPE="text/css">
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<Title>CRM - Remote Order - Order Saving</Title>
<SCRIPT LANGUAGE=javascript>
<!--
function order_conf()
{
	if (confirm("Your order has been successfully saved in the system under this number: <%=request("order")%>. \n \n Do you want to display the order confirmation report now?"))
	{
		window.location = "../order/ordconfrpt.asp?OrderNo=<%=request("order")%>";
	}
}
//-->
</SCRIPT>

</head>
<body <%IF Trim(Session("rep"))="" Then%>
onLoad="order_conf()"
<%End IF%>
>

<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><BR><BR><BR></p>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><BR></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
<!--	
	<TD colspan=14 background="../images/bground.gif">

	<font size="2" face="Arial">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
		<%set connTemp=server.createobject("ADODB.Connection")
			set RSTemp=server.createobject("ADODB.Recordset")
			connTemp.open Application("DataConnectionString")
			sqlTemp="select * from customer where account='" & session("customerid") & "'"
			RSTemp.open sqlTemp,connTemp,1,3
			%>
              Your currently selected customer is <%=Session("customerid")%>
             <%response.write " - "&RSTemp("btname")%></font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="repcust.asp" ><font color=white size="2" face="Arial"><b>Get Customer</b></font></a>
	-->
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected customer is <b><%=Session("customerid")%> - <%=RSTemp.fields("btname").value%></b></P>
	</TD>
	<TD align=right><a href="repcust.asp"><b>Get Customer</b></a></TD>
	<!-- ARD -->
</TR>
</table>
 <%Set RSTemp=nothing
  ConnTemp.close
  Set ConnTemp=nothing
  End IF%>
<Table width=95% height=50 align=center border=1>
<TR>
<TD class=title>Catalog</TD>
</TR>
</Table>


<%IF compWork = "Y" Then%>
<br>
  <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%" class=MessageFont>
      Your order has been successfully saved in the system under this number: <strong><%=Request("order")%></strong>
        </td>
    </tr>
  </table>

<%END IF%>

</BODY>
</HTML>

