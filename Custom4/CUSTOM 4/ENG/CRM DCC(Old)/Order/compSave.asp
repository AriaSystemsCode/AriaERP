<%@ Language=VBScript %>
<%
'
'Response.Write Session("ID") 
'Response.end
Response.CacheControl  = "no-cache"

Response.AddHeader  "Pragma", "no-cache"

Response.Expires = -1
Response.Buffer = true
%>
<%

IF Trim(Session("ID")) = "" And Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 


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
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<Title>CRM - Remote Order - Order Saving</Title>
<link REL="stylesheet" HREF="../images/<%=session("Theme")%>/order.css" TYPE="text/css">
<SCRIPT LANGUAGE=javascript>
<!--
function order_conf()
{
//	document.location.replace;
		//if (confirm("Your order has been successfully saved in the system under this number: <%=request("order")%>. \n \n Do you want to display the order confirmation report now?"))
		if (confirm("Your order has been successfully saved in the system under this number: <%=Session("orderVal")%>. \n \n Do you want to display the order confirmation report now?"))
	{
		//window.location = "ordconfrpt.asp?OrderNo=<%=request("order")%>";
		window.location = "ordconfrpt.asp?OrderNo=<%=Session("orderVal")%>";
	}
}
//-->
</SCRIPT>

</head>
<!--body <%IF not Len(Trim(Session("rep")))>0 Then%>
onload="order_conf()"
<%End IF%>
-->
<body onload="order_conf()">
<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>

<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>

	<!-- ARD -->
	<TD colspan=13>
	<%IF Trim(Session("customerid")) <> ""  Then%>
		<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=session("rscust").fields("btname").value%></P>
	<%else%>
		&nbsp
	<%end if%>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>
<%Set RSTemp=nothing
  

End IF%>

<Table width=95% align=center height=50 border=1>
<TR>
<TD class=Title>Remote Order</TD>
</TR>
</Table><br>

<%IF compWork = "Y" Then%>

<br>
  
  <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr>
      <!--td width="100%" class=MessageFont>Your order has been successfully saved in the system under this number: <strong><%=Request("order")%></strong></td-->
      <td width="100%" class=MessageFont>Your order has been successfully saved in the system under this number: <strong><%=Session("orderVal")%></strong></td>      
    </tr>
  </table>

<%END IF
	Session("Season") = ""
	Session("Division") = ""
'wma resume next to avoid errors after payments 05/25/2004 start
on error resume next	
	if isobject(Session("RSLine")) then
		set Session("RSLine") = nothing
	end if
	if not Session("RSLine") is nothing then
		if Session("RSLine").state <> 0 then
			Session("RSLine").close
		end if
	end if
	Set Session("RSLine") = Nothing
	if isobject(Session("rsCharges")) then
		set Session("rsCharges") = nothing
	end if
	
	'Session.Abandon
	Session("RSLine")= connTemp.excute("select * from ordline where .F.")
	 ConnTemp.close
	 Set ConnTemp=nothing

on error goto 0
'wma resume next to avoid errors after payments 05/25/2004 start	
%>
</BODY>
</HTML>

