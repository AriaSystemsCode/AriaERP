<%@ Language=VBScript %>
<%
Response.CacheControl  = "no-cache"
Response.AddHeader  "Pragma", "no-cache"
Response.Expires = -1
'Response.Expires = 0
Response.Buffer = true
%>

<%
Session("storevalue")=""
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'	Response.redirect "default.htm"
	Response.redirect "default.asp"
End if

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF


%>
<html>
<head>
<Title>CRM - Select Customer</Title>
<LINK rel="stylesheet" type="text/css" href="images/<%=Session("THEME")%>/common.css">
<SCRIPT LANGUAGE=javascript>
<!--
function SubmitNext()
{
	document.FORM2.action = 'orderhsave.asp';
	document.FORM2.submit()
}
//-->
</SCRIPT>

</head>
<body topmargin="0" leftmargin="0" marginheight="0" marginwidth="0" bgcolor="#aecae6">
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<P><BR><BR><BR><BR></P>

  <table border="0" width=95% cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%" ><font face=arial size=2 ><b>Choose <%=session("CustField")%></b></font></td>
    </tr>
  </table>
<form action="common/Repfindcustomer.asp?Cust=C" method="post" id="form1" name="form1">
<table bordercolor="#111111" border="1" width=95% style="border-collapse: collapse" align=center>
			<td Align="center" width="80%" class="dark_cell" >
			<font color=white><%=session("CustField")%> :</font><INPUT name=txtCustomer size="5"  maxlength="5" value="<%=Request("txtCustomer")%>">
			<font color=white>Name:</font><INPUT name=txtCustomerName size="20"  maxlength="30" value="<%=Request("txtCustomerName")%>">  
			<font color=white>Phone:</font><INPUT name=txtCustomerPhone size="15"  maxlength="15" value="<%=Request("txtCustomerPhone")%>">
			<%if Session("Authority")="Full" then%>
			<font color=white>Status:</font>
			<select name="selectStatus">
				<option value="ALL">ALL</option>
				<option value="A">Active</option>
				<option value="X">Cancelled</option>
				<option value="H">Hold</option>
				<option value="P">Potential</option>
			</select>
			<%end if%>
		</td>
		<td Align="left" class="dark_cell" >
			&nbsp;<input type="submit" name="B2" value="Get <%=session("CustField")%>">&nbsp;<input type="reset" name="B3" value="Reset"></td>
     </tr>

</table>
</form>
<BR><BR>
  <table border="0" width=95% cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%" align=center><font face=arial size=2 ><B>You have to select customer first</B></font></td>
    </tr>
  </table>
</body>
</html>
