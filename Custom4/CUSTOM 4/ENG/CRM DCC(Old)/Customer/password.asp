<%@ Language=VBScript %>
<%Response.Buffer = true
 Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>
<%
if Trim(Session("ID")) = "" and trim(Session("customerid"))="" then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
	parent.location.href="../login.asp"
	</script>
<%end if

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	else
		custid = Ucase(Trim(Session("customerid")))
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF


if trim (Session("rep")) = "" then
	CurCust = Session("ID")
else 
	CurCust = Session("CustomerID")
end if
if Request.QueryString ("save") = "T" then
	Set cnnSQL = server.CreateObject("ADODB.Connection")
	cnnSQL.Open Application("SqlServer")
	Dim rsUser
	set rsUser = Server.CreateObject("ADODB.RecordSet")
	rsUser.Open "select * from syuuser where cuser_id = '" &trim(Session("customerid"))& "'",cnnSQL, 1,4
	
	'wma 
	'Prepare the user password[start]
	Dim objPriv
	set objPriv = Server.CreateObject("AriaWebSecurity.Privileges")	
	strPass = objPriv.GetPassword(Trim(Request.Form("txtPWD")))	

	if rsUser.EOF then ' add new
			Set objFoxConn = server.CreateObject("ADODB.Connection")
			objFoxConn.Open Application("DataConnectionString")
			
			Dim rsCustomer
			set rsCustomer = Server.CreateObject("ADODB.RecordSet")
			rsCustomer.Open "select * from customer where type+account+store like 'M"& ucase(trim(Session("customerid"))) &"%'",objFoxConn,1,4	 
			
			rsUser.AddNew
			rsUser("cuser_id")  = trim(rsCustomer("Account"))
			rsUser("cusr_name") = Trim(rsCustomer("Btname"))
			rsUser("cusr_pass") = strPass
			'rsUser("cwe_mail") = strEmail
			rsUser("cusr_levl") = "A"
			rsUser("profile")   = "C"
			rsUser.UpdateBatch
			
			'strSQL ="Insert Into syuuser"
			'strSQL = strSQL & "cuser_id,cusr_name, cusr_pass, cusr_levl, profile"
			'strSQL = strSQL & "Values ("
			'strSQL = strSQL & " '"& trim(rsCustomer("Account")) &"' "
			'strSQL = strSQL & " ,'"& Replace(trim(rsCustomer("Btname")),"'","''" &"' "
			'strSQL = strSQL & " ,'"& strPass &"' "
			'strSQL = strSQL & " ,'A' "			
			'strSQL = strSQL & " ,'C' "												
			'strSQL = strSQL & ")"
			'Response.Write strSQL
			'Response.End 
			
				
			rsCustomer.Close 
			set rsCustomer = nothing
			
			objFoxConn.Close
			set objFoxConn = nothing
	else		
		rsUser("cusr_pass") = strPass
		rsUser.UpdateBatch ()
		'Prepare the user password[end]
	end if
	
	rsUser.Close
	set rsUser = nothing		
	%>
	<script language="javascript">
		window.close();
	</script>
	<%		
	'wma
end if
%>	

<html>
<head>
<title>CRM - Customer Profile</title>
<meta http-equiv="Content-Type" content="text/html;">
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/customer.css">
<!-- #INCLUDE FILE="../common/format.asp" -->
<SCRIPT LANGUAGE=javascript>
<!--

function do_validate()
{
	//password
	if (document.chgPWD.txtPWD.value == '')
	{
		alert ("Please enter password!");
		document.chgPWD.txtPWD.focus();
		return false;
	}
	//confirm password
	if (document.chgPWD.txtCnfPWD.value == '')
	{
		alert ("Please enter password confirmation!");
		document.chgPWD.txtCnfPWD.focus();
		return false;
	}
	//password & confirm password must be the same
	if (document.chgPWD.txtPWD.value != document.chgPWD.txtCnfPWD.value)
	{
		alert ("Password and Confirm password must be the same!");
		document.chgPWD.txtCnfPWD.focus();
		return false;
	}

	return true;
}


//-->
</SCRIPT>
</head>

</body>
<Table border=0 width=95% align=center>
	<tr>
	<td class="Title">
		Change User Password
	</td>
	</tr>
</Table>
<div align="center">
<center>
<form name="chgPWD" action="password.asp?save=T" method="post">	
<table width=95%  border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
  <tr>
  	<td Align="left" valign="center" width="8%" class="dark_cell"><strong>Password</strong></td>
      <td Align="left" width="16%" class="light_cell">
  		<input type="password" name="txtPWD" size="30" maxlength="8" >
      </td>
 </tr>
 <tr>
  	<td Align="left" valign="center" width="8%" class="dark_cell"><strong>Confirm Password</strong></td>
      <td Align="left" width="16%" class="light_cell">
  		<input type="password" name="txtCnfPWD" size="30" maxlength="8">
      </td>
 </tr>
	 
 <tr>
			
  	<td colspan=4 class="dark_cell" align=center>
			
  			<input type="submit" value=" Save " id="savecust" name="savecust">
      
  	</td>
  </tr>
 </FORM>
</table></center>
</div>
</body>
</html>