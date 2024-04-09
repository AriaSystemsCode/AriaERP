<%@ Language=VBScript %>
<%Response.Buffer = true%>
<%
'*******************************************************************************
'Page Name:  CustUsers.asp
'Date     :  03/08/2003
'Developer:  mms (Mai Maged)
'modified :  by wal 23/5/2005 to work for crm customer users >> issue 127795
'Purpose  :  Allows customer to add new user & edit properties of existing users.
'********************************************************************************
%>
<html>
<head>
<title>Users List</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">

<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
</head>
<%
dim cnnSQL
set cnnSQL = server.CreateObject ("ADODB.Connection")
cnnSQL.Open Application("SqlServer") 
'check user privilge to view this page
Dim strMsg
strMsg = ""
If Trim(strMsg) <> "" then
	Response.Write strMsg
	Response.End 
Else
	dim strCust
	strCust  = request("UserID")
	
	'wal_ check if i m deleting a user
	if Request.QueryString ("dele") = "T" then
		strSql = "Delete From syuuser Where profile ='"& Trim(strCust) & "' and cuser_id='"& trim(request("ID")) &"'"
		cnnSQL.Execute (strSql)
	end if
'Get all users for the selected customer
	Dim strSql, rsUsers
	set rsUsers = Server.CreateObject("ADODB.RecordSet")
	strSql = "Select * From syuuser Where profile ='"& Trim(strCust) & "' ORDER BY cusr_name ASC"
	rsUsers.Open strSql,cnnSQL
'wal_check to add warehouse if exists
	set rsWare  = server.CreateObject ("ADODB.Recordset")
	rsware.Source  = "select * from customerwarecode where ccustid='" & Trim(request("UserID")) & "'"
	rsware.ActiveConnection = Application("SqlServer") 
	rsware.CursorType = 1
	rsware.LockType  = 3
	rsware.Open 
	if not rsware.eof then		
		if not isnull(rsware("cwarecode"))then 
			strWareCode = rsware("cwarecode")
		end if
		'wal_130731 get style group too
		if not isnull(rsware("cStyleGroup")) then
			strStyGroup = rsware("cStyleGroup")
		end if
	end if
	rsware.close
'mms - Get all users names and make lists of them each list contains 24 names - 24/8/2003 [start]
	Dim strNames, arrNamesList, intListNO
	if rsUsers.RecordCount > 0 then
		rsUsers.MoveFirst 
		intListNO = Int(rsUsers.RecordCount / 24) + 1
		Redim arrNamesList(intListNO - 1)
		for i = 0 to UBound(arrNamesList)
			strNames= ""
			for j = (i*24)+1 to (i+1)*24
				if j <= rsUsers.RecordCount then
					strNames = strNames & "','" & Trim(rsUsers("cusr_name"))
					rsUsers.MoveNext
				Else
					exit for
				End if
			Next
			strNames = mid(strNames,3)
			arrNamesList(i) = strNames
		Next
		rsUsers.MoveFirst 
	End if
'mms - Get all users names and make lists of them each list contains 24 names - 24/8/2003 [end]
'wal_131300 get the price codes
dim rsPrice
set rsPrice  = server.CreateObject ("ADODB.recordset")
dim cnnDB
set cnnDB=server.CreateObject("ADODB.connection")
cnnDB.Open Application("DataConnectionString")

' Get the Currency of this Company
CompanyID = Session("CompanyID")

strgetCompanyCode = "select CcurrCode  from Syccomp where Ccomp_id = '" & Trim(CompanyID) & "'"
	
set rsgetCompanyCode = server.CreateObject("adodb.recordset")
set rsgetCurrency = server.CreateObject("adodb.recordset")
set rsgetCurrencyAlign = server.CreateObject("adodb.recordset")
set SysConn = server.CreateObject("adodb.connection")
SysConn.Open Application("SystemConnectionString")

rsgetCompanyCode.Open strgetCompanyCode , SysConn ,2 ,4 
if not rsgetCompanyCode.EOF then
	rsPrice.Open "select * from cstprich where cCurrcod='" &Trim(rsgetCompanyCode.Fields("CcurrCode").Value)& "' order by priccode",cnnDB,1,3
else
	rsPrice.Open "select * from cstprich order by priccode",cnnDB,1,3
end if
%>
<SCRIPT LANGUAGE=javascript>
<!--

//function to validate the form inputs
function validate()
{
	//ID
	if (document.form1.txtID.value == '')
	{
		alert ("Please enter user ID!");
		document.form1.txtID.focus();
		return false;
	}
	
	//name
	if (document.form1.txtName.value == '')
	{
		alert ("Please enter user name!");
		document.form1.txtName.focus();
		return false;
	}
		
	
	//email
	if (document.form1.txtEmail.value != '')
	{
		if (!((document.form1.txtEmail.value.indexOf('@') != -1) && (document.form1.txtEmail.value.indexOf('.')!=-1)))
		{
			alert("Please enter valid e-mail address.");
			document.form1.txtEmail.focus();
			return false;
		}	
	}
	//password
	if (document.form1.txtPass.value == '')
	{
		alert ("Please enter password!");
		document.form1.txtPass.focus();
		return false;
	}
	//confirm password
	if (document.form1.txtConfPass.value == '')
	{
		alert ("Please enter password confirmation!");
		document.form1.txtConfPass.focus();
		return false;
	}
	//password & confirm password must be the same
	if (document.form1.txtPass.value != document.form1.txtConfPass.value)
	{
		alert ("Password and Confirm password must be the same!");
		document.form1.txtConfPass.focus();
		return false;
	}	
}
function deleUser(custID,userID)
{
	document.form1.action = "custusers.asp?dele=T&UserID="+custID+"&ID="+userID;
	document.form1.submit ();
}
function openwindow(strFile) 
{  
	window.open(strFile,'','toolbar=no,status=no,scrollbars=no,location=no,resizable=yes,menubar=no,directories=no,top=' + ((window.screen.height-500)/2) + ',left=' + ((window.screen.width-800)/2) + ',width=800,height=500')
}
//-->
</SCRIPT>
<BODY>
<TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
  <TR> 
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=8 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=39 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=33 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=60 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=10 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=47 HEIGHT=1></TD>
    <TD width="100%"> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=6 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=125 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=10 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=13 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=361 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=30 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=8 HEIGHT=1></TD>
  </TR>
  <TR> 
    <TD COLSPAN=6> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_01.jpg" WIDTH=197 HEIGHT=8></TD>
    <TD background="../../images/<%=Session("Theme")%>/Login1_02.jpg"> <IMG SRC="images/<%=Session("Theme")%>/Login1_02.jpg" WIDTH=6 HEIGHT=8></TD>
    <TD COLSPAN=6> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_03.jpg" WIDTH=547 HEIGHT=8></TD>
  </TR>
  <TR> 
    <TD COLSPAN=3 ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_04.jpg" WIDTH=80 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/DCC logo.jpg" WIDTH=60 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_06.jpg" WIDTH=10 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_07.jpg" WIDTH=47 HEIGHT=59></TD>
    <TD ROWSPAN=2 background="../../images/<%=Session("Theme")%>/Login1_08.jpg"> <IMG SRC="images/<%=Session("Theme")%>/Login1_08.jpg" WIDTH=6 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_09.jpg" WIDTH=125 HEIGHT=59></TD>
    <TD COLSPAN=5> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_10.jpg" WIDTH=422 HEIGHT=30></TD>
  </TR>
  <TR> 
    <TD COLSPAN=5> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_11.jpg" WIDTH=422 HEIGHT=29></TD>
  </TR>
  <TR> 
    <TD COLSPAN=6> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_12.jpg" WIDTH=197 HEIGHT=8></TD>
    <TD background="../../images/<%=Session("Theme")%>/Login1_13.jpg"> <IMG SRC="images/<%=Session("Theme")%>/Login1_13.jpg" WIDTH=6 HEIGHT=8></TD>
    <TD COLSPAN=6> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_14.jpg" WIDTH=547 HEIGHT=8></TD>
  </TR>
</TABLE>
<BR>
<%
	if strMessage <> "" then
		Response.Write strMessage
	End if
%>
<table width="95%" border=1 align=center>
 <tr> 
  <td colspan="5" class="Dark_Cell"><div align="right"><a href="Users.asp"><font color="#FFFFFF">
  Back to Users' Security</font></a></div></td>
</tr>
<tr>
	<td colspan=5 align=center class='Title'>Users List for <%=Trim(strCust)%></td>
</tr>
<%if rsUsers.BOF and rsUsers.EOF then%>
	<tr>
		<td colspan=5 align=center>No users found</td>
	</tr>
<%Else%>
<tr>
	<td class='Dark_Cell'><b>ID</b></td>
	<td class='Dark_Cell'><b>Name</b></td>
	<td class='Dark_Cell'><b>User Level</b></td>
	<td class='Dark_Cell' colspan=2><b>Email</b></td>
	
</tr>
		<%
		do while not rsUsers.EOF 
		%>
<tr>
	<td><a href="EditUser.asp?Profile=<%=request("UserID")%>&cUserActivated=yes&UserID=<%=rsUsers("cuser_id")%>"><%=rsUsers("cuser_id")%></a></td>
	<td><%=rsUsers("cusr_name")%></td>
	<td>
		<%if rsUsers.Fields.Item("cusr_levl").Value = "A" then
			Response.Write "Administrator" 
		else
			Response.Write "Operator" 					
		end if%>
	</td>
	<td><%=rsUsers("cwe_mail")%>&nbsp;</td>
	<td class='Light_Cell'width=10 align=center><INPUT TYPE=button value="Delete"  onclick="deleUser('<%=request("UserID")%>','<%=rsUsers("cuser_id")%>');"></td>
</tr>
		<%
			rsUsers.MoveNext
		Loop
		%>
</table>
<%End if%>
<br>
<form name="form1" id="form1" action="AddNewUser.asp" method=post>
<table width="43%" border=1 align=center>
<tr>
	<td colspan=3 class="title" align=center><font size="2" face="Verdana, Arial, Helvetica, sans-serif"><b>Add new User</b></font></td>
</tr>
<tr>
	<td class='Dark_Cell'>ID</td>
	<td colspan=2 class='Light_Cell'><input type="text" name="txtID" size="10" maxlength="10" >*</td>
</tr>
 <tr> 
  <td class='Dark_Cell'>Name</td>
  <td colspan="2" class='Light_Cell'><input name="txtName" type="text" id="txtName" size="30" maxlength="35">*</td>
</tr>
<tr> 
  <td class='Dark_Cell'>Email</td>
  <td colspan="2" class='Light_Cell'><input name="txtEmail" type="text" id="txtEmail" size="30" maxlength="200"></td>
</tr>
 <!--wal_039385 add the filed for warehouse-->
<tr> 
  <td class='Dark_Cell'>Ware house</td>
  <td colspan="2" class='Light_Cell'>
  	<input name="txtWareCode" type="text" id="txtWareCode" value="<%=strWareCode%>" size="20" maxlength="6" readonly>
  	<input type=button value="Select" onclick="openwindow('getWareCode.asp');" id=button1 name=button1>
  </td>
</tr>
<!--wal_130731 add the filed for style group-->
<tr> 
  <td class='Dark_Cell'>Style Group</td>
  <td colspan="2" class='Light_Cell'>
  	<input name="txtStyGroup" type="text" id="txtStyGroup" value="<%=strStyGroup%>" size="20" maxlength="6">
  	<input type=button value="Select" onclick="openwindow('getStyGroup.asp');" id=button1 name=button1>
  </td>
</tr>
<!--wal_131300 add the filed for style profile and price code-->
<tr> 
  <td class='Dark_Cell'>Style Profile</td>
  <td colspan="2" class='Light_Cell'>
  	<input name="txtStyProfile" type="text" id="txtStyProfile" value="" size="20" maxlength="10">
  	<input type=button value="Select" onclick="openwindow('getStyProfile.asp');" id=button1 name=button1>
  </td>
</tr>
<tr> 
  <td class='Dark_Cell'>Price Code</td>
  <td colspan="2" class='Light_Cell'>
	<select name="lstPrice">
		<option value="">--Select--
		<%if not rsPrice.EOF then
			do while not rsPrice.EOF%>
				<option value="<%=rsPrice("priccode")%>"><%=rsPrice("priccode")%>
		<%	rsPrice.MoveNext 
			loop
		 else%>
			<option value="">None
		<% end if%>
	</select>
  </td>
</tr>
<tr> 
  <td class='Dark_Cell'>Password</td>
  <td colspan="2" class='Light_Cell'><input name="txtPass" type="password" id="txtPass" size="10" maxlength="8">*</td>
</tr>
<tr> 
  <td class='Dark_Cell'>Confirm Password</td>
  <td colspan="2" class='Light_Cell'><input name="txtConfPass" type="password" id="txtConfPass" size="10" maxlength="8">* 
  </td>
</tr>
<tr>
	<td class='Dark_Cell'>Level</td>
	<td class='Light_Cell'><input type="radio" name="radLevel" value="A">Administrator</td>
	<td class='Light_Cell'><input type="radio" name="radLevel" value="O" checked>Operator</td>
</tr>
<tr> 
	<td class="Dark_Cell" colspan=3>(*) Indicates Required Field</td>
</tr>
<tr>
	<td colspan=3 align="right"> 
		<input type="submit" name="sub1" value="Submit" onClick="return validate();">
		<input type="reset" value="Reset">
	</td>
</tr>
</table>
<input type="hidden" name="Profile" value=<%=Trim(UCase(strCust))%>>
</form>
</BODY>
<%
	
End if
%>
</HTML>
<%
rsUsers.Close()
Set rsUsers = Nothing
%>