<%@ Language=VBScript %>
<%Response.Buffer=true
Response.Expires=-1

Function CheckLogin(User) 'NEK
		SR_textfile = server.mappath("../Admin/CrmSetup/Setup/SR.txt")
		Set FileSystem = server.CreateObject("Scripting.FileSystemObject")
		FileIsExists=FileSystem.FileExists(SR_textfile) 
		if (FileIsExists) then 'File Exists
			set File=FileSystem.OpenTextFile(SR_textfile,1)
			ss = File.readAll
			Arrusers=split(ss,chr(13)+chr(10))
			foundInFile = false
			for i=lbound(Arrusers) to ubound(Arrusers)
				if User = trim(Arrusers(i)) then
					foundInFile = true
				end if	 
			next
			if (foundInFile) then
				CheckLogin = true
			else
				CheckLogin = false
			end if
		else 'File Doesn't Exists
			CheckLogin = false
		end if		
End Function

IF Trim(Session("rep")) = "" AND trim(Session("ID"))= "" THEN
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
	</script>	
<%END IF 


IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	'IF Trim(Session("customerid")) = ""  Then
	'	Response.Redirect("../repcust.asp")
	'END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF
IF Trim(Session("customerid")) <> ""  Then
	if not Session("rsCust").eof  then
		if Session("rsCust").fields("Status")="P" then
			Response.Redirect("../Common/Msgs.asp")
		end if
	end if
End if

set connTemp=server.createobject("ADODB.Connection")
set RSTemp=server.createobject("ADODB.Recordset")
connTemp.open Application("DataConnectionString")
sqlTemp="select * from customer where account='"&session("customerid")&"'"
RSTemp.open sqlTemp,connTemp,1,3




%>

<html>

<head><LINK REL=stylesheet HREF="../images/<%=Session("THEME")%>/invoice.css" TYPE="text/css">
<meta http-equiv="Content-Type" content="text/html; charset=windows-1256">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
<title>CRM - Credit Card Collection</title>
	</head>
	<script language="javascript" src="../common/checkForm.js"></script>
<!--#include file="../common/checkDateFormat.asp"-->
<body>
<%if Session("DateFormat")="" then
DateFormat()
end if%>
	

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
  ConnTemp.close
  Set ConnTemp=nothing

End IF%>
  
<SCRIPT LANGUAGE = JScript>
function getcustomer()
{document.location ="../common/repfindcustomer.asp?Cust=I"

}
function dateValidate(formId,len)
{
//		alert(formId);
	if(formId=="text2")
	{
		
		if((len==2)||(len==5))
		{
			form1.text2.value = form1.text2.value+"/";
		}
	}
	if(formId=="text3")
	{
		if((len==2)||(len==5))
		{
	
			form1.text3.value = form1.text3.value+"/";
		}
	}
}

function FormCheck(form)
{
	//Order No
	if (isNaN(document.form1.txtOrderNoStart.value)){
		alert("The Order must be a number!");
		document.form1.txtOrderNoStart.focus();
		return (false);
	}
	if (isNaN(document.form1.txtOrderNoEnd.value)){
		alert("The Order must be a number!");
		document.form1.txtOrderNoEnd.focus();
		return (false);
	}
/*
	//Account
	if (isNaN(document.form1.txtAccountStart.value)){
		alert("The Account must be a number!");
		document.form1.txtAccountStart.focus();
		return (false);
	}
	if (isNaN(document.form1.txtAccountEnd.value)){
		alert("The Account must be a number!");
		document.form1.txtAccountEnd.focus();
		return (false);
	}

	//Rep
	if (isNaN(document.form1.txtRepStart.value)){
		alert("The Rep. Id must be a number!");
		document.form1.txtRepStart.focus();
		return (false);
	}
	if (isNaN(document.form1.txtRepEnd.value)){
		alert("The Rep. Id must be a number!");
		document.form1.txtRepEnd.focus();
		return (false);
	}	
*/

	//PNP Transaction	
	if (isNaN(document.form1.txtPNPTrans.value)){
		alert("The PNP Transaction must be a number!");
		document.form1.txtPNPTrans.focus();
		return (false);
	}
		
//check to see if the first date is spaces .. 
	var checkOK = " ";
	var checkStr = document.form1.text2.value;
	var allValid = true;
	var allSpace = "";
	for (i = 0;  i < checkStr.length;  i++)
	{
		ch = checkStr.charAt(i);
		for (j = 0;  j < checkOK.length;  j++)
			if (ch == checkOK.charAt(j))
			break;
		if (j == checkOK.length)
		{
		allValid = false;
		break;
		}
		if (ch != ",")
		allSpace += ch;
	}
	if (!allValid)
	{
		//check if the date is date .. 
		var t;
		if (document.form1.text2.value!="")
		{
			t = Date.parse(document.form1.text2.value)
			if (!t) 
			{
				alert("please enter valid date or leave blank!");
				document.form1.text2.focus();
				return false;
			}
		}
	}
	
//check to see if the second date is spaces .. 
	var checkOK = " ";
	var checkStr = document.form1.text3.value;
	var allValid = true;
	var allSpace = "";
	for (i = 0;  i < checkStr.length;  i++)
	{
		ch = checkStr.charAt(i);
		for (j = 0;  j < checkOK.length;  j++)
			if (ch == checkOK.charAt(j))
				break;
		if (j == checkOK.length)
		{
			allValid = false;
			break;
		}
		if (ch != ",")
		allSpace += ch;
	}
	if (!allValid)
	{
		var b;
		if (document.form1.text3.value!="")
		{
			b = Date.parse(document.form1.text3.value)
			if (!b)
			{	
				alert("please enter valid date or leave blank!");
				document.form1.text3.focus();
				return false;
			}
		}
	}
		
	return true;
}
</SCRIPT>
<Table width=95% height=50 align=center border=1>
<TR>
<TD class=title>Credit Card Collection</TD>
</TR>
</Table>

	<%
	If Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If	
	If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"CREDITCOL") <= 0 Then
	%>
	<Table border=0 width=95% align=center>
		<tr>
		<td class="Title"><font color="Yellow">
			You're not authorized to view this page</font>
		</td>
		</tr>
	</Table>
	<%
		Response.End 
	End If
	%>
	
	
	<%
	if session("UseOnlinePayment") = "T" then 'use online payment - wma
	'if trim(session("GateWay")) = "PlugNPay.com (MemberShip Managment)" then  'use PNP Membership
	'end if					
	else	'don't use online payment
		%>
		<Table border=0 width=95% align=center>
			<tr><td class="Title"><font color="Yellow">
				This page working only with an Online Payment Option
			</font>	</td></tr>
		</Table>
		<%
		Response.End 
	end if
	%>
	

<br>
<table border="0" width=95% cellspacing="0" cellpadding="0" align=center>
  <tr>
    <td width="100%">

		<b>
		Specify Your Report Criteria.</b>

	</td>
  </tr>
</table>

<p>

<%
Set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
Set rsInvoiceLength = server.CreateObject("ADODB.RecordSet")
strSQL = "SELECT Nfld_wdth FROM Sequence WHERE Cseq_type='INVOICE'"
rsInvoiceLength.Open strSQL, conn
%>
</p>

<FORM action="CreditCollectionResponse.asp" method=post id=form1 name=form1 onsubmit="return FormCheck(form1)">


<input type="hidden" name="SearchStoresFlag" value>
<div align="center">
<center>
<table border="1" bordercolor="#111111" WIDTH=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<%IF Len(trim(session("rep")))>0 Then%>
<%END IF%>

<tr>
    <td width=20% class="dark_cell"><strong>Customer Name</strong></td>
	<td colspan=2>
			<table border=0 width="100%" cellspacing=0 cellpadding=0>
				<tr>
					<td class="light_cell" width="10%" nowrap >&nbsp;<INPUT name="txtCustomerName"  maxlength="30" style="width:160px">&nbsp;&nbsp;</td>
				</tr>
			</table>
    </td>
</tr>

<tr>
    <td width=20% class="dark_cell"><strong>Sales Name</strong></td>
	<td colspan=2>
			<table border=0 width="100%" cellspacing=0 cellpadding=0>
				<tr>
					<td class="light_cell" width="10%" nowrap >&nbsp;<INPUT name="txtRepName" maxlength="24" style="width:160px">&nbsp;&nbsp;</td>	
				</tr>
			</table>
    </td>
</tr>

<tr>
    <td width=20% class="dark_cell"><strong>Order#</strong></td>
	<td colspan=2>
			<table border=0 width="100%" cellspacing=0 cellpadding=0>
				<tr>
					<td class="light_cell" width="10%" nowrap >&nbsp;<INPUT name="txtOrderNoStart" maxlength="6" size="10">&nbsp;&nbsp;</td>
					<td class="light_cell" width="5%" nowrap ><strong>To</strong></td>
					<td width="80%" class="light_cell">&nbsp;<INPUT name="txtOrderNoEnd" maxlength="6" size="10" ></td>
				</tr>
			</table>
    </td>
</tr>

<tr>
    <td width=20% class="dark_cell"><strong>Account</strong></td>
	<td colspan=2>
			<table border=0 width="100%" cellspacing=0 cellpadding=0>
				<tr>
					<td class="light_cell" width="10%" nowrap >&nbsp;<INPUT name="txtAccountStart" maxlength="6" size="10">&nbsp;&nbsp;</td>
					<td class="light_cell" width="5%" nowrap ><strong>To</strong></td>
					<td width="80%" class="light_cell">&nbsp;<INPUT name="txtAccountEnd" maxlength="6" size="10" ></td>
				</tr>
			</table>
    </td>
</tr>

<%if not CheckLogin(Session("rep")) then 'If Session("Authority")<>"Full" then
	Session("Authority")=""
  else
	Session("Authority")="Full"
%>
	<tr>
	    <td width=20% class="dark_cell"><strong>Rep. Id</strong></td>
		<td colspan=2>
				<table border=0 width="100%" cellspacing=0 cellpadding=0>
					<tr>
						<td class="light_cell" width="10%" nowrap >&nbsp;<INPUT name="txtRepStart" maxlength="6" size="10">&nbsp;&nbsp;</td>
						<td class="light_cell" width="5%" nowrap ><strong>To</strong></td>
						<td width="80%" class="light_cell">&nbsp;<INPUT name="txtRepEnd" maxlength="6" size="10" ></td>
					</tr>
				</table>
	    </td>
	</tr>
<%end if%>
<tr>
    <td width=20% class="dark_cell"><strong>Trasaction Date</strong></td>
	<td colspan=2>
			<table border=0 width="100%" cellspacing=0 cellpadding=0>
				<tr>
					<td class="light_cell" width="10%" nowrap >
						<%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
							&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')">&nbsp;&nbsp;
						<%else%>
							&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')">&nbsp;&nbsp;
						<%end if%>	
					</td>
					<td class="light_cell" width="5%" nowrap ><strong>To</strong></td>
					<td width="80%" class="light_cell">
					<%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>				
						&nbsp;<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')"> &nbsp;&nbsp;
					<%else%>
						&nbsp;<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')"> &nbsp;&nbsp;
					<%end if%>	
					</td>
				</tr>
			</table>
    </td>
</tr>
<tr>
    <td width=20% class="dark_cell"><strong>PNP Transaction</strong></td>
	<td colspan=2>
			<table border=0 width="100%" cellspacing=0 cellpadding=0>
				<tr>
					<td class="light_cell" width="10%" nowrap >&nbsp;<INPUT name="txtPNPTrans" maxlength="19" style="width:160px">&nbsp;&nbsp;</td>
					<td class="light_cell" width="5%" nowrap ><strong></strong></td>
					<td width="80%" class="light_cell">&nbsp;</td>
				</tr>
			</table>
    </td>
</tr>
<tr>
	<td class="dark_cell"><strong>Report Type</strong></td>
    <td class="light_cell" colspan=2>
		&nbsp;<SELECT name=lstType style="width:160px">
		<!--OPTION selected value="ALL">All</OPTION-->	
		<OPTION value="Deposit">Deposit</OPTION>
		<OPTION value="Invoice">Invoice</OPTION>
		</SELECT>
	</td>
</tr>
 <!--tr>
		<td class="dark_cell"><strong>All Customers</strong></td>
		<td class="light_cell" colspan=2>
			<input type=checkbox name=chkCust <%if trim(custid) = "" then%>checked<%end if%>>
		</td>
</tr-->
<tr>
	<td class="dark_cell" colspan="3" align="right">
		<INPUT type="submit" value="View Report" id=submit1 name=submit1>
		<INPUT type="reset" value="Reset" id=reset1 name=reset1> 
            
	</td>
</tr>
    </table>
                  </center>
                </div>
</FORM>

<%
conn.Close
Set conn=Nothing
Set rsInvoiceLength=Nothing
%>
</body>
</html>