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
<title>CRM - Check Invoice</title>
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


	<%
	If Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
	If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"INVPAYMNT") <= 0 Then
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

function FormCheck()
{
// check to see if the Number entered is a valid number.
	var checkOK = " ";
	var checkStr = document.form1.text1.value;
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
	/*if (!allValid)
	{
		var checkOK = "0123456789";
		var checkStr = document.form1.text1.value;
		var allValid2 = true;
		var allNum = "";
		for (i = 0;  i < checkStr.length;  i++)
		{
			ch = checkStr.charAt(i);
			for (j = 0;  j < checkOK.length;  j++)
				if (ch == checkOK.charAt(j))
				break;
			if (j == checkOK.length)
			{
			allValid2 = false;
			break;
			}
			if (ch != ",")
			allNum += ch;
		}
		if (!allValid2)
		{
			alert("The Invoice must be a number!");
			document.form1.text1.focus();
			return (false);
		}
	}*/
	
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
<TD class=title>Check Invoice Payment</TD>
</TR>
</Table>

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


	
	
	If Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
	'If  Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"CHECKINV") <= 0 Then
	if not CheckLogin(Session("rep")) then 'If Session("Authority")<>"Full" then 
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


<br>
<table border="0" width=95% cellspacing="0" cellpadding="0" align=center>
  <tr>
    <td width="100%">

		<b>
		Check invoice payment by entering invoice number or you can select a date range with invoice payment.</b>

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

<FORM action="InvoicePaymentResponse.asp" method=post id=form1 name=form1 onsubmit="return FormCheck(form1)">


<input type="hidden" name="SearchStoresFlag" value>
<div align="center">
<center>
<table border="1" bordercolor="#111111" WIDTH=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<%IF Len(trim(session("rep")))>0 Then%>
<%END IF%>
<tr>
    <td class="dark_cell">Invoice #</td>
            
    <td colspan=2 class="light_cell">
		&nbsp;<INPUT id=text1 name="txtInvoiceNo" maxlength=<%=rsInvoiceLength("Nfld_wdth")%> size="<%=Cint(rsInvoiceLength("Nfld_wdth"))+1%>">
    </td>
</tr>	        
	
<tr>
	<td class="dark_cell">Invoice Date</td>
    <td class="light_cell">
	<%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
		&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')">
    <%else%>
		&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')">
	<%end if%>	
    </td>
    <td class="light_cell">
		<font face="Arial" size="2" color="#000080"><strong>&nbsp;To</strong></font> 
		<%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>				
			&nbsp;<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')"> 
		<%else%>
			&nbsp;<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')"> 
		<%end if%>	
    </td>
            
</tr>
 <tr>
		<td class="dark_cell"><strong>All Customers</strong></td>
		<td class="light_cell" colspan=2>
			<input type=checkbox name=chkCust <%if trim(custid) = "" then%>checked<%end if%>>
		</td>
</tr>
<tr>
	<td class="dark_cell" colspan="3" align="right">
		<INPUT type="submit" value="Search" id=submit1 name=submit1>
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