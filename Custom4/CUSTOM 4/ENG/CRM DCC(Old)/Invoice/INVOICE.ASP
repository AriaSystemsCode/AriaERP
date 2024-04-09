<%@ Language=VBScript %>
<%Response.Buffer=true
if session("ID") = "" then
	Session("ID") = Application("ID")
	Dim Conn
	Set Conn = server.CreateObject("ADODB.Connection")
	Conn.Open Application("DataConnectionString")

	Set Session("RSCust") = Server.CreateObject("ADODB.RecordSet")
	strsql = "Select * from customer where type+account+store like'M" & Ucase(trim(Session("ID"))) & "%'"

	'WMA change Session("RSCust") to be updatable [Start]
	'Session("RSCust").open strsql,Conn
	Session("RSCust").open  strSql,conn , 2, 4
	'Session("ID")  = trim(Session("RSCust")("account"))
	'Response.Write "<font size=2>" & Session("ID")

end if
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.htm"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>
	
<%END IF

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
End IF
%>
<html>

<head><LINK REL=stylesheet HREF="../images/<%=Session("Theme")%>/invoice.css" TYPE="text/css">
<meta http-equiv="Content-Type" content="text/html; charset=windows-1256">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<title>CRM - Check Invoice</title>
</head>
<!--#include file="../common/checkDateFormat.asp"-->
<body>
<%if Session("DateFormat")="" then
DateFormat()
end if%>
<script LANGUAGE="javascript" src="checkForm.js"></script>
<script language="Javascript">
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
</script>

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
	<P>Your currently selected <%=session("CustField")%> is <b><%=Session("customerid")%> - <%=RSTemp.fields("btname").value%></b></P>
	</TD>
	<TD align=right><a href="repcust.asp"><b>Get <%=session("CustField")%></b></a></TD>
	<!-- ARD -->
</TR>
</table>
 <%Set RSTemp=nothing
  ConnTemp.close
  Set ConnTemp=nothing

End IF%>


<Table width=95% height=50 align=center border=1>
<TR>
<TD class=title>Check Invoice Status</TD>
</TR>
</Table>
<%
'wal_130674 correct check on privilage case multi user
	if Application("CustomerLoginUsing")= "User"  then
		strAppUserVar = session("userID")
	ElseIf Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"CHECKINV") <= 0 Then
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

<SCRIPT LANGUAGE = JScript>
function getcustomer()
{
	document.location ="../repfindcustomer.asp"
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
	if (!allValid)
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
				alert("Please enter a valid date or leave blank!");
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
				alert("Please enter a valid date or leave blank!");
				document.form1.text3.focus();
				return false;
			}
		}
	}
		
	return true;
}
</SCRIPT>
<%
set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
set rsInvoiceLength = server.CreateObject("ADODB.RecordSet")

strSQL = "SELECT Nfld_wdth FROM Sequence WHERE Cseq_type='INVOICE'"
rsInvoiceLength.Open strSQL, conn
%>
  <FORM action="invoiceResponse.asp" method=post id=form1 name=form1 onsubmit="return FormCheck(form1)">
    <div align="center">
      <center>
    <table  width="95%"  align=center>
    <tr>
      <td width="100%"><strong>Enter your searching criteria to select the Invoice(s) you want to check:</strong></td>
    </tr>
	</table>
    <table WIDTH=95% border="1" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0" >
    <%if len(trim(session("rep")))>0 then%>
        <tr><td class="dark_cell">Customer</td>
        <td class="light_cell">
        &nbsp;<input name=txtcustid value=<%=session("customerid")%> size="20">
        </td>
        <td class="light_cell">
    <input type="button" name="button2"   value="Get Customer" style="width=150" onclick="getcustomer()" id="button5" style="height: 24; width: 80" > 
				<input type="hidden" name="SearchStoresFlag" value>
        </td>
        </tr>
     <%end if%>
        <tr>
            <td class="dark_cell">Invoice #</td>
            
            <td colspan=2 class="light_cell">
				&nbsp;<INPUT id=text1 name="txtInvoiceNo" maxlength=<%=rsInvoiceLength("Nfld_wdth")%> size="<%=Cint(rsInvoiceLength("Nfld_wdth"))+1%>">
            </td>
		</tr>	        
	
        <tr>
			<td class="dark_cell">Invoice Date</td>
            <td class="light_cell" width=10%>
            <%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
   				&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')">
            <%else%>
				&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')">
			<%end if%>	
            </td>
            <td class="light_cell"><strong>To</strong>
				<%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>				
					&nbsp;<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')"> 
				<%else%>
					&nbsp;<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')"> 
				<%end if%>	
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
</body>
<%
session("customer")=""
conn.Close
set conn=nothing
set rsInvoiceLength=nothing
Session("BeginDate")=""
Session("EndDate")=""
%>
</html>