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
if not Session("rsCust").eof then
if Session("rsCust").fields("Status")="P" then
		Response.Redirect("../Common/Msgs.asp")
end if
end if

set connTemp=server.createobject("ADODB.Connection")
set RSTemp=server.createobject("ADODB.Recordset")
connTemp.open Application("DataConnectionString")
sqlTemp="select * from customer where account='"&session("customerid")&"'"
RSTemp.open sqlTemp,connTemp,1,3
%>
<html>
<head>
<!--<LINK REL=stylesheet HREF="crmmain.css" TYPE="text/css">-->
<Title>CRM - Check Return Authorization Status</Title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</head>
<body>

<SCRIPT LANGUAGE=javascript>
<!--
function SubmitNext()
{
	document.FORM2.action = 'orderhsave.asp';
	document.FORM2.submit()
}
//-->
</SCRIPT>

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

	<TD colspan=13>
	<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=RSTemp.fields("btname").value%></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>

</TR>
</table>
 <%Set RSTemp=nothing
  ConnTemp.close
  Set ConnTemp=nothing

End IF%>

<Table align=center width=95% border=1>
<TR>
<TD Class="Title">Check R/A status</TD>
</TR>
</Table>
	<%
	If Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
	If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"CHECKRA") <= 0 Then
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



<%IF compWork = "Y" Then%>

<SCRIPT LANGUAGE = JScript>
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
			alert("The RA must be a number");
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
				alert("Please enter a valid date or leave balnk!");
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
set rsRanoLength = server.CreateObject("ADODB.RecordSet")

strSQL = "SELECT Nfld_wdth FROM Sequence WHERE Cseq_type='RANO'"
rsRanoLength.Open strSQL, conn
%>
<br>
  <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%"><b><font face="Arial" size="2">Check Return Authorization status by entering
        R/A number or you can select a date range and R/A status.</font></b></td>
    </tr>
  </table>
<br>

<FORM action="ReturnStatusResponse.asp" method=post id=form1 name=form1 onsubmit="return FormCheck(form1)">

    <div align="center">
      <center>

    <table border="1" bordercolor="#111111" WIDTH=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
        <tr>
            <td class="dark_cell">Return Authorization #</td>
            
            <td colspan=2 class="light_cell">
				&nbsp;<INPUT id=text1 name="txtRanoNo" maxlength="<%=rsRanoLength("Nfld_wdth")%>" size="<%=Cint(rsRanoLength("Nfld_wdth"))+1%>">
            </td>
		</tr>	        
        <tr>
			<td class="dark_cell">Entered date</td>
            <td class="light_cell">
				&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=11>
            </td>
            <td class="light_cell">&nbsp;To
				<INPUT id=text3 name="txtEndDate" maxlength=10 size=11> 
            </td>
        </tr>
        <tr>
			<td class="dark_cell">Status</td>
        		
        	<td colspan=2 class="light_cell">
				&nbsp;<SELECT id=select1 name=selectStatus>
					<OPTION selected value="ALL">All</OPTION>
					<OPTION value="O">Open</OPTION>				
					<OPTION value="C">Complete</OPTION>
					<OPTION value="E">Electronic</OPTION>
				</SELECT>
             
			</td>
		</tr>
        <tr>
			<td colspan="3" class="dark_cell" align="right">
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
conn.Close
set conn=nothing
set rsRanoLength=nothing
End IF
%>