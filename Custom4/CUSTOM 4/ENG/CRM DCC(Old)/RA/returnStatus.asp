<%@ Language=VBScript %>
<%
Response.Buffer=true
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
IF Session("ID")="" And Session("rep")="" Then
	'Response.Redirect("../default.asp")%>
	<script language="javascript">
	parent.location.href ="../login.asp"
	</script>
<%End IF

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
      
%>

<HTML>
<HEAD>
<LINK REL=stylesheet HREF="../images/<%=session("Theme")%>/order.css" TYPE="text/css">
<Title>CRM - Check Return Authorization Status</Title>
</head>
<!--#include file="../common/checkDateFormat.asp"-->
<body >
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
			alert("The RA must be a number!");
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
<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><BR><BR></p>

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
		
              Your currently selected customer is <%=Session("customerid")%>
             <%response.write " - "%><%=Session("rscust").fields("btname").value%></font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="repcust.asp" ><font color=white size="2" face="Arial"><b>Get Customer</b></font></a>
	-->
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected <%=Session("custfield")%> is <b><%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></b></P>
	</TD>
	<TD align=right><a href="repcust.asp"><b>Get <%=Session("custfield")%></b></a></TD>
	<!-- ARD -->
</TR>
</table>
 <%End IF%>
<Table width=95% height=50 border=1 align=center>
<TR>
<TD class=title>Check RA Status</TD>
</TR>
</Table>
<%
'wal_130674 correct check on privilage case multi user
	if Application("CustomerLoginUsing")= "User"  then
		strAppUserVar = Session("userID")
	ElseIf Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
	'If Trim(Session("Rep")) = "" Then
		'strAppUserVar = Session("ID")
	'Else
		'strAppUserVar = Session("Rep")
	'End If
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

<%
set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")


'RecordSets
set rsRanoLength = server.CreateObject("ADODB.RecordSet")

strSQL = "SELECT Nfld_wdth FROM Sequence WHERE Cseq_type+cseq_group like 'RANO%'"
rsRanoLength.Open strSQL, conn
%>
<FORM action="ReturnStatusResponse.asp" method=post id=form1 name=form1 onsubmit="return FormCheck()">

    <div align="center">
      <center>

  <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%"><strong>Enter your searching criteria to select the Return Authorization(s) you want to check:</strong></td>
    </tr>
  </table>
    <table border="1" WIDTH=95% style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
        <tr>
            <td class="dark_cell">Return Authorization #</td>
            
            <td colspan=2 class="light_cell">
				&nbsp;<INPUT id=text1 name="txtRanoNo" maxlength="<%=rsRanoLength("Nfld_wdth")%>" size="<%=Cint(rsRanoLength("Nfld_wdth"))+1%>" value="">
            </td>
		</tr>	        
        <tr>
			<td class="dark_cell">
				<strong>Entered date</strong> 
            </td>
            <td class="light_cell" width=10%>
            <%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
				&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')">            
            <%else%>
				&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')">
			<%end if%>	
            </td>
            <td class="light_cell">
				<strong>&nbsp;To</strong> 
            <%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
    			<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onChange="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onBlur="DateFormat(this,this.value,event,true,'3')"> 
            <%else%>				
				<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onChange="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onBlur="DateFormat(this,this.value,event,true,'1')"> 
			<%end if%>	
            </td>
        </tr>
        <tr>
			<td class="dark_cell">
				<strong>Status</strong> 
			</td>
        		
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
conn.Close
set conn=nothing
set rsRanoLength=nothing
End If
%>
</html>