<%@ Language=VBScript %>
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>

<%
Response.Buffer=true
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'	Response.redirect "../default.asp"%>
<script language="javascript">
	parent.location.href = "../default.asp"
</script>
<%End if

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = ""  Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF
if not Session("rsCust").eof  then
	if Session("rsCust").fields("Status")="P" then
		Response.Redirect("../Common/Msgs.asp")
	end if
end if
 %>
<html>
<head>
<Title>CRM - Check Order Status</Title>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("Theme")%>/Order.css">
</head>

<body bgcolor="#aecae6" topmargin="0" leftmargin="0">
<%if trim(UCase(Session("DateFormat")))="" then
DateFormat()
end if
'Response.Write "<font color=white size=20>Date Format :"&Session("DateFormat")&"</font>"
%>

<!--#include file="../common/checkDateFormat.asp"-->
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
			alert("The Order must be a number!");
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
				alert("Please enter valid date or leave blank!");
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
				alert("Please enter valid date or leave blank!");
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
<P><BR><BR><Br></P>
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
	<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=session("rscust").fields("btname").value%></P>
	</TD>
	<TD align=right><a href="repcust.asp">Get <%=session("CustField")%></a></TD>
	<!-- ARD -->
</TR>
</table>
 <%
End IF%>
<div align="center">
  <center>
<Table width=95% border=1 height="50">
<TR>
<TD class="title">Check Order Status</TD>
</TR>
</Table>

  </center>
</div>

<%IF compWork = "Y" Then

set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
set rsOrderLength = server.CreateObject("ADODB.RecordSet")

strSQL = "SELECT Nfld_wdth FROM Sequence WHERE Cseq_type+cseq_group like 'ORDER%'"
rsOrderLength.Open strSQL, conn
%>
  <br>
  <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%">

<b>

You can check for the order status using the following search criteria.</b>
      </td>
    </tr>
  </table>
<script LANGUAGE="javascript" src="checkForm.js">
</script>
<FORM action="OrdStatusResponse.asp" method=post id=form1 name=form1 onsubmit="return FormCheck(form1)">

<Table border=0 width=95% align=center> <TR><TD>
    <div align="center">
      <center>
    <table border="1" bordercolor="#111111" width=95% cellspacing="0" class="dark_cell" style="border-collapse: collapse" cellpadding="0">
        <tr>
            <td class="dark_cell">
				<strong>Order 
            #</strong> 
            </td>
            
            <td colspan=2 class="light_cell">
				&nbsp;<INPUT id=text1 name="txtOrderNo" maxlength="<%=rsOrderLength("Nfld_wdth")%>" size="<%=Cint(rsOrderLength("Nfld_wdth"))+1%>">
            </td>
			        
		</tr>
        <tr>
			<td class="dark_cell">
				<strong>Entered date</strong> 
            </td>
            <td class="light_cell">
            <%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
                &nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')"> 
            <%else%>
            	&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')"> 
			<%end if%>
            </td>
            <td class="light_cell">
				<strong>&nbsp;To</strong> 
				<%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
					&nbsp;<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')"> 
				<%else%>
					&nbsp;<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')"> 
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
				<OPTION value="H">On Hold</OPTION>
				<OPTION value="B">Bid</OPTION>
				<OPTION value="X">Cancelled</OPTION>
				</SELECT>
			</td>
		</tr>
        <tr>
			<td colspan="3" align="right" class="dark_cell">
				<INPUT type="submit" value="Search" id=submit1 name=submit1>
				<INPUT type="reset" value="Reset" id=reset1 name=reset1> 
            
			</td>
        </tr>
    </table>
      </center>
    </div>
</TD></TR></Table>
</FORM>

<%End IF%>
</body>
<%
conn.Close 
set conn=nothing
set rsOrderLength=nothing
Session("BeginDate") = ""
Session("EndDate") = ""
%>