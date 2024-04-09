<%@ Language=VBScript %>
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>

<%
Response.Buffer=true
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../login.asp"%>
	<script language="javascript">
	parent.location.href ="../login.asp"
	</script>
<%End if

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
<HEad>
<Title>CRM - Order Confirmation</Title>
<LINK rel="stylesheet" type="text/css" href="../images/<%=session("Theme")%>/order.css">
</head>
<!--#include file="../common/checkDateFormat.asp"-->

<body>
<%if trim(UCase(Session("DateFormat")))="" then
DateFormat()
end if

%>
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

	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected customer is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>
 <%

End IF%>

<Table width=95% align=center height=50 border=1>
<TR>
<TD class=title>Order Confirmation</TD>
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
	'If Trim(Session("Rep")) = "" Then
		'strAppUserVar = Session("ID")
	'Else
		'strAppUserVar = Session("Rep")
	'End If
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"ORDCONF") <= 0 Then
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



<%IF compWork = "Y" Then

set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
set rsOrderLength = server.CreateObject("ADODB.RecordSet")

strSQL = "SELECT Nfld_wdth FROM Sequence WHERE Cseq_type+cseq_group like 'ORDER'"
rsOrderLength.Open strSQL, conn
%>
<FORM action="Ord_Conf_Resp.asp" method=post id=form1 name=form1 onsubmit="return FormCheck(form1)">
<Table border=0 width=95% align=center>
	<tr>
		<Td>
			  <table border="0" width="95%" cellspacing="0" cellpadding="0" >
					<tr>
					  <td width="100%"><strong>Enter your searching criteria to select the order(s) you want to check:</strong></td>
					</tr>
			  </table>
		</td>
	</tr>
<TR><TD>
    <div align="center">
      <center>
      
    <table border="1" width="100%" cellspacing="0" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0">
        <tr>
            <td class="dark_cell">
                <strong>
				Order # 
                </strong> 
            </td>
            
            <td colspan=2 class="light_cell">
				&nbsp;<INPUT id=text1 name="txtOrderNo" maxlength="<%=rsOrderLength("Nfld_wdth")%>" size="<%=Cint(rsOrderLength("Nfld_wdth"))+1%>">
            </td>
			        
		</tr>
        <tr>
			<td class="dark_cell">
                <strong>
				Entered date 
                </strong> 
            </td>
            <td class="light_cell" width="10%">
            <%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
				&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')">
            <%else%>
				&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')">
			<%end if%>	
            </td>
            <td class="light_cell">
                <strong>&nbsp;To </strong> 
                <%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
					&nbsp;<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')"> 
                <%else%>
                	&nbsp;<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')"> 
				<%end if%>
            </td>
        </tr>
        <tr>
			<td class="dark_cell">
                <strong>
				Status 
                </strong> 
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
			<td class="dark_cell" colspan="3" align="right">
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