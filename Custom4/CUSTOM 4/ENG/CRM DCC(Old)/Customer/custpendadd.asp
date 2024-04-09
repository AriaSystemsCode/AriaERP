<%@ Language=VBScript %>
<%response.buffer=true
If trim(Session("customerid"))="" Then
	Response.redirect "../repcust.asp"
End If%>

<html>
<head>
<title>CRM - Contact Management</title>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/common.css">
<Script language=JavaScript>
function CheckDate()
{ 
	if (document.form1.dtrandate.value=="" )
	{
	}
	else	
	{
		dateObjfrom =new Date(document.form1.dtrandate.value);
		if (!dateObjfrom.getDate())
		{
			alert ("Please enter valid date.");
			document.form1.dtrandate.focus();
			return false;
		}
		
}
	
}


</script>

</head>
<body>
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
	<P>Your currently selected <%=Session("CustField")%> is <%=Session("customerid")%> - <%=RSTemp.fields("btname").value%></P>
	</TD>
	<TD align=right><a href="repcust.asp">Get <%=Session("CustField")%></a></TD>
	<!-- ARD -->
	</TD>
</TR>

</table>
<%
Set Conn=Server.CreateObject("ADODB.Connection")
Set ConnSys=Server.CreateObject("ADODB.Connection")
Set RS=Server.CreateObject("ADODB.Recordset")
Set RSSys=Server.CreateObject("ADODB.Recordset")
Conn.open Application("DataConnectionString")
ConnSys.open Application("SystemConnectionString")
sqls="select * from contact where ccont_id='"&Session("customerid")&"'"
RS.open sqls,conn,1,3
'sqlsTemp="select distinct * from syschdul where ccompleted='N' And  ccont_id='"&Session("Customerid")&"'" 
'RSSys.Open sqlsTemp,connSys,1,3
If  1=1 Then
'	response.write "No records Found."

%><table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
		<tr><td><br>
<font face="Arial" size="2"><b>Schedule new activity with the currently selected <%=Session("CustField")%> :</b></font>
</td></tr></table><br	>
<div align="center">
  <center>
<table width="95%" height="199" bordercolor="#111111" border="1" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<script Language="JavaScript" Type="text/javascript"><!--
function FrontPage_Form1_Validator(theForm)
{

  if (theForm.txtsubject.value == "")
  {
    alert("Please enter a value for the \"Subject\" field.");
    theForm.txtsubject.focus();
    return (false);
  }

  if (theForm.txtsubject.value.length > 30)
  {
    alert("Please enter at most 30 characters in the \"Subject\" field.");
    theForm.txtsubject.focus();
    return (false);
  }

  if (theForm.txtpriority.value == "")
  {
    alert("Please enter a value for the \"Priority\" field.");
    theForm.txtpriority.focus();
    return (false);
  }

  if (theForm.txtpriority.value.length > 1)
  {
    alert("Please enter at most 1 characters in the \"Priority\" field.");
    theForm.txtpriority.focus();
    return (false);
  }

  var checkOK = "0123456789-.";
  var checkStr = theForm.txtpriority.value;
  var allValid = true;
  var validGroups = true;
  var decPoints = 0;
  var allNum = "";
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
    if (ch == ".")
    {
      allNum += ".";
      decPoints++;
    }
    else
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit characters in the \"Priority\" field.");
    theForm.txtpriority.focus();
    return (false);
  }

  if (decPoints > 1 || !validGroups)
  {
    alert("Please enter a valid number in the \"Priority\" field.");
    theForm.txtpriority.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseFloat(allNum);
  if (chkVal != "" && !(prsVal >= "1" && prsVal <= "9"))
  {
    alert("Please enter a value greater than or equal to \"1\" and less than or equal to \"9\" in the \"txtpriority\" field.");
    theForm.txtpriority.focus();
    return (false);
  }

  if (theForm.lstreason.selectedIndex < 0)
  {
    alert("Please select one of the \"Reason\" options.");
    theForm.lstreason.focus();
    return (false);
  }

  if (theForm.dtrandate.value == "")
  {
    alert("Please enter a value for the \"Date\" field.");
    theForm.dtrandate.focus();
    return (false);
  }

  if (theForm.dtrandate.value.length > 10)
  {
    alert("Please enter at most 10 characters in the \"Date\" field.");
    theForm.dtrandate.focus();
    return (false);
  }

  if (theForm.txttime.value == "")
  {
    alert("Please enter a value for the \"Time\" field.");
    theForm.txttime.focus();
    return (false);
  }

  if (theForm.txttime.value.length > 11)
  {
    alert("Please enter at most 11 characters in the \"Time\" field.");
    theForm.txttime.focus();
    return (false);
  }

  if (theForm.txtduration.value == "")
  {
    alert("Please enter a value for the \"Duration\" field.");
    theForm.txtduration.focus();
    return (false);
  }

  if (theForm.txtduration.value.length > 3)
  {
    alert("Please enter at most 3 characters in the \"Duration\" field.");
    theForm.txtduration.focus();
    return (false);
  }

  var checkOK = "0123456789-.";
  var checkStr = theForm.txtduration.value;
  var allValid = true;
  var validGroups = true;
  var decPoints = 0;
  var allNum = "";
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
    if (ch == ".")
    {
      allNum += ".";
      decPoints++;
    }
    else
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit characters in the \"Duration\" field.");
    theForm.txtduration.focus();
    return (false);
  }

  if (decPoints > 1 || !validGroups)
  {
    alert("Please enter a valid number in the \"Duration\" field.");
    theForm.txtduration.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseFloat(allNum);
  if (chkVal != "" && !(prsVal >= "1" && prsVal <= "999"))
  {
    alert("Please enter a value greater than or equal to \"1\" and less than or equal to \"999\" in the \"txtduration\" field.");
    theForm.txtduration.focus();
    return (false);
  }
  return (true);
}
//--></script><form action="custpendaddaction.asp" method=post id=form1 name=FrontPage_Form1 onsubmit="return FrontPage_Form1_Validator(this)" language="JavaScript">

<tr>
<td width="750" colspan="4" class="dark_cell" >
<p align="center">
<font size="2" face="Arial">
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<font color="#000080">
<INPUT id=radio1 name=lstact type=radio value="C"  checked style="height: 20; width: 19"></font></font><b><U><font size="2" face="Arial">C</font></U><font size="2" face="Arial">all</font></b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<font color="#0000FF" size="2" face="Arial"><INPUT id=radio1 name=lstact type=radio value="A"  style="height: 20; width: 21"></font><b><U><font size="2" face="Arial">A</font></U><font size="2" face="Arial">ppointment&nbsp;&nbsp;</font></b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<font color="#0000FF" size="1"><INPUT id=radio1 name=lstact type=radio value="T"   style="height: 20; width: 21"></font><b><U><font size="2" face="Arial">T</font></U><font size="2" face="Arial">o-Do</font></b>
</p>
  </td>
</tr>
<tr><td width="92" height="28" valign=top class="dark_cell"><STRONG><font size="2" face="Arial">Contact</font>
      </STRONG>

</td>
<td width="92" height="28" class="light_cell">
<font color="#0000FF" size="2">
  <%
  If not RS.EOF  and not RS.BOF Then
					RS.MoveFirst
	%></font><font size="2">
</font>
<font color="#0000FF" size="2">
  			  <select size="1" name=lstcontact  >
			 <%do while not RS.eof
				%>
						<option value="<%=trim(RS("contact"))%>" <%If trim(request("lstcontact"))=trim(RS("contact")) then response.write "selected"%>><%=trim(RS("contact"))%>
				<%RS.movenext
				Loop%>
			</select><font size="2">
<%Else%>
</font>
			 <select size="1" name=lstcontact disabled >
				<option value="">N/A 
			  </select><font size="2">
<%End If%>
</font>
</font>
</td>
<td width="92" height="28" class="dark_cell"><STRONG><font size="2" face="Arial">Subject</font> 
      </STRONG>

  </td>
<td width="92" height="28" class="light_cell">
<font size="2">
<!--<select size="1" name=lstcontact  onchange="return changeact()">--></font>
<font color="#0000FF" size="2">
<input name=txtsubject  size=30 maxlength=30></font><font size="2"> </font>
</td>

</tr>
<tr>
<td width="92" height="28" valign=top class="dark_cell"><strong><font size="2" face="Arial">Notes</font></strong></td>
<td width="92" height="28" class="light_cell">
      
        <font size="2" face="Arial"><textarea rows="5" name="txtnotes" cols="23"></textarea></font>
      </td>

</tr>
<tr>
  <td width="92" height="28" class="dark_cell" ><strong><font size="2" face="Arial">Priority</font></strong>

</td>
<td width="92" height="28" class="light_cell" >
<font size="2" face="Arial" color="#000080"><strong>
<input name="txtpriority" size="1" maxlength="1" ></strong></font>&nbsp;&nbsp;1 - 9
      
  </td>
<td width="92" height="28" class="dark_cell"><font size="2" face="Arial"><STRONG>Reason&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  </STRONG></font>

  </td>
<td width="92" height="28" class="light_cell" >
<font color="#0000FF" size="2">
<%
Set dbrs1=server.CreateObject("ADODB.recordset")
Set rstemp=server.CreateObject("ADODB.recordset")  
sqltemp="select distinct * from codes where cfld_name='CTRANRESON' AND CDEFCODE='N'"
sqls="select distinct * from codes where cfld_name='CTRANRESON' AND CDEFCODE='N'"
dbrs1.open sqls,conn,1,3
rstemp.open sqltemp,conn,1,3%></font><font size="2"> </font>&nbsp;<select id=lstreason name=lstreason  size="1" >
  
  <%do while not rstemp.eof
				If trim(rstemp("cdiscrep"))=trim(dbrs1("cdiscrep")) Then
					rstemp.MoveNext 
				Else%>
					<option value="<%=rstemp("ccode_no")%>" <%If trim(rstemp("cdiscrep"))=trim(request("lstreason")) Then response.write "selected" %>><%=rstemp("Cdiscrep")%>
                    <%rstemp.movenext
				End If
  Loop%>
  
  </select>
</td>
</tr>
<tr>
<!--<td width="85" height="46"><STRONG><font color="#0000FF" size="2">Phone</font></STRONG>

</td>
<td width="118" height="46">
<font color="#0000FF" size="2">
<%'Set DBRS=server.Createobject("ADODB.Recordset")
'If trim(request("lstcontact"))="" Then
'	Sqlstat="select * from contact where  ccont_id='"&Session("customerid")&"'"
'	DBRS.Open sqlstat,Conn,3,3
'	DBRS.MoveFirst 
'Else
'	Sqlstat="select * from contact where  contact='"&trim(request("lstcontact"))&"'"
'	DBRS.Open sqlstat,Conn,3,3
'End If
%>

<input  id=txtphone name=txtphone size="20" value="<%'=trim(DBRS("phone"))%>" >
</font>
</td>
-->
</tr>
<tr>
  <td width="92" height="28" class="dark_cell"><strong><font size="2" face="Arial">Date</font></strong>

</td>
<td width="150" height="28" class="light_cell">
<font color="#0000FF" size="2" face="Arial">
<input  id=dtrandate name=dtrandate size="10" maxlength="10"></font><font face="Times New Roman" size="2">MM/DD/YY</font>
  </td>
<td width="92" height="28" class="dark_cell"><strong><font size="2" face="Arial">Time</font></strong>

  </td>
<td width="92" height="28" class="light_cell">
<font size="2">
<!--<select size="1" name=lstcontact  onchange="return changeact()">--></font><font color="#0000FF" size="2" face="Arial">
<input  id=txttime name=txttime size="11" maxlength="11"></font><font face="Times New Roman" size="2">HH:MM</font>
</td>

</tr>
<tr>
<td width="97" height="25" class="dark_cell">
<strong><font size="2" face="Arial">Duration</font></strong>
</td>
<td width="225" height="25" class="light_cell">
<font color="#0000FF" size="2" face="Arial">
<input  id=txtduration name=txtduration size="3" maxlength="3"></font><font size="2" face="Arial">
 </font> Min.
</td>
</tr>
<tr>
  <td height=23 colspan="4" bordercolorlight="#6394D6" bordercolordark="#6495D0" class="dark_cell"  align=right>
<input type=submit name =submit  value="Add"  onClick="return CheckDate()">
</td></tr>
</table>
  </center>
</div>
</FORM>

<%
Conn.close
Connsys.close
Set Conn=nothing
Set Connsys=nothing
End IF%>
</body>