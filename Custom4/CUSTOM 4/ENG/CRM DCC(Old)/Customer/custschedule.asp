<%@ Language=VBScript %>
<%response.buffer=true
response.expires=-1%>
<%
If trim(Session("customerid"))="" Then
	Response.redirect "../repcust.asp"
End If%>

<html>
<head>
<title>CRM - Contact Management</title>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/common.css">
<script language="JavaScript">
function CheckDate()
{ 
	if (document.FORM1.dttrandate.value=="" )
	{
	}
	else	
	{
		dateObjfrom =new Date(document.FORM1.dttrandate.value);
		if (!dateObjfrom.getDate())
		{
			alert ("Please enter valid date!");
			document.FORM1.dttrandate.focus();
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
<p><br><br></p>

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
	<P>Your currently selected customer is <%=Session("customerid")%> - <%=RSTemp.fields("btname").value%></P>
	</TD>
	<TD align=right><a href="repcust.asp"><b>Get Customer</b></a></TD>
	<!-- ARD -->
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
sqlsTemp="select * from syschdul where ccompleted='N' And cseqnumber='"&request("SEQ")&"'" 
RSSys.Open sqlsTemp,connSys,1,3
If  RSSys.EOF And  RSSys.BOF Then
	response.write "<b>No records Found.</b>"
Else
%>
<table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
		<tr><td>
<br>
<b>Modify the following activity for the currently selected customer :</b>
</td></tr></table>
<br>
<div align="center">
  <center>
<table width="95%" height="199" bordercolor="#111111" border="1" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<script Language="JavaScript"><!--
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

  if (decPoints > 1)
  {
    alert("Please enter a valid number in the \"Priority\" field.");
    theForm.txtpriority.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseFloat(allNum);
  if (chkVal != "" && !(prsVal >= "1" && prsVal <= "9"))
  {
    alert("Please enter a value greater than or equal to \"1\" and less than or equal to \"9\" in the \"Priority\" field.");
    theForm.txtpriority.focus();
    return (false);
  }

  if (theForm.dttrandate.value == "")
  {
    alert("Please enter a value for the \"Date\" field.");
    theForm.dttrandate.focus();
    return (false);
  }

  if (theForm.dttrandate.value.length > 10)
  {
    alert("Please enter at most 10 characters in the \"Date\" field.");
    theForm.dttrandate.focus();
    return (false);
  }

  if (theForm.txttime.value == "")
  {
    alert("Please enter a value for the \"Time\" field.");
    theForm.txttime.focus();
    return (false);
  }
  if (theForm.txttime.value != "")
  {
		var TimeNo = isNaN(theForm.txttime.value);
/*		if (TimeNo)
			
		else
			
			//return(false);
  */}

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

  if (decPoints > 1)
  {
    alert("Please enter a valid number in the \"Duration\" field.");
    theForm.txtduration.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseFloat(allNum);
  if (chkVal != "" && !(prsVal >= "1" && prsVal <= "999"))
  {
    alert("Please enter a value greater than or equal to \"1\" and less than or equal to \"999\" in the \"Duration\" field.");
    theForm.txtduration.focus();
    return (false);
  }
  return (true);
}
//--></script><FORM id=FORM1 name=FrontPage_Form1 method=post  action="custschaction.asp?SEQ=<%=trim(RSSys("cseqnumber"))%>" onsubmit="return FrontPage_Form1_Validator(this)" >

		<tr>
		<td colspan="4" class="dark_cell" >
        <p align="center">
			<INPUT id=radio1  name=radio1 type=radio value="C" <%If RSSys("ctrantype")="C" Then response.write "checked"%> style="height: 20; width: 19"><b><U>C</U>all&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        </b><INPUT id=radio1 name=radio1 type=radio  value="A" <%If RSSys("ctrantype")="A" Then response.write "checked"%> style="height: 20; width: 21"><b><U>A</U>ppointment&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        </b><INPUT id=radio1 name=radio1 type=radio value="T" <%If RSSys("ctrantype")="T" Then response.write "checked"%>   style="height: 20; width: 21"><b><U>T</U>odo</b>
        </p>
		</td>
		</tr>
		<tr><td width="92" height="28" class="dark_cell"><strong>Contact</strong>
		</td>
		<td width="92" height="28" class="light_cell">
		<%If trim(RSsys("contact"))<>"" Then%>
		<select size="1" name=lstcontact  >
		<option value="<%=trim(RSsys("contact"))%>" <%If trim(request("lstcontact"))=trim(RSSys("contact")) then response.write "selected"%>><%=trim(RSsys("contact"))%>
		<%End If%> 
		<%If not RS.EOF and not rs.BOF then
					  RS.MoveFirst
						do while not RS.eof
								If trim(RSsys("contact"))<>trim(RS("contact")) Then %>
									<option value="<%=trim(RS("contact"))%>" <%If trim(request("lstcontact"))=trim(RS("contact")) then response.write "selected"%>><%=trim(RS("contact"))%>
								    <%	RS.movenext
								Else
									RS.Movenext
								End If
						Loop%>
		</select> 
		<%Else%>
		<select size="1" name=lstcontact  disabled>
		<option value="">N/A 
		</select>
		<%End IF%>
   </td>
		<td width="92" height="28" class="dark_cell"><STRONG>Subject 
		      </STRONG>
		</td>
		<td width="126" height="28" class="light_cell">
		
		<!--<select size="1" name=lstcontact  onchange="return changeact()">-->
        <input name=txtsubject value="<%=trim(rssys("csubject"))%>" size=30 maxlength="30"> 
		</td>
		</tr>
		<tr>
		<td width="92" height="28" valign=top class="dark_cell"><strong>Notes</strong></td>
        <td width="92" height="28" class="light_cell">
		      <textarea rows="5" name="txtnotes" cols="23"><%=rssys("mnotes")%></textarea>
		      </td>
		</tr>
		<tr>
		  <td width="92" height="28" class="dark_cell"><strong>
          <font size="2" face="Arial">Priority</font></strong>
		</td>
		<td width="92" height="28" class="light_cell">
		<font size="2" face="Arial" color="#000080"><strong>
        <input name="txtpriority" value="<%If trim(rssys("cpriority")) <>"" Then response.write trim(rssys("cpriority")) %>" size="1" maxlength="1" ></strong>
		  </font>
		      1 - 9
		  </td>
		<td width="92" height="28" class="dark_cell"><STRONG>Reason
		  </STRONG>
		</td>
		<td width="92" height="28" class="light_cell">
		          <%        Set dbrs1=server.CreateObject("ADODB.recordset")
		          			Set rstemp=server.CreateObject("ADODB.recordset")  
							sqltemp="select distinct * from codes where cfld_name='CTRANRESON' AND CDEFCODE='N'"
							sqls="select distinct * from codes where cfld_name='CTRANRESON' AND CCODE_NO='"&RSsys("ctranreson")&"'AND CDEFCODE='N'"
							dbrs1.open sqls,conn,1,3
							rstemp.open sqltemp,conn,1,3
							%>
							 <select id=lstreason name=lstreason  size="1" >
							 <%if not dbrs1.eof then%>
								<option  name="<%=dbrs1("ccode_no")%>" <%If trim(dbrs1("cdiscrep"))=trim(request("lstreason")) Then response.write "selected" %>><%=dbrs1("Cdiscrep")%>
							<%else%>	
								<option  value="XXXXXX">No Data</option>
							<%end if%>
						<%'do while not rstemp.eof
							'	If trim(rstemp("cdiscrep"))=trim(dbrs1("cdiscrep")) Then
							'		rstemp.MoveNext 
							'	Else%>
									<option value="<%'=rstemp("ccode_no")%>" <%'If trim(rstemp("cdiscrep"))=trim(request("lstreason")) Then response.write "selected" %>><%'=rstemp("Cdiscrep")%>
								<%'rstemp.movenext
								'End If
						'Loop%>  
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
		  <td width="92" height="28" class="dark_cell"><strong>Date</strong>

		</td>
		<td width="92" height="28" class="light_cell">
        <input  id=dttrandate name=dttrandate  value="<%If trim(rssys("dtrandate"))="12:00:00 AM" Then response.write " " Else response.write trim(rssys("dtrandate"))%>" size="10" maxlength="10">MM/DD/YY&nbsp;&nbsp;
		  </td>
		<td width="92" height="28" class="dark_cell"><strong>Time</strong>

		  </td>
		<td width="92" height="28" class="light_cell">
		
		<!--<select size="1" name=lstcontact  onchange="return changeact()">-->
        <input  id=txttime0 name=txttime value="<%=trim(rssys("ctrantime"))%>" size="11" maxlength="11">HH:MM
		</td>

		</tr>
		<tr>
		  <td width="92" height="28" class="dark_cell"><strong>Result</strong>

		</td>
		<td width="92" height="28" class="light_cell">
		<%
			  set dbrs1=server.CreateObject("ADODB.recordset")
			  set dbrs2=server.CreateObject("ADODB.recordset")
				sqls2="select * from codes where cfld_name='CRESULTCD' AND CDEFCODE='N'"
				dbrs2.open sqls2,conn,3,3
				dbrs2.movefirst
				sqls="select distinct * from codes where cfld_name='CRESULTCD' AND CCODE_NO='"&rssys("CRESULTCD")&"'AND CDEFCODE='N'"
				dbrs1.open sqls,conn,3,3
			%>
		
			<select id=lstresult name=lstresult  size="1">
			<%if not dbrs1.eof then%>
			  <option name="<%=dbrs1("ccode_no")%>" <%If trim(dbrs1("cdiscrep"))=trim(request("lstresult")) Then response.write "selected" %>><%=dbrs1("Cdiscrep")%>
			<%else%>
				<option value="XXXX">No Results</option>
			<%end if%>    
		  <%
		  'do while not dbrs2.eof
			'			If  trim(dbrs1("cdiscrep"))=trim(dbrs2("cdiscrep")) Then
			'				dbrs2.MoveNext 
			'			Else%>
							<option value="<%'=dbrs2("ccode_no")%>" <%'If trim(dbrs2("cdiscrep"))=trim(request("lstresult")) Then response.write "selected" %>><%'=dbrs2("Cdiscrep")%>
		                    <%dbrs2.movenext
			'			End If
		  'Loop%>
		  
		  </select> 
		
		  </td>

		</tr>
		<tr>
		<td width="92" height="28" class="dark_cell">
		<strong>Duration</strong>
		</td>
		<td width="92" height="28" class="light_cell">
		        <input  id=txtduration name=txtduration size="3" value="<%=trim(rssys("nestdur"))%>" maxlength="3"> Min.
		
		</td>
		</tr>
		<td  height="23" colspan=4 align=center class="dark_cell">
		<input type=submit name=submit value="Complete Activity" onclick="return CheckDate()">
		<input type=submit name =submit value="         Update        "  onclick="return CheckDate()">
        <input type=Reset value="        Reset          "  name="cancel">
		</td>
		</form>
		</table>
		</center>
</div>
		</FORM>
<%End IF%>
</BODY>
</HTML>