<%@ Language=VBScript %>
<%response.buffer=true
Response.Expires=-1%>
<%
If trim(Session("customerid"))="" Then
	Response.redirect "../repcust.asp"
End If%>
<html>
<head>
<title>CRM - Contact Management</title>
<meta http-equiv="Content-Type" content="text/html;">
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/common.css">
<SCRIPT LANGUAGE="JavaScript">
function anyCheck(form) {
var total = 0;
var max = form.seqnum.length;
for (var idx = 0; idx < max; idx++) {
if (eval("document.form1.seqnum[" + idx + "].checked") == true ) {
    total += 1;
   }
}
if (total==0 )
{
alert("Please,select one of the activities to remove!");
return false;
}
return true;
}
var checkflag = "false";
function check(field) {
if (checkflag == "false") {
for (i = 0; i < field.length; i++) {
field[i].checked = true;}
checkflag = "true";
return "Uncheck All"; }
else {
for (i = 0; i < field.length; i++) {
field[i].checked = false; }
checkflag = "false";
return "Check All"; }
}

</script><SCRIPT LANGUAGE=javascript>
<!--
function SubmitNext()
{
	document.FORM2.action = 'orderhsave.asp';
	document.FORM2.submit()
}
//-->
</SCRIPT>
</head>
<body>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br><br></p>

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
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"PENDACT") <= 0 Then
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
on error resume next
'IF trim(request("ERR"))="T" Then
	'response.write "<font color=navy face=Arial size=2><b>Please,select one of the activities to remove!</b></font>"
'End If
'Session("customerid")="MA100"
set conn=server.CreateObject("ADODB.connection")
set rs=server.CreateObject("ADODB.recordset")
Conn.open Application("SystemConnectionString")
set conn2=server.CreateObject("ADODB.connection")
Conn2.open Application("DataConnectionString")
IF trim(request("SEQ"))<>"" Then
	sqls1="select  * from syschdul where cseqnumber='"&request("SEQ")&"'"
	set rs1=server.CreateObject("ADODB.recordset")
	rs1.Open sqls1,conn,1,3
'dtran2=trim(request("dttran2"))
'	txtuserid2=trim(request("txtuserid2"))
'	txtcontact2=trim(request("txtcontact2"))
'	txtsubject2=trim(request("txtsubject2"))
'	rs1("dtrandate")=trim(cdate(request("dttran2")))
'	rs1("cuser_id")=txtuserid2 
'	rs1("contact")=txtcontact2
'	rs1("csubject")=txtsubject2
'	rs1.UpdateBatch
End IF
sqlstat="select  * from syschdul where ccont_id='"&Session("customerid")&"' AND cuser_id='"&Session("rep")&"' AND ccompleted='Y'"
rs.open sqlstat,conn ,3,3
'sqls2="select distinct * from codes where cfld_name='CRESULTCD' AND CCODE_NO='"&rs("CRESULTCD")&"' AND CDEFCODE='N'"
'rs2.Open sqls2,conn2,3,3

If  RS.EOF And  RS.BOF Then%>
<table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
		<tr><td>
<br>
<b>Activity summary for the currently selected <%=Session("CustField")%> :</b>
</td></tr></table>
<table  border="1" bordercolor="#111111" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0" align=center>
	<tr>
		<td align="left" width="80" class="dark_cell">
            <b>Activity</b>
		</td>
		<td align="left" width="80" class="dark_cell">
            <b>Date</b>
		</td>
		<td align="left" width="138" class="dark_cell">
            <b>Contact</b>
		</td>
		<td align="left" width="104" class="dark_cell">
            <b>Result</b>
		</td>
		<td align="left" width="183" class="dark_cell">
            <b>Subject</b>
		</td>
	</tr>
	<TR>
		<TD class=light_cell align=center colspan=5>No records found</TD>
	</TR>
</Table>


<%Else
	%>
<table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
		<tr><td>
<br>
<b>Activity summary for the currently selected <%=Session("CustField")%> :</b>
</td></tr></table>
<br>
<table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
	<tr>
		<td width=100%>
<div align="center">
  <center>
<table  border="1" bordercolor="#111111" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<tr><td width="28" class="dark_cell">&nbsp;</td>
		<td align="left" width="80" class="dark_cell">
            <b>Activity</b>
		</td>
		<td align="left" width="80" class="dark_cell">
            <b>Date</b>
		</td>
		<td align="left" width="138" class="dark_cell">
            <b>Contact</b>
		</td>
		<td align="left" width="104" class="dark_cell">
            <b>Result</b>
		</td>
		<td align="left" width="183" class="dark_cell">
            <b>Subject</b>
		</td>
	</tr>
	
<form action="custhistaction.asp" method=post id=form1 name=form1  >
		<%do while not rs.eof%>
			<tr>
			<td align="center" width="28" class="light_cell"><font color="navy" size="2" face="Arial"><input type="checkbox" name="seqnum" value="<%=RS("cseqnumber")%>" ></font></td>
					
							<td align="left" width="80" class="light_cell"><%
    if rs("ctrantype")="C" then 
    Response.Write "Call"
   end if
    if rs("ctrantype")="A" then 
    Response.Write "Appointment"
   end if
    if rs("ctrantype")="T" then 
    Response.Write "To do"
   end if
    %>&nbsp;</td>
							<td align="left" width="80" class="light_cell"><%If trim(rs("dtrandate"))="12:00:00 AM" Then response.write " " Else response.write trim(rs("dtrandate"))%>&nbsp;</td>
							<td align="left" width="138" class="light_cell"><%If trim(rs("contact"))="" Then response.write "N/A" else Response.Write rs("contact") %>&nbsp;</td>
							<td align="left" width="112" class="light_cell">
			<%set rs2=server.CreateObject("ADODB.recordset")
			sqls2="select distinct * from codes where cfld_name='CRESULTCD' AND CCODE_NO='"&rs("CRESULTCD")&"' AND CDEFCODE='N'"
			rs2.Open sqls2,conn2,3,3
			response.write rs2("Cdiscrep")
			rs2.close
			set rs2=nothing%>
    	</font>&nbsp;</td>
							<td align="left" width="183" class="light_cell"><font color="navy" size="2" face="Arial"><%Response.Write rs("csubject")%></font>&nbsp;</td>
    
			</tr>
		
		<%rs.MoveNext
loop%>
 </table>
  </center>
</div>
 <table align=center border=0>
<tr>
<td >
<input type=button value="Check All"  onClick="this.value=check(this.form.seqnum)"> 
</td>
<td>
<input type=submit value="Remove" id=remove name=remove onClick="anyCheck(this.form)"></td>
</tr>
 </form>
 <%End If%>
<%rs.Close
conn.Close
set rs=nothing
set conn=nothing%>
</BODY>