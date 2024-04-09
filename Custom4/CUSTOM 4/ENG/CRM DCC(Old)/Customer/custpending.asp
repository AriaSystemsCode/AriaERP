<%@ Language=VBScript %>
<%response.buffer=true
response.expires=-1%>
<%
If trim(Session("customerid"))="" Then
	Response.redirect "../repcust.asp"
End If%>


<html>
<head>
<Title>CRM - Contact Management</Title>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/common.css">

</head>
<script language="JavaScript">
<!--
function anyCheckEdit(form) {
var total = 0;
if (eval("form1.seqnum"))
{
	if (isNaN(form1.seqnum.length))
	{
		if(form1.seqnum.checked)total=1;		
	}	
	else
	{
		var max = form1.seqnum.length;
		for (var idx = 0; idx < max; idx++)
		 {
			if(eval("document.form1.seqnum[" + idx + "].checked") == true)
			
				total += 1;
		 }
	}
	
}

if (total==0 )
{
	alert("Please,select one of the contacts to Edit!");	
	return false;
}
return true;	
}
function anyCheckDelete(form){
var total = 0;
if (eval("form1.seqnum"))
{
	if (isNaN(form1.seqnum.length))
	{
		if(form1.seqnum.checked)total=1;		
	}	
	else
	{
		var max = form1.seqnum.length;
		for (var idx = 0; idx < max; idx++)
		 {
			if(eval("document.form1.seqnum[" + idx + "].checked") == true)
			
				total += 1;
		 }
	}
}
if (total==0 )
{
	alert("Please,select one of the contacts to Edit!");	
	return false;
}
return true;	
}


//-->
</script>
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
	
	</TD>
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
If trim(request("ERR"))="T" Then
'	response.write "<font color=navy face=Arial size=2><b>Please,select one of the activities to remove or modify!</b></font>"
End IF
Set conn=server.CreateObject("ADODB.connection")
Set rs=server.CreateObject("ADODB.recordset")
Conn.open Application("SystemConnectionString")
sqlstat="select  * from syschdul where ccont_id='"&Session("customerid")&"' AND cuser_id='"&Session("rep")&"' AND ccompleted='N'"
rs.ActiveConnection = Application("SystemConnectionString")
rs.open sqlstat,conn ',1,3
If  RS.EOF And  RS.BOF Then%>
		<form action="custpendaction.asp" method=post id=form1 name=form1 >
		<table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
		<tr><td><br>
		<b>Your open activities with the currently selected <%=Session("CustField")%> :</b>
		</td></tr>
		</table>	<br>	


			<table  border="1" bordercolor="#111111" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0" align=center>
			<tr>
				<td align="left" width="88" class="dark_cell">Activity</td>
				<td align="left" width="67" class="dark_cell">Date</td>
				<td align="left" width="67" class="dark_cell">Time</td>
				<td align="left" width="119" class="dark_cell">Contact</td>
				<td align="left" width="137" class="dark_cell">Reason</td>
				<td align="left" width="198" class="dark_cell">Subject</td>
			</tr>
			<TR>
				<TD colspan=6 class=light_cell align=center>No records found.</TD>
			</TR>
			<TR>
				<TD colspan=6 class=dark_cell align=right><input type=submit value="     Add     " id=add name=add></TD>
			</TR>

</Table>
</Form>
<%Else%>
		
		<table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
		<tr><td><br>
		<b>Your open activities with the currently selected customer :</b>
		</td></tr>
		</table>	<br>	
		<table border="0" width="100%" cellspacing="0" cellpadding="0">
			
			<tr>
				<td width=100%>
		<div align="center">
          <center>
		<table  border="1" bordercolor="#111111" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
			<tr>
				<td width="31" class="dark_cell">&nbsp;</td>
				<td align="left" width="88" class="dark_cell">
		            <b><font color="navy" size="2" face="arial">Activity</font></b>
				</td>
				<td align="left" width="67" class="dark_cell">
		            <b><font color="navy" size="2" face="arial">Date</font></b>
				</td>
				<td align="left" width="67" class="dark_cell">
		            <b><font color="navy" size="2" face="arial">Time</font></b>
				</td>
				<td align="left" width="119" class="dark_cell">
		            <b><font color="navy" size="2" face="arial">Contact</font></b>
				</td>
				<td align="left" width="137" class="dark_cell">
		            <b><font color="navy" size="2" face="arial">Reason</font></b>
				</td>
				<td align="left" width="198" class="dark_cell">
		            <b><font color="navy" size="2" face="arial">Subject</font></b>
				</td>
			</tr>
			
		<form action="custpendaction.asp" method=post id=form1 name=form1 >
		<input type="hidden" name="Typeline" value>
				<%do while not rs.eof%>
					<tr>
							<td align="center" width="31" class="light_cell"><font color="navy" size="2" face="Arial">
							<input type="radio" value="<%=RS("cseqnumber")%>" name="seqnum"></font></td>
							<td align="left" width="88" class="light_cell"><font color="navy" size="2" face="Arial">
							<%
							If rs("ctrantype")="C" Then 
								 Response.Write "Call"
							End If
							If rs("ctrantype")="A" Then 
								 Response.Write "Appointment"
							End If
							If rs("ctrantype")="T" Then 
								 Response.Write "To do"
							End If
							%>
							</font>&nbsp;</td>
								<td align="left" width="66" class="light_cell"><font color="navy" size="2" face="Arial"><%If trim(RS("dtrandate"))="12:00:00 AM" Then response.write " " Else response.write trim(RS("dtrandate"))%></font>&nbsp;</td>
								<td align="left" width="68" class="light_cell"><font color="navy" size="2" face="Arial"><%Response.Write rs("ctrantime") %></font>&nbsp;</td>
								<td align="left" width="120" class="light_cell"><font color="navy" size="2" face="Arial"><%If trim(rs("contact"))="" Then Response.Write "N/A" Else response.write rs("contact") %></font>&nbsp;</td>
								<td align="left" width="139" class="light_cell"><font color="navy" size="2" face="Arial">
								<%
								Set dbconn=server.CreateObject("ADODB.connection")
								dbconn.Open Application("DataConnectionString")
								Set dbrs1=server.CreateObject("ADODB.recordset")
								sqls="select distinct * from codes where cfld_name='CTRANRESON' AND CCODE_NO='"&rs("ctranreson")&"'AND CDEFCODE='N'"
								dbrs1.open sqls,dbconn
		    				Do while not dbrs1.EOF%>
									<%=dbrs1("Cdiscrep")%>
		    				<%dbrs1.MoveNext
		    				Loop
		    				dbrs1.Close
		    				dbconn.Close
								Set dbrs1=nothing
								Set dbconn=nothing%>
								</font>&nbsp;</td>
									<td align="left" width="198" class="light_cell"><font color="navy" size="2" face="Arial"><%Response.Write rs("csubject")%></font>&nbsp;</td>
		    
					</tr>
				
				<%rs.MoveNext
				loop%>
		 </table>
		  </center>
        </div>
		 <table align=center border=0>
		<tr >
		<td ><input type=submit value="     Add     " id=add name=add></td>
		<td ><input type=submit value=" Modify  " id=modify name=modify onClick="anyCheckEdit(this.form)"></td>
		<td ><input type=submit value="Remove" id=remove name=remove onClick="anyCheckDelete(this.form)"></td></tr>

		 </form>
 <%End If%>
<%
rs.Close
conn.Close
set rs=nothing
set conn=nothing

%>
</body>