<%@ Language=VBScript %>
<HTML>
<HEAD><LINK REL=stylesheet HREF="crmmain.css" TYPE="text/css">
<title>CRM - Choose Customer</title>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
</HEAD>
<BODY bgcolor="#aecae6" topmargin="0" leftmargin="0">

<Center>
<OBJECT classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
          codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0"
 WIDTH=80% HEIGHT=125 id=ShockwaveFlash1>
 <param name="_cx" value="16325">
 <param name="_cy" value="3307">
 <param name="Movie" value="flash/rebNav.swf">
 <param name="Src" value="flash/rebNav.swf">
 <param name="WMode" value="Transparent">
 <param name="Play" value="0">
 <param name="Loop" value="0">
 <param name="Quality" value="Medium">
 <param name="SAlign" value>
 <param name="Menu" value="0">
 <param name="Base" value>
 <param name="Scale" value="ExactFit">
 <param name="DeviceFont" value="0">
 <param name="EmbedMovie" value="0">
 <param name="BGColor" value="AECAE6">
 <param name="SWRemote" value><embed src="flash/rebNav.swf" loop="false" menu="false" quality="medium" wmode="transparent" bgcolor="#AECAE6" WIDTH="100%" HEIGHT="172" TYPE="application/x-shockwave-flash" PLUGINSPAGE="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash">
</OBJECT>
</center>

<FORM action="findcustomer.asp" method=POST id=form1 name=form1>
<CENTER>
<table bgcolor="#6495d0" bordercolor="#aecae6" border="1" width="50%" >
	 <tr>
		<td Align="middle" valign="center" >
			<INPUT name=txtCustomer size="6"  maxlength="6" value="<%=Request("txtCustomer")%>">  
		</td>
     </tr>
     <tr>
		<td Align="middle" valign="center" ><input type="submit" name="B2" value="Search"><input type="reset" name="B3" value="Reset">
		</td>
     </tr>
</table>
</CENTER>
</FORM>
<BR>
<BR>

<%' Display the second table %>
<%
set conn=server.CreateObject("ADODB.connection")
conn.Open "dsn=CRM;uid=aria;pwd=aria"

set rsCehckCustomerExist =Server.CreateObject ("ADODB.Recordset")
strSQL = "SELECT Account,Stname FROM Customer WHERE Type='M' AND (Salesrep='" & Session("Rep") & "' OR Rep2='" & Session("Rep") & "') AND Account ='" & Trim(Ucase(Request("txtCustomer"))) & "'"
rsCehckCustomerExist.Open strSQL,conn
if rsCehckCustomerExist.BOF AND rsCehckCustomerExist.EOF then
				set rsCustomers = server.CreateObject ("ADODB.recordset")
				strSQL = "SELECT Account,Stname FROM Customer WHERE Type='M' AND (Salesrep='" & Session("Rep") & "' OR Rep2='" & Session("Rep") & "') AND Account Like '" & Trim(Ucase(Request("txtCustomer"))) & "%'"
				rsCustomers.Open strSQL,conn
				'Response.Write strSQL & "<BR><BR>"
				'if rsCheckRep.EOF and rsCheckRep.BOF then
				'	Response.Write("Access denied. Invalid user ID or password.")
				'else
				'	Response.Write "welcome"
				'end if
				%>
				<%
				if rsCustomers.EOF and rsCustomers.BOF then
				Response.Write " NO such customer exist"
				else
				%>
				<CENTER>
				<table align=center border="1" width="80%" bgcolor="#6495D0" bordercolor="#AECAE6">
					<TR>
						<TD width=25%><Font face="Arial" color="#000080"><Strong>Customer ID</Strong></Font>
						</TD>
						<TD Width=50%><Font face="Arial" color="#000080"><Strong>Customer Name</Strong></Font>
						</TD>
					</TR>
					<%rscustomers.movefirst
					do while not rsCustomers.eof%>
					<TR>
					<TD width="25%">
						<Font face="Arial" color="#ffffff">
						<a href="Repcustomer.asp?custid=<%=rsCustomers("account")%>"><%=rsCustomers("account")%></a>
						</font>
					</td>
					<TD width="25%">
						<Font face="Arial" color="#ffffff"><%=rsCustomers("Stname")%></font>
					</td>
					<%rsCustomers.MoveNext()
						loop%>

				</table>
				</CENTER>
				<%end if ' for the customers if exist or not%>
<%
else
	Response.Redirect ("repcustomer.asp")
end if ' for if that cusomer no. writen exist or not..

%>
<%session("customerid")=request("custid")
%>
</BODY>
</HTML>
