<%@ Language=VBScript %>
<%Response.Buffer=true%>
<HTML>
<HEAD>
<Title>CRM - Invalid Login</Title>
<LINK REL=stylesheet HREF="crmmain.css" TYPE="text/css">
<body bgcolor="#aecae6" topmargin="0" leftmargin="0" background="images/tile1.gif">
<%session("ID")=""
Session("customerid") = ""


set conn=server.CreateObject("ADODB.connection")
Set SYSConn = server.CreateObject("ADODB.connection")
set Session("RSCust")=server.CreateObject("ADODB.recordset")


Set objFpcmd = server.CreateObject("FPCMD.FPCMD")


conn.Open Application("DataConnectionString")
SYSConn.Open Application("SystemConnectionString")

set rsCheckRep = server.CreateObject ("ADODB.recordset")
strSQL = "SELECT * FROM Salesrep WHERE Repcode='" & Ucase(Request("UserName")) & "'"
rsCheckRep.Open strSQL,conn

if rsCheckRep.EOF and rsCheckRep.BOF then%>
	<div align="center">
	<center>
  <table border="0" width="100%" cellspacing="0" cellpadding="0">
    <tr>
      <td width="100%" align="center"><p>
				<object align="baseline" classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"      codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" height="125" id="ShockwaveFlash1" width="80%" border="0">
					<param name="_cx" value="11748">
					<param name="_cy" value="3307">
					<param name="Movie" value="flash/emptyNav.swf">
					<param name="Src" value="flash/emptyNav.swf">
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
					<param name="SWRemote" value>
					<embed src="flash/emptyNav.swf" align="baseline" border="0" width="100" height="172" loop="false" menu="false" quality="medium" wmode="transparent" bgcolor="#AECAE6" type="application/x-shockwave-flash" pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash" designtimesp="22693" DESIGNTIMESP="23127" DESIGNTIMESP="23294" DESIGNTIMESP="19128">
				</object>
				</p>
      </td>
    </tr>
    <tr>
      <td width="100%" align="center"><p></p></td>
    </tr>
	</table>
  </center>
</div>
<Table width=80% border=0 align=center>
<TR>
<TD align=center><Font face=arial size=2 color=#000080><b><br>
<%	Response.Write("Invalid user ID or password.")%>
</b></font></TD></TR></TAble>
<%
else
	rsCheckRep.Close
	strSQL = "SELECT * FROM syuuser WHERE cuser_id='" & Ucase(Request("UserName")) & "'"
	rsCheckRep.Open strSQL,SYSConn
	
	
if Trim(rsCheckRep("cusr_pass")) = objfpcmd.docmd("return sys(2007,'" & Ucase(Request("Pswd")) & "')") then
	Session("Rep") = Ucase(Request("UserName"))
	Response.redirect("Repcust.asp")
Else
%>

	<div align="center">
	<center>
  <table border="0" width="100%" cellspacing="0" cellpadding="0">
    <tr>
      <td width="100%" align="center"><p>
				<object align="baseline" classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"       codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" height="125" id="ShockwaveFlash1" width="80%" border="0">
					<param name="_cx" value="11748">
					<param name="_cy" value="3307">
					<param name="Movie" value="flash/emptyNav.swf">
					<param name="Src" value="flash/emptyNav.swf">
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
					<param name="SWRemote" value>
					<embed src="flash/emptyNav.swf" align="baseline" border="0" width="100" height="172" loop="false" menu="false" quality="medium" wmode="transparent" bgcolor="#AECAE6" type="application/x-shockwave-flash" pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash" designtimesp="22693" DESIGNTIMESP="23127" DESIGNTIMESP="23294" DESIGNTIMESP="19128">
				</object>
				</p>
      </td>
    </tr>
    <tr>
      <td width="100%" align="center"><p></p></td>
    </tr>
	</table>
  </center>
</div>
<Table width=80% border=0 align=center>
<TR>
<TD align=center><Font face=arial size=2 color=#000080><b><br>


<%
Response.Write("<BR>Invalid user ID or password.")		
end if
	
end if ' for the sales rep not exist.


%>
</body></html>
