<%@ Language=VBScript %>
<%Response.Expires=-1
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
	</script>	
<%END IF 

%>

<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<title>CRM - Find Store</title>
</HEAD>
<%
Session("ConnectionString") = "dsn=CRM;uid=aria;pwd=aria"
Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open Application("DataConnectionString")



strStore =Ucase( Trim(Request.Form("txtSearchStore"))) & "%"
strCity	 =Ucase( Trim(Request.Form("txtSearchCity"))) & "%"
strState =Ucase( Trim(Request.Form("txtSearchState"))) & "%"
strZipCode =Ucase( Trim(Request.Form("txtSearchZipCode"))) & "%"
%>
<BODY leftmargin="0" topmargin="0" bgColor=#aecae6 background="images/tile1.gif">
<%
if len(Trim(Session("ID")))>0 then
	compWork = "Y"
	
Set rsStoreResult = Server.CreateObject("ADODB.RECORDSET")
strSql = "Select * from Customer where Store like '" & strStore & "' AND Caddress3 like '" & strCity & "' AND Caddress4 like '" & strState & "' AND Caddress5 like '" & strZipCode & "' And Account='" & Session("ID") & "' Order By Type"
rsStoreResult.Open strSql,Conn

%>

<div align="center">
  <center>
<table border="0" width="100%" cellspacing="0" cellpadding="0">
  <tr>
    <td width="100%" align="center">
<p>
<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"   codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" WIDTH=80% HEIGHT=125 id="ShockwaveFlash1">
 <param name="_cx" value="13653">
 <param name="_cy" value="3307">
 <param name="Movie" value="../flash/CustNav.swf">
 <param name="Src" value="../flash/CustNav.swf">
 <param name="WMode" value="Transparent">
 <param name="Play" value="0">
 <param name="Loop" value="0">
 <param name="Quality" value="High">
 <param name="SAlign" value>
 <param name="Menu" value="0">
 <param name="Base" value>
 <param name="Scale" value="ExactFit">
 <param name="DeviceFont" value="0">
 <param name="EmbedMovie" value="0">
 <param name="BGColor" value="AECAE6">
 <param name="SWRemote" value><embed src="../flash/CustNav.swf" loop="false" menu="false" quality="medium" wmode="transparent" bgcolor="#AECAE6" WIDTH="100%" HEIGHT="172" TYPE="application/x-shockwave-flash" PLUGINSPAGE="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash">
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
<%End IF%>


<%IF Len(Trim(Session("rep")))>0  Then %>

	<%
	compWork = "Y"

	IF Len(Trim(Session("customerid")))=0 Then
		Response.Redirect("Repcust.asp")
	End IF

	Set rsStoreResult = Server.CreateObject("ADODB.RECORDSET")
	strSql = "Select * from Customer where Store like '" & strStore & "' AND Caddress3 like '" & strCity & "' AND Caddress4 like '" & strState & "' AND Caddress5 like '" & strZipCode & "' And Account='" & Session("customerid") & "' Order By Type"
	rsStoreResult.Open strSql,Conn

%>

<div align="center">
  <center>
<table border="0" width="100%" cellspacing="0" cellpadding="0">
  <tr>
    <td width="100%" align="center">
<p>
<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"    codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" WIDTH=80% HEIGHT=125 id="ShockwaveFlash1">
 <param name="_cx" value="13653">
 <param name="_cy" value="3307">
 <param name="Movie" value="flash/rebNav.swf">
 <param name="Src" value="flash/rebNav.swf">
 <param name="WMode" value="Transparent">
 <param name="Play" value="0">
 <param name="Loop" value="0">
 <param name="Quality" value="High">
 <param name="SAlign" value>
 <param name="Menu" value="0">
 <param name="Base" value>
 <param name="Scale" value="ExactFit">
 <param name="DeviceFont" value="0">
 <param name="EmbedMovie" value="0">
 <param name="BGColor" value="AECAE6">
 <param name="SWRemote" value><embed src="flash/rebNav.swf" loop="false" menu="false" quality="medium" wmode="transparent" bgcolor="#AECAE6" WIDTH="100%" HEIGHT="172" TYPE="application/x-shockwave-flash" PLUGINSPAGE="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash">
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
<%End if%>


<%
IF rsStoreResult.EOF and rsStoreResult.bof  Then
	Response.Write("No Records Found.")
Else
	rsStoreResult.MoveFirst 
%>


<%IF compWork = "Y" Then%>
<br>

<%'---------------------------------------------------------------------------------
  '								Print Results Part					
  '---------------------------------------------------------------------------------
%>


<table align=center border="1" width="750" bgcolor="#6495D0" bordercolor="#AECAE6">
<TR>
	<TD width=25%><Font size="2" face="Arial" color="#000080"><Strong>Store</Strong></Font>
	</TD>
	<TD Width=50%><font size="2" face="Arial" color="#000080"><strong>Store Name</strong></font>
	</TD>
	<TD Width=25%><Font size="2" face="Arial" color="#000080"><Strong>City</Strong></Font>
	</TD>
	<TD Width=25%><Font size="2" face="Arial" color="#000080"><Strong>Store</Strong></Font>
	</TD>
	<TD Width=25%><Font size="2" face="Arial" color="#000080"><Strong>Zip Code</Strong></Font>
	</TD>
</TR>
<%
Do While Not rsStoreResult.EOF 
strTemp  = "<TR>"
Response.Write(strTemp)

strTemp  = "<TD width=""25%""><Font face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)

' see if the store is the main or not
if Trim(rsStoreResult("Type"))= "M" then
	strTemp = "<a href=""OrdResoulveStore.asp?Store=MAIN"" >Main</a>"
else
	strTemp = "<a href=""OrdResoulveStore.asp?Store=" & rsStoreResult("Store") & """>" & rsStoreResult("Store") & "</a>"
end if
Response.Write(strTemp)

strTemp  = "<TD width=""25%""><Font size=""2"" face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
Response.Write(rsStoreResult("Stname")) 

strTemp  = "<TD width=""25%""><Font size=""2"" face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
Response.Write(rsStoreResult("Caddress3"))

strTemp  = "<TD width=""25%""><Font size=""2"" face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
Response.Write(rsStoreResult("Caddress4")) 

strTemp  = "<TD width=""25%""><Font size=""2"" face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
Response.Write(rsStoreResult("Caddress5"))  

rsStoreResult.MoveNext 
Loop
End if
%>

</table>


<P>&nbsp;</P>
<%End IF%>
</BODY>
</HTML>
