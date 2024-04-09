<%@ Language=VBScript %>
<%Response.Expires=-1%>
<%
if Trim(Session("ID")) = "" then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 

%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
</HEAD>
<%
Session("ConnectionString") = "dsn=CRM;uid=aria;pwd=aria"
Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open(Session("ConnectionString"))

Dim rsdumm
Set rsdumm = server.CreateObject("adodb.recordset")
set rsdumm = Session("rsRetStyStruct")

Session("rsRetStyStruct").MoveFirst

IF Session("rsRetStyStruct").EOF And Session("rsRetStyStruct").BOF Then
Else
	Dim strStyle, strcoll, strasd
	strStyle = ""
	DO While Not Session("rsRetStyStruct").Eof
		strcoll = Ucase(request(Trim(Session("rsRetStyStruct").Fields("cisegsdes").Value)))
		'Session(Trim(Session("rsRetStyStruct").Fields("cisegsdes").Value)) = strcoll
		intTemp = Session("rsRetStyStruct").Fields("nisegsize")
		intCount = cdbl(intTemp) - Len(strcoll)
		Do While intCount > 0
			intCount = intCount - 1
			strcoll = strcoll & "%"
		Loop


		strStyle = strStyle & strcoll & Trim(Session("rsRetStyStruct").Fields("cisegsepr").Value)
		Session("rsRetStyStruct").MoveNext
	Loop
	Session("rsRetStyStruct").movefirst
	str1 = request(Trim(Session("rsRetStyStruct").Fields("cisegsdes").Value))
	str1= str1 & "%"

End IF

Set RSSTyResult = Server.CreateObject("ADODB.RECORDSET")
'strSql = "Select * from style where style like '" & strStyle & "' And season='" & Session("Season") & "' And cdivision='" & Trim(Session("Division"))  & "' And status='A'"
strSql = "Select * from Style where Style like '" & strStyle & "' And Cdivision='" & Trim(Session("selectDivision"))  & "' And status='A'"    
RSSTyResult.Open strSql,Conn
%>
<BODY leftmargin="0" topmargin="0" bgColor=#aecae6  background="Tile1.gif">

<div align="center">
  <center>
  <table border="0" width="100%" cellspacing="0" cellpadding="0">
    <tr>
      <td width="100%" align="center"><p>
<OBJECT align=baseline classid=clsid:D27CDB6E-AE6D-11cf-96B8-444553540000 
   codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" 
height=142 id=ShockwaveFlash1 width=800 border = 0>
    <param name="_cx" value="21167">
    <param name="_cy" value="3704">
    <param name="Movie" value="flash/CustNav.swf">
    <param name="Src" value="flash/CustNav.swf">
    <param name="WMode" value="Transparent">
    <param name="Play" value="0">
    <param name="Loop" value="0">
    <param name="Quality" value="Medium">
    <param name="SAlign" value>
    <param name="Menu" value="0">
    <param name="Base" value>
    <param name="Scale" value="ShowAll">
    <param name="DeviceFont" value="0">
    <param name="EmbedMovie" value="0">
    <param name="BGColor" value="AECAE6">
    <param name="SWRemote" value><embed src="flash/CustNav.swf" align="baseline" border="0" width="100" height="172" loop="false" menu="false" quality="medium" wmode="transparent" bgcolor="#AECAE6" type="application/x-shockwave-flash" pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash" designtimesp="22693" DESIGNTIMESP="23127" DESIGNTIMESP="23294" DESIGNTIMESP="19128">
</OBJECT>
</p>
      </td>
    </tr>
    <tr>
      <td width="100%" align="center"><p></p></td>
    </tr>
  </table>
  </center>
</div>
<BR>
<%
IF RSSTyResult.EOF and RSSTyResult.bof  Then
	Response.Write("No Records Found.")
Else
	RSSTyResult.MoveFirst 
%>
<table align=center border="1" width="800" bgcolor="#6495D0" bordercolor="#AECAE6">
<TR>
	<TD width=25%><Font size="2" face="Arial" color="#000080"><Strong>Style</Strong></Font>
	</TD>
	<TD Width=50%><Font size="2" face="Arial" color="#000080"><Strong>Description</Strong></Font>
	</TD>
	<TD Width=25%><Font size="2" face="Arial" color="#000080"><Strong>Price</Strong></Font>
	</TD>
</TR>
<%
Do While Not RSSTyResult.EOF 
strTemp  = "<TR>"
Response.Write(strTemp)

strTemp  = "<TD width=""25%""><Font size=""2"" face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
'<A href="sorderh.asp">Remote Order Entry</A>
'strTemp = "<a href=""ReturnResolvSty.asp?sty=" & RSSTyResult("style") & " &desc1=" & RSSTyResult("desc1") & ">" & RSSTyResult("style") & "</a>"
strTemp = "<a href=""ReturnResolvSty.asp?sty=" & RSSTyResult("style") & "&desc1=" & RSSTyResult("desc1") & """>" & RSSTyResult("style") & "</a>"
Response.Write(strTemp)

strTemp  = "<TD width=""25%""><Font size=""2"" face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
Response.Write(RSSTyResult("desc1")) 

strTemp  = "<TD width=""25%""><Font size=""2"" face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
Response.Write(RSSTyResult("pricea")) 

RSSTyResult.MoveNext 
Loop
End if
%>

</table>


<P>&nbsp;</P>

</BODY>
</HTML>
