<%@ Language=VBScript %>
<%Response.Expires=-1

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
set rsdumm = Session("RSStyStruct")

Session("RSStyStruct").MoveFirst

IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
Else
	Dim strStyle, strcoll, strasd
	strStyle = ""
	DO While Not Session("RSStyStruct").Eof
		strcoll = Ucase(request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value)))
		'Session(Trim(Session("RSStyStruct").Fields("cisegsdes").Value)) = strcoll
		intTemp = Session("RSStyStruct").Fields("nisegsize")
		intCount = cdbl(intTemp) - Len(strcoll)
		Do While intCount > 0
			intCount = intCount - 1
			strcoll = strcoll & "%"
		Loop


		strStyle = strStyle & strcoll & Trim(Session("RSStyStruct").Fields("cisegsepr").Value)
		Session("RSStyStruct").MoveNext
	Loop
	Session("RSStyStruct").movefirst
	str1 = request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value))
	str1= str1 & "%"

End IF

Set RSSTyResult = Server.CreateObject("ADODB.RECORDSET")
strSql = "Select * from style where style like '" & strStyle & "' And season='" & Session("Season") & "' And cdivision='" & Trim(Session("Division"))  & "' And status='A'"  
RSSTyResult.Open strSql,Conn
%>
<BODY leftmargin="0" topmargin="0" bgColor=#aecae6>
<Center><!-- text used in the movie-->
<!--ustomer elationship anagement -->
<OBJECT classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
      codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0"
 WIDTH=780 HEIGHT=140 id=ShockwaveFlash1>
 <PARAM NAME=movie VALUE="CustNav.swf"> 
 <PARAM NAME=loop VALUE=false> 
 <PARAM NAME=menu VALUE=false> 
 <PARAM NAME=quality VALUE=medium> 
 <PARAM NAME=wmode VALUE=transparent> 
 <PARAM NAME=bgcolor VALUE=#AECAE6> 
 <EMBED src="CustNav.swf" loop=false 
 menu=false quality=medium wmode=transparent bgcolor=#AECAE6  WIDTH=100% HEIGHT=172 TYPE="application/x-shockwave-flash" PLUGINSPAGE="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash"></EMBED>
</OBJECT>
</center>
<BR>
<%
IF RSSTyResult.EOF and RSSTyResult.bof  Then
	Response.Write("No Records Found.")
Else
	RSSTyResult.MoveFirst 
%>
<table align=center border="0" width="80%" bgcolor="#6495D0" bordercolor="#AECAE6">
<TR>
	<TD width=25%><Font face="Arial" color="#000080"><Strong>Style</Strong></Font>
	</TD>
	<TD Width=50%><Font face="Arial" color="#000080"><Strong>Description</Strong></Font>
	</TD>
	<TD Width=25%><Font face="Arial" color="#000080"><Strong>Price</Strong></Font>
	</TD>
</TR>
<%
Do While Not RSSTyResult.EOF 
strTemp  = "<TR>"
Response.Write(strTemp)

strTemp  = "<TD width=""25%""><Font face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
'<A href="sorderh.asp">Remote Order Entry</A>
'strTemp = "<a href=""resolvsty.asp?sty=" & RSSTyResult("style") & " &desc1=" & RSSTyResult("desc1") & ">" & RSSTyResult("style") & "</a>"
strTemp = "<a href=""resolvsty.asp?sty=" & RSSTyResult("style") & "&desc1=" & RSSTyResult("desc1") & """>" & RSSTyResult("style") & "</a>"
Response.Write(strTemp)

strTemp  = "<TD width=""25%""><Font face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
Response.Write(RSSTyResult("desc1")) 

strTemp  = "<TD width=""25%""><Font face=""Arial"" color=""#ffffff"">"
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
