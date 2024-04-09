<%@ LANGUAGE="VBSCRIPT" %>
<%
if Trim(Session("ID")) = "" then
'Response.redirect "default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
	<%End if



Session("ConnectionString") = "dsn=CRM;uid=aria;pwd=aria"
Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open Application("DataConnectionString")

Set RSCodes = Server.CreateObject("ADODB.RECORDSET")
strSql = "Select ccode_no, cdiscrep, cdefcode from codes where cfld_name='CSTYGROUP' And cdefcode='N'"
RSCodes.Open strSql,Conn


%>
<html>

<head>
<title>CRM - Remote Order - Style Selection Criteria</title>

</head>

<body bgcolor="#aecae6" topmargin="0" leftmargin="0" background="images/tile1.gif">
<div align="center">
  <center>
<table border="0" width="100%" cellspacing="0" cellpadding="0">
  <tr>
    <td width="100%" align="center">
<p>
<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" WIDTH=80% HEIGHT=125 id="ShockwaveFlash1">
 <param name="_cx" value="13653">
 <param name="_cy" value="3307">
 <param name="Movie" value="flash/CustNav.swf">
 <param name="Src" value="flash/CustNav.swf">
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
 <param name="SWRemote" value><embed src="flash/CustNav.swf" loop="false" menu="false" quality="medium" wmode="transparent" bgcolor="#AECAE6" WIDTH="100%" HEIGHT="172" TYPE="application/x-shockwave-flash" PLUGINSPAGE="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash">
</object>
</p>
    </td>
  </tr>
  <tr>
    <td width="100%" align="center"><p><img width=80% border="0" src="images/remoteorder0001.jpg"></p></td>
  </tr>
</table>
  </center>
</div>


<Center>
<form action="findstyle.asp?firsttime=T" method="post">
	<p><font size="2" face="Aria"><strong> </p>
   <table bgcolor="#6495d0" bordercolor="#aecae6" border="1" width="50%" >
		 <TR>
			<td Align="middle" valign="center" width="20%">
				<font color="ivory" size="2" face="arial">
				<strong>
				<table>
				<%
				IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
				Else
					strTemp = "<TD bgColor=#6495D0 colspan=10>"
					Response.Write(strTemp)
					Session("RSStyStruct").MoveFirst
					DO While Not Session("RSStyStruct").Eof
						IF Len(Session("getstyle"))=0 Then
							strValue = ""
						Else
							strValue = Session(Trim(Session("RSStyStruct").fields("cisegsdes")))
						End IF
						
						
						strTemp ="<font face=Arial color=#000080 size=2>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</font>" 
						strTemp = strTemp & " " & Trim(Session("RSStyStruct").fields("cisegsepr")) & " "
						Response.Write(strTemp)
						Session("RSStyStruct").MoveNext
					Loop
				End IF
				%>
				</table>
				</strong></font>
			</TD>
			<TD Align="middle" valign="center" width="20%">
				<FONT color=ivory face=arial size=2>
				<STRONG>Group</STRONG></FONT><BR>
			</TD>
		 </TR>
		 <tr>
			<td Align="middle" valign="center" >

			<%
				IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
				Else
					Session("RSStyStruct").MoveFirst
					DO While Not Session("RSStyStruct").Eof
						'strTemp = "<INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" 
						if Request("firsttime")="T" then
							'Response.Write("bardo da7'al")
							strTemp = "<INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Request((Trim(Session("RSStyStruct").fields("cisegsdes")))) & ">"
							'Response.Write strTemp & "Dakhal"
							
						else 
							strTemp = "<INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) & ">"
							'Response.Write strTemp
							'Response.Write("da7'al")
						end if
						'strTemp = strTemp & Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) & ">"
						strTemp = strTemp & " " & Trim(Session("RSStyStruct").fields("cisegsepr")) & " "
						'Response.Write(session(Trim(Session("RSStyStruct").fields("cisegsdes"))))
						Response.Write(strTemp)
						Session("RSStyStruct").MoveNext
					Loop
				End IF
			%>

				
				<!--<input size="19" maxlength="19" name="Style" maxlength=19>-->



			</td>
      <td Align="middle" valign="center" colSpan=1>
				<SELECT name=Group size=1> 
					<OPTION selected value="ALL">All
				<%IF Not RSCodes.EOF And Not RSCodes.BOF Then%>
					<%Dim strTemp%>
					<%RSCodes.MoveFirst %>
					<% DO While Not RSCodes.EOF %>
						<OPTION value="<%=RSCodes("ccode_no")%>"><%=RSCodes("cdiscrep")%>
						<%RSCodes.MoveNext %>
					<%Loop%>
				<%END IF%>
					</SELECT>
				
			</td>
     </tr>
     <tr>
			<td Align="middle" valign="center" ><input type="submit" name="B2" value="Search"><input type="reset" name="B3" value="Reset"></td>
      <td Align="middle" valign="center" colspan="4" >&nbsp;</td>
     </tr>
    </table>
</form>
</Center>
<%
Session("ConnectionString") = "dsn=CRM;uid=aria;pwd=aria"
Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open Application("DataConnectionString")

Dim rsdumm
Set rsdumm = server.CreateObject("adodb.recordset")
set rsdumm = Session("RSStyStruct")

Session("RSStyStruct").MoveFirst

IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
Else
	Dim strStyle, strcoll, strasd
	strStyle = ""
	DO While Not Session("RSStyStruct").Eof
		if Request("firsttime")="T" then
			strcoll = Ucase(Request(Trim(Session("RSStyStruct").fields("cisegsdes"))))
			'Response.Write "from Form" & "<BR>"
		else
			strcoll= Ucase(Session(Trim(Session("RSStyStruct").fields("cisegsdes"))))
			'Response.Write "from Session" & "<BR>"
		end if
		'if Request("firsttime") = 0 then 
		'	strcoll = Ucase(request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value)))
		'	Response.Write strcoll
		'else
		'	strcoll = Session(Trim(Session("RSStyStruct").fields("cisegsdes")))
		'	Response.Write strcoll
		'end if
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
'if Session("Season")= "*" then
	strSql = "Select * from style where style like '" & strStyle & "' And status='A'"  
'else
'	strSql = "Select * from style where style like '" & strStyle & "' And season='" & Session("Season") & "' And cdivision='" & Trim(Session("Division"))  & "' And status='A'"  
'end if

'strSql = "Select distinct style from spck_lin  where account='*****' or account='"  & Session("custID") & "'"


Set RSSTyResult = Server.CreateObject("ADODB.RECORDSET")

RSSTyResult.Open strSql,Conn

'RSSTyResult.movefirst
'do while not RSSTyResult.eof
'strlist = strlist & RSSTyResult("style") & ", "
'RSSTyResult.movenext
'loop

'RSSTyResult.close
'strSql = "Select * from style where style in(" & strlist & ")"

'response.write(strlist)
'RSSTyResult.Open strSql,Conn



%>

<BR>
<%
IF RSSTyResult.EOF and RSSTyResult.bof  Then
	Response.Write("No Records Found.")
Else
	RSSTyResult.MoveFirst 
%>
<table align=center border="1" width="800" bgcolor="#6495D0" bordercolor="#AECAE6">
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

<p>&nbsp;</p>
</body>
</html>
