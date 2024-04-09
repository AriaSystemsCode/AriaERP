<%@ LANGUAGE="VBSCRIPT" %>
<%
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
	<%End if


Session("ConnectionString") = "Provider=MSDATASHAPE;DSN=CRM;SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=YES;"
Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open(Session("ConnectionString"))

Set RSCodes = Server.CreateObject("ADODB.RECORDSET")
strSql = "Select ccode_no, cdiscrep, cdefcode from codes where cfld_name='CSTYGROUP' And cdefcode='N'"
RSCodes.Open strSql,Conn


%>
<html>

<head>
<title>CRM - Remote Order - Style Selection Criteria</title>

</head>

<body bgcolor="#aecae6" topmargin="0" leftmargin="0" background="images/tile1.gif">
<%
if len(Trim(Session("ID")))>0 then
	compWork = "Y"
%>

<div align="center">
  <center>
<table border="0" width="100%" cellspacing="0" cellpadding="0">
  <tr>
    <td width="100%" align="center">
<p>
<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"  codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" WIDTH=80% HEIGHT=125 id="ShockwaveFlash1">
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
    <td width="100%" align="center"><p><img width=80% height=50 border="0" src="images/remoteorder0001.jpg"></p></td>
  </tr>
</table>
  </center>
</div>
<%ENd IF%>
<%IF Len(Trim(Session("rep")))>0  Then %>

	<%
	compWork = "Y"

	IF Len(Trim(Session("customerid")))=0 Then
		Response.Redirect("Repcust.asp")
	End IF
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
    <td width="100%" align="center"><p><img width=80% height=50 border="0" src="images/remoteorder0001.jpg"></p></td>
  </tr>
</table>
  </center>
</div>
<%END IF%>

<%IF compWork = "Y" Then%>



<Center>
<form action="catfindsty.asp?firsttime=T" method="post" id=form1 name=form1>
	<p><font size="2" face="Aria"><strong> </p>
   <table bgcolor="#6495d0" bordercolor="#aecae6" border="1" width="80%" >
<!--		 <TR>
			<td Align="middle" valign="center" width="20%">
				<font color="ivory" size="2" face="arial">
				<strong>
				<table>
				<%
				'IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
				'Else
					'strTemp = "<TD bgColor=#6495D0 colspan=10>"
					'Response.Write(strTemp)
					'Session("RSStyStruct").MoveFirst
					'DO While Not Session("RSStyStruct").Eof
					'	IF Len(Session("getstyle"))=0 Then
					'		strValue = ""
					'	Else
					'		strValue = Session(Trim(Session("RSStyStruct").fields("cisegsdes")))
					'	End IF
					'	
					'	
					'	strTemp ="<font face=Arial color=#000080 size=2>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</font>" 
					'	strTemp = strTemp & " " & Trim(Session("RSStyStruct").fields("cisegsepr")) & " "
					'	Response.Write(strTemp)
					'	Session("RSStyStruct").MoveNext
					'Loop
				'End IF
				%>
				</table>
				</strong></font>
			</TD>
			<TD Align="middle" valign="center" width="20%">
				<FONT color=ivory face=arial size=2>
				<STRONG>Group</STRONG></FONT><BR>
			</TD>
		 </TR>-->
		 <tr>
			<td Align="left" valign="center" width=50%>

			<%
				IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
				Else
					Session("RSStyStruct").MoveFirst
					DO While Not Session("RSStyStruct").Eof
						'strTemp = "<INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" 
						if Request("firsttime")="T" then
							strTemp = "<font face=Arial color=#000080 size=2><b>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "<b></font><INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Request((Trim(Session("RSStyStruct").fields("cisegsdes")))) & ">"
							
						else 
							strTemp = "<font face=Arial color=#000080 size=2><b>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</b></font><INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) & ">"
							'Response.Write strTemp
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
      <td Align="left" valign="center" colSpan=1><font face=Arial color=#000080 size=2><b>Group</b></font>
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
			<td align=left><input type="submit" name="B2" value="Search"><input type="reset" name="B3" value="Reset">
			</td>
     </tr>
     
    </table>
</form>
</Center>
<%
Session("ConnectionString") = "Provider=MSDATASHAPE;DSN=CRM;SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=YES;"
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

' To handle the case of the rep.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF


'The next Select statment to verifay the styles from the style file  - ARD - 
'if Session("Season")= "*" then
'	strSql = "Select * from style where style like '" & strStyle & "' And cdivision='" & Trim(Session("Division"))  & "' And status='A'"  
'else
'	strSql = "Select * from style where style like '" & strStyle & "' And season='" & Session("Season") & "' And cdivision='" & Trim(Session("Division"))  & "' And status='A'"  
'end if


'The next select statment is to verifay the style from pack file - ARD - 
if Session("Season")= "*" then
	strhdr= "Select pack_id from spck_hdr where cdivision='" & Trim(Session("Division"))  & "' And type='P' and (account='*****' or account='" & CustID & "') and pack_id like 'WEB%'"  
	strlin = "select distinct spck_lin.*, style.*  from spck_lin,style Where type='P' and (account='*****' or account='" & CustID & "') and spck_lin.style = style.style"
  strSql = "SHAPE {" & strhdr & "}  AS spck_hdr APPEND ({" & strlin & " }  AS packlin RELATE 'pack_id' TO 'pack_id') AS packlin"
else
	strhdr= "Select pack_id from spck_hdr where cdivision='" & Trim(Session("Division"))  & "' And type='P' and (account='*****' or account='" & CustID & "') and season='" & Session("Season") & "' and pack_id like 'WEB%'"  
	strlin = "select distinct spck_lin.*, style.*  from spck_lin,style Where type='P' and (account='*****' or account='" & CustID & "') and spck_lin.style = style.style"
  strSql = "SHAPE {" & strhdr & "}  AS spck_hdr APPEND ({" & strlin & " }  AS packlin RELATE 'pack_id' TO 'pack_id') AS packlin"
end if


Set RSSTyResult = Server.CreateObject("ADODB.RECORDSET")
RSSTyResult.Open strSql,Conn,2,4

Set RSSTyleFound = Server.CreateObject("ADODB.RECORDSET")
%>

<BR>
<%
IF RSSTyResult.EOF and RSSTyResult.bof  Then
	Response.Write("No Records Found.")
Else
	RSSTyResult.MoveFirst 
%>
<table align=center border="1" width="80%" bgcolor="#6495D0" bordercolor="#AECAE6">
<TR>
	<TD width=20%><Font face="Arial" color="#000080" size=2><Strong>Style</Strong></Font>
	</TD>
	<TD Width=20%><Font face="Arial" color="#000080" size=2><Strong>Description</Strong></Font>
	</TD>
	<TD Width=20%><Font face="Arial" color="#000080" size=2><Strong>Price</Strong></Font>
	</TD>
	<TD Width=20%><Font face="Arial" color="#000080" size=2><Strong>Image</Strong></Font>
	</TD>

</TR>
<%
	RSSTyResult.MoveFirst
	Do while not RSSTyResult.EOF
			Set RSTEmp = RSSTyResult.Fields("packlin").Value  
			if not (RSTEmp.eof and RSTEmp.bof) then
				RSTEmp.MoveFirst
				Do while not RSTEmp.EOF

					strTemp  = "<TR><TD width=20% ><Font face=""Arial"" color=""#ffffff"" size=2>"
					Response.Write(strTemp)
					strtemp = "<a href=""catresolvsty.asp?sty=" & trim(RSTEmp("style")) & "&desc1=" & trim(RSTEmp("desc1")) & """>" & trim(RSTEmp("style")) & "</a>"
					Response.Write(strTemp & "</font></td>")
					
					strTemp  = "<TD width=20% ><Font face=""Arial"" color=""#ffffff"" size=2>"
					Response.Write(strTemp)
					Response.Write(trim(RSTEmp("desc1")))
					Response.Write("</font></td>")
					
					strTemp  = "<TD width=20% ><Font face=""Arial"" color=""#ffffff"" size=2>"
					Response.Write(strTemp)
					Response.Write(trim(RSTEmp("pricea"))) 
					Response.Write("</font></td>")

					strTemp  = "<TD width=20% ><Font face=""Arial"" color=""#ffffff"" size=2>"
					Response.Write(strTemp)
					Response.Write("<a href=""catshwimg.asp?name=" & trim(RSTEmp("cstymajor")) & """>" & "Show Image</a>") 
					Response.Write("</font></td>")

					RSTEmp.MoveNext
				Loop
			end if	
		RSSTyResult.MoveNext
	Loop


'Do While Not RSSTyResult.EOF 
'strTemp  = "<TR>"
'Response.Write(strTemp)
'
'strTemp  = "<TD width=""25%""><Font face=""Arial"" color=""#ffffff"">"
'Response.Write(strTemp)
''<A href="sorderh.asp">Remote Order Entry</A>
''strTemp = "<a href=""resolvsty.asp?sty=" & RSSTyResult("style") & " &desc1=" & RSSTyResult("desc1") & ">" & RSSTyResult("style") & "</a>"
'strTemp = "<a href=""resolvsty.asp?sty=" & RSSTyResult("style") & "&desc1=" & RSSTyResult("desc1") & """&cat=T>" & RSSTyResult("style") & "</a>"
'Response.Write(strTemp)
'
'strTemp  = "<TD width=""25%""><Font face=""Arial"" color=""#ffffff"">"
'Response.Write(strTemp)
'Response.Write(RSSTyResult("desc1")) 
'
'strTemp  = "<TD width=""25%""><Font face=""Arial"" color=""#ffffff"">"
'Response.Write(strTemp)
'Response.Write(RSSTyResult("pricea")) 
'
'RSSTyResult.MoveNext 
'Loop
End if
%>

</table>

<p>&nbsp;</p>
<%End IF%>
</body>
</html>
