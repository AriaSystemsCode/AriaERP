g804
<%@ LANGUAGE="VBSCRIPT" %>

<%Response.Buffer = true%>
<%Session("Group")=Group
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
	location.href="../login.asp"
	</script>
<%End if

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" and Request.QueryString ("Type") <> "M"Then
		'Response.Redirect("../repcust.asp")%>
		<script language="javascript">
			location.href ="../repcust.asp"
		</script>
		
	<%END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF
session("Curr_disc") = 0
set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

dim cnnSQL, rsStyGroup
set cnnSQL  = server.CreateObject ("Adodb.connection")
cnnSQL.Open Application("SqlServer") 
'Response.Write  Application("DataConnectionString")
'Response.End 

Set RSCodes = Server.CreateObject("ADODB.RECORDSET")
strSql = "Select ccode_no, cdiscrep, cdefcode from codes where cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam like 'NCSTYGROUP%' and cdiscrep <> '' Order By cCode_No"
RSCodes.Open strSql,Conn

IF Not IsObject(Session("RSStyStruct")) Then
	Set Session("RSStyStruct") = Server.CreateObject("ADODB.RecordSet")
	strsql = "select * from icistru where citemrecty+cisegno like'U%'"
	Session("RSStyStruct").open strsql,conn
End IF
'wal_131300 check if there a style profile saved for the logged user
if trim(session("styProfile"))<> "" then
	'get the info for thAT profile
	
	
	'set rsStyProfileHdr = server.CreateObject ("Adodb.recordset")
	'set rsStyProfileDt = server.CreateObject ("Adodb.recordset")
	set rsStyGroup = server.CreateObject ("Adodb.recordset")
	
	arrStyle = split(trim(session("strStyles")),",")
	set rsStyGroup = server.CreateObject ("Adodb.recordset")
	arrStyle = split(trim(session("strStyles")),",")
	'if ubound(arrStyle) > 0 then
	strsql = "select distinct FOXStyle.cstyGroup from styleprofiledetail , OPENROWSET('MSDASQL', '"
	strSQL = strSQL + Application("DataConnectionString")		
	strSQL = strSQL + "', 'Select cstyGroup ,style From  style') FOXStyle where cStyleProfileCode='"&trim(session("styProfile"))&"' and FOXStyle.style = styleprofiledetail.cStyle COLLATE Latin1_General_CI_AS"
	'for i=0 to ubound(arrStyle)
		'strSql = strSql & "style =" & arrStyle(i)
		'if i <> UBound(arrStyle) then
			'strSql = strSql & " OR "
		'End if
					
	'next
	'strsql = strsql & " order by style"
	'Response.Write "<font size=2>"&strsql
	'Response.End 
	rsStyGroup.Open strsql,cnnSQL,1,3
	'rsStyGroup.Open "select distinct cstyGroup from style where style in (" &trim(application("strStyles"))& ")",conn,1,3
	if rsStyGroup.RecordCount = 1 then
		Group =  trim(rsStyGroup("cstyGroup"))
		'check to apply search 
		if Search= "" then
			Search = "Y"
		end if
	else
		Group = Request("Group")	
	end if
else
	'if Group = "" then
	Group = Request("Group")
end if

%>

<html>
<head>
<SCRIPT LANGUAGE=javascript>
<!--

function hideloadingmsg() 

{
	document.all.loadingmsg.style.display = 'none';
    document.all.loadingmsg.style.visibility = 'hidden';

}

//-->

</SCRIPT>

<title>CRM - Style Selection Criteria</title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</head>
<body onLoad="hideloadingmsg()">
<div name='loadingmsg' id='loadingmsg' align=center style='display:inline;visibility:visible'>
	<p align=center>
		<A align=center style="font-family=verdana;font-size=12px;font-weight=bold;"><font color=red>Please standby, ..Loading data from server.</font></A>
	</p>
</DIV>

<SCRIPT LANGUAGE=javascript>
<!--
function openwindow(filename) 
{  
	window.open(filename,'','toolbar=no,status=no,scrollbars=no,location=no,menubar=no,directories=no,top=' + ((window.screen.height-400)/2) + ',left=' + ((window.screen.width-800)/2) + ',width=800,height=400')
}

//-->
</SCRIPT>


<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
	<TD colspan=13>
	<%IF Trim(Session("customerid")) <> ""  Then%>
		<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=session("rscust").fields("btname").value%></P>
	<%else%>
		&nbsp
	<%end if%>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
</TR>
</table>


 <%End IF%>
<Table border=1 align=center width=95%>
<TR>
<TD class=Title>Get Style</TD>
</TR>
</TAble>
<Br>

<%IF compWork = "Y" Then%>
<form action="findstyle.asp?firsttime=F&LoginType=<%=Request("LoginType")%>" method="post" name ="form1" id="form1">
	<p><font size="2" face="Aria"><strong> </p>
    <div align="center">
      <center>
   <table bordercolor="#111111" border="1" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0" >
		 <tr>
			<td Align="left" valign="center" width=50% class="dark_cell">
			<%
				IF Not(Session("RSStyStruct").EOF And Session("RSStyStruct").BOF) Then
					Session("RSStyStruct").MoveFirst
					DO While Not Session("RSStyStruct").Eof
						if Trim(Request("firsttime"))<>"" then
							strValue = Request.Form(Trim(Session("RSStyStruct").fields("cisegsdes")))
							Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) = strValue
						else
							strValue = Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) 
							'strValue = Session(Trim(Session("RSStyStruct").fields("cisegsdes")))
						end if
						strRequiredStyle = Trim(strRequiredStyle) & Trim(strValue)
						strRequiredStyle = Ucase(strRequiredStyle) & "%"
						if Trim(Request("firsttime"))="F" then
							'strTemp = "<font face=Arial color=#000080 size=2><b>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</b></font><INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Ucase(Request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value))) & ">"
							strTemp = "<font class=dark_cell><strong>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</b></font><INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Ucase(Request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value))) & "></strong></font>"
						else 
							strTemp = "<font class=dark_cell><strong>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "<b></font><INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) & "></strong></font>"
						end if
						'wal_get the description for style filed
						if Session("RSStyStruct").FIELDS("LSEGENDMAJ") = True then 
							strStyleName =  Trim(Session("RSStyStruct").fields("cisegsdes"))
						end if
						strTemp = strTemp & " " & Trim(Session("RSStyStruct").fields("cisegsepr")) & " "
						strRequiredStyle = strRequiredStyle & Trim(Session("RSStyStruct").fields("cisegsepr"))
						Response.Write(strTemp)
						Session("RSStyStruct").MoveNext
					Loop
					'Response.Write("<font size=3>Style"&strRequiredStyle)
				End IF
				
			%> &nbsp;</td>
		<%IF Session("M_STYVIEW") = "G" Then
			'wal_131300 check if i've a tyle profile then display the list of grpoups per user
			if trim(session("styProfile"))<> "" then%>
			<td Align="left" valign="center" colSpan=1 class="dark_cell">Group
					<SELECT name=Group size=1> 
						  <%if rsStyGroup.RecordCount  > 1 then%>
								<OPTION  value="ALL">All
						  <%end if%>
					
					<%IF Not RSCodes.EOF And Not RSCodes.BOF Then%>
						
						<%RSCodes.MoveFirst 
							do while not rsStyGroup.EOF 
								RSCodes.Filter = "ccode_no='"& trim(rsStyGroup("cstyGroup")) & space(6 - len(trim(rsStyGroup("cstyGroup")))) & "'"
											
								if not RSCodes.EOF then
									if Group = trim(rsStyGroup("cStyGroup")) Then
										Response.Write("<option value=""" & Trim(RSCodes.Fields("ccode_no").Value )& """ selected>" & RSCodes.Fields("cdiscrep").Value )
									Else
										Response.Write("<option value=""" & Trim(RSCodes.Fields("ccode_no").Value )& """>" & RSCodes.Fields("cdiscrep").Value )
									End IF
								end if
								RSCodes.Filter = ""
							rsStyGroup.MoveNext 
							loop
					END IF%>
					</SELECT>
				</td>	
			<%				
			else
				'wal_130731 check on style group per user
				if trim(session("StyGroup")) = "" then
					arrFound= split(session("showcatalogval"),",")
				else
					arrFound = split(trim(UCASE(session("StyGroup"))),",")
				end if
			%>
				<td Align="left" valign="center" colSpan=1 class="dark_cell">Group
					<SELECT name=Group size=1> 
						  <%if ubound(arrFound) > 0 then%>
								<OPTION value="ALL">All
						  <%end if%>
					
					<%IF Not RSCodes.EOF And Not RSCodes.BOF Then%>

						<%RSCodes.MoveFirst 
							for intcount =0 to ubound(arrFound)
								'Response.Write "<option>" & arrFound(intcount) & space(6 - len(arrFound(intcount))) & "</option>"
								RSCodes.Filter = "ccode_no='"& trim(arrFound(intcount)) & space(6 - len(trim(arrFound(intcount)))) & "'"
								if not rscodes.EOF then
									if Group =  arrFound(intcount) Then
										Response.Write("<option value=""" & Trim(RSCodes.Fields("ccode_no").Value )& """ selected>" & RSCodes.Fields("cdiscrep").Value )
									Else
										Response.Write("<option value=""" & Trim(RSCodes.Fields("ccode_no").Value )& """>" & RSCodes.Fields("cdiscrep").Value )
									End IF
								end if 					
								RSCodes.Filter = ""
							next
							
					END IF%>
					</SELECT>
				</td>
		<%end if
		End IF%>
			<td align=left class="dark_cell"><input type="submit" name="B2" value="Search"><input type="reset" name="B3" value="Reset">
			</td>
     </tr>
    </table>
      </center>
    </div>
</form>

<%

strStyle = strRequiredStyle
' To handle the case of the rep.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF
'Response.Write "<font size=5>" & Session("Season") & "<hr>"
'HDM[Start]
'WAL_Apply condition on season and division[start]
Dim strSeason, strDivision
'NEK [Start] 
'strSeason = Trim(Session("StyleColor"))
'strDivision = Trim(Session("CatDivision"))
'Response.Write "Division = " & Session("Division") &"--"&Trim(Session("Season"))
if Trim(Session("Season"))="All" then
	strSeason = "NONE"
else
	strSeason = Trim(Session("Season"))
end if 
if strSeason = "ALL" or strSeason = "" or strSeason = "NONE" or strSeason = "*" then
	strSeason	="NONE"
end if 
'Response.Write Trim(Session("Division"))
if Trim(Session("Division"))="" Or Trim(Session("Division"))="*" or UCase(Trim(Session("Division")))="ALL" then
	strDivision = "NONE"
else
	strDivision = Trim(Session("Division"))
end if 

'NEK [End]
'If Trim(UCase(strSeason)) = "NONE" or len(Trim(UCase(strSeason)))>4Then
If Trim(UCase(strSeason)) = "NONE" or instr(Trim(UCase(strSeason)),",") <>0 Then
	'Multiple Seasons WMA 5/24/2004 START
	'strSeasonCondition = " Season in('" & Replace(Trim(Session("StyleColor")),",","','") & "') "	
	strSeasConArr  = split(Session("StyleColor"),",")
	intSeasConCount = int(UBound(strSeasConArr)/24)
	strSeasonCondition = strSeasonCondition & " AND ("
	for i = 0 to  intSeasConCount 
		for j = i*24 to (i*24+24) -1
			if j = UBound(strSeasConArr) then
				strSQLall = strSQLall + "'" &  strSeasConArr(j) & "'"
			elseif j < UBound(strSeasConArr) then
				if ((j+1) mod 24 = 0 and j<>0) then
					strSQLall = strSQLall + "'" &  strSeasConArr(j) & "'"			
				else
					strSQLall = strSQLall + "'" &  strSeasConArr(j) & "',"			
				end if	
			end if		
		next		
		if i =0 then 'first item
			strSeasonCondition = strSeasonCondition & " Season in("& strSQLall &") " 
		else
			strSeasonCondition = strSeasonCondition & " or Season in(" & strSQLall & ") " 
		end if
		strSQLall = ""
	next
	strSeasonCondition = strSeasonCondition & " )"
    'Multiple Seasons WMA 5/24/2004 END

Else
	strSeasonCondition = " Season='" & strSeason & "'"
End If

If Trim(UCase(strDivision)) = "NONE" Then
	strDivisionCondition = ""
Else
	If Trim(strSeasonCondition) ="" Then
		strDivisionCondition = " cdivision='" & strDivision & "'"
	Else
		strDivisionCondition = " And cdivision='" & strDivision & "'"
	End If
End If
Dim strSeasDivWhere

strSeasDivWhere = strSeasonCondition & strDivisionCondition
If trim(strSeasDivWhere) <> "" And Left(Trim(strSeasDivWhere),3) <> "AND" Then
	strSeasDivWhere = " AND " & strSeasDivWhere
End If
'WAL_Apply condition on season and division[end]
'HDM [END]
'Response.Write "<font size=3> LoginType = " &Request("LoginType") &"</font>"
select case Request("LoginType")
case "T"' OTS
	'wal_131300 add condition to add only selected style in the user profile
	if trim(session("styProfile")) = "" then
		if trim(Group)="" or trim(Ucase(Group))="ALL" then
			'wal_130731 check on style group per user
			if trim(session("StyGroup")) = "" then
				strSQL = "SELECT * FROM Style where " & MakeQuery("status+cstygroup", Session("ShowCatalogVal"), 10, false) & " and style like '" & strRequiredStyle & "' "
			else
				strSQL = "SELECT * FROM Style where " & MakeQuery("status+cstygroup", session("StyGroup"), 10, false) & " and style like '" & strRequiredStyle & "' "
				
			end if
			strSQL = strSQL& strSeasDivWhere & " order by style"
		else
			strSQL = "SELECT * FROM Style where style like '" & strRequiredStyle & "' and status+cstygroup like 'A" & Group&"' "
			strSQL = strSQL& strSeasDivWhere & " order by style"
		end if
	else
		if trim(Group)="" or trim(Ucase(Group))="ALL" then
			
			strSQL = "SELECT style.* FROM styleprofiledetail, "
			strsql = strsql & " OPENROWSET('MSDASQL', '"
			strSQL = strSQL + Application("DataConnectionString")		
			strSQL = strSQL + "', 'Select * From style where status+cstygroup like ""A%"" and style like """ & strRequiredStyle & """ ') Style "
			strsql = strsql & " where cStyleProfileCode='"&trim(session("styProfile"))&"' and style.style = styleProfileDetail.cStyle  COLLATE Latin1_General_CI_AS"
		
			
			strSQL = strSQL& strSeasDivWhere & " order by style"
		else
			strSQL = "SELECT style.* FROM styleprofiledetail, "
			strsql = strsql & " OPENROWSET('MSDASQL', '"
			strSQL = strSQL + Application("DataConnectionString")		
			strSQL = strSQL + "', 'Select * From style where style like """ & strRequiredStyle & """ and status+cstygroup like ""A" & Group&"""') Style "
			strsql = strsql & " where cStyleProfileCode='"&trim(session("styProfile"))&"' and style.style = styleProfileDetail.cStyle  COLLATE Latin1_General_CI_AS"
		
			'strSQL = "SELECT * FROM Style where style like '" & strRequiredStyle & "' and status+cstygroup like 'A" & Group&"' "
			strSQL = strSQL& strSeasDivWhere & " order by style"
		end if
	end if
	'strSQL = "SELECT * FROM Style where style like'%' and status = 'A' order by style "
case "R"'R/A
	'wal_131300 add condition to add only selected style in the user profile
	if trim(session("styProfile")) = "" then
		if trim(Group)="" or trim(Ucase(Group))="ALL" then
			'wal_130731 check on style group per user
			if trim(session("StyGroup")) = "" then
				strSQL = "SELECT * FROM Style where " & MakeQuery("status+cstygroup", Session("ShowCatalogVal"), 10, false) & " and style like '" & strRequiredStyle & "' "
			else
				strSQL = "SELECT * FROM Style where " & MakeQuery("status+cstygroup", session("StyGroup"), 10, false) & " and style like '" & strRequiredStyle & "' "
				
			end if
			strSQL = strSQL& strSeasDivWhere & " order by style"
		else
			strSQL = "SELECT * FROM Style where style like '" & strRequiredStyle & "' and status+cstygroup like 'A" & Group&"' "
			strSQL = strSQL& strSeasDivWhere & " order by style"
		end if
	else
		if trim(Group)="" or trim(Ucase(Group))="ALL" then
			
			strSQL = "SELECT style.* FROM styleprofiledetail, "
			strsql = strsql & " OPENROWSET('MSDASQL', '"
			strSQL = strSQL + Application("DataConnectionString")		
			strSQL = strSQL + "', 'Select * From style where status+cstygroup like ""A%"" and style like """ & strRequiredStyle & """ ') Style "
			strsql = strsql & " where cStyleProfileCode='"&trim(session("styProfile"))&"' and style.style = styleProfileDetail.cStyle  COLLATE Latin1_General_CI_AS"
		
			
			strSQL = strSQL& strSeasDivWhere & " order by style"
		else
			strSQL = "SELECT style.* FROM styleprofiledetail, "
			strsql = strsql & " OPENROWSET('MSDASQL', '"
			strSQL = strSQL + Application("DataConnectionString")		
			strSQL = strSQL + "', 'Select * From style where style like """ & strRequiredStyle & """ and status+cstygroup like ""A" & Group&"""') Style "
			strsql = strsql & " where cStyleProfileCode='"&trim(session("styProfile"))&"' and style.style = styleProfileDetail.cStyle  COLLATE Latin1_General_CI_AS"
		
			'strSQL = "SELECT * FROM Style where style like '" & strRequiredStyle & "' and status+cstygroup like 'A" & Group&"' "
			strSQL = strSQL& strSeasDivWhere & " order by style"
		end if
	end if
	'hdm no rushmore as the index is not created on the table
	'if Group="" or Group="ALL" then
	'	if trim(Session("ShowCatalogVal"))  = "" then
	'	else
	'		'wal_130731 check on style group per user
	'		if trim(application("StyGroup")) = "" then
	'			strSQL = "SELECT * FROM Style where " & MakeQuery("status+cstygroup", Session("ShowCatalogVal"), 10, false) & " and style like '" & strRequiredStyle & "' "
	'		else
	'			strSQL = "SELECT * FROM Style where " & MakeQuery("status+cstygroup", application("StyGroup"), 10, false) & " and style like '" & strRequiredStyle & "' "
	'					
	'		end if
	'		'strSQL = "SELECT * FROM Style where " & MakeQuery("status+cstygroup", Session("ShowCatalogVal"), 10, false) & " and  style like '" & strRequiredStyle&"' "
	'		'wal_131300 add condition to add only selected style in the user profile
	'		if session("styProfile")<> "" then
	'			strSql = strSQL & " and style in (" &trim(application("strStyles"))& ")"
	'		end if
	'		strSQL = strSQL& strSeasDivWhere & " order by style"
	'	end if
	'else
	'	strSQL = "SELECT * FROM Style where  style like '"&strRequiredStyle&"' and status+cstygroup = 'A" & Group&"'"
	'	'wal_131300 add condition to add only selected style in the user profile
	'	if session("styProfile")<> "" then
	'		strSql = strSQL & " and style in (" &trim(application("strStyles"))& ")"
	'	end if
	'	strSQL = strSQL& strSeasDivWhere & " order by style"
	'end if
Case "O"
	'Response.Write "<font size=3>M_STYVIEW ==="&Session("M_STYVIEW")&"</font>"
	IF Session("M_STYVIEW") = "G" Then
		'Case OF Group
		IF Group = "ALL" or Group = "" then
			
			'wal_131300 add condition to add only selected style in the user profile
			if trim(session("styProfile"))<> "" then
				strSQL = "SELECT style.* FROM styleprofiledetail, "
				strsql = strsql & " OPENROWSET('MSDASQL', '"
				strSQL = strSQL + Application("DataConnectionString")		
				strSQL = strSQL + "', 'Select * From style where style like """ & strRequiredStyle & """ and status+cstygroup like ""A%""') Style "
				strsql = strsql & " where cStyleProfileCode='"&trim(session("styProfile"))&"' and style.style = styleProfileDetail.cStyle  COLLATE Latin1_General_CI_AS"
		
				'strsql = "select * from style where status+cstygroup like 'A%' and style like '" & strRequiredStyle & "' "
				'strSql = strSQL & " and style in (" &trim(application("strStyles"))& ")"
				'arrStyle = split(trim(application("strStyles")),",")
				'if ubound(arrStyle) > 0 then
				'	strSql = strSQL & " And ("
				'	for i=0 to ubound(arrStyle)
				'		strSql = strSql & "style =" & arrStyle(i)
				'		if i <> UBound(arrStyle) then
				'			strSql = strSql & " OR "
				'			
				'		End if
				'					
				'	next
				'	strsql = strsql & ")"
						
				'else'only one record
				'	strSql = strSQL & " and style ='" &arrStyle(0)&"' "
				'end if
				strSQL = strSQL& strSeasDivWhere & " order by style"
			elseif trim(Session("ShowCatalogVal"))  = "" then
				
				'wal_131300 add condition to add only selected style in the user profile
				if trim(session("styProfile"))<> "" then
					strSQL = "SELECT style.* FROM styleprofiledetail, "
					strsql = strsql & " OPENROWSET('MSDASQL', '"
					strSQL = strSQL + Application("DataConnectionString")		
					strSQL = strSQL + "', 'Select * From style where style like """ & strRequiredStyle & """ and status+cstygroup like ""A%""') Style "
					strsql = strsql & " where cStyleProfileCode='"&trim(session("styProfile"))&"' and style.style = styleProfileDetail.cStyle  COLLATE Latin1_General_CI_AS"
				else
					strsql = "select * from style where status+cstygroup like 'A%' and style like '" & strRequiredStyle & "' "
					'strSql = strSQL & " and style in (" &trim(application("strStyles"))& ")"
				end if
				strSQL = strSQL& strSeasDivWhere & " order by style"
			else
				'wal_130731 check on style group per user
				if trim(session("StyGroup")) = "" then
					strSQL = "SELECT * FROM Style where " & MakeQuery("status+cstygroup", Session("ShowCatalogVal"), 10, false) & " and style like '" & strRequiredStyle & "' "
				else
					strSQL = "SELECT * FROM Style where " & MakeQuery("status+cstygroup", session("StyGroup"), 10, false) & " and style like '" & strRequiredStyle & "' "
					
				end if
				
				'strsql = "select * from style where " & MakeQuery("status+cstygroup", Session("ShowCatalogVal"), 10, false) & " and style like '" & strRequiredStyle & "' "
				strSQL = strSQL& strSeasDivWhere & " order by style"
			end if
		Else
			
			'wal_131300 add condition to add only selected style in the user profile
			if trim(session("styProfile"))<> "" then
				strSQL = "SELECT style.* FROM styleprofiledetail, "
				strsql = strsql & " OPENROWSET('MSDASQL', '"
				strSQL = strSQL + Application("DataConnectionString")		
				strSQL = strSQL + "', 'Select * From style where style like """ & strRequiredStyle & """ and status+cstygroup like ""A" & Group&"""') Style "
				strsql = strsql & " where cStyleProfileCode='"&trim(session("styProfile"))&"' and style.style = styleProfileDetail.cStyle  COLLATE Latin1_General_CI_AS"
			else
				strsql = "select * from style where status+cstygroup = 'A" & Group & "' and style like '" & strRequiredStyle & "' "
				'strSql = strSQL & " and style in (" &trim(application("strStyles"))& ")"
			end if
			strSQL = strSQL& strSeasDivWhere & " order by style"
		End IF
	Else
		'Case Of Packs
		'ARD - 301633,7 [Start]
		set connSQL= server.CreateObject ("ADODB.Connection")
		strcon = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source="&Trim(Application("SqlServerName"))&";uid="&Trim(Session("SqlUserName"))&";pwd="&Trim(Session("SqlPassWord"))
		connSQL.Open Application("SqlServer")

		'Get packs ID from SQl Server tables
		set rsPacks = Server.CreateObject ("ADODB.Recordset")
		strSQL= "select CustGroup.PackID from CustClassification ,CustGroup  "
		strSQL = strSQL & " where CustClassification.CustGroup=CustGroup.GroupID "
		strSQL = strSQL & " AND CustClassification.CustID='" & Trim(CustID) & "'"
		rsPacks.Open strSQL,connSQL,1,3



		'GET the Pack_id Information.
		IF Not (rsPacks.EOF And rsPacks.BOF) Then
			strsql = "Select PACK_ID, ACCOUNT, SEASON, CDIVISION from spck_hdr where "
			strsql = strsql & " type+account+pack_id = 'P*****" & Trim(rsPacks.Fields("PackID").Value) & "'"
		Else
			strsql = "Select PACK_ID, ACCOUNT, SEASON, CDIVISION from spck_hdr where "
			strsql = strsql & " type+account+pack_id like 'P" & Trim(CustID) & "WEB%" & "'"
		End IF

		Set RShdr = Server.CreateObject("ADODB.RECORDSET")
		RShdr.Open strSql,Conn,2,4
		'Response.Write "<font size=3>strSql==" & strSql&"</font>"
		IF Not(RShdr.EOF and RShdr.BOF) Then
'			Response.Write "Season "&Session("season")
			
			IF Session("Season") = "*" Then
				strsql = "select spck_lin.style as spckSty, style.* "
				strsql = strsql & " from SPCK_LIN, STYLE "
				strsql = strsql & " where SPCK_LIN.TYPE+SPCK_LIN.ACCOUNT+SPCK_LIN.PACK_ID+SPCK_LIN.STYLE like"
				strsql = strsql & " 'P" & Trim(rshdr.Fields("ACCOUNT").Value) & Trim(rshdr.Fields("PACK_ID").Value)  & "%'"
				strsql = strsql & " and SPCK_LIN.STYLE = STYLE.STYLE  order by spckSty group by style.style"
			Else
				strsql = "select spck_lin.style as spckSty, style.* "
				strsql = strsql & " from SPCK_LIN, STYLE "
				strsql = strsql & " where SPCK_LIN.TYPE+SPCK_LIN.ACCOUNT+SPCK_LIN.PACK_ID+SPCK_LIN.STYLE like"
				strsql = strsql & " 'P" & Trim(rshdr.Fields("ACCOUNT").Value) & Trim(rshdr.Fields("PACK_ID").Value)  & "%'"
				'Multiple Seasons WMA 5/24/2004 START
				'strsql = strsql & " and SPCK_LIN.STYLE = STYLE.STYLE and style.season='"&Session("Season")&"' order by spckSty group by style.style" 'wma multiple seasons
				'strsql = strsql & " and SPCK_LIN.STYLE = STYLE.STYLE and style.season in('" & Replace(Trim(Session("StyleColor")),",","','") & "') order by spckSty group by style.style"
				strsql = strsql & " and SPCK_LIN.STYLE = STYLE.STYLE "
				strSeasConArr  = split(Session("StyleColor"),",")
				intSeasConCount = int(UBound(strSeasConArr)/24)
				strSQL = strSQL & " AND ("
				for i = 0 to  intSeasConCount 
					for j = i*24 to (i*24+24) -1
						if j = UBound(strSeasConArr) then
							strSQLall = strSQLall + "'" &  strSeasConArr(j) & "'"
						elseif j < UBound(strSeasConArr) then
							if ((j+1) mod 24 = 0 and j<>0) then
								strSQLall = strSQLall + "'" &  strSeasConArr(j) & "'"			
							else
								strSQLall = strSQLall + "'" &  strSeasConArr(j) & "',"			
							end if	
						end if		
					next		
					if i =0 then 'first item
						strSQL = strSQL & " style.Season in("& strSQLall &") " 
					else
						strSQL = strSQL & " or style.Season in(" & strSQLall & ") " 
					end if
					strSQLall = ""
				next
				strSQL = strSQL & " )"
				'wal_131300 add condition to add only selected style in the user profile
				if trim(session("styProfile"))<> "" then
					arrStyle = split(trim(session("strStyles")),",")
					if ubound(arrStyle) > 0 then
						strSql = strSQL & " And ("
						for i=0 to ubound(arrStyle)
							strSql = strSql & "style =" & arrStyle(i)
							if i <> UBound(arrStyle) then
								strSql = strSql & " OR "
							
							End if
									
						next
						strsql = strsql & ")"
						
					else'only one record
						strSql = strSQL & " and style ='" &arrStyle(0)&"' "
					end if
					'strSql = strSQL & " and style in (" &trim(application("strStyles"))& ")"
				end if
				strSQL = "order by spckSty group by style.style"
				'Multiple Seasons WMA 5/24/2004 END
				  
			End IF
		Else
			%>
			<Table width=95% border=0 align=center>
				<TR>
					<TD><strong>No styles found1</strong></TD>
				</TR>
			</Table>
			<%
			Response.End 
		End IF
		
	'ARD - 301633,7 [End]
	End IF
case else
End select

'Response.Write("<br><font size=3>"&strSQL&rsStyGroup.RecordCount&"</font><br>")
Const NumPerPage  = 25

Dim CurPage
If Request.QueryString("CurPage") = "" then
    CurPage = 1 'We're on the first page
Else
    CurPage = Request.QueryString("CurPage")
End If

Set RSSTyResult = Server.CreateObject("ADODB.RECORDSET")
RSSTyResult.CursorLocation = 2
RSSTyResult.CacheSize = NumPerPage
RSSTyResult.CursorType = 3

'Response.Write "strsql =="&strsql&"<Br><br>"
'Response.End 
 'Response.write "Make Query==" & MakeQuery("status+cstygroup", Session("ShowCatalogVal"), 10, false)
if trim(session("styProfile"))= "" then
	RSSTyResult.Open  strsql, conn,1,3
else
	RSSTyResult.Open  strsql, cnnSql,1,3
end if

IF Not(RSSTyResult.EOF AND RSSTyResult.BOF) Then
	RSSTyResult.PageSize = NumPerPage
	TotalPages = RSSTyResult.PageCount 
	RSSTyResult.AbsolutePage = CurPage
End IF
Dim Count
Count = 0
'response.Write "RECORDCOUNT =="&RSSTyResult.RecordCount &"<Br><br>"
IF ( RSSTyResult.EOF and RSSTyResult.bof)  Then
	%>
	<CENTER>
		<table width=95% align=center>
			<tr>
				<td>
					<strong>No Styles Found.</strong>
				</td>
			</tr>
		</table>
	</CENTER>
	<%
Else
	'check if result is one record then redirect directly
	if RSSTyResult.RecordCount  = 1 then
		select case Request("logintype")
			case "T"
				Response.Redirect "../OTS/OTSresolvesty.asp?sty="&trim(RSSTyResult("style"))&"&desc1="&trim(RSSTyResult("desc1"))
			case "R"
				Response.Redirect "../RA/Returnresolvsty.asp?sty="&trim(RSSTyResult("style"))&"&desc1="&trim(RSSTyResult("desc1"))
			case "O"
				if Session("M_STYVIEW") = "P" then
					'Response.Redirect "../order/resolvsty.asp?Type="&Request.QueryString ("Type")&"&sty="&trim(RSSTyResult("style"))&"&desc1="&trim(RSSTyResult("desc1"))&"&price="&GetStyPrice(trim(RSSTyResult("style")),1)
					Response.Redirect "../order/resolvsty.asp?Type="&Request.QueryString ("Type")&"&sty="&trim(RSSTyResult("style"))&"&desc1="&trim(RSSTyResult("desc1"))&"&price="&GetStyPrice(trim(RSSTyResult("style")),1)&"&seasonCode="&trim(RSSTyResult("Season"))&""
				Else
					'Response.Redirect "../order/resolvsty.asp?Type="&Request.QueryString ("Type")&"&sty="&trim(RSSTyResult("style"))&"&desc1="&trim(RSSTyResult("desc1"))&"&price="&GetStyPrice(trim(RSSTyResult("style")),1)
					Response.Redirect "../order/resolvsty.asp?Type="&Request.QueryString ("Type")&"&sty="&trim(RSSTyResult("style"))&"&desc1="&trim(RSSTyResult("desc1"))&"&price="&GetStyPrice(trim(RSSTyResult("style")),1)&"&seasonCode="&trim(RSSTyResult("Season"))&""
				End IF
						
		end select
	end if
'	RSSTyResult.MoveFirst
%>
<div align="center">
  <center>
<table border="1" width="95%" cellpadding="0" cellspacing="0" style="border-collapse: collapse" bordercolor="#111111">
<TR>
	<TD width=22% class="dark_cell"><%=strStyleName%></TD>
	<TD Width=30% class="dark_cell">Description</TD>
	<!--wal add coulmn to display color-->
	<TD Width=15% class="dark_cell">Color</TD>
	<%IF Session("M_STYVIEW") = "G" and Group= "ALL" Then%>
	<TD class=dark_cell>Group</TD>
	<%End IF%>
	<TD width=5% class="dark_cell">Price</TD>
	<TD Width=10% class="dark_cell">Image</TD>

</TR>

<%
	Dim rsTemSty
	Set rsTemSty = server.CreateObject("ADODB.RECORDSET")
	'First Get the price level for this customer
	Session("PriceLvl")=GetCustPriceLvl(custid)
	'open record with color code
    Set rsColorDsc = server.CreateObject("ADODB.Recordset")
	strsql = "SELECT DISTINCT * FROM CODES WHERE CDEFCODE+CRLTFIELD+CFLD_NAME='NNCOLOR'"
	rsColorDsc.Open strsql,Conn 
	Do While Not RSSTyResult.EOF And Count < NumPerPage
		
					select case Request("LoginType")
					case "O"
						if Session("M_STYVIEW") = "P" then
							strsql = "select * from style where style = '" & trim(RSSTyResult.Fields("style").Value) & "' order by style"
							rsTemSty.Open strsql,conn
						End IF
					End select
					strTemp  = "<TR><TD width=22% class=light_cell>"
					Response.Write(strTemp)
					'get color description
					rsColorDsc.Filter = "ccode_no='" & Trim(mid(trim(RSSTyResult.Fields("style").Value),session("styleSize")+2,session("colorSize"))) & "'"
					if not rscolordsc.EOF then
						strColor=trim(RSColorDsc.Fields("cdiscrep").Value)
					end if
					rsColorDsc.Filter  = ""
					select case Request("logintype")
						case "T"%>
							<a href="../OTS/OTSresolvesty.asp?sty=<%=trim(RSSTyResult("style"))%>&desc1=<%=trim(RSSTyResult("desc1"))%>&color=<%=strColor%>">&nbsp;<%=trim(RSSTyResult("style"))%></a>
						<%case "R"%>
							<a href="../RA/Returnresolvsty.asp?sty=<%=trim(RSSTyResult("style"))%>&desc1=<%=trim(RSSTyResult("desc1"))%>&color=<%=strColor%>">&nbsp;<%=trim(RSSTyResult("style"))%></a>
						<%case "O"%>
							<%if Session("M_STYVIEW") = "P" then%>
				  				<a href="../order/resolvsty.asp?Type=<%=Request.QueryString ("Type")%>&sty=<%=RSSTyResult("style")%>&desc1=<%=trim(rsTemSty("desc1"))%>&color=<%=strColor%>&price=<%=GetStyPrice(trim(RSSTyResult("style")),1)%>&seasonCode=<%=trim(RSSTyResult("Season"))%>">&nbsp;<%=trim(RSSTyResult("style"))%>	</a>
				  				<!--a href="../order/resolvsty.asp?Type=<%=Request.QueryString ("Type")%>&sty=<%=RSSTyResult("style")%>&desc1=<%=trim(rsTemSty("desc1"))%>&price=<%=GetStyPrice(trim(RSSTyResult("style")),1)%>">&nbsp;<%=trim(RSSTyResult("style"))%></a-->
				  				<!--../order/resolvsty.asp?sty=<%=trim(RSSTyResult("style"))%>&desc1=<%=trim(rsTemSty("desc1"))%>-->
							<%Else
								if session("PriceCode")="" then%>
									<a href="../order/resolvsty.asp?Type=<%=Request.QueryString ("Type")%>&sty=<%=RSSTyResult("style")%>&desc1=<%=trim(RSSTyResult("desc1"))%>&color=<%=strColor%>&price=<%=GetStyPrice(trim(RSSTyResult("style")),1)%>&seasonCode=<%=trim(RSSTyResult("Season"))%>">&nbsp;<%=trim(RSSTyResult("style"))%></a>
							   <%else%>
									<a href="../order/resolvsty.asp?Type=<%=Request.QueryString ("Type")%>&sty=<%=RSSTyResult("style")%>&desc1=<%=trim(RSSTyResult("desc1"))%>&color=<%=strColor%>&price=<%=GetPriceCode(trim(RSSTyResult("style")),session("PriceCode"))%>&seasonCode=<%=trim(RSSTyResult("Season"))%>">&nbsp;<%=trim(RSSTyResult("style"))%></a>
								<%end if%>
								<!--../order/resolvsty.asp?sty=<%=trim(RSSTyResult("style"))%>&desc1=<%=trim(RSSTyResult("desc1"))%>-->
							<%End IF%>
					
					<%end select%>
					
					<%
					strTemp  = "<TD width=30% class=light_cell>"
					Response.Write(strTemp)
					if Session("M_STYVIEW") = "P" and Request("logintype")="O"  then
						Response.Write("&nbsp;"&trim(rsTemSty("desc")))
					Else
						Response.Write("&nbsp;"&trim(RSSTyResult("desc")))
					End IF
					Response.Write("</td>")
					
					strTemp  = "<TD width=15% class=light_cell>"& strColor' RSColorDsc.Fields("cdiscrep").Value
					Response.Write(strTemp)
					Response.Write("</td>")
					IF Session("M_STYVIEW") = "G" and Group= "ALL" Then
						Response.Write("<TD class=light_cell>")
						
						RSCodes.Filter = "ccode_no='"& Trim(RSSTyResult.Fields("cstygroup").Value)& space(6-(len(Trim(RSSTyResult.Fields("cstygroup").Value)))) &"'"
						if not rscodes.EOF then
							Response.Write("&nbsp;"&rscodes.Fields("cdiscrep").Value)
						end if
						Response.Write("</td>")
					End IF
					
					strTemp  = "<TD width=5% class=light_cell align=center>"
					Response.Write(strTemp)
					'Ard - 301633 - Add the price [Start]
					'NEK [Start] -Adding Currency and its Alignment
					'wal_131300 check if there is a price code saved for the user then get the price according to the one saved
					'response.write session("PriceCode")
					if session("PriceCode")="" then'no price
						if Session("CurrencyAlign")="LEFT" then
							Response.Write(Session("Currency") & FormatNumber(GetStyPrice(trim(RSSTyResult("style")),1)))
						else
							Response.Write(FormatNumber(GetStyPrice(trim(RSSTyResult("style")),1)) & Session("Currency"))
						end if 
					else
						if Session("CurrencyAlign")="LEFT" then
							Response.Write(Session("Currency") & FormatNumber(GetPriceCode(RSSTyResult("style"),session("PriceCode"))))
						else
							Response.Write(FormatNumber(GetPriceCode(RSSTyResult("style"),session("PriceCode"))) & Session("Currency"))
						end if 
					end if	
					'NEK [End] -Adding Currency and its Alignment
					'Response.Write(trim(RSTEmp("pricea"))) 
					'Ard - 301633 - Add the price [End]
					Response.Write("</td>")

					strTemp  = "<TD width=10% class=light_cell>"
					Response.Write(strTemp)
					if Session("M_STYVIEW") = "P" and Request("logintype")="O" then
						Response.Write("<a href=""javascript:openwindow('../order/ordshwimg.asp?name=" & trim(rsTemSty("cstymajor")) & "')""> "  & "&nbsp;Show Image</a>") 
					Else
						Response.Write("<a href=""javascript:openwindow('../order/ordshwimg.asp?name=" & trim(RSSTyResult("cstymajor")) & "')""> "  & "&nbsp;Show Image</a>") 
					End IF
					'Response.Write("<a href=""../order/ordshwimg.asp?name=" & trim(RSTEmp("cstymajor")) & """>" & "Show Image</a>") 
					Response.Write("</td>")

					RSSTyResult.MoveNext
					Count = Count + 1
					if Session("M_STYVIEW") = "P" and Request("logintype")="O" then
						rsTemSty.Close 
					End IF
				Loop
		
End if

%>
</table>
  </center>
</div>
  </center>
</div>
<Table border=0 width=95% align=center><TR><TD>
<%

	IF count > 0 Then
		Response.Write("<Center><font face=Arial size=2>Page " & CurPage & " of " & TotalPages & "</font></center>")
		Response.Write "<table align=center border=0><tr><td>"
    if CurPage > 1 then
			'We are not at the beginning, show the back button%>
			<a href="findstyle.asp?firsttime=&Logintype=<%=Request("LoginType")%>&curpage=<%=curpage - 1%>&Group=<%=Group%>&Type=<%=Request.QueryString("Type")%>"><img border=0 src="../images/<%=Session("theme")%>/back.gif"></a>
    <%End If

		if CInt(CurPage) <> CInt(TotalPages) then
        'We are not at the end, show a next button%>
			<a href="findstyle.asp?firsttime=&Logintype=<%=Request("LoginType")%>&curpage=<%=curpage+ 1%>&Group=<%=Group%>&Type=<%=Request.QueryString("Type")%>"><img border=0 src="../images/<%=Session("theme")%>/next.gif"></a>
    <%End If
		End IF
		%>
</TD></TR></Table>
<%End IF%>

<%
'*!*************************************************************
'*! Name      : GetStyPrice
'*! Developer : Ahmed M. Reda
'*! Date      : 06/21/2001
'*! Purpose   : Get style Price if there a contract or not
'*!*************************************************************
'*! Calls       : None
'*!*************************************************************
'*! Passed Parameters : strStyle    : Style code
'*!                     Qty         : Quantity
'*!*************************************************************
'*! Return      : Style price
'*!*************************************************************


FUNCTION GetStyPrice(strStyle,Qty)
Dim StyConn
Dim RSStyOrdHdr
Set StyConn = server.CreateObject("ADODB.Connection")
StyConn.Open Application("DataConnectionString")
Set RSStyOrdHdr = server.CreateObject("ADODB.RECOrdSET")
strtempsql = ""
strtempsql = strtempsql & "SELECT ORDER FROM ORDHDR WHERE ACCOUNT+CORDTYPE+order like '" & CUSTID & "C%'"
strtempsql = strtempsql & " and status <> 'X' and status <> 'B' and start >= {" & Date() & "} and complete <= {" & Date() & "}"


RSStyOrdHdr.open strtempsql,StyConn


GetStyPrice = 0
IF Not(RSStyOrdHdr.EOF And RSStyOrdHdr.BOF) Then ' Contract Exist
	Dim RSStyLine
	Set RSStyLine = server.CreateObject("adodb.recordset")
	strtempsql = ""
	strtempsql = strtempsql & "SELECT * FROM ORDLINE WHERE ORDER = '" & RSSTYORDHDR.FIELDS("ORDER").VALUE & "' and style='" & style & "'"
	RSStyLine.Open strtempsql,StyConn
	If Not(RSStyLine.EOF And RSStyLine.Bof)then 
		GetStyPrice = RSStyLine.Fields("Price").Value 
	End IF

Else ' Contract Not Exist
	'Response.Write strtempsql
	'Response.End 
	'HDM don't go to read the proce level from customer file it's already read in Session("PriceLvl")
	strtempsql = ""
	'strtempsql = strtempsql & "Select * from customer where type+account+store ='M"&custid&"'"
	'Set RSSTYCustomer = server.CreateObject("ADODB.Recordset")
	'RSSTYCustomer.Open strtempsql,StyConn
	'IF Not(RSSTYCustomer.EOF And RSSTYCustomer.BOF) Then
	IF Trim(Session("PriceLvl")) <> "" Then
		strtempsql = "select * from style where style='" & strStyle & "' order by Style"
		'Response.Write(strtempsql)
		Set RSSTYStyle = server.CreateObject("adodb.recordset")
		RSSTYStyle.Open strtempsql,StyConn
		IF Not(RSSTYStyle.EOF And RSSTYStyle.BOF) Then
			'Response.Write("ok")
			'Select Case RSSTYCustomer.Fields("pricelvl").Value 
			Select Case Trim(Session("PriceLvl"))
			'Case "A"
			'	GetStyPrice = RSSTYStyle.Fields("Pricea").Value 
			'Case "B"
			'	GetStyPrice = RSSTYStyle.Fields("Priceb").Value 
			'Case "C"
			'	GetStyPrice = RSSTYStyle.Fields("Pricec").Value 
			Case "Q"
				IF cint(Qty) < cint(RSSTYStyle.Fields("natqtyb").Value) then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Pricea").Value)
				End if
				
				IF cint(qty) > cint(RSSTYStyle.Fields("natqtyb").Value) and cint(qty) < cint(RSSTYStyle.Fields("natqtyc").Value) then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Priceb").Value)
				End if
				
				IF cint(qty) > cint(RSSTYStyle.Fields("natqtyc").Value) then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Pricec").Value) 
				End if
			Case Else
				GetStyPrice = CDbl(RSSTYStyle.Fields(Trim(Session("PriceLvl"))).Value) 
			End select
		End IF
	End IF
End IF
GetStyPrice = FormatNumber(CDbl(GetStyPrice),2)
'Response.Write GetStyPrice
'Response.End 
End Function
%>
</body>
</html>
<%
function makeQuery(fieldname, ValuesList, NoOfItems, notflag)
	dim ValuesArray
	ValuesArray = split(ValuesList, ",", -1, 1)
	strQuery="( "
	for i=0 to ubound(ValuesArray) 
		if (i mod NoOfItems) = 0 then
			if i <> 0 then
				if notflag = false then
					strQuery = strQuery & ") or "
				else
					strQuery = strQuery & ") and "
				end if
			end if
			strQuery = strQuery & fieldname
			if notflag = false then
				strQuery = strQuery & " IN ( "
			else
				strQuery = strQuery & " not in ( "
			end if
		end if
		strQuery = strQuery & "'A" & ValuesArray(i) & "'"
		if (((i+1) mod NoOfItems) <> 0) and i <> ubound(ValuesArray)  then
			strQuery = strQuery & ","
		end if
		
		if i = ubound(ValuesArray) then
				strQuery = strQuery & ") "
		end if
	next 
	strQuery = strQuery & ")"
	makeQuery = strQuery
end function
'HDM [Start] new function to determine the price level for the customer
Function GetCustPriceLvl(strCustID)

	Dim StyConn
	Dim RSStyOrdHdr
	Set StyConn = server.CreateObject("ADODB.Connection")
	StyConn.Open Application("DataConnectionString")

	strtempsql = ""
	strtempsql = "Select * from customer where type+account+store ='M"&custid&"'"
	Set RSSTYCustomer = server.CreateObject("ADODB.Recordset")
	RSSTYCustomer.Open strtempsql,StyConn
	IF Not(RSSTYCustomer.EOF And RSSTYCustomer.BOF) Then
		If trim(RSSTYCustomer("pricelvl")) = "" Then
			GetCustPriceLvl = "Pricea"
		ElseIf UCase(RSSTYCustomer("pricelvl")) = "Q" Then
			GetCustPriceLvl = RSSTYCustomer("pricelvl")
		Else
			GetCustPriceLvl = Trim("Price"&RSSTYCustomer("pricelvl"))
		End If
	End If
	'wal_add check if no customer the use default price[start]
	if trim(custid)  = "" then
		GetCustPriceLvl = "Pricea"
	end if
End Function
'wal_131300 [Start] new function get the price in price code file
Function GetPriceCode(strStyle,strCode)
	Set StyConn = server.CreateObject("ADODB.Connection")
	StyConn.Open Application("DataConnectionString")
	
	set rsStyPrice = server.CreateObject("ADODB.Recordset")
	set rsPriceCode = server.CreateObject("ADODB.Recordset")
	if session("CcurrCode")  = "" then
		rsstyprice.Open "select * from CSTPRICE where priccode = '"&trim(strCode)&"' and stydv = '"&strStyle&"' ",StyConn,1,3
	else
		rsstyprice.Open "select * from CSTPRICE where priccode = '"&trim(strCode)&"' and stydv = '"&strStyle&"' and ccurrcod = '"& trim(session("CcurrCode")) &"' ",StyConn,1,3
	end if
	'Response.Write "<br> select * from CSTPRICE where priccode = '"&trim(strCode)&"' and stydv = '"&strStyle&"' <br>"& rsstyprice.EOF	
	if not rsstyprice.EOF then
		'validate the profile date
		rspricecode.Open "select * from cstprich where priccode = '"&trim(strCode)&"'",StyConn,1,3
		'Response.Write rspricecode("dvldprfr")
		'Response.End 
		if not isnull(rspricecode("dvldprfr")) and not isnull(rspricecode("dvldprto")) and rspricecode("dvldprto") <> "" and rspricecode("dvldprfr") <> "" then
			'check that there are value
			if (date() > rspricecode("dvldprfr") and date() < rspricecode("dvldprto")) then
				if isnull(rsstyprice("pricedv")) or cdbl(rsstyprice("pricedv")) = 0 or rsstyprice("pricedv") = ""then
					GetPriceCode = GetStyPrice(trim(strStyle),1)
				else
					GetPriceCode = rsstyprice("pricedv")
				end if
			else
				GetPriceCode = GetStyPrice(trim(strStyle),1)
			end if
		else'not valid date then get the default price
			if isnull(rsstyprice("pricedv")) or cdbl(rsstyprice("pricedv")) = 0 or rsstyprice("pricedv") = ""then
				GetPriceCode = GetStyPrice(trim(strStyle),1)
			else
				GetPriceCode = rsstyprice("pricedv")
			end if
			'GetPriceCode = rsstyprice("pricedv")'GetStyPrice(trim(strStyle),1)
		end if
	else
		GetPriceCode = GetStyPrice(trim(strStyle),1)
	end if
end function
%>