<%@ LANGUAGE="VBSCRIPT" %>

<%Response.Buffer = true%>
<%
'Response.Write "<br>" & "<font size=3>" &session("strStyles") 
'check if i m submmiting then get the last values selected into the session var and close window
if request("submit") = "T" then
	'session("strRmvStr") = Request.Form ("hidStr")
	'check the removed records
	if Request.Form ("hidStr") <> "" then
		arTokens = Split(Request.Form ("hidStr"),",")
		For intLoop = 0 To uBound(arTokens)
			'check if its checked before then don't add
			if instr(session("strRmvStr"),Trim(arTokens(intLoop))) <=0 then
				if session("strRmvStr") = "" then
					session("strRmvStr") = "'" & Trim(arTokens(intLoop))& "'"
				else
					session("strRmvStr") = session("strRmvStr") & ", " & "'" & Trim(arTokens(intLoop)) & "'"
				end if
			end if
		next
	
	end if	
	if Request.Form ("chkID") <> "" then
		arTokens = Split(Request.Form ("chkID"),", ")
		For intLoop = 0 To uBound(arTokens)
			'check if its checked before then don't add
			if instr(session("strStyles"),Trim(arTokens(intLoop))) <=0 then
				if session("strStyles") = "" then
					session("strStyles") = "'" & Trim(arTokens(intLoop))& "'"
				else
					session("strStyles") = session("strStyles") & ", " & "'" & Trim(arTokens(intLoop)) & "'"
				end if
			end if
		next
	
	end if	%>
	<script language="JavaScript" type="text/JavaScript">
	<!--
		opener.document.form1.action = 'AddProfile.asp?selectSty=T&code=<%=Request.QueryString ("code")%>';
		opener.document.form1.submit();
		window.close();
	//-->
	</script>
<%
end if	
Session("Group")=Request("Group")
session("Curr_disc") = 0
set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
'Response.Write  Application("DataConnectionString")
'Response.End 

strFile = "reb"
compWork = "Y"
custid = Session("customerid")

Set RSCodes = Server.CreateObject("ADODB.RECORDSET")
strSql = "Select ccode_no, cdiscrep, cdefcode from codes where cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam like 'NCSTYGROUP%' and cdiscrep <> '' Order By cCode_No"
RSCodes.Open strSql,Conn

IF Not IsObject(Session("RSStyStruct")) Then
	Set Session("RSStyStruct") = Server.CreateObject("ADODB.RecordSet")
	strsql = "select * from icistru where citemrecty+cisegno like'U%'"
	Session("RSStyStruct").open strsql,conn
End IF

'check if i m coming first time then clear the session 
if request("first") = "T" then
	'session("strStyles") = ""
	'session("strRmvStr") = ""
	'check if there are stores saved for the user in the db
	'dim rsUserStr
	'set rsUserStr = server.CreateObject ("ADODB.recordset")
	'rsUserStr.Open "select * from privileges where  cUser_ID='" & Request("UserID") & "' and Profile='" & Request("CustID") & "' and cTokenID like 'STR%'",cnConnection
	'if not rsUserStr.EOF then
	'	do while not rsUserStr.EOF 
	'		session("strStyles") = session("strStyles") & ", " & rsUserStr("cTokenID")
	'	rsUserStr.MoveNext 
	'	loop
	'end if
else
	'request the values deleted if unchecked to delete them later from the table
	'session("strRmvStr") = Request.Form ("hidStr")
	'check the removed records
	if Request.Form ("hidStr") <> "" then
		arTokens = Split(Request.Form ("hidStr"),",")
		For intLoop = 0 To uBound(arTokens)
			'check if its checked before then don't add
			if instr(session("strRmvStr"),Trim(arTokens(intLoop))) <=0 then
				if session("strRmvStr") = "" then
					session("strRmvStr") = "'" & Trim(arTokens(intLoop))& "'"
				else
					session("strRmvStr") = session("strRmvStr") & ", " & "'" & Trim(arTokens(intLoop)) & "'"
				end if
			end if
		next
	
	end if	
	'check if there are values selected
	if Request.Form ("chkID") <> "" then
		'check if its checked before then don't add it 
		arTokens = Split(Request.Form ("chkID"),", ")
		For intLoop = 0 To uBound(arTokens)
			'check if its checked before then don't add
			if instr(session("strStyles"),Trim(arTokens(intLoop))) <=0 then
				if session("strStyles") = "" then
					session("strStyles") = "'" & Trim(arTokens(intLoop)) & "'"
				else
					session("strStyles") = session("strStyles") & ", " & "'" & Trim(arTokens(intLoop))& "'"
				end if
			end if
		next
		
	end if	
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
function chk(val)
{
	var oldVal;
	if (val.checked == false)
	{
		oldVal = document.form1.hidStr.value;
		if (oldVal == '')
			document.form1.hidStr.value  =  val.value;
		else
			document.form1.hidStr.value  = oldVal + "," + val.value;
	}
}
//-->

</SCRIPT>

<title>CRM - Style Selection Criteria</title>
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
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
function chkStores()
{
	document.form1.action = "findstyle.asp?submit=T&code=<%=Request.QueryString ("code")%>";
	document.form1.submit ();
	//window.close();
}
//-->
</SCRIPT>

<p><br><br><br></p>

<table bordercolor="#111111" border="0" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">

</Table>
<Table border=1 align=center width=95%>
<TR>
<TD class=Title>Select Style</TD>
</TR>
</TAble>
<Br>

<%IF compWork = "Y" Then%>
<form action="findstyle.asp?code=<%=Request.QueryString ("code")%>" method="post" name ="form1" id="form1">
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
						end if
						strRequiredStyle = Trim(strRequiredStyle) & Trim(strValue)
						strRequiredStyle = Ucase(strRequiredStyle) & "%"
						if Trim(Request("firsttime"))="F" then
							strTemp = "<font class=dark_cell><strong>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</b></font><INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Ucase(Request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value))) & "></strong></font>"
						else 
							strTemp = "<font class=dark_cell><strong>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "<b></font><INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) & "></strong></font>"
						end if
						
						strTemp = strTemp & " " & Trim(Session("RSStyStruct").fields("cisegsepr")) & " "
						strRequiredStyle = strRequiredStyle & Trim(Session("RSStyStruct").fields("cisegsepr"))
						Response.Write(strTemp)
						Session("RSStyStruct").MoveNext
					Loop
				End IF
				
			%> &nbsp;</td>
		<%IF Session("M_STYVIEW") = "G" Then
			arrFound= split(session("showcatalogval"),",")%>
			<td Align="left" valign="center" colSpan=1 class="dark_cell">Group
				<SELECT name=Group size=1> 
					<OPTION selected value="ALL">All
				<%IF Not RSCodes.EOF And Not RSCodes.BOF Then%>
					<%Dim strTemp%>
					<%RSCodes.MoveFirst 
						for intcount =0 to ubound(arrFound)
							RSCodes.Filter = "ccode_no='"& trim(arrFound(intcount)) & space(6 - len(trim(arrFound(intcount)))) & "'"
							if not rscodes.EOF then
								if Request("Group") =  arrFound(intcount) Then
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
		<%End IF%>
			<td align=left class="dark_cell"><input type="submit" name="B2" value="Search"><input type="reset" name="B3" value="Reset">
			</td>
     </tr>
    </table>
      </center>
    </div>

<%

strStyle = strRequiredStyle

'WAL_Apply condition on season and division[start]
Dim strSeason, strDivision

if Trim(Session("Season"))="All" then
	strSeason = "NONE"
else
	strSeason = Trim(Session("Season"))
end if 
if strSeason = "ALL" or strSeason = "" or strSeason = "NONE" or strSeason = "*" then
	strSeason	="NONE"
end if 
'Response.Write Trim(Session("Division"))
if Trim(session("catDivision"))="" Or Trim(Session("catDivision"))="*" or UCase(Trim(Session("catDivision")))="ALL" then
	strDivision = "NONE"
else
	strDivision = Trim(Session("catDivision"))
end if 

If Trim(UCase(strSeason)) = "NONE" or instr(Trim(UCase(strSeason)),",") <> 0Then
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


if trim(Request("Group"))="" or trim(Ucase(Request("Group")))="ALL" then
	strSQL = "SELECT * FROM Style where " & MakeQuery("status+cstygroup", Session("ShowCatalogVal"), 10, false) & " and style like '" & strRequiredStyle & "' "
	strSQL = strSQL& strSeasDivWhere & " order by style"
else
	strSQL = "SELECT * FROM Style where style like '" & strRequiredStyle & "' and status+cstygroup like 'A" & Request("Group")&"' "
	strSQL = strSQL& strSeasDivWhere & " order by style"
end if

'Response.Write("<br><font size=3>"&strSQL&"</font><br>")
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

RSSTyResult.Open  strsql, conn,1,3

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

%>
<div align="center">
  <center>
<table border="1" width="95%" cellpadding="0" cellspacing="0" style="border-collapse: collapse" bordercolor="#111111">
<TR>
	
	<TD width=5% class="dark_cell">></TD>
	<TD width=22% class="dark_cell">Style</TD>
	<TD Width=45% class="dark_cell">Description</TD>
	<TD class=dark_cell>Group</TD>

</TR>

<%
	
	Do While Not RSSTyResult.EOF And Count < NumPerPage%>
		<TR>
			<TD class="light_cell" ><input type=checkbox name="chkID" onclick="chk(this);"value="<%=RSSTyResult("style")%>" <%if instr(session("strStyles"),RSSTyResult("style")) > 0 and instr(session("strRmvStr"),RSSTyResult("style")) <= 0 then%>checked<%end if%>></td>
			<TD class="light_cell" ><%=trim(RSSTyResult("style"))%></TD>
			<TD class="light_cell" >&nbsp<%=trim(RSSTyResult("desc1"))%></TD>
			<%
				RSCodes.Filter = "ccode_no='"& Trim(RSSTyResult.Fields("cstygroup").Value)& space(6-(len(Trim(RSSTyResult.Fields("cstygroup").Value)))) &"'"
				if not rscodes.EOF then
					strGrp = rscodes.Fields("cdiscrep").Value
				end if
			%>
			<TD class="light_cell" >&nbsp<%=trim(strGrp)%></TD>
		</TR>
<%
		RSSTyResult.MoveNext
		Count = Count + 1
		
	Loop
		
End if

%>
</table>
  </center>
</div>
  </center>
</div>
<input type=hidden name="hidStr" value="<%=session("strRmvStr")%>">
<Table border=0 width=95% align=center><TR><TD>
<%

	IF count > 0 Then
		Response.Write("<Center><font face=Arial size=2>Page " & CurPage & " of " & TotalPages & "</font></center>")
		Response.Write "<table align=center border=0><tr><td>"
    if CurPage > 1 then
			'We are not at the beginning, show the back button%>
			<a href="javascript:document.form1.action='findstyle.asp?code=<%=Request.QueryString ("code")%>&curpage=<%=curpage - 1%>&Group=<%=Request("Group")%>&Type=<%=Request.QueryString("Type")%>';document.form1.submit();"><img border=0 src="../../images/<%=Session("theme")%>/back.gif"></a>
    <%End If

		if CInt(CurPage) <> CInt(TotalPages) then
        'We are not at the end, show a next button%>
			<a href="javascript:document.form1.action='findstyle.asp?code=<%=Request.QueryString ("code")%>&curpage=<%=curpage+ 1%>&Group=<%=Request("Group")%>&Type=<%=Request.QueryString("Type")%>';document.form1.submit();"><img border=0 src="../../images/<%=Session("theme")%>/next.gif"></a>
    <%End If
		End IF
		%>
</TD></TR></Table>
<%End IF%>
<br>
<div align=center><input type=button value="Select" onclick="chkStores()"></div>
</form>
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

%>