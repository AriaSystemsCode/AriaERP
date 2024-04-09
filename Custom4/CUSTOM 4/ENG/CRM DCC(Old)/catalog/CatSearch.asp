<%@ Language=VBScript %>
<%'Response.Buffer  = true
Response.CacheControl = "no-cache" 
Response.AddHeader "Pragma", "no-cache" 
Response.Expires = -1 
%>
<%
'WMH Outer Paging [Start]
const NumOfGrp = 3
Const NumPerPage = 3
Dim CurPage
Dim intUpLimit
Dim intLowLimit

IF Request.QueryString("CurPage") = "" Then
	CurPage = 1
Else
	CurPage = Request.QueryString("CurPage")
End IF
Session("ConnectionString") = "Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("DataPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"
Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open(Session("ConnectionString"))
'NEK[Start]Freeing Session(Trim(Session("RSStyStruct").fields("cisegsdes")))
Session("StoreValue") = ""
Session("Type") = "R"
'NEK[End]
'get the info for thAT profile
dim cnnSQL, rsStyGroup
set cnnSQL  = server.CreateObject ("Adodb.connection")
cnnSQL.Open Application("SqlServer") 
Search = Request("search")
'wal_131300 check if there a style profile saved for the logged user
if trim(session("styProfile")) <> "" then
	
	
	'set rsStyProfileHdr = server.CreateObject ("Adodb.recordset")
	'set rsStyProfileDt = server.CreateObject ("Adodb.recordset")
	set rsStyGroup = server.CreateObject ("Adodb.recordset")
	arrStyle = split(trim(application("strStyles")),",")
	'if ubound(arrStyle) > 0 then
		strsql = "select distinct FOXStyle.cstyGroup from styleprofiledetail , OPENROWSET('MSDASQL', '"
		strSQL = strSQL + Application("DataConnectionString")		
		strSQL = strSQL + "', 'Select cstyGroup ,style From  style') FOXStyle where cStyleProfileCode='"&trim(session("styProfile"))&"' and FOXStyle.style = styleprofiledetail.cStyle COLLATE Latin1_General_CI_AS"
		
		rsStyGroup.Open strsql,cnnSQL,1,3
		'Response.Write session("styProfile")
		'Response.End 
	'else'only one record
		'rsStyGroup.Open "select distinct cstyGroup from style where style ='" &arrStyle(0)&"' ",conn,1,3
	'end if
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
	intLowLimit = (CurPage * NumPerPage) - NumPerPage
	intUpLimit  = intLowLimit + NumPerPage-1
	IF intUpLimit > rsStyGroup.RecordCount Then
		intUpLimit = rsStyGroup.RecordCount 
	End IF
else
	'wal_130731 check if there is a defualt style group for this user then apply it
	if trim(session("StyGroup")) = "" then
		IF arrFound = "" Then  arrFound = split(session("showcatalogval"),",")
		'check if i ve one group
		if ubound(arrFound) > 0 then
			Group = Request("Group")
		else
			Group =  arrFound(0)
			'check to apply search 
			if Search= "" then
				Search = "Y"
			end if
		end if
	else
		IF arrFound = "" Then  arrFound = split(trim(UCASE(session("StyGroup"))),",")
		Group =  trim(UCASE(session("StyGroup")))
		'check to apply search 
		if Search= "" then
			Search = "Y"
		end if
	end if
	intLowLimit = (CurPage * NumPerPage) - NumPerPage
	intUpLimit  = intLowLimit + NumPerPage-1
	IF intUpLimit > ubound(arrFound)  Then
		intUpLimit = ubound(arrFound) 
	End IF
	'Response.Write "<br><br><br><Br><font size=5>grp"&ubound(arrFound)&"</font>"
end if
'Response.Write "<font size=5><br><br>Search=="&session("styleSize")&session("colorSize")


'Response.Write "<font size=5> Group=="&arrFound(0)&"</font>"
ShowNextButton = True
'WMH Outer Paging [End]
Dim rsCode
set rsCode = server.CreateObject ("ADODB.Recordset")
rsCode.open "Select cfld_name, Cdiscrep, Ccode_no,CrLtd_nam,crltfield from Codes where cdefcode+crltfield+cfld_name ='NNROYALTY' or  cdefcode+crltfield+cfld_name ='NNCDISCCODE' or  cdefcode+crltfield+cfld_name ='NNCPURCODE' order by cDiscrep",conn


%>
<HTML>
<HEAD>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/Catalog.css" TYPE="text/css">
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<Title><%=session("CatalogField")%> Search</Title>
<!--Create a new arrya to hold the groups names-->
<SCRIPT LANGUAGE=javascript>
	var NameGRPDESC = new Array();
	function chgFilter(val)
	{
		var val;
		document.frmMain.action = 'Catsearch.asp'
		document.frmMain.submit();
	
	}
</SCRIPT>

</HEAD>
<BODY>
<%
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
	location.href="../default.asp"
	</script>
<%
End if

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		'Response.Redirect("../repcust.asp")%>
		<script language="javascript">
			location.href ="../repcust.asp"
		</script>
		
	<%END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF

Set RSCodes = Server.CreateObject("ADODB.RECORDSET")
strSql = "Select ccode_no, cdiscrep, cdefcode from codes where cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam like 'NCSTYGROUP%' and cdiscrep <> '' Order By cCode_No"
RSCodes.Open strSql,Conn

IF Not IsObject(Session("RSStyStruct")) Then
	Set Session("RSStyStruct") = Server.CreateObject("ADODB.RecordSet")
	strsql = "select * from icistru where citemrecty+cisegno like 'U%'"
	Session("RSStyStruct").open strsql,conn
End IF
%>

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
	<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
</TR>
</table>
 <%End IF%>

<BR><BR>
<Table width=95% align=center border=1>
	<TR>
		<TD class=title><%=session("CatalogField")%></TD>
	</TR>
	</Table>

<!--WMH Top Form  [Start-------------------------------------------------------------------->
<form action="CatSearch.asp?search=Y" method="post" name ="frmMain" id="frmMain">
	<p><font size="2" face="Aria"><strong> </p>
    <div align="center">
    <center>
	<%
	'wal_130674 correct check on privilage case multi user
	if Application("CustomerLoginUsing")= "User"  then
		strAppUserVar = session("userID")
	ElseIf Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
	'Response.Write "<font size=2>" & Session("userID") & Trim(Application(strAppUserVar & "Lvl"))
	If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"CATALOG") <= 0 Then
	%>
	<Table border=0 width=95% align=center>
		<tr>
		<td class="Title"><font color="Yellow">
			You're not authorized to view this page</font>
		</td>
		</tr>
	</Table>
	<%Response.End 
	End If
	%>

   <table bordercolor="#111111" border="0" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0" >
   <tr>
	<td align=right><a href="../order/orderhsave.asp?From=Cat">Check shopping cart</a></td>
   </tr>
   </table><br>
   <table bordercolor="#111111" border="1" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0" >
   
   <tr>
		<td Align="left" valign="center" width=20% class="dark_cell">
		<%
		IF Not(Session("RSStyStruct").EOF And Session("RSStyStruct").BOF) Then
			Session("RSStyStruct").MoveFirst
			DO While Not Session("RSStyStruct").Eof
				strValue = Request.Form(Trim(Session("RSStyStruct").fields("cisegsdes")))
				Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) = strValue
				strRequiredStyle = Trim(strRequiredStyle) & Trim(strValue)	
				strRequiredStyle = Ucase(strRequiredStyle) & "%"
				if Session("RSStyStruct")("lSegEndMaj") = true then
					if Search="Y" then
						strTemp = "<font class=dark_cell>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</font> <INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Ucase(Request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value))) & ">"
						strName = Trim(Session("RSStyStruct").fields("cisegsdes"))
						'Response.Write "<font size=5>strName = " & strName & " and strTemp = " & strTemp & "</font>"
					else 
						strTemp = "<font class=dark_cell>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</font><INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) & ">"
						strName = Trim(Session("RSStyStruct").fields("cisegsdes"))
					end if
					Response.Write(""&strTemp) 					
					exit do 					
				else
					if Search="Y" then
						strTemp = "<font class=dark_cell>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</font> <INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Ucase(Request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value))) & ">"
						strName = Trim(Session("RSStyStruct").fields("cisegsdes"))
						'Response.Write "<font size=5>strName = " & strName & " and strTemp = " & strTemp & "</font>"
					else 
						strTemp = "<font class=dark_cell>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</font><INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) & ">"
						strName = Trim(Session("RSStyStruct").fields("cisegsdes"))
					end if
				Response.Write(""&strTemp) 					
				end if	
						
				strRequiredStyle = strRequiredStyle & Trim(Session("RSStyStruct").fields("cisegsepr"))
				Session("RSStyStruct").MoveNext
			Loop
				'Response.Write "<font size=5>strRequiredStyle="&strRequiredStyle&"</font>"
			
		End IF
		%> </td>
	
        <%IF Session("M_STYVIEW") = "G" Then%>
        <td Align="left" width=40% valign="center" class="dark_cell">
        Group
		   <SELECT name=Group size=1> 
				
				<%IF Not RSCodes.EOF And Not RSCodes.BOF Then%>
					<%Dim strTemp%>
					<%RSCodes.MoveFirst 
						'wal_131300 check if i've a tyle profile then display the list of grpoups per user
						if trim(session("styProfile"))<> "" then
							if rsStyGroup.RecordCount  > 1 then%>
								<OPTION value="ALL">All
						<%  end if
							do while not rsStyGroup.EOF 
								RSCodes.Filter = "ccode_no='"& trim(rsStyGroup("cstyGroup")) & space(6 - len(trim(rsStyGroup("cstyGroup")))) & "'"
								
								if not RSCodes.EOF then
									if Group =  trim(rsStyGroup("cStyGroup")) Then
										Response.Write("<option value=""" & Trim(RSCodes.Fields("ccode_no").Value )& """ selected>" & RSCodes.Fields("cdiscrep").Value )
									Else
										Response.Write("<option value=""" & Trim(RSCodes.Fields("ccode_no").Value )& """>" & RSCodes.Fields("cdiscrep").Value )
									End IF
								end if
								RSCodes.Filter = ""
							rsStyGroup.MoveNext 
							loop
						else
							'arrFound= split(session("showcatalogval"),",")
							if ubound(arrFound) > 0 then%>
								<OPTION value="ALL">All
						<%end if
						  for intcount =0 to ubound(arrFound)
								
								RSCodes.Filter = "ccode_no='"& trim(arrFound(intcount)) & space(6 - len(trim(arrFound(intcount)))) & "'"
								'Response.Write "<option>" & arrFound(intcount) & space(6 - len(arrFound(intcount))) & "<option>"
								if not RSCodes.EOF then
									if Group =  arrFound(intcount) Then
										Response.Write("<option value=""" & Trim(RSCodes.Fields("ccode_no").Value )& """ selected>" & RSCodes.Fields("cdiscrep").Value )
									Else
										Response.Write("<option value=""" & Trim(RSCodes.Fields("ccode_no").Value )& """>" & RSCodes.Fields("cdiscrep").Value )
									End IF
								end if
								RSCodes.Filter = ""
							next
						end if
				END IF%>
			</SELECT>	
		</td>
		<%End IF%>
		<%'WAL_037223_add another related filters [start]
		  'wal_check if to display or not
		  if session("UseCode") = "T" then
		%>
			<td align=left width=20% class="dark_cell">Select Code
				<select name="selFilter" onchange="chgFilter(this.value);">
					<option value="">All
					<option value="ROYALTY" <%if trim(Request.Form ("selFilter")) = "ROYALTY" then%>selected<%end if%>>Royalty
					<option value="CDISCCODE" <%if trim(Request.Form ("selFilter")) = "CDISCCODE" then%>selected<%end if%>>Discount
					<option value="CPURCODE" <%if trim(Request.Form ("selFilter")) = "CPURCODE" then%>selected<%end if%>>Pur. Group
				</select>
			</td>
		<%else%><td align=left width=20% class="dark_cell"></td>
		<%end if%>
		<!--td align=left class="dark_cell">
			<select name="selOper">
				<%if Request.Form ("selFilter")= "" then%>
					<option value="">None
				<%else%>
					<option value="like">Like
					<option value="=" <%if trim(Request.Form ("selOper")) = "=" then%>selected<%end if%>>=
					<option value=">" <%if trim(Request.Form ("selOper")) = ">" then%>selected<%end if%>>>
					<option value="<" <%if trim(Request.Form ("selOper")) = "<" then%>selected<%end if%>><</option>
					<option value=">=" <%if trim(Request.Form ("selOper")) = ">=" then%>selected<%end if%>>>=
					<option value="<=" <%if trim(Request.Form ("selOper")) = "<=" then%>selected<%end if%>><= 
					<option value="<>" <%if trim(Request.Form ("selOper")) = "<>" then%>selected<%end if%>><>
					
				<%end if%>
			</select>
		</td-->
		<td align=left width=20% class="dark_cell">
		<%'wal_check if to display or not
		  if session("UseCode") = "T" then
		%>
		<%'Response.Write "<FONT size=2 color=red>cfld_name = '" &trim(Request.Form ("selFilter"))& "'" %>
			<select name=selValue>
			<%if Request.Form ("selFilter") <> "" then
				if rsCode.eof then%>
					<option value="">None
			<%  else
					rsCode.Filter = "cfld_name = '" &trim(Request.Form ("selFilter"))& "'" 
					if not rsCode.eof then%>
					<%
						do while not rsCode.eof%>
							<option value="<%=rsCode("Ccode_no")%>" <%if Request.Form ("selValue") = rsCode("Ccode_no") then%>selected<%end if%>><%=rsCode("cDiscrep")%>
						<%rsCode.movenext
						  loop
						  rsCode.Filter = ""
					end if
				end if
			  else%>
				<option value="">None
			<%end if%>
			</select>
		<%end if%>
		</td>
		<%'WAL_037223_add another related filters [end]%>
		<td align=left class="dark_cell"><input type="submit" name="B2" value="Search"><input type="reset" name="B3" value="Reset">
		</td>
     </tr>
    </table>
    </form>
<!--WMH Top Form [End]---------------------------------------------------------------------->    
<Form name=frm01 method=post>

<%
'WMH [Start]
'Response.Write "strStyle===="&Request.QueryString("strStyle")&"<br>"
if Request.QueryString("strStyle")= "" then
	strStyle = Request(strName)
else 		
	strStyle = Request("strStyle")
end if
strStyle = Request(strName)
'Response.Write "<font size=3>strStyle===="&strName&"<br>"
'ARD - Search result [Start]
If Search = "Y" then

Dim strSeason, strDivision
strSeason = Trim(Session("StyleColor"))
strDivision = Trim(Session("CatDivision"))
'If Trim(UCase(strSeason)) = "NONE" Then
'	strSeasonCondition = ""
'Else
'	strSeasonCondition = " Season='" & strSeason & "'"
'End If
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
'Response.Write "strSeasDivWhere=="&strSeasDivWhere
If trim(strSeasDivWhere) <> "" And Left(Trim(strSeasDivWhere),3) <> "AND" Then
	strSeasDivWhere = " AND " & strSeasDivWhere
End If
'WAL_ add condition on other filters[start]
if Request.Form ("selFilter") <> "" then	
	if trim(Request.Form ("selOper")) = "like" then
		strCond = " AND "& trim(Request.Form ("selFilter")) &" like '" & trim(Request.Form ("selValue")) &"%'"
	else
		strCond = " AND "& trim(Request.Form ("selFilter")) &" like '" & trim(Request.Form ("selValue")) &"'"
	end if
end if
'WAL_ add condition on other filters[end]
Dim rsStySearch 
Set rsStySearch = server.CreateObject("adodb.recordset")
'wal_add a recordset to hold the desc separtly
Dim rsStyDesc
Set rsStyDesc = server.CreateObject("adodb.recordset")
'NEK [Start]If the Session : Session("M_STYVIEW") ="P" then Trim(Group) will always be "" 
'so it will always enter this function and it is not supposed to 
if Trim(Group)="ALL" Or Trim(Group)="" then
	
	'wal_131300 add condition to add only selected style in the user profile
	if session("styProfile")= "" then
		strsql = "select DISTINCT style.cstymajor,Style.CSTYGROUP, codes.cdiscrep from style,codes where cstymajor like '" & UCASE(Trim(strRequiredStyle)) & "' and " &makeQuery("CSTYgroup", session("showcatalogval"), 10, false) & " And cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam like 'NCSTYGROUP%'  and style.cstygroup=codes.ccode_no And style.status='A' " & strSeasDivWhere &strCond & " and codes.crltfield='N'"
		'wal_add a select statment to get the desc separte becuase i want distinct style major
		strSqlDesc = "select distinct desc1,cstymajor from style where cstymajor like '" & UCASE(Trim(strRequiredStyle)) & "' and " &makeQuery("CSTYgroup", session("showcatalogval"), 10, false) & " And style.status='A' " & strSeasDivWhere &strCond
	else
		strsql = "select DISTINCT style.cstymajor,Style.CSTYGROUP, codes.cdiscrep from styleprofiledetail,"
		strsql = strsql & " OPENROWSET('MSDASQL', '"
		strSQL = strSQL + Application("DataConnectionString")		
		strSQL = strSQL + "', 'Select * From style')Style, "
		strsql = strsql & " OPENROWSET('MSDASQL', '"
		strSQL = strSQL + Application("DataConnectionString")		
		strSQL = strSQL + "', 'Select * From codes')Codes "
		
		strsql = strsql & " where Style.cstymajor like '" & UCASE(Trim(strRequiredStyle)) & "' and " &makeQuery("CSTYgroup", session("showcatalogval"), 10, false) & " And cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam like 'NCSTYGROUP%'  and style.cstygroup=codes.ccode_no And style.status='A' And cStyleProfileCode='"&trim(session("styProfile"))&"'" & strSeasDivWhere &strCond & " and codes.crltfield='N' and style.style = styleProfileDetail.cStyle COLLATE Latin1_General_CI_AS"
		
		'wal_add a select statment to get the desc separte becuase i want distinct style major
		strSqlDesc = "select distinct desc1,cstymajor from styleprofiledetail,"
		strSqlDesc = strSqlDesc & " OPENROWSET('MSDASQL', '"
		strSqlDesc = strSqlDesc + Application("DataConnectionString")		
		strSqlDesc = strSqlDesc + "', 'Select * From style')Style, "
		strSqlDesc = strSqlDesc & " OPENROWSET('MSDASQL', '"
		strSqlDesc = strSqlDesc + Application("DataConnectionString")		
		strSqlDesc = strSqlDesc + "', 'Select * From codes')Codes "
		strSqlDesc = strSqlDesc + " where cstymajor like '" & UCASE(Trim(strRequiredStyle)) & "' and " &makeQuery("CSTYgroup", session("showcatalogval"), 10, false) & " And style.status='A' and style.style = styleProfileDetail.cStyle  COLLATE Latin1_General_CI_AS And cStyleProfileCode='"&trim(session("styProfile"))&"' " & strSeasDivWhere &strCond
		'arrStyle = split(trim(application("strStyles")),",")
		'if ubound(arrStyle) > 0 then
		'else
		'end if
	end if
	
else
	'wal_131300 add condition to add only selected style in the user profile
	if trim(session("styProfile"))= "" then
		strsql = "select DISTINCT style.cstymajor,Style.CSTYGROUP, codes.cdiscrep from style,codes where status+cstygroup like 'A" & Group & "%' and  cstymajor Like '" &UCASE(Trim(strRequiredStyle))& "' And cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam like 'NCSTYGROUP%' And style.cstygroup=codes.ccode_no "	&strSeasDivWhere &strCond  & " and codes.crltfield='N'"
		strSqlDesc = "select distinct desc1,cstymajor from style where status+cstygroup like 'A" & Group & "%' and  cstymajor Like '" &UCASE(Trim(strRequiredStyle))& "'" &strSeasDivWhere &strCond 
	else
		strsql = "select DISTINCT style.cstymajor,Style.CSTYGROUP, codes.cdiscrep from styleprofiledetail,"
		strsql = strsql & " OPENROWSET('MSDASQL', '"
		strSQL = strSQL + Application("DataConnectionString")		
		strSQL = strSQL + "', 'Select * From style')Style, "
		strsql = strsql & " OPENROWSET('MSDASQL', '"
		strSQL = strSQL + Application("DataConnectionString")		
		strSQL = strSQL + "', 'Select * From codes')Codes "
		
		strsql = strsql & " where status+cstygroup like 'A" & Group & "%' and  cstymajor Like '" &UCASE(Trim(strRequiredStyle))& "' And cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam like 'NCSTYGROUP%'  and style.cstygroup=codes.ccode_no And style.status='A' And cStyleProfileCode='"&trim(session("styProfile"))&"'" & strSeasDivWhere &strCond & " and codes.crltfield='N' and style.style = styleProfileDetail.cStyle COLLATE Latin1_General_CI_AS"
		
		'wal_add a select statment to get the desc separte becuase i want distinct style major
		strSqlDesc = "select distinct desc1,cstymajor from styleprofiledetail,"
		strSqlDesc = strSqlDesc & " OPENROWSET('MSDASQL', '"
		strSqlDesc = strSqlDesc + Application("DataConnectionString")		
		strSqlDesc = strSqlDesc + "', 'Select * From style')Style, "
		strSqlDesc = strSqlDesc & " OPENROWSET('MSDASQL', '"
		strSqlDesc = strSqlDesc + Application("DataConnectionString")		
		strSqlDesc = strSqlDesc + "', 'Select * From codes')Codes "
		strSqlDesc = strSqlDesc + " where status+cstygroup like 'A" & Group & "%' and  cstymajor Like '" &UCASE(Trim(strRequiredStyle))& "'  And style.status='A' and style.style = styleProfileDetail.cStyle  COLLATE Latin1_General_CI_AS And cStyleProfileCode='"&trim(session("styProfile"))&"' " & strSeasDivWhere &strCond
		
	end if
end if
'wal_131300 add condition to add only selected style in the user profile
'if session("styProfile")<> "" then
'	arrStyle = split(trim(application("strStyles")),",")
'	if ubound(arrStyle) > 0 then
'		strSql = strSQL & " And ("
'		strSqlDesc = strSqlDesc & " and ("
'		for i=0 to ubound(arrStyle)
'			strSql = strSql & "style =" & arrStyle(i)
'			strSqlDesc = strSqlDesc & "style =" & arrStyle(i)
'			if i <> UBound(arrStyle) then
''				strSql = strSql & " OR "
'				strSqlDesc = strSqlDesc & " OR "
'			End if
'					
'		next
'		strsql = strsql & ")"
'		strSqlDesc = strSqlDesc & ")"
'		'Response.Write "<font size=2>"&strsql
'		'Response.End 
'		'rsStyGroup.Open strsql,conn,1,3
'	else'only one record
'		strSql = strSQL & " and style ='" &arrStyle(0)&"' "
'		strSqlDesc = strSqlDesc & " and style ='" &arrStyle(0)&"' "
'	end if
'	'strSql = strSQL & " and style in (" &trim(application("strStyles"))& ")"
'	'strSqlDesc = strSqlDesc & " and style in (" &trim(application("strStyles"))& ")"
'end if
'	Response.Write(strSql)
	'Response.End 
	rsStySearch.CursorType = 3
	rsStyDesc.CursorType = 3
	if trim(session("styProfile"))= "" then
		rsStySearch.Open strsql	,conn
		rsStyDesc.Open strSqlDesc,conn
	else
		rsStySearch.Open strsql	,cnnSQL
		rsStyDesc.Open strSqlDesc,cnnSQL
	end if
	'WAL_get valid groups
	'i.e groups with styles[start]
	intFound = 0
	if trim(session("styProfile")) =  "" then
		For intctr = 0 to Ubound(arrFound)
			rsStySearch.Filter = "CSTYGROUP = '" & Trim(arrFound(intctr)) & "'"
			IF Not(rsStySearch.EOF and rsStySearch.BOF) Then
				if strGroupFound = "" Then
					strGroupFound = rsStySearch.Fields("cstygroup").Value 
				Else
					strGroupFound = strGroupFound & "," & rsStySearch.Fields("cstygroup").Value 
				End IF
			End IF
		'Response.Write "strGroupFound=="&strGroupFound
			rsStySearch.Filter = ""
		Next
	else
		rsStyGroup.MoveFirst 
		do while not rsStyGroup.EOF 							
			rsStySearch.Filter = "CSTYGROUP = '" & trim(rsStyGroup("cstyGroup")) & "'"
			IF Not(rsStySearch.EOF and rsStySearch.BOF) Then
				if strGroupFound = "" Then
					strGroupFound = rsStySearch.Fields("cstygroup").Value 
				Else
					strGroupFound = strGroupFound & "," & rsStySearch.Fields("cstygroup").Value 
				End IF
			End IF
		'Response.Write "strGroupFound=="&strGroupFound
			rsStySearch.Filter = ""
		rsStyGroup.MoveNext 
		loop
	end if
		
	strvalidGroup = Split(strGroupFound,",")
	
	IF intUpLimit > ubound(strvalidGroup)  Then
		intUpLimit = ubound(strvalidGroup) 
	End IF%>
	<SCRIPT LANGUAGE=javascript>
		<!--
		function do_load()
		{
		// WMH Outer Paging [Star]
			for(intcont=<%=intLowLimit%> ; intcont < <%=intUpLimit + 1%> ; intcont++)
		// WMH Outer Paging [End]	
			{
				strtry = eval('Grpnames[' + intcont + ']')
	//			document ("NEK=="+eval(strtry).length);
				if(eval(strtry).length > 5)
					{
						intarcount = 5
						objnext = eval('document.frm01.next' + intcont)
						objnext.src = '../images/<%=Session("Theme")%>/next.gif'
					}
				else
					intarcount = eval(strtry).length
				for(intiner=0 ;intiner< intarcount  ; intiner++)
				{
					var strtemp = eval('Grpnames[' + intcont + ']') + '['+ intiner +']'	
					var strStyMajInfo = eval('Grpnames[' + intcont + ']') + 'DESC' +'['+ intiner +']'
					var strImgDesc
					
					//To put the style esc. [Start]
					strImgDesc = ''
					<%
					'response.write "M_STYINFO==="&Session("M_STYINFO")
					Select Case Session("M_STYINFO")
						Case "C"
							%>
							strStyMajInfo = eval(strtemp)
							<%
						Case "D"
							%>
							strStyMajInfo = eval(strStyMajInfo)
							<%
						Case "B"
						%>
							strStyMajInfo = eval(strtemp) + ' - ' + eval(strStyMajInfo)
						<%
					End Select
					%>
					//alert(strStyMajInfo)
					objDiv = eval('DIV' + intcont + intiner)
					eval('objDiv').innerText = strStyMajInfo
					objImg = eval('document.frm01.img' + intcont + intiner)
					objImg.src = '../styimg/' + eval(strtemp) + '.jpg'
					objHref = eval('document.all.a' +  intcont + intiner)

					var strGrpName = eval('Grpnames[' + intcont + ']');
					strGrpName = strGrpName.substr(3,strGrpName.length);
					objHref.href = 'catstyle.asp?PageID=S&style=' + eval(strtemp) + '&GRP=' + strGrpName;
					
				}
			}
		}


		function do_loadSingle()
		{
		// WMH [Start]
		/*	var intCountLimit
			for(intcont=0 ; intcont < 3  ; intcont++)
			{
				for(inerloop = 0 ; inerloop <5 ; inerloop++)
				{
					strtry = eval('Grpnames[0]')
					
				}
			}
		*/
		// WMH Outer Paging [Star]
				
				intcont=0
				strtry = eval('Grpnames[' + intcont + ']')
				if(eval(strtry).length > 15)
					{
						intarcount = 15
						objnext = eval('document.frm01.next' + intcont)
						objnext.src = '../images/<%=Session("Theme")%>/next.gif'
					}
				else
					intarcount = eval(strtry).length
				for(intiner=0 ;intiner< intarcount  ; intiner++)
				{
					var strtemp = eval('Grpnames[' + intcont + ']') + '['+ intiner +']'	
					var strStyMajInfo = eval('Grpnames[' + intcont + ']') + 'DESC' +'['+ intiner +']'
					var strImgDesc
					
					//To put the style esc. [Start]
					strImgDesc = ''
					<%
					
					Select Case Session("M_STYINFO")
						Case "C"
							%>
							strStyMajInfo = eval(strtemp)
							<%
						Case "D"
							%>
							strStyMajInfo = eval(strStyMajInfo)
							<%
						Case "B"
						%>
							strStyMajInfo = eval(strtemp) + ' - ' + eval(strStyMajInfo)
						<%
					End Select
					%>
					objDiv = eval('DIV' + intcont + intiner)
					eval('objDiv').innerText = strStyMajInfo
					//[End]
					objImg = eval('document.frm01.img' + intcont + intiner)
					objImg.src = '../styimg/' + eval(strtemp) + '.jpg'
					
					objHref = eval('document.all.a' +  intcont + intiner)

					var strGrpName = eval('Grpnames[' + intcont + ']');
					strGrpName = strGrpName.substr(3,strGrpName.length);
					
					
					objHref.href = 'catstyle.asp?PageID=S&style=' + eval(strtemp) + '&GRP=' + strGrpName//eval('Grpnames[' + intcont + ']')
					
				}
		//WMH [End]
		}



		function move_next(GrpNum)
		{
			strtry = eval('Grpnames[' + GrpNum + ']');
			intGrpCount = eval(strtry).length;
			intCurrSty = 5*(intCurpage[GrpNum] - 1) + 1;
			
			
			if(intCurrSty + 4 < intGrpCount )
			{
				intCurpage[GrpNum] ++;
				intCurrSty = intCurrSty + 5;
			
				if(intCurrSty > intGrpCount)
					intGrpEnd = intGrpCount + 1;
				else
					if(intCurrSty + 5 > intGrpCount)
						intGrpEnd = intGrpCount + 1;
					else
						intGrpEnd = intCurrSty + 5;
				
				var intcounter = 0;
				for(intcont=0 ;intcont<5  ; intcont++)
				{
					objHref = eval('document.all.a' +  GrpNum + intcont)
					objHref.href = '#';
				}
				
				for(intcont = intCurrSty ; intcont < intGrpEnd ; intcont++)
				{
					objImg = eval('document.frm01.img' + GrpNum + intcounter);
					inttemp = intcont - 1;
					var strtemp = eval('Grpnames[' + GrpNum + ']') + '['+ inttemp +']';
					objImg.src = '../styimg/' + eval(strtemp) + '.jpg';
					objHref = eval('document.all.a' +  GrpNum + intcounter)

					var strStyMajInfo = eval('Grpnames[' + GrpNum + ']') + 'DESC' +'['+ inttemp +']'
					var strImgDesc
					
					//To put the style esc. [Start]
					strImgDesc = ''
					<%
					
					Select Case Session("M_STYINFO")
						Case "C"
							%>
							strStyMajInfo = eval(strtemp)
							<%
						Case "D"
							%>
							strStyMajInfo = eval(strStyMajInfo)
							<%
						Case "B"
						%>
							strStyMajInfo = eval(strtemp) + ' - ' + eval(strStyMajInfo)
						<%
					End Select
					%>

					objDiv = eval('DIV' + GrpNum + intcounter)
					eval('objDiv').innerText = strStyMajInfo

					var strGrpName = eval('Grpnames[' + GrpNum + ']');
					strGrpName = strGrpName.substr(3,strGrpName.length)

					
					objHref.href = 'catstyle.asp?PageID=S&style=' + eval(strtemp) + '&GRP=' + strGrpName
					intcounter ++;
				}
					
				for(intcont = intcounter ; intcont < 5 ; intcont++)
				{
					objImg = eval('document.frm01.img' + GrpNum + intcont);
					objImg.src = '../images/<%=Session("Theme")%>/transperant.gif';
					
					objDiv = eval('DIV' + GrpNum + intcont);
					eval('objDiv').innerText = "";
					
					
				}
				if(eval(intCurrSty + 5) <= intGrpCount)
				{
					objnext = eval('document.frm01.next' + GrpNum)
					objnext.src = '../images/<%=Session("Theme")%>/next.gif'
				}
				else
				{
					objnext = eval('document.frm01.next' + GrpNum)
					objnext.src = '../images/<%=Session("Theme")%>/transperant.gif'
				}

				objback = eval('document.frm01.back' + GrpNum)
				objback.src = '../images/<%=Session("Theme")%>/back.gif'
			}	
		}

		// WMH to Navigte 15 items
		function move_nextSingle(GrpNum)
		{
			strtry = eval('Grpnames[' + GrpNum + ']');
			intGrpCount = eval(strtry).length;
			intCurrSty = 15*(intCurpage[GrpNum] - 1) + 1;
			
			
			if(intCurrSty + 14 < intGrpCount )
			{
				intCurpage[GrpNum] ++;
				intCurrSty = intCurrSty + 15;
			
				if(intCurrSty > intGrpCount)
					intGrpEnd = intGrpCount + 1;
				else
					if(intCurrSty + 15 > intGrpCount)
						intGrpEnd = intGrpCount + 1;
					else
						intGrpEnd = intCurrSty + 15;
				
				var intcounter = 0;
				for(intcont=0 ;intcont<15  ; intcont++)
				{
					objHref = eval('document.all.a' +  GrpNum + intcont)
					objHref.href = '#';
				}
				
				for(intcont = intCurrSty ; intcont < intGrpEnd ; intcont++)
				{
					objImg = eval('document.frm01.img' + GrpNum + intcounter);
					inttemp = intcont - 1;
					var strtemp = eval('Grpnames[' + GrpNum + ']') + '['+ inttemp +']';
					objImg.src = '../styimg/' + eval(strtemp) + '.jpg';
					objHref = eval('document.all.a' +  GrpNum + intcounter)

					var strStyMajInfo = eval('Grpnames[' + GrpNum + ']') + 'DESC' +'['+ inttemp +']'
					var strImgDesc
					
					//To put the style esc. [Start]
					strImgDesc = ''
					<%
					
					Select Case Session("M_STYINFO")
						Case "C"
							%>
							strStyMajInfo = eval(strtemp)
							<%
						Case "D"
							%>
							strStyMajInfo = eval(strStyMajInfo)
							<%
						Case "B"
						%>
							strStyMajInfo = eval(strtemp) + ' - ' + eval(strStyMajInfo)
						<%
					End Select
					%>

					objDiv = eval('DIV' + GrpNum + intcounter)
					eval('objDiv').innerText = strStyMajInfo

					var strGrpName = eval('Grpnames[' + GrpNum + ']');
					strGrpName = strGrpName.substr(3,strGrpName.length)
					
					objHref.href = 'catstyle.asp?PageID=S&style=' + eval(strtemp) + '&GRP=' + strGrpName
					intcounter ++;
				}
					
				for(intcont = intcounter ; intcont < 15 ; intcont++)
				{
					objImg = eval('document.frm01.img' + GrpNum + intcont);
					objImg.src = '../images/<%=Session("Theme")%>/transperant.gif';
					
					objDiv = eval('DIV' + GrpNum + intcont);
					eval('objDiv').innerText = "";
				}
				

				if(intCurrSty + 15 < intGrpCount)
				{
					objnext = eval('document.frm01.next' + GrpNum)
					objnext.src = '../images/<%=Session("Theme")%>/next.gif'
				}
				else
				{
					objnext = eval('document.frm01.next' + GrpNum)
					objnext.src = '../images/<%=Session("Theme")%>/transperant.gif'
				}

				objback = eval('document.frm01.back' + GrpNum)
				objback.src = '../images/<%=Session("Theme")%>/back.gif'
			}	
		}

		function move_back(GrpNum)
		{
			
			if(intCurpage[GrpNum] - 1 > 0)
			{
				intCurpage[GrpNum] --;
				strtry = eval('Grpnames[' + GrpNum + ']');
				intGrpCount = eval(strtry).length;
				intCurrSty = 5*(intCurpage[GrpNum] - 1) + 1;

				intGrpEnd = intCurrSty + 4 ;

				var intcounter = 0;
				for(intcont = intCurrSty - 1 ; intcont < intGrpEnd ; intcont++)
				{
					objImg = eval('document.frm01.img' + GrpNum + intcounter);
					inttemp = intcont ;
					var strtemp = eval('Grpnames[' + GrpNum + ']') + '['+ inttemp +']';

					var strStyMajInfo = eval('Grpnames[' + GrpNum + ']') + 'DESC' +'['+ inttemp +']'
					var strImgDesc
					
					strImgDesc = ''
					<%
					
					Select Case Session("M_STYINFO")
						Case "C"
							%>
							strStyMajInfo = eval(strtemp)
							<%
						Case "D"
							%>
							strStyMajInfo = eval(strStyMajInfo)
							<%
						Case "B"
						%>
							strStyMajInfo = eval(strtemp) + ' - ' + eval(strStyMajInfo)
						<%
					End Select
					%>


					objDiv = eval('DIV' + GrpNum + intcounter)
					eval('objDiv').innerText = strStyMajInfo
					
					objImg.src = '../styimg/' + eval(strtemp) + '.jpg';
					objHref = eval('document.all.a' +  GrpNum + intcounter)


					var strGrpName = eval('Grpnames[' + GrpNum + ']');
					strGrpName = strGrpName.substr(3,strGrpName.length)
					
					objHref.href = 'catstyle.asp?PageID=S&style=' + eval(strtemp) + '&GRP=' + strGrpName
					intcounter ++;
				}
					
				for(intcont = intcounter ; intcont < 5 ; intcont++)
				{
					objImg = eval('document.frm01.img' + GrpNum + intcont);
					objImg.src = '../images/<%=Session("Theme")%>/transperant.gif';
				}
				
				if(intGrpEnd  < intGrpCount)
				{
					objnext = eval('document.frm01.next' + GrpNum)
					objnext.src = '../images/<%=Session("Theme")%>/next.gif'
				}
				else
				{
					objnext = eval('document.frm01.next' + GrpNum)
					objnext.src = '../images/<%=Session("Theme")%>/transperant.gif'
				}

				if(intCurpage[GrpNum] > 1)
				{
					objback = eval('document.frm01.back' + GrpNum)
					objback.src = '../images/<%=Session("Theme")%>/back.gif'
				}
				else
				{
					objback = eval('document.frm01.back' + GrpNum)
					objback.src = '../images/<%=Session("Theme")%>/transperant.gif'
				}
			}	
			
		}


		function move_backSingle(GrpNum)
		{
			
			if(intCurpage[GrpNum] - 1 > 0)
			{
				intCurpage[GrpNum] --;
				strtry = eval('Grpnames[' + GrpNum + ']');
				intGrpCount = eval(strtry).length;
				intCurrSty = 15*(intCurpage[GrpNum] - 1) + 1;

				intGrpEnd = intCurrSty + 14 ;

				var intcounter = 0;
				for(intcont = intCurrSty - 1 ; intcont < intGrpEnd ; intcont++)
				{
					objImg = eval('document.frm01.img' + GrpNum + intcounter);
					inttemp = intcont ;
					var strtemp = eval('Grpnames[' + GrpNum + ']') + '['+ inttemp +']';

					var strStyMajInfo = eval('Grpnames[' + GrpNum + ']') + 'DESC' +'['+ inttemp +']'
					var strImgDesc
					
					strImgDesc = ''
					<%
					
					Select Case Session("M_STYINFO")
						Case "C"
							%>
							strStyMajInfo = eval(strtemp)
							<%
						Case "D"
							%>
							strStyMajInfo = eval(strStyMajInfo)
							<%
						Case "B"
						%>
							strStyMajInfo = eval(strtemp) + ' - ' + eval(strStyMajInfo)
						<%
					End Select
					%>


					objDiv = eval('DIV' + GrpNum + intcounter)
					eval('objDiv').innerText = strStyMajInfo
					
					objImg.src = '../styimg/' + eval(strtemp) + '.jpg';
					objHref = eval('document.all.a' +  GrpNum + intcounter)


					var strGrpName = eval('Grpnames[' + GrpNum + ']');
					strGrpName = strGrpName.substr(3,strGrpName.length)
					
					objHref.href = 'catstyle.asp?PageID=S&style=' + eval(strtemp) + '&GRP=' + strGrpName
					intcounter ++;
				}
					
				for(intcont = intcounter ; intcont < 15 ; intcont++)
				{
					objImg = eval('document.frm01.img' + GrpNum + intcont);
					objImg.src = '../images/<%=Session("Theme")%>/transperant.gif';
				}
				
				if(intGrpEnd  < intGrpCount)
				{
					objnext = eval('document.frm01.next' + GrpNum)
					objnext.src = '../images/<%=Session("Theme")%>/next.gif'
				}
				else
				{
					objnext = eval('document.frm01.next' + GrpNum)
					objnext.src = '../images/<%=Session("Theme")%>/transperant.gif'
				}

				if(intCurpage[GrpNum] > 1)
				{
					objback = eval('document.frm01.back' + GrpNum)
					objback.src = '../images/<%=Session("Theme")%>/back.gif'
				}
				else
				{
					objback = eval('document.frm01.back' + GrpNum)
					objback.src = '../images/<%=Session("Theme")%>/transperant.gif'
				}
			}	
			
		}
		//-->
	</SCRIPT>
	<%'WAL_get valid groups
	'i.e groups with styles[end]
	'WMH [Start]
	Response.Write("<p>Total Records Available ")
	Response.Write(rsStySearch.RecordCount) & "</p><br><br>"
	'Response.End 
	'WMH [Start]
	intRecordCount = rsStySearch.RecordCount
	if rsStySearch.RecordCount = 1 then	
		Response.Redirect("Catstyle.asp?PageID=S&style=" & rsStySearch("cstymajor") & "&GRP=" & rsStySearch("cstygroup"))
	'WMH [Start]
	elseif rsStySearch.RecordCount = 0 then	
		ShowNextButton = False
	'WMH [End]
	else'more than 1 record	
		if Trim(Group)="ALL" then
			arrFound= split(session("showcatalogval"),",")%>
			<SCRIPT LANGUAGE=javascript>
			<!--
				var Grpnames = new Array();
				var intCurpage = new Array();
				<%For Intcont=0 to ubound(strvalidGroup)%>
					Grpnames[<%=Intcont%>] = '<%response.write("GRP" & Trim(strvalidGroup(Intcont)))%>'
					intCurpage[<%=Intcont%>] = 1
				<%Next%>
			//-->
			</SCRIPT>
			<%
			IF Request.QueryString("CurPage") = "" Then
				CurPage = 1
			Else
				CurPage = Request.QueryString("CurPage")
			End IF
			For intcount = intLowLimit to intUpLimit
				rsStySearch.Filter = "CSTYGROUP = '" & Trim(strvalidGroup(intcount)) & "'"
				
				'WMH the next line cause error in case select Group and Style and there aren't spcific group [Start]
				%>
					<SCRIPT LANGUAGE=javascript>
					<!--
					//hossam
						var <%Response.Write("GRP" & Trim(strvalidGroup(intcount)))%> = new Array();
						var <%Response.Write("GRP" & Trim(strvalidGroup(intcount)))& "DESC"%> = new Array();
						NameGRPDESC[<%=intcount%>]='<%=Trim(rsStySearch.fields("cdiscrep").value)%>'
						<%intcounter = 0%>
						<%Do while not rsStySearch.EOF 
							'get the style desc
							rsStyDesc.movefirst()
							rsStyDesc.filter = "cstymajor = '"& Trim(rsStySearch.fields("cstymajor").value) &"'"
							if not rsStyDesc.eof then
								strDesc = rsStyDesc("desc1")
							end if
							rsStyDesc.filter = ""
						%>
							<%Response.Write("GRP" & Trim(strvalidGroup(intcount)))%>[<%=intcounter%>] = '<%=Trim(rsStySearch.fields("cstymajor").value)%>'
							
							<%Response.Write("GRP" & Trim(strvalidGroup(intcount)))& "DESC"%>[<%=intcounter%>] = '<%=trim(strDesc)%>'
							<%intcounter = intcounter + 1%>
							<%rsStySearch.MoveNext%>
						<%Loop%>
					//-->
					</SCRIPT>
				<%
			Next
		    For intloop=intLowLimit to intUpLimit
		       intresult = intloop mod 2%>
			   <table width=800 align=center>
				  <TR>
						<%
						IF intresult > 0 Then
							Response.Write("<TD>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<TD>")
						End IF
						
						Dim intfrstelmnt
						intfrstelmnt = NumOfGrp * (CurPage - 1) + 1
						For intinerloop=0 to 4%>
							<td valign='top'><a id="a<%=intloop%><%=intinerloop%>" ><img name="img<%=intloop%><%=intinerloop%>" id="img<%=intloop%><%=intinerloop%>" width=83 height=109 src="../images/<%=Session("Theme")%>/transperant.gif" border=0></a><br>
							<Div id="DIV<%=intloop%><%=intinerloop%>" name="DIV<%=intloop%><%=intinerloop%>">
							</Div>
							</td>
						<%Next%>
						<TD>
						<Table>
							<tr>
								<td colspan=2 align=center><Div><b><Script Language="JAVAScript">document.write(NameGRPDESC[<%=intloop%>])</script></b></Div></td>
							</tr>
							<tr>
								<TD><img name="back<%=intloop%>" id="back<%=intloop%>" onclick="javascript:move_back(<%=intloop%>)" src="../images/<%=Session("Theme")%>/transperant.gif" width=70 height=20></TD>
								<TD><img name="next<%=intloop%>" id="next<%=intloop%>" onclick="javascript:move_next(<%=intloop%>)" src="../images/<%=Session("Theme")%>/transperant.gif" width=70 height=20></TD>
							</tr>
						</table>
						</td>
				 </TR>
			 </Table>
		  <%Next%>
	        
		  <SCRIPT LANGUAGE=javascript>
			<!--
			do_load()
			//-->
		  </SCRIPT>

		<%else 'In the Case of choosing one group%>
			<SCRIPT LANGUAGE=javascript>
			<!--
					var Grpnames = new Array();
					var intCurpage = new Array();
					Grpnames[0] = '<%Response.Write("GRP" & Group)%>'
					intCurpage[0] = 1
			//-->
			</SCRIPT>
			<%if not(rsStySearch.EOF and rsStySearch.BOF) then%>
					<SCRIPT LANGUAGE=javascript>
					<!--
						var <%Response.Write("GRP" & Group)%> = new Array();
						var <%Response.Write("GRP" & Group& "DESC")%> = new Array();
						<%intcounter = 0%>
						<%Do while not rsStySearch.EOF 
							'get the style desc
							rsStyDesc.movefirst()
							rsStyDesc.filter = "cstymajor = '"& Trim(rsStySearch.fields("cstymajor").value) &"'"
							if not rsStyDesc.eof then
								strDesc = rsStyDesc("desc1")
							end if
							rsStyDesc.filter = ""
						%>
							<%Response.Write("GRP" & Group)%>[<%=intcounter%>] = '<%=Trim(rsStySearch.fields("cstymajor").value)%>'
							
							<%Response.Write("GRP" & Group& "DESC")%>[<%=intcounter%>] = '<%=Trim(strDesc)%>'
							<%intcounter = intcounter + 1%>
							<%rsStySearch.MoveNext%>
						<%Loop%>
					//-->
					</SCRIPT>
         <%end if		
		   intloop=0%>
	
	
	
  <table width=800 align=center> 
    <%
		'IF intresult > 0 Then Response.Write("<TD>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<TD>")End IF
		%>
    <TR>
      <%
		For intinerloop=0 to 4%>
        <td > 
        <table>
		<tr>
		<td>
			<a id="a<%=intloop%><%=intinerloop%>" ><img name="img<%=intloop%><%=intinerloop%>" id="img<%=intloop%><%=intinerloop%>" width=83 height=109 src="../images/<%=Session("Theme")%>/transperant.gif" border=0></a><br> 
			<Div id="DIV<%=intloop%><%=intinerloop%>" name="DIV<%=intloop%><%=intinerloop%>"> 
			</Div>
		</td>
		</tr>
		</table>
		</td>
      <%Next%>
    </tr>
    <%
'WMH Make it three rows in case one Group [Start]		
		'IF intresult > 0 Then Response.Write("<TD>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<TD>")End IF
		%>
    <TR>
      <!--td>&nbsp;</td-->
	  <%
		For intinerloop=5 to 9%>
	    <td>
			<table>
			<tr>
			<%Response.Write("<TD>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</TD>")%>
			<td>
				<a id="a<%=intloop%><%=intinerloop%>" ><img name="img<%=intloop%><%=intinerloop%>" id="img<%=intloop%><%=intinerloop%>" width=83 height=109 src="../images/<%=Session("Theme")%>/transperant.gif" border=0></a> 
				<Div id="DIV<%=intloop%><%=intinerloop%>" name="DIV<%=intloop%><%=intinerloop%>"></Div>
			</td>
			</tr>
			</table>
        </td>
      
      <%Next%>
    </tr>
    <%
		'IF intresult > 0 Then Response.Write("<TD>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<TD>")End IF
		%>
    <TR>
      <%
		For intinerloop=10 to 14%>
      <td> 
		<table>
		<tr>
		<td>
			<a id="a<%=intloop%><%=intinerloop%>" ><img name="img<%=intloop%><%=intinerloop%>" id="img<%=intloop%><%=intinerloop%>" width=83 height=109 src="../images/<%=Session("Theme")%>/transperant.gif" border=0></a><br> 
			<Div id="DIV<%=intloop%><%=intinerloop%>" name="DIV<%=intloop%><%=intinerloop%>"> 
			</Div>
		</td>
		</tr>
		</table>
	  </td>
      <%Next%>
    </tr>
    <TR>
      <TD colspan="2" ALIGN=CENTER></TD>
      <td></td>
    </TR>
  </Table>
	
	<Center><img name="back<%=intloop%>" id="back<%=intloop%>" onclick="javascript:move_backSingle(<%=intloop%>)" src="../images/<%=Session("Theme")%>/transperant.gif"  height=20>
	<img name="next<%=intloop%>" id="next<%=intloop%>" onclick="javascript:move_nextSingle(<%=intloop%>)" src="../images/<%=Session("Theme")%>/transperant.gif"  height=20>
	</Center>

<!--'WMH Make it three rows in case one Group [End]			-->

<%
ShowNextButton = false			

%>

<SCRIPT LANGUAGE=javascript>
<!--
do_loadSingle()
//-->
</SCRIPT>
<%		end if
	end if
end if
'ARD - Search result [Start]
%>
<P>&nbsp;</P>
</form>
<Center>
<%
'Response.Write "<font size=5>" &  Session("season") & "<hr>"
'Response.Write "<font size=5>" &  strSQL & "<hr>"
'WMH Outer Paging [Start]
IF Cint(CurPage) > 1 THEN'show prev %>
	<a href="CatSearch.asp?CurPage=<%=CurPage-1%>&Search=<%=Search%>&Group=<%=Group%>&<%=Trim(strName)%>=<%=strStyle%>"><img src="../images/<%=Session("Theme")%>/back.gif" border=0></a>&nbsp;
<%END IF
'Show next Button
IF Search = "Y" THEN 
	'Response.Write(NumOfGrp)
	
	'Get total No. of pages that will be used to dispaly the current recordset groups
	If ubound(strvalidGroup) Mod NumPerPage > 0 Then
		intNoOfGroupPages = Int(ubound(strvalidGroup) / NumPerPage)+1
	Else
		intNoOfGroupPages = Int(ubound(strvalidGroup) / NumPerPage)
	End If
	
	
	'Response.Write(Int(CurPage) < intNoOfGroupPages and ShowNextButton)
	If Int(CurPage) < intNoOfGroupPages+1 and ShowNextButton then
	%>
		<a href="CatSearch.asp?CurPage=<%=CurPage+1%>&Search=<%=Search%>&Group=<%=Group%>&<%=Trim(strName)%>=<%=strStyle%>"><img src="../images/<%=Session("Theme")%>/next.gif" border=0></a>
	<%
	Else
		'Response.Write("No Next")
	End IF
End if
'WMH Outer Paging [End]
%>
</Center>
<p>
</BODY>
</HTML>
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
				strQuery = strQuery & " in ( "
			else
				strQuery = strQuery & " not in ( "
			end if
		end if
		strQuery = strQuery & "'" & ValuesArray(i) & "'"
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