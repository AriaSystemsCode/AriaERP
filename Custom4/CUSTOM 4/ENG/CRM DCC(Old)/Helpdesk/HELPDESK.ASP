<%@ Language=VBScript %>
<% Response.CacheControl = "no-cache" 
 Response.AddHeader "Pragma", "no-cache" 
 Response.Expires = -1 
 Response.Buffer=true
 
IF Trim(Session("ID")) = "" AND Trim(Session("rep"))= "" Then
'	Response.redirect "../default.asp"%>
<script language="javascript">
	parent.location.href ="../login.asp"
</script>
<%END IF

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
End IF
%>

<HTML>
<HEAD>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/help.css">
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<title>CRM - Helpdesk - Open Issues</title>
<Script language=JavaScript>
function submitIt()
{ 
	if (document.Form1.txtDateStart.value=="" )
	{
	}
	else	
	{
		dateObjfrom =new Date(document.Form1.txtDateStart.value);
		if (!dateObjfrom.getDate())
		{
			alert ("Please enter valid date or leave blank!");
			document.Form1.txtDateStart.focus();
			return false;
		}
		
}
if (document.Form1.txtDateEnd.value=="" )
	{
	}
	else	
	{
		dateObjto =new Date(document.Form1.txtDateEnd.value);
		if (!dateObjto.getDate())
		{
			alert ("Please enter valid date or leave blank!");
			document.Form1.txtDateEnd.focus();
			return false;
		}
	}
	document.Form1.submit()
}


</script>
<%
'-------------------------------------------------------------------------------------
'---------------------------------------------------------------------------------------
'---------------------------------------------------------------------------------------
	Function GetIssueType(strType)
		'The  following 3 lines are commented by aak because this connection is never used within the file
		'Dim cnnFoxConnection
		'Set cnnFoxConnection = Server.CreateObject("ADODB.Connection")
		'cnnFoxConnection.Open Application("DataConnectionString")'"dsn=crm;Deleted=Yes"
		Dim rsIssueType
		Set rsIssueType = Server.CreateObject("ADODB.Recordset")
		strCodesSql = "Select cDiscrep From Codes Where cdefcode+crltfield+cfld_name = 'NNCISSUTYPE' and cCode_no='" & Trim(strType) & "'"
		rsIssueType.Open strCodesSql,conn
		'Response.Write "strCodesSql == "& rsIssueType
		if rsIssueType.BOF and rsIssueType.EOF then
			Response.Write "N/A"
		else
		GetIssueType = rsIssueType("cDiscrep")
		end  if
	End Function
	
	
	Function GetIssueStatus(strStatus)
	
		Select Case strStatus
			Case "W" 
				GetIssueStatus = "In Work"
		  Case "T"
				GetIssueStatus = "In Testing"
				
			Case "R"
				GetIssueStatus = "Waiting for Reply"
			
			Case "C"
				GetIssueStatus = "Completed"
			Case "O"
				GetIssueStatus = "Open"
			Case "X"
				GetIssueStatus = "Canceled"

			Case Else
				GetIssueStatus = "N/A"
		End Select
	End Function


'---------------------------------------------------------------------------------------
'---------------------------------------------------------------------------------------
'---------------------------------------------------------------------------------------

	'Provider=MSDATASHAPE;DSN=DS99;UID=;SourceType=DBF;Exclusive=No;
	'BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;
	Dim RSCust
	Dim Conn
	Session("ConnectionString") = Application("DataConnectionString")
	Set Conn = Server.CreateObject("ADODB.Connection")
	Conn.Open(Session("ConnectionString"))

	Dim strCustCode 'As String
	Dim strCustName 'As String
	Dim strStatus 'As String
	Dim strSupStatus 'As String

		strCustCode = Session("RSCust").Fields("account")
		strCustName = Session("RSCust").Fields("btName")
		strLocation = "Main"
		
		Select Case Session("RSCust").Fields("Status")
			Case "A"
				strStatus = "Active"
				
			Case "P"
				strStatus = "Potential"
			Case "H"
				strStatus = "Hold"
			Case "X"
				strStatus = "Canceled"
		End Select

		
		
		Dim strAlTime , strUsedTime , strRemTime
		
	
		
	'Collect All issues that maches the filter criteria
	Dim cnnSql
	Set cnnSql = Server.CreateObject("ADODB.Connection")
	
	Dim strSqlConnString
	
	strSqlConnString = Application("SqlServer")
	
	cnnSql.Open strSqlConnString

 	Dim rsIssues
	Set rsIssues = Server.CreateObject("ADODB.RecordSet")
    
	'set records per page
	Const NumPerPage = 15
	
	'Retrive what page we are Currently on
	Dim CurrPage
	
	IF Request.QueryString("Currpage") = "" Then
		CurrPage = 1
	Else
		CurrPage = Request.QueryString("Currpage")
	End IF
	
	'Set cursor location
	rsIssues.CursorLocation = 2
	
	'set cachsize to no. of records/page
	rsIssues.CacheSize = NumPerPage
'---------------------------------------------------------------------------------------
' build header Query
'---------------------------------------------------------------------------------------
'strSQL = "SELECT DISTINCT SuIsshdr.CIssueNo, SuIsshdr.DIssStart, SuIsshdr.CIssType, SuIsshdr.CIssPrior, SuIsshdr.CissSubjct, SuIsshdr.CissStat, SuIsshdr.nIssPercnt, SuIsshdr.DIssComp, Codes.Cdiscrep "
''''''''''''''NEK
strSQL = "SELECT DISTINCT SuIsshdr.CIssueNo, SuIsshdr.DIssStart, SuIsshdr.CIssType, SuIsshdr.CIssPrior, SuIsshdr.CissSubjct, SuIsshdr.CissStat, SuIsshdr.nIssPercnt, SuIsshdr.DIssComp, '                              ' as Codes_CDiscrep "
''''''''''''''NEK
strconnection = Application("DataConnectionString")
'Response.End 
set rsIsEmpty = Server.CreateObject("ADODB.RecordSet")
strSQLFox ="SELECT * FROM Codes WHERE Cdefcode+crltfield+cfld_name='NNCISSUTYPE'"

set rsSQLFox = server.CreateObject("adodb.recordset")
rsSQLFox.Open strSQLFox , Session("ConnectionString")
rsIsEmpty.Open "Select * FROM SuIssDt", cnnSql
strCodes = ""
		do while not rsSQLFox.eof
			if trim(strCodes) = "" then
				strCodes = 	"'" & rsSQLFox("cCode_no")&"'"
			else
				strCodes = strCodes & ",'" & rsSQLFox("cCode_no")&"'"	
			end if	
		rsSQLFox.movenext
		loop


'Response.write "<font size=3>String Codes ="& strCodes & "<br></font>"		

if rsIsEmpty.BOF and rsIsEmpty.EOF then
		'strSQL = strSQL & "FROM SuIssHdr, OPENROWSET('MSDASQL', '" & strconnection & "', 'SELECT * FROM Codes WHERE Cdefcode=""N"" AND crltfield=""N"" AND Cfld_name=""CISSTYPE""') AS Codes  WHERE "
		''''strSQL = strSQL & "FROM SuIssHdr , OPENROWSET('MSDASQL', '" & strconnection & "', 'SELECT * FROM Codes WHERE Cdefcode+crltfield+cfld_name=""NNCISSTYPE""') AS Codes  WHERE "
		Response.Write("^^^" & strCodes & "^^^")
		If strCodes <> "" Then
			strSQL = strSQL & "FROM SuIssHdr  WHERE CIssType in (" & strCodes & ") and "
		Else
			strSQL = strSQL & "FROM SuIssHdr WHERE "
		End If 	
		reportname = "Summary.rpt"
else
			'Response.Write("<font size=3>^^^" & strCodes & "^^^</font>")
		If strCodes <> "" Then
			strSQL = strSQL & "FROM SuIssHdr,SuIssDt  WHERE  CIssType in (" & strCodes & ") and "
		Else	
			strSQL = strSQL & "FROM SuIssHdr WHERE"
		End If 	
end if 


'Build the wehre conditions according to the input of the user.
if Trim(Request.Form("txtDateStart")) = "" then
else
	strSQL = strSQL & " SuIsshdr.DIssStart >= '" & Request.Form("txtDateStart") &"' AND "
end if

if Trim(Request.Form("txtDateEnd"))="" then
else
	strSQL = strSQL & " SuIsshdr.DIssStart<= '" & Request.Form("txtDateEnd") & "' AND "
end if

select case Request.Form("lstIssType")
	
	case "A"
		strSQL = strSQL
	Case Else
		strSQL = strSQL & " SuIsshdr.CIssType = '" & Trim(Request.Form("lstIssType")) & "' AND"
end select

select case Request.Form("lstIssStatus")
	'IssueStatus
	Case "A" 
		strSQL  = strSQL & " cCust_Id='"& Session("ID") & "' AND (cIssStat='O' OR cIssStat='T' or cIssStat='W' OR cIssStat='R' OR cIssStat='C' OR cIssStat='X') "
	case else
		strSQL = strSQL & " SuIsshdr.CissStat = '" & Request.Form("lstIssStatus") & "' "
end select

select case Request.Form("lstIssPri")
	case "A"
		strSQL = strSQL
	case else 
		strSQL = strSQL & " AND SuIsshdr.CIssPrior = '" & Request.Form("lstIssPri") &  "' "
end select

select case Request.Form("lstAnswered")
	case "N" 
		if rsIsEmpty.BOF and rsIsEmpty.EOF then 
			strSQL = strSQL & " AND (Not exists (Select * From SuIssDt AS Det2 WHERE Det2.CIssueNo = SuIsshdr.CIssueNo))"
		else
			strSQL = strSQL & " AND "
			strSQL = strSQL & " ((SuIssDt.cRespType = 'N' AND SuIssDt.CIssueNo = SuIsshdr.CIssueNo AND SuIssDt.intLineNo = (Select MAX(Det.intLineNo) From SuIsshdr AS hdr, SuIssDt As Det Where Det.CIssueNo = SuIsshdr.CIssueNo ))"
			strSQL = strSQL & " OR (Not exists (Select * From SuIssDt AS Det2 WHERE Det2.CIssueNo = SuIsshdr.CIssueNo)))"
		end if 
	case "Y" 
		if rsIsEmpty.BOF and rsIsEmpty.EOF then
			strSQL = strSQL & " AND (Exists (SELECT * FROM SuIssDt)) " 
		else 
			strSQL = strSQL & " AND SuIssDt.cRespType = 'Y' "
			strSQL = strSQL & " AND SuIssDt.CIssueNo = SuIsshdr.CIssueNo "
			strSQL = strSQL & " AND SuIssDt.intLineNo = (Select MAX(Det.intLineNo) From SuIsshdr AS hdr, SuIssDt As Det Where Det.CIssueNo = SuIsshdr.CIssueNo )"
		end if
	case "B"
		strSQL = strSQL 
end select
'Get Current Customer ID
strSQL = strSQL & " AND CCust_ID= '" & Session("ID") & "'"

'StrSQL = strSQL & " AND Codes.Ccode_no = SuIssHdr.CIssType"
'Build query for sort

select case Request.Form("lstSortBy")
	case "SC"
		strSQL = strSQL & " Order By SuIsshdr.CissStat "
	case "PC"
		strSQL = strSQL & " Order By SuIsshdr.CIssPrior "
	case "T"
		strSQL = strSQL & " Order By SuIsshdr.CIssType "
	case "SS"
		strSQL = strSQL & " Order By SuIsshdr.DIssStart "
	case "I"
		strSQL = strSQL & " Order By SuIsshdr.CIssueno "
end select
'response.write("<font size=3>" & strSQL & "</font><br>")
'response.end
'on error resume next
	rsIssues.Open strSQL , cnnSql 

	if err > o then
		response.write(err.description)
		response.end
	end if
	
IF rsIssues.EOF And rsIssues.BOF Then
Else
	rsIssues.PageSize = NumPerPage
	TotalPages = rsIssues.PageCount 
	rsIssues.AbsolutePage = CurrPage
End IF
	
%>


<SCRIPT LANGUAGE=javascript>
<!--
//function submitIt()
//{
//	document.Form1.submit()
	
//}
function PrintReport(strType)
{

	document.Form1.lstReportFormat.Value = strType
	var flg = navigator.appVersion  ;
	
	if(flg.indexOf("MSIE") != -1)
		document.Form1.action = "OpenIssueReport.asp?strFormat=" + strType + "&viewer=ActiveX";
	else
		document.Form1.action = "OpenIssueReport.asp?strFormat=" + strType + "&viewer=Java-Plug-in";	
	
	
	
	document.Form1.submit()
	//return true
}

//-->
</SCRIPT>
<SCRIPT LANGUAGE=javascript>
<!--
function lfShowMenu()
{
//alert(document.getElementById('MainMenu').style.top)
document.getElementById('fMenu').style.display = 'block'
}

function lfHideMenu()
{
document.getElementById('fMenu').style.display = 'none'
}

function hilit(src)
{
	strObj = src.name
	document.getElementById(strObj).style.backgroundColor = "#cd853f"
	document.getElementById(strObj).style.cursor = 'hand'
	
}

function dehilit(src)
{
	strObj = src.name
	document.getElementById(strObj).style.backgroundColor = "#c0c0c0"
	
	
}

//-->
</SCRIPT>
<link REL="stylesheet" HREF="crmmain.css" TYPE="text/css">
<body>
<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><BR><BR><BR></p>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><BR></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
<!--	
	<TD colspan=14 background="../images/bground.gif">

	<font size="2" face="Arial">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
		<%set connTemp=server.createobject("ADODB.Connection")
			set RSTemp=server.createobject("ADODB.Recordset")
			connTemp.open Application("DataConnectionString")
			sqlTemp="select * from customer where account='" & session("customerid") & "'"
			RSTemp.open sqlTemp,connTemp,1,3
			%>
              Your currently selected customer is <%=Session("customerid")%>
             <%response.write " - "&RSTemp("btname")%></font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="repcust.asp" ><font color=white size="2" face="Arial"><b>Get Customer</b></font></a>
	-->
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected <%=Session("custfield")%> is <b><%=Session("customerid")%> - <%=RSTemp.fields("btname").value%></b></P>
	</TD>
	<TD align=right><a href="repcust.asp"><b>Get <%=Session("custfield")%></b></a></TD>
	<!-- ARD -->
</TR>
</table>
 <%Set RSTemp=nothing
  ConnTemp.close
  Set ConnTemp=nothing
  End IF%>

<Table width=95% height=50 align=center border=1>
<TR>
<TD class=title>Helpdesk</TD>
</TR>
</Table>

<%
'wal_130674 correct check on privilage case multi user
	if Application("CustomerLoginUsing")= "User"  then
		strAppUserVar = Application("userID")
	ElseIf Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"HLPDESK") <= 0 Then
%>
<Table border=0 width=95% align=center>
	<tr>
	<td class="Title"><font color="Yellow">
		You're not authorized to view this page</font>
	</td>
	</tr>
</Table>
<%
	Response.End 
End If
%>

<SCRIPT LANGUAGE=javascript>
<!--
function nextprev(CurrPage)
{
	document.Form1.action = 'helpdesk.asp?CurrPage='+ CurrPage + '&Criteria=O';
	document.Form1.submit()
}
//-->
</SCRIPT>
<%
if StrCodes =  "" then%>
<table cellspacing=0 cellpadding=0 width=95% align=center>
	<tr>
		<Td>&nbsp;</td>
	</tr>
	<tr>
		<td class=MessageFont >No Issue Types are avaialable in the System.</td>
	</tr>
<table>	
	<%Response.End 
end if 	
%>
<FORM Name="Form1" id= "Form1" method="post" Action="Helpdesk.asp?Criteria=O">

<div align="center">
  <center>
	<Table width=95% align=center border=0 align=center>
		<TR>
			<TD><STRONG>For questions or comments, please enter the following information:</STRONG></TD>
		</TR>
	</table>

<table width=95% border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<TR>
		
		<TD Width=33% class="dark_cell">
			<P><%=session("CustField")%> ID</P>
		</TD>
		<TD Width=33% class="dark_cell">
			<P><%=session("CustField")%> Name</P>
		</TD>
		<TD Width=34% class="dark_cell">
			<P >Status</P>
		</TD>
	</TR>
	<TR>
		<TD class="light_cell" >
			<P >&nbsp;<INPUT id=txtCustID size=10 name=txtCustID disabled Value="<%=strCustCode%>"></P>
		</TD>
		<TD class="light_cell">
			<P >
			&nbsp;<INPUT id=txtCustName name=txtCustName disabled Value="<%=strCustName%>" size="20">
			</P>
		</TD>
		<TD class="light_cell">
			<P >&nbsp;<INPUT id=txtStatus style="WIDTH: 63px; HEIGHT: 22px" size=9 name=txtStatus disabled Value=<%Response.Write(strStatus)%>></P>
		</TD>
	</TR>
</TABLE>
  </center>
</div>
<%'------------------------------------------------------------------------
'				End of first table ...
'------------------------------------------------------------------------%>
<div align="center">
  <center>

<table width=95% border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<tr><%If Not bolLock Then%>
		<td class="dark_cell" colspan=4>
			Open Issue List
		</TD>
								
	</TR>
	<TR>
		<TD class="dark_cell">Show issues of type</TD>
		<TD class="dark_cell">Show Issues of status</TD>
		<TD class="dark_cell">Priority</TD>
		<td class="dark_cell">Detail Status</td>
	</TR>
	<TR>
		<TD width="20%" class="light_cell">
					&nbsp;<SELECT style="WIDTH: 145px" id=lstIssType name=lstIssType>
          
						<%
									Dim rsIssueType
									Set rsIssueType = Server.CreateObject("ADODB.Recordset")
									Dim strCodesSQL
									Dim strDefCodeSQL
									strDefCodeSQL = "Select cCode_no, cDiscrep From Codes Where cdefcode+crltfield+cfld_name = 'DNCISSUTYPE'"
									If Request("Criteria") = "X" Then
										rsIssueType.Open strDefCodeSQL,Conn
										Dim strDefCode
										If Not rsIssueType.EOF Then
											strDefCode = Trim(rsIssueType("cCode_no"))
										End IF
										rsIssueType.Close()
									Else
										strDefCode = ""
									End IF
									strCodesSql = "Select cCode_no, cDiscrep From Codes Where cdefcode+crltfield+cfld_name = 'NNCISSUTYPE'"
									rsIssueType.Open strCodesSql,Conn
									'Response.Write("<Option>" & rsIssueType.recordcount)
								%>
          
								<OPTION value='A'>All
								<% Do While Not rsIssueType.EOF %>
													
											<OPTION 
												<%If Request("lstIssType") = rsIssueType("cCode_no") Then 
														Response.Write("Selected")
													End If 
												%>
														value="<%Response.Write(trim(rsIssueType("cCode_no")))%>">
												<%Response.Write(Trim(rsIssueType("cDiscrep")))
																		
																	
												%>
																	
								<%
											rsIssueType.MoveNext()
									Loop
								%>
					</SELECT>
		</TD>
		<TD width="20%" class="light_cell">
			<input type='hidden' Name='lstReportFormat' ID='lstReportFormat'>
							&nbsp;<SELECT style="WIDTH: 162px" id=lstIssStatus name=lstIssStatus>
								<option <%If Request("lstIssStatus")="A" Then Response.Write("Selected")%> value="A">All
								<OPTION <%If Request("lstIssStatus")="O" Then Response.Write("Selected")%> value='O'>Open
								<OPTION <%If Request("lstIssStatus")="C" Then Response.Write("Selected")%> value='C'>Completed
							</SELECT>
		</TD>
		<TD class="light_cell">
			&nbsp;<SELECT  id=lstIssPri name=lstIssPri>
				<OPTION Selected value="A">ALL
				<OPTION <%If Request("lstIssPri")="V" Then Response.Write("Selected")%> value="V">Very Urgent
				<OPTION <%If Request("lstIssPri")="U" Then Response.Write("Selected")%> value="U">Urgent
				<OPTION <%If Request("lstIssPri")="N" Then Response.Write("Selected")%> value="N">Normal
				<OPTION <%If Request("lstIssPri")="L" Then Response.Write("Selected")%> value="L">Low
			</SELECT>
		</TD>
		<td class="light_cell">
			&nbsp;<SELECT  id=lstAnswered name=lstAnswered>
				<OPTION <%If Request("lstAnswered")="B" Then Response.Write("Selected")%> value="B">Both
				<OPTION <%If Request("lstAnswered")="Y" Then Response.Write("Selected")%> value="Y">Answered
				<OPTION <%If Request("lstAnswered")="N" Then Response.Write("Selected")%> value="N">Un-Answered
				
				
			</SELECT>
		</td>
		
	</tr>
	<tr>
		<td Colspan=3 class="dark_cell">Submitted</td>
		<TD class="dark_cell">Sort By</TD>
	</tr>
	<tr>
		<td class="light_cell">
			&nbsp;<INPUT type="text" id=txtDateStart name=txtDateStart size=10 maxlength=10  Value=<% IF Request("Criteria")="O" Then Response.Write(Request("txtDateStart"))%>>
			 &nbsp;&nbsp; To
		</td>
		<td class="light_cell">
			&nbsp;<INPUT type="text" id=txtDateEnd name=txtDateEnd size=10 maxlength=10  Value=<% IF Request("Criteria")="O" Then Response.Write(Request("txtDateEnd"))%>>
		</td>
		<td class="light_cell">
		</td>
		<td class="light_cell">
        	&nbsp;<SELECT  id=lstSortBy name=lstSortBy>
						<OPTION <%If Request("lstSortBy")="SC" Then Response.Write("Selected")%> value="SC">Status
						<OPTION <%If Request("lstSortBy")="PC" Then Response.Write("Selected")%> value="PC">Priority
						<OPTION <%If Request("lstSortBy")="T" Then Response.Write("Selected")%> value="T">Type
						<OPTION <%If Request("lstSortBy")="SS" Then Response.Write("Selected")%> value="SS">Submit Date
						<OPTION <%If Request("lstSortBy")="I" Then Response.Write("Selected")%> value="I">Issue No.
					</SELECT>
		</td>
	</tr>
	<tr>
		<TD colspan=3 valign='top' class="dark_cell">
													
												
		</TD>
		<TD  colspan=1 align=right class="dark_cell">
		  <input type="Submit" name="cmdSetFilter" onClick="return submitIt()" Value="Set Filter">
		</TD>
	</TR>
</TABLE>

  </center>
</div>

<%'----------------------------------------------------------------
'					end of second table ..
'----------------------------------------------------------------%>
<div align="center">
  <center>
								
<table width="95%" border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
								<TR>
									<TD class="dark_cell" ><strong>&nbsp;Seq. No</strong></TD>
									<TD class="dark_cell" ><strong>&nbsp;Sub. date</strong></TD>
									<TD class="dark_cell" ><strong>&nbsp;Type</strong></TD>
									<TD class="dark_cell" ><strong>&nbsp;Subject</strong></TD>
									<TD class="dark_cell" ><strong>&nbsp;Status</strong></TD>
									<TD class="dark_cell" Align=right><strong>%&nbsp;</strong></TD>
								</TR>
        
				<%
'---------------------------------------------------------------------------------------
'----------------------------		Display Search Result			--------------------
'---------------------------------------------------------------------------------------
								Dim bolEOF
								bolEOF = False
								'Response.write "<font size=3>"&rsIssues.RecordCount&"</font><br>"
								'Response.write "<font size=3>"&strSQL&"</font>"
								If rsIssues.EOF And rsIssues.BOF Then
								%>
								<tr>
									<td Colspan=6 align='center' class="light_cell">
										<font size="2" color="#000080" face="Arial">No Issues match the filter</font>
										<%
											bolEOF = True
										%>
									</td>
								</tr>
								<%
								Else
										Dim strLastAction
										strLastAction = ""
										Dim strComp
						
										Dim Count
										Count = 0
									
										Do While Not rsIssues.EOF And Count < rsIssues.PageSize 
											'if oldIssueNo <> trim(rsIssues("cIssueNo")) then
											'	oldIssueNo = trim(rsIssues("cIssueNo"))
	
												Call PrintIssueHd()
												Count = Count + 1
											'end if	
										rsIssues.MoveNext()
										Loop
							End If
								

				%>
        
        <TR>

          <TD colspan=2 class="dark_cell"><I>
          <%If bolEOF Then
          else%>
						Click the sequence # for more details
          <%End If%>
          
          </i></TD>
          <TD align=right colspan=4 class=dark_Cell><a href=Issue.asp class="link">Add New Issue</a>
          </TD>
        </TR>



<%Else%>
                                <td>
	<P>
		Dear customer, we are sorry your Support Status is <b>Hold.</b> Please contact our Customer Support.
	</p>

<%End If%>

<%
Sub PrintIssueHd()%>
		<TR>
		
		<TD class=light_cell><a href="NewEditIssue.asp?IssNo=<%Response.Write(rsIssues("cIssueNo"))%>">&nbsp;<%Response.Write(rsIssues("cIssueNo"))%></a>&nbsp;</TD>
		<TD class=light_cell>&nbsp;<%Response.Write(rsIssues("DIssStart"))%>&nbsp;</TD>
		<TD class=light_cell>&nbsp;<%Response.Write(GetIssueType(rsIssues("CIssType")))%>&nbsp;</TD>
		<TD class=light_cell>&nbsp;<%Response.Write(rsIssues("CissSubjct"))%>&nbsp;</TD>
		<TD class=light_cell>&nbsp;<%Response.Write(GetIssueStatus(rsIssues("CissStat")))%>&nbsp;</TD>
		<TD class=light_cell align=right>&nbsp;<%Response.Write(rsIssues("nIssPercnt"))%>&nbsp;</TD>
		<%
		If rsIssues("DIssComp") = "12:00:00 AM" Then
			strComp = "N/A"
		Else
			strComp = rsIssues("DIssComp")
		End If
%>
		</TR>
<%End Sub
%>		
</table>
  </center>
</div>
<%

Response.Write("<Center><BR>")

IF Not bolEOF Then
Response.Write("<Font size=2>Page " & CurrPage & " of " & TotalPages & "</font><BR><BR>")
	IF CurrPage > 1 Then%>
		<a href="javascript:nextprev('<%=CurrPage - 1%>')">
<img src="../images/<%=Session("Theme")%>/back.gif"></a>
	<%End IF
	Response.Write("  ")
	IF  rsIssues.EOF And rsIssues.BOF Then
	Else
		IF Cint(CurrPage) <> Cint(TotalPages)   And Cint(TotalPages) > 0Then%>
			<a href="javascript:nextprev('<%=CurrPage + 1%>')">
<img src="../images/<%=Session("Theme")%>/next.gif"></a>
		<%End IF
	End IF
End IF
Response.Write("</Center>")
%>

</FORM>
<BR>
</BODY>