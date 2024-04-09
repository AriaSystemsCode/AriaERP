<%Response.Buffer = true%>
<html>
<%
Dim strAppPath
	Dim strFilePath

	strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))

	If Right(strAppPath,1) = "\" Then
		strFilePath = "admin\crmsetup\setup\setup.txt"
	Else
		strFilePath = "\admin\crmsetup\setup\setup.txt"
	End If

	'Response.Write(strAppPath & strFilePath)
Dim objFile
	Set objFile = Server.CreateObject("Scripting.FileSystemObject")
	on error resume next
	Set objTxtFile = objFile.OpenTextFile(strAppPath & strFilePath,1)
	
	If err.number<>0 then
			response.write "You must setup CRM first."
			response.write "<br><a href=""../CrmSetup\crmSetup.asp"">Setup CRM</a>"
			Response.End
	else
		strFile = objTxtFile.ReadAll
		Dim strArSetups
			strArSetups = Split(strFile," AND ", -1 , 1)

			'Declare Vartiables To Hold the temporary key and values
			Dim strKey
			Dim strValue
			For intLoop = 0 To UBound(strArSetups)
				strArKeyValue = Split(strArSetups(intLoop) , "=" , -1 , 1)
				Application(strArKeyValue(0)) = strArKeyValue(1)
				Session(strArKeyValue(0)) = strArKeyValue(1)
			Next
			Application("DataConnectionString") = "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("DataPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"
			Application("SystemConnectionString") = "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("SystemPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"
		objTxtFile.Close
		Set objTxtFile = Nothing
	End If
	On Error GoTo 0
%>
<head>
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
<meta http-equiv="Content-Type" content="text/html; charset=windows-1252">
<meta name="GENERATOR" content="Microsoft FrontPage 4.0">
<meta name="ProgId" content="FrontPage.Editor.Document">
<title>CRM - Administrator - Open Issues</title>
<script LANGUAGE="javascript">
<!--
function DispMod(objMe,strvalue,objform)
{
	intListLen = document.Form1.lstAppl.options.length;
	
	for (intLoop = 0 ; intLoop < intListLen; intLoop++)
	{
		strobj = document.Form1.lstAppl.options(intLoop).value;
		obj = eval('objform.' + strobj);
		obj.style.display='none' ;
	}
	objToDisp = eval('objform.' + strvalue);
	objToDisp.style.display='block';
}


function ActionPrintRep()
{
	var flg = navigator.appVersion  ;
	
	if(flg.indexOf("MSIE") != -1)
		document.Form1.action = "Report/OpenIssueARIAReport.asp?viewer=ActiveX";
	else
		document.Form1.action = "Report/OpenIssueARIAReport.asp?viewer=Java-Plug-in";	
}

//to choos the report
function lfShowMenu()
{
document.getElementById('fMenu').style.display = 'block'
}

function lfHideMenu()
{
document.getElementById('fMenu').style.display = 'none'
}

function lfsubform()
{
	document.Form1.lstReportFormat.value = 'S';
	var flg = navigator.appVersion  ;
	
	if(flg.indexOf("MSIE") != -1)
		document.Form1.action = "Report/OpenIssueARIAReport.asp?viewer=ActiveX";
	else
		document.Form1.action = "Report/OpenIssueARIAReport.asp?viewer=Java-Plug-in";	
	document.Form1.submit();	
}

function lflngform()
{
	document.Form1.lstReportFormat.value = 'D';
	var flg = navigator.appVersion  ;
	
	if(flg.indexOf("MSIE") != -1)
		document.Form1.action = "Report/OpenIssueARIAReport.asp?viewer=ActiveX";
	else
		document.Form1.action = "Report/OpenIssueARIAReport.asp?viewer=Java-Plug-in";	
	document.Form1.submit();
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

//form check
function checkempty(FieldToCheck)
{
	var checkOK = " ";
	var allvalid = true;
	for (i = 0;  i < FieldToCheck.length;  i++)
	{
		ch = FieldToCheck.charAt(i);
		if (ch != " ")
		{
		 	allvalid = false;
			break;
		}
	}
	return allvalid;
}

function checkForm()
{
	var t;
	if ( checkempty(document.Form1.txtStartDate.value) == false)
	{
		t = Date.parse(document.Form1.txtStartDate.value)
		if (!t) 
		{
			alert("please enter valid start date or let it empty");
			return false;
		}	
	}
	
	if ( checkempty(document.Form1.txtEndDate.value) == false)
	{
		t = Date.parse(document.Form1.txtEndDate.value)
		if (!t) 
		{
			alert("please enter valid start date or let it empty");
			return false;
		}	
	}
	
	return true;	
}

function SubmitNavigation(formaction)
{
	document.Form1.action = formaction;
	document.Form1.submit();
}
//-->
</script>
</head>


<%
'93014
Session("AdmID") = Request.ServerVariables("LOGON_USER")
'Connection to db
set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open Application("DataConnectionString")'"dsn=crm;Deleted=Yes"

%>

<BODY bgcolor="#AECAE6" topmargin="0" leftmargin="0" background="../images/tile1.gif">
<TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
  <TR> 
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=8 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=39 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=33 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=60 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=10 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=47 HEIGHT=1></TD>
    <TD width="100%"> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=6 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=125 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=10 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=13 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=361 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=30 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=8 HEIGHT=1></TD>
  </TR>
  <TR> 
    <TD COLSPAN=6> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_01.jpg" WIDTH=197 HEIGHT=8></TD>
    <TD background="../../images/<%=Session("Theme")%>/Login1_02.jpg"> <IMG SRC="images/<%=Session("Theme")%>/Login1_02.jpg" WIDTH=6 HEIGHT=8></TD>
    <TD COLSPAN=6> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_03.jpg" WIDTH=547 HEIGHT=8></TD>
  </TR>
  <TR> 
    <TD COLSPAN=3 ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_04.jpg" WIDTH=80 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_05.jpg" WIDTH=60 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_06.jpg" WIDTH=10 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_07.jpg" WIDTH=47 HEIGHT=59></TD>
    <TD ROWSPAN=2 background="../../images/<%=Session("Theme")%>/Login1_08.jpg"> <IMG SRC="images/<%=Session("Theme")%>/Login1_08.jpg" WIDTH=6 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_09.jpg" WIDTH=125 HEIGHT=59></TD>
    <TD COLSPAN=5> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_10.jpg" WIDTH=422 HEIGHT=30></TD>
  </TR>
  <TR> 
    <TD COLSPAN=5> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_11.jpg" WIDTH=422 HEIGHT=29></TD>
  </TR>
  <TR> 
    <TD COLSPAN=6> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_12.jpg" WIDTH=197 HEIGHT=8></TD>
    <TD background="../../images/<%=Session("Theme")%>/Login1_13.jpg"> <IMG SRC="images/<%=Session("Theme")%>/Login1_13.jpg" WIDTH=6 HEIGHT=8></TD>
    <TD COLSPAN=6> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_14.jpg" WIDTH=547 HEIGHT=8></TD>
  </TR>
</TABLE>
<BR>
<table bordercolor="#111111" border="0" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
    <TR> 
     
      <th align=right><a href="../Default.asp"><font size=2 face=arial>Back to 
        Admin Page</font></a></th>
    </TR>
  
</Table>

<%
strAppUserVar = session("strAppUserVar")
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"AHELPDESK") <= 0 Then
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

<FORM action="default.asp" method=POST id="Form1" name="Form1" >
<table bordercolor="#111111" border="1" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
              <tr>
                <td colspan="6"><STRONG>Open Issue(s) List</font></STRONG></td>
              </tr>
              <tr>
                <td><B>Issue type</font></B></td>
                <td><B>Issue #</font></B></td>
                <td><B>Issue status</font></B></td>
                <td><B>Priority</font></B></td>
                <td><B>Customer ID</font></B></td>
                <td><B>Detail Status</font></B></td>
              </tr>
              <tr>
				<td>
					<select id="lstIssType" name="lstIssType" size="1" >
					    <option value="A">All
						<%set ConnFox = Server.CreateObject("ADODB.Connection")
						ConnFox.Open Application("DataConnectionString")'"dsn=crm;Deleted=Yes"
						set rsIssueTypes = Server.CreateObject("ADODB.RecordSet")
						strSQL = "SELECT Cdiscrep, Ccode_no,Cdefcode FROM Codes WHERE Cfld_name='CISSUTYPE' AND Crltfield='N' AND Cdefcode='N' "
						rsIssueTypes.Open strSQL, ConnFox
							
						do while not rsIssueTypes.EOF
						%>
							<option <%if rsIssueTypes("Ccode_no")=Request("lstIssType") then Response.Write("Selected") end if%> value="<%=rsIssueTypes("Ccode_no")%>"><%=rsIssueTypes("Cdiscrep")%>
							 
						<%rsIssueTypes.MoveNext()
						Loop%>
                    </select>
                 </td>
                <td><input type="text" name="txtIssNo" size="10" value="<%=Request.Form ("txtIssNo")%>"></td>
                <td><select id="lstIssStatus" name="lstIssStatus" size="1" tabindex="1">
                      <option <%If Request("lstIssStatus")="A" Then Response.Write("Selected")%> value="A">All</option>
                      <option <%If Request("lstIssStatus")="O" Then Response.Write("Selected")%> value="O">Open
                      <!--<option <%'If Request("lstIssStatus")="W" Then Response.Write("Selected")%> value="W">In Work
                      <option <%'If Request("lstIssStatus")="T" Then Response.Write("Selected")%> value="T">In Testing
                      <option <%'If Request("lstIssStatus")="R" Then Response.Write("Selected")%> value="R">Waiting for Reply-->
                      <option <%If Request("lstIssStatus")="C" Then Response.Write("Selected")%> value="C">Completed</option>
                      <!--<option <%'If Request("lstIssStatus")="X" Then Response.Write("Selected")%> value="X">Cancelled</option>-->
                    </select></td>
                <td><select id="lstIssPri" name="lstIssPri">
                      <option <%If Request("lstIssPri")="A" Then Response.Write("Selected")%> value="A">All
                      <option <%If Request("lstIssPri")="V" Then Response.Write("Selected")%> value="V">Very Urgent
                      <option <%If Request("lstIssPri")="U" Then Response.Write("Selected")%> value="U">Urgent
                      <option <%If Request("lstIssPri")="N" Then Response.Write("Selected")%> value="N">Normal
                      <option <%If Request("lstIssPri")="L" Then Response.Write("Selected")%> value="L">Low</option>
                    </select></td>
                <td><input type="text" name="txtCustomer" size="10" value="<%=Request.Form ("txtCustomer")%>" maxlength="5"></td>
                <td>
					<SELECT id=lstAnswered name=lstAnswered>
						<OPTION value="B">Both
						<OPTION <%If Request("lstAnswered")="Y" Then Response.Write("Selected")%> value="Y">Answered
						<OPTION <%If Request("lstAnswered")="N" Then Response.Write("Selected")%> value="N">Unanswered
					</SELECT>
				</td>
              </tr>
              <tr>
                <td><B>Submission Date</font></B></td>
                <td></td>
                <td><B>Sort By</font></B></td>
                
              </tr>
              <tr>
                
                <td><input type="text" name="txtStartDate" size="15" value="<%=Request.Form ("txtStartDate")%>"><B>&nbsp;To</font></B></td>
                <td width="10"><input type="text" name="txtEndDate" size="15" value="<%=Request.Form ("txtEndDate")%>"></td>
                <td width="10"><SELECT id=lstSortBy name=lstSortBy>
						<OPTION <%If Request("lstSortBy")="SC" Then Response.Write("Selected")%> value="SC">Status
						<OPTION <%If Request("lstSortBy")="PC" Then Response.Write("Selected")%> value="PC">Priority
						<OPTION <%If Request("lstSortBy")="T" Then Response.Write("Selected")%> value="T">Type
						<OPTION <%If Request("lstSortBy")="SS" Then Response.Write("Selected")%> value="SS">Submit Date
						<OPTION <%If Request("lstSortBy")="I" Then Response.Write("Selected")%> value="I">Issue No.
					</SELECT></td>
				<td></td>
				<td valign="top">
					<INPUT type = "Hidden" Name = "lstReportFormat" ID = "lstReportFormat">
					<SPAN Name="MainMenu" ID='MainMenu' onmouseover="return lfShowMenu()" onmouseout="return lfHideMenu()" style='cursor:hand'>
						<U><B>Print Report</font></B></U>
						<br>
						<SPAN style="BACKGROUND-COLOR: #c0c0c0; BORDER-BOTTOM: #808080 thin groove;
								 BORDER-LEFT: #808080 thin groove; BORDER-RIGHT: #808080 thin groove;
								 BORDER-TOP: #808080 thin groove; COLOR: #4b0082;
								 DISPLAY: none;POSITION:absolute; WIDTH:150px" Name="fMenu" ID="fMenu">

								<p ID='SBAR' name="SBAR" onClick="return lfsubform()" style="MARGIN: 0px" align=center
								 onMouseOver="return hilit(this)"
								 onMouseOut="return dehilit(this)" >Summary report </p>
								 
								<p ID='DBAR' name="DBAR" onClick="return lflngform()" style="MARGIN: 0px" align=center
								 onMouseOver="return hilit(this)"
								 onMouseOut="return dehilit(this)" >Detailed report</p>
						</SPAN>
					</SPAN>
				</td>
				<td valign="top" align="left" ><input type="submit" value="Set Filter" name="B1" onclick="return checkForm()"></td>
              </tr>
</table>
</FORM>
<%
'Get the Issues that match the search criteria 
'Set Conn = Server.CreateObject("ADODB.Connection")
'HDM use the application variable that holds the connection info
'ConnString = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source=" & Trim(Session("SQLServer")) & ";UID="& Trim(Session("SqlUserName"))& ";PWD="&Trim(Session("SqlPassWord"))
'Conn.Open ConnString
'Conn.Open Application("SqlServer")
'Response.Write "<font size=3>" & Application("SqlServer")
'Response.End 
if trim(Application("SqlServer")) = "" then
	Response.Write "Invalid Database connection!Please close your explorer and open it again"
	Response.End 
end if
set conn=server.CreateObject("ADODB.connection")
'Conn.Open ConnString'Application("SqlServer")
If Session("DBType") = "ORACLE" then
	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=OraOLEDB.Oracle;Data Source="&Trim(session("SQLServer"))&";user id=" & Trim(Session("SqlUserName")) & ";password=" & Trim(Session("SqlPassWord")) & ""	
Else
	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog="&Trim(session("DBName"))&";Data Source="&Trim(session("SQLServer"))&";uid=" & Trim(Session("SqlUserName")) & ";pwd=" & Trim(Session("SqlPassWord")) & ""
End If
Conn.Open Application("SqlServer")
if len(err.Description) > 0 then
	Response.Write err.Description 
	Response.Write ("<br><a href=javascript:window.history.back();>Back</a>")
	Response.End
end if
on error goto 0
Set rsIssues = Server.CreateObject("ADODB.RecordSet")
set rsIsEmpty = Server.CreateObject("ADODB.RecordSet")
rsIsEmpty.Open "Select * FROM SuIssDt", Conn
'Build the Query
IF Request.Form("lstAnswered")<>"B" then
	if rsIsEmpty.BOF and rsIsEmpty.EOF then
		strSQL = "SELECT DISTINCT SuIssHdr.* FROM SuIssHdr WHERE 1<>2 "
	else
		strSQL = "SELECT DISTINCT SuIssHdr.* FROM SuIssHdr,SuIssDt WHERE 1<>2 "
	end if 
else 
	strSQL = "SELECT DISTINCT SuIssHdr.* FROM SuIssHdr WHERE 1<>2 "
end if

'Issue Type
if Trim(Request.Form("lstIssType")) <> "A" then 
	strSQL = strSQL & " AND CIssType ='" & Trim(Request.Form("lstIssType")) & "'"
else 
	strSQL = strSQL
end if

'Issue No
if Trim(Request.Form("txtIssNo"))<>"" then
	strSQL = strSQL & " AND SuIssHdr.CIssueNo='" & Request.Form("txtIssNo") & "'"
else 
	strSQL = strSQL 
end if

'IssueStatus
if Trim(Request.form("lstIssStatus"))<>"A" then
	strSQL = strSQL & " AND CissStat='" & Trim(Request.form("lstIssStatus")) & "'"
else 
	strSQL = strSQL
end if

'IssuePriority
if Trim(Request.form("lstIssPri")) <> "A" then
	strSQL = strSQL & " AND CIssPrior='" & Trim(Request.form("lstIssPri")) & "'"
else 
	strSQL = strSQL
end if
'Custoemr ID
if Trim(Request.form("txtCustomer"))<>"" then
	strSQL = strSQL & " AND CCust_Id='" & Trim(Request.form("txtCustomer")) & "'"
else 
	strSQL = strSQL 
end if


'Start Date
if Trim(Request.Form("txtStartDate"))<>"" then
	strSQL = strSQL & " AND DIssStart>='" & Trim(Request.Form("txtStartDate")) & "'"
else 
	strSQL = strSQL 
end if

'End date
if Trim(Request.Form("txtEndDate"))<>"" then
	strSQL = strSQL & " AND DIssStart<='" & Trim(Request.Form("txtEndDate")) & "'"
else
	strSQL = strSQL 
end if

'Tracking Refrence
if Trim(Request.Form("txtTrackRef"))<>"" then
	strSQL = strSQL & " AND CTrackRef='" & Trim(Request.Form("txtTrackRef")) & "'"
else 
	strSQL = strSQL
end if

select case Request.Form("lstAnswered")
	case "N" 
		if rsIsEmpty.BOF and rsIsEmpty.EOF then 
			strSQL = strSQL & " AND (Not exists (Select * From SuIssDt AS Det2 WHERE Det2.CIssueNo = SuIsshdr.CIssueNo))"
		else
			strSQL = strSQL & " AND "
			strSQL = strSQL & " ((SuIssDt.cRespType = 'N' AND SuIssDt.CIssueNo = SuIsshdr.CIssueNo AND SuIssDt.intLineNo = (Select MAX(Det.intLineNo) From SuIssDt As Det Where Det.CIssueNo = SuIssHdr.CIssueNo ))"
			strSQL = strSQL & " OR (Not exists (Select * From SuIssDt AS Det2 WHERE Det2.CIssueNo = SuIsshdr.CIssueNo)))"
		end if
	case "Y"
		if rsIsEmpty.BOF and rsIsEmpty.EOF then
			strSQL = strSQL & " AND (Exists (SELECT * FROM SuIssDt)) " 
		else 
			strSQL = strSQL & " AND SuIssDt.cRespType = 'Y' "
			strSQL = strSQL & " AND SuIssDt.CIssueNo = SuIsshdr.CIssueNo "
			strSQL = strSQL & " AND SuIssDt.intLineNo = (Select MAX(Det.intLineNo) From SuIssDt As Det Where Det.CIssueNo = SuIssHdr.CIssueNo )"
		end if
	case "B"
		strSQL = strSQL 
end select

'Sort by
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
		strSQL = strSQL & " Order By SuIsshdr.Cissueno "	
end select
set rsIsEmpty = nothing
'Response.Write strSQL
%>

<table bordercolor="#111111" border="1" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
              <tbody>
                <tr>
                  <td  >
                    <p align="left"><B>Seq. No</font></B></p>
                  </td>
                  <td >
                    <p align="left"><B>Submission Date</font></B></p>
                  </td>
                  <td >
                    <p align="left"><B>Type</font></B></p>
                  </td>
                  <td >
                    <p align="left"><B>Subject</font></B></p>
                  </td>
                  <td >
                    <p align="left"><B>Status</font></B></p>
                  </td>
                  <td >
                    <p align="left"><B>%</font></B></p>
                  </td>

                </tr>
<%

Const NumPerPage = 25

Dim CurPage
If Request.QueryString("CurPage") = "" then
    CurPage = 1 'We're on the first page
Else
    CurPage = Request.QueryString("CurPage")
End If
rsIssues.CursorLocation = 3 ' adUseClient
rsIssues.CacheSize = NumPerPage
rsIssues.Open strSQL, conn



if not rsIssues.BOF and not rsIssues.EOF then
	rsIssues.MoveFirst
	rsIssues.PageSize = NumPerPage

	Dim TotalPages
	TotalPages = rsIssues.PageCount

	rsIssues.AbsolutePage = CurPage

	Dim count


%>

	
<%
	Set ConnFox = Server.CreateObject ("ADODB.Connection")
	strConnPara = Application("DataConnectionString")'"Provider=MSDATASHAPE;DSN=crm;SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=Yes;"
	ConnFox.Open strConnPara
	
	Set rsTypeDesc = Server.CreateObject("ADODB.RecordSet")
	Set rsActionDesc = Server.CreateObject("ADODB.RecordSet")
%>
				<%'rsIssues.MoveFirst()
				Count = 0
				do while not rsIssues.EOF And Count < rsIssues.PageSize
					
						strSQL = "SELECT TypeDesc.Cdiscrep AS TypeDescription FROM Codes AS TypeDesc, Codes As ActionDesc"
						strSQL = strSQL & " WHERE TypeDesc.Cdefcode='N' AND TypeDesc.Crltfield='N' AND TypeDesc.Cfld_name ='CISSTYPE' AND TypeDesc.Ccode_no='" & rsIssues("CIssType") & "'"
				
						'Response.Write strSQL
						rsTypeDesc.Open strSQL, ConnFox
					if not rsTypeDesc.BOF and not rsTypeDesc.EOF  then
						strSQL = "SELECT ActionDesc.Cdiscrep AS ActionDescription  FROM Codes As ActionDesc"
						strSQL = strSQL & " WHERE ActionDesc.Cdefcode='N' AND ActionDesc.Crltfield='N' AND ActionDesc.Cfld_name ='CACTION' AND ActionDesc.Ccode_no='" & rsIssues("CAriaAct") & "'"
						rsActionDesc.Open strSQL, ConnFox
						%>
						<tr>
						  <td valign="top" bgcolor="ivory">
							<a href="TrkIssue.asp?IssueNo=<%=rsIssues("CIssueNo")%>&Type=<%=rsIssues("CIssType")%>"><%=rsIssues("CIssueNo")%></font></a>
						  </td>
						  <td valign="top" bgcolor="ivory"><%=rsIssues("DIssStart")%></font></td>
						  <td valign="top" bgcolor="ivory"><%if Not(rsTypeDesc.BOF and rsTypeDesc.EOF) then Response.write rsTypeDesc("TypeDescription") else Response.Write "N/A" end if%></font></td>
						  <td valign="top" bgcolor="ivory"><%=rsIssues("CIssSubjct")%></font></td>
						  <td valign="top" bgcolor="ivory"><%select case rsIssues("CIssStat")
									case "O" 
										Response.write "Open"
									case "W"
										Response.write "In Work"
									case "T"
										Response.write "In Testing"
									case "R"
										Response.write "Waiting for Reply"
									case "C"
										Response.write "Completed"
									case "X"
										Response.write "Cancelled"
						      end select%></font>
						  </td>
						  <td valign="top" bgcolor="ivory"><%=rsIssues("nIssPercnt")%></font></td>
						</tr>
						<%rsActionDesc.Close()
					end if
					rsTypeDesc.Close()
					
					rsIssues.MoveNext()
					Count = Count + 1
  
               loop%>

              </tbody>
            </table>
<table width = 95% align=center>
<tr>
	<form action="" name="form1" id="form1" method="post">
	<%if trim(Request.QueryString ("curpage")) = "" then
		searchQuery = strSQL
	else
		searchQuery = Request.form("strSQL")
	end if
	%>
	<Input type = "HIDDEN" name ="strSQL" Value ="<%=searchQuery%>">
	<td colspan = 5 align="center">
	<%Response.Write("<font face=""Arial"" size=""2"" color=""#000080""><strong>" & "Page " & CurPage & " of " & TotalPages & "<br></strong></font>")
	if CurPage > 1 then
        'We are not at the beginning, show the prev button%>
        <a href="javascript:SubmitNavigation('Default.asp?CurPage=<%=curpage-1%>')"><IMG src="../../images/STANDARD/back.gif"></a>
	<%End If

	if CInt(CurPage) <> CInt(TotalPages) then
        'We are not at the end, show a next button%>
       <a href="javascript:SubmitNavigation('Default.asp?CurPage=<%=curpage+1%>')" ><IMG src="../../images/STANDARD/next.gif"></a>
    <%End If
%>
	</td>
	</form>
</tr>
</table>
<%else%>
<table width=95% align=center>
<TR>
	<td Colspan=9 align='center' bgcolor="ivory">No issues match your criteria</font><BR></td>
</tr>
</table>
<%end if 'Recordset contain data or not %>
<br>
</body>

</html>
