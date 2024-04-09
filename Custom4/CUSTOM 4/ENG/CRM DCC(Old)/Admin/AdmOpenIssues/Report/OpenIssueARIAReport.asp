<%@ Language=VBScript %>
<%'if Trim(Session("AdmID"))="" then
'	Response.Redirect "Default.asp"
'end if
%>
<%
Set Conn = Server.CreateObject("ADODB.Connection")
ConnString = Application("SqlServer")
Conn.Open ConnString
'RecordSets
set session("rsIssues") = server.CreateObject("ADODB.RecordSet")
session("rsIssues").LockType = 4

'''''''''''''''''''''''''''''''''''''''''''''''
%>

<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<TITLE>CRM - Administrator - Helpdesk - Issue <%if Request.Form("lstReportFormat")="S" then Response.Write "Summary" else Response.Write "Detail" end if%> Report</TITLE>
<SCRIPT LANGUAGE=javascript>
<!--
function goback()
{
	//history.back();
}
//-->
</SCRIPT>
</HEAD>

<BODY bgcolor="#AECAE6" topmargin="0" leftmargin="0" background="../images/tile1.gif">
<br>
<CENTER>
<a href="../default.asp" onclick="return goback()"><IMG SRC="../../../images/back.gif"></a>
</CENTER>
<BR>
<%
'Choose Report
if Request.Form("lstReportFormat")="S" then
	reportname = "Summary.rpt"
else
	reportname = "Detail.rpt"
end if

'---------------------------------------------------------------------------------------
' build header Query
'---------------------------------------------------------------------------------------
strSQL = "SELECT DISTINCT SuIsshdr.CIssueNo,SuIsshdr.DIssStart,SuIsshdr.CIssType,SuIsshdr.CIssPrior,SuIsshdr.CissSubjct,SuIsshdr.CissStat,SuIsshdr.nIssPercnt,SuIsshdr.DIssComp, Codes.Cdiscrep  "

set rsIsEmpty = Server.CreateObject("ADODB.RecordSet")
rsIsEmpty.Open "Select * FROM SuIssDt", Conn
if rsIsEmpty.BOF and rsIsEmpty.EOF then
		strSQL = strSQL & "FROM SuIssHdr,OPENROWSET('MSDASQL', '" & Application("DataConnectionString") & "','SELECT * FROM Codes WHERE Cdefcode=""N"" AND crltfield=""N"" AND Cfld_name=""CISSTYPE""') AS Codes WHERE 1<>2 "
		reportname = "Summary.rpt"
	else
		strSQL = strSQL & "FROM SuIssHdr,SuIssDt,OPENROWSET('MSDASQL', '" & Application("DataConnectionString") & "','SELECT * FROM Codes WHERE Cdefcode=""N"" AND crltfield=""N"" AND Cfld_name=""CISSTYPE""') AS Codes WHERE 1<>2 "
end if 
'Build the wehre conditions according to the input of the user.

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

StrSQL = strSQL & " AND Codes.Ccode_no = SuIssHdr.CIssType"
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

session("rsIssues").Open strSQL, conn

if session("rsIssues").EOF And session("rsIssues").BOF then
	Response.Write "<font color=""#FFFFFF"">No Records Exist</font>"
	Response.End
else

'---------------------------------------------------------------------------------------
' End of header Query 
'---------------------------------------------------------------------------------------

If Not IsObject (session("oApp")) Then                              
	Set session("oApp") = Server.CreateObject("Crystal.CRPE.Application")
End If                                                                
Path = Request.ServerVariables("PATH_TRANSLATED")                     
While (Right(Path, 1) <> "\" And Len(Path) <> 0)                      
iLen = Len(Path) - 1                                                  
Path = Left(Path, iLen)                                               
Wend                                                                  
If IsObject(session("oRpt")) then
	Set session("oRpt") = nothing
End if
Set session("oRpt") = session("oApp").OpenReport(path & reportname, 1)
'session("oRpt").MorePrintEngineErrorMessages = False
'session("oRpt").EnableParameterPrompting = False

session("oRpt").DiscardSavedData
set Database = session("oRpt").Database
set Tables = Database.Tables
set Table1 = Tables.Item(1)
Table1.SetPrivateData 3,session("rsIssues")

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'---------------------------------------------------------------------------------------
' Begin Detail Query
'---------------------------------------------------------------------------------------
if rsIsEmpty.BOF and rsIsEmpty.EOF then
else
if Request.Form("lstReportFormat")="D" then
	set session("rsIssuesDetail") = server.CreateObject("ADODB.RecordSet")
	strSQL =" SELECT SuIssDt.cRespBy , SuIssDt.mRespAct, SuIssDt.DRespDate, SuIssDt.tRespTime, SuIssDt.CIssueNo FROM SuIssDt Order By CIssueNo, intLineNo "
	session("rsIssuesDetail").Open strSQL , conn


	'Subreport
	set session("subRep") = session("oRpt").OpenSubReport("IssueDetail")
	set subDatabase = session("subRep").Database
	set subTables = subDatabase.Tables
	set subTable1 = subTables.Item(1)
	subTable1.SetPrivateData 3, session("rsIssuesDetail")
End if

end if 









On Error Resume Next                                                  
session("oRpt").ReadRecords                                           
If Err.Number <> 0 Then                                               
  Response.Write "An Error has occured on the server in attempting to access the data source"
Else

  If IsObject(session("oPageEngine")) Then                              
  	set session("oPageEngine") = nothing
  End If
set session("oPageEngine") = session("oRpt").PageEngine
End If                                                                

viewer = Request.QueryString ("Viewer")
'viewer = "Java using Browser JVM"
'This line collects the value passed for the viewer to be used, and stores
'it in the "viewer" variable.

If cstr(viewer) = "ActiveX" then
%>
<!-- #include file="../../../crystal/SmartViewerActiveX.asp" -->
<%
ElseIf cstr(viewer) = "Netscape Plug-in" then
%>
<!-- #include file="../../../crystal/ActiveXPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "JVM" then
%>
<!-- #include file="../../../crystal/SmartViewerJava.asp" -->
<%
ElseIf cstr(viewer) = "Java-Plug-in" then
%>
<!-- #include file="../../../crystal/JavaPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "HTML Frame" then
	Response.Redirect("../../../crystal/htmstart.asp")
Else
	Response.Redirect("../../../crystal/rptserver.asp")
End If
'The above If/Then/Else structure is designed to test the value of the "viewer" varaible
'and based on that value, send down the appropriate Crystal Smart Viewer.

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
%>
<%End if ' for the Query doens't contain data.%>

<BR>
<br>
<CENTER>
<a href="../default.asp" onclick="return goback()"><IMG SRC="../../../images/back.gif"></a>
</CENTER>
<BR>
</BODY>
</HTML>
<%
''''''''''''''''''''''''''''''''''''''''''


%>
