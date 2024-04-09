<%@ Language=VBScript %>
<%
'Response.Write(Session("CurCust"))
'if Trim(Session("ID")) = "" then
	'Response.redirect "../../default.asp"
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
strRepType = Request.QueryString("strFormat")
%>

<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<title>CRM - Helpdisk - Open Issue -<%if strRepType = "S" then Response.Write "Summary" else Response.Write "Detail" end if  %> Report</title>
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
<BR>
<CENTER>
<a href="helpdesk.asp" onclick="return goback()"><IMG SRC="../Images/back.gif"></a>
</CENTER>
<br>
<%

'Choose Report
strRepType = Request.QueryString("strFormat")
if strRepType="S" then
	reportname = "Summary.rpt"
else
	reportname = "Detail.rpt"
end if

'---------------------------------------------------------------------------------------
' build header Query
'---------------------------------------------------------------------------------------
strSQL = "SELECT DISTINCT SuIsshdr.CIssueNo,SuIsshdr.DIssStart,SuIsshdr.CIssType,SuIsshdr.CIssPrior,SuIsshdr.CissSubjct,SuIsshdr.CissStat,SuIsshdr.nIssPercnt,SuIsshdr.DIssComp, Codes.Cdiscrep "

set rsIsEmpty = Server.CreateObject("ADODB.RecordSet")
rsIsEmpty.Open "Select * FROM SuIssDt", Conn
if rsIsEmpty.BOF and rsIsEmpty.EOF then
		strSQL = strSQL & "FROM SuIssHdr, OPENROWSET('MSDASQL', '" & Application("DataConnectionString") & "', 'SELECT * FROM Codes WHERE Cdefcode=""N"" AND crltfield=""N"" AND Cfld_name=""CISSTYPE""') AS Codes  WHERE "
		reportname = "Summary.rpt"
	else
		strSQL = strSQL & "FROM SuIssHdr,SuIssDt,OPENROWSET('MSDASQL', '" & Application("DataConnectionString") & "', 'SELECT * FROM Codes WHERE Cdefcode=""N"" AND crltfield=""N"" AND Cfld_name=""CISSTYPE""') AS Codes WHERE "
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

StrSQL = strSQL & " AND Codes.Ccode_no = SuIssHdr.CIssType"
'Build query for sort
select case Request.Form("lstSortBy")
	case "SC"
		strSQL = strSQL & " Order By SuIsshdr.CissStat,SuIsshdr.DIssComp "
	case "PC"
		strSQL = strSQL & " Order By SuIsshdr.CIssPrior,SuIsshdr.DIssComp "
	case "T"
		strSQL = strSQL & " Order By SuIsshdr.CIssType "
	case "SS"
		strSQL = strSQL & " Order By SuIsshdr.DIssStart,SuIsshdr.CissStat "
end select


'Response.Write strSQL
session("rsIssues").Open strSQL, conn
'session("rsIssues").Close
'session("rsIssues").Fields.Append "TypeDesc", 7, 30
'session("rsIssues").Open strSQL, conn
'Response.Write(strSQL)
if session("rsIssues").EOF And session("rsIssues").BOF then%>
	<div align="center">
	<center>
	<table border="0" width="100%" cellspacing="0" cellpadding="0">
		<tr>
			<td width="100%" align="center"><p>
					<OBJECT align=baseline classid=clsid:D27CDB6E-AE6D-11cf-96B8-444553540000 
					          codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" 
					height=125 id=ShockwaveFlash1 width=80% border = 0>
					    <param name="_cx" value="11748">
					    <param name="_cy" value="3307">
					    <param name="Movie" value="../flash/emptyNav.swf">
					    <param name="Src" value="../flash/emptyNav.swf">
					    <param name="WMode" value="Transparent">
					    <param name="Play" value="0">
					    <param name="Loop" value="0">
					    <param name="Quality" value="Medium">
					    <param name="SAlign" value>
					    <param name="Menu" value="0">
					    <param name="Base" value>
					    <param name="Scale" value="ExactFit">
					    <param name="DeviceFont" value="0">
					    <param name="EmbedMovie" value="0">
					    <param name="BGColor" value="AECAE6">
					    <param name="SWRemote" value><embed src="../flash/emptyNav.swf" align="baseline" border="0" width="100" height="172" loop="false" menu="false" quality="medium" wmode="transparent" bgcolor="#AECAE6" type="application/x-shockwave-flash" pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash" designtimesp="22693" DESIGNTIMESP="23127" DESIGNTIMESP="23294" DESIGNTIMESP="19128">
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
	<CENTER>
	<table width=80%>
		<tr>
			<td align="left"><font face="Arial" size="2" color="#000080"><strong>Can't view the report, there is no data matching your search criteria.</strong></font></td>
		</tr>
	</table>
	</CENTER>
	
	<%Response.End
	
	
	
	
	
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
if strRepType<>"S" then
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

viewer = Request("viewer")
'This line collects the value passed for the viewer to be used, and stores
'it in the "viewer" variable.

If cstr(viewer) = "ActiveX" then
%>
<!-- #include file="../crystal/SmartViewerActiveX.asp" -->
<%
ElseIf cstr(viewer) = "Netscape Plug-in" then
%>
<!-- #include file="../crystal/ActiveXPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "Java using Browser JVM" then
%>
<!-- #include file="../crystal/SmartViewerJava.asp" -->
<%
ElseIf cstr(viewer) = "Java using Java Plug-in" then
%>
<!-- #include file="../crystal/JavaPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "HTML Frame" then
	Response.Redirect("../crystal/htmstart.asp")
Else
	Response.Redirect("../crystal/rptserver.asp")
End If
'The above If/Then/Else structure is designed to test the value of the "viewer" varaible
'and based on that value, send down the appropriate Crystal Smart Viewer.

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
%>
<%End if ' for the Query doens't contain data.%>

<BR>
<CENTER>
<a href="helpdesk.asp" onclick="return goback()"><IMG SRC="../Images/back.gif"></a>
</CENTER>
<br>
</BODY>
</HTML>
<%
''''''''''''''''''''''''''''''''''''''''''


%>
