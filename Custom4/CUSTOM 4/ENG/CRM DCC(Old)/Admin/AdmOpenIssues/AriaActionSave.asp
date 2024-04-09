<%Response.Buffer = true%>
<%Response.Buffer = true%>
<%if Trim(Session("AdmID"))="" then
	'Response.Redirect "Default.asp"
end if
%>
<HTML>
<TITLE>ARIA Systems</TITLE>
</body>
<%
	set ConnFox = Server.CreateObject("ADODB.Connection")
	ConnFox.Open Application("DataConnectionString")'"Provider=MSDATASHAPE;DSN=crm;SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=YES;"	
	
'WMH [Start]
'	Dim objOpenIssue
'
'	Set objOpenIssue = Server.CreateObject("UIOpnIss.UIOpenIssue")
'	'HDM [Start] use the Application variable that holds the connection string instead of this rubbish
'	'objOpenIssue.ConParameter = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source=" & Trim(Session("SQLServer")) & ";UID="& Trim(Session("SqlUserName"))& ";PWD="&Trim(Session("SqlPassWord"))
'	objOpenIssue.ConParameter = Application("SqlServer")
'	'HDM[End]
'	
'	objOpenIssue.Load("WHERE CIssueNo='" & Request("IssueNo") & "'")
'	If objOpenIssue Is Nothing Then
'		'Response.Write("Empty")
'	End If

	Dim cnnSql, strSqlConnString
	Set cnnSql = Server.CreateObject("ADODB.Connection")
	strSqlConnString = Application("SqlServer")
	cnnSql.Open strSqlConnString

	Dim rsOpenIssueHdr,rsOpenIssueDt,strMySQL

	'Alternate OojectOpenIssue
	Set rsOpenIssueHdr = Server.CreateObject("ADODB.RecordSet")
	rsOpenIssueHdr.LockType = 3
	rsOpenIssueHdr.CursorType = adOpenKeyset
	strMySQL = "Select * from SuIssHdr WHERE CIssueNo='" & Request("IssueNo") & "'" 			
	rsOpenIssueHdr.Open strMySQL, cnnSql

		
	'Alternate Child
	Set rsOpenIssueDt = Server.CreateObject("ADODB.RecordSet")
	rsOpenIssueDt.LockType = 3
	rsOpenIssueDt.CursorType = adOpenKeyset
	strMySQL = "Select * from SuIssDt WHERE CIssueNo='" & Request("IssueNo") & "'" 		
	rsOpenIssueDt.Open strMySQL, cnnSql

'WMH [End]


%>
<%
set ConnFox = Server.CreateObject("ADODB.Connection")

ConnFox.Open Application("DataConnectionString")'"dsn=crm;Deleted=Yes"
'------------------------------------------------------------------------------------------------------------
'			Save The Track Action if the Save Action is Pressed.
'------------------------------------------------------------------------------------------------------------

' WMH [Start]
'This is unused Function
'Function GetSequence(strSeqType)
'		Dim objSeq
'		Set objSeq = Server.CreateObject("DBGenSeq.DBSequence")
'		GetSequence = objSeq.GetSequence(Application("DataConnectionString") , strSeqType)
'		
'		Set objSeq = Nothing
'End Function
' WMH [End]
'HDM why establish a new connection? Complated
Function GetLineNo(strIssue)
		'Connect to the detail file and get the maximum lineno for the response
		'Dim strConn
		'HDM[Start] make sure to connect using application varialbe that holds the connection string
		'strConn = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source="&Trim(Session("SqlServer"))&";UID="& Trim(Session("SqlUserName"))& ";PWD="&Trim(Session("SqlPassWord"))
		'strConn = Application("SqlServer")
		'HDM [End]
		
		'Dim objConn
		
		'Set objConn = Server.CreateObject("ADODB.Connection")
		
		'objConn.Open strConn
		
		Dim rsMax
		Set rsMax = Server.CreateObject("ADODB.RecordSet")
		rsMax.Open "SELECT MAX(intLineNo) AS IssLineNo FROM SuIssDt WHERE cissueno = '" & strIssue & "'" , cnnSql
		If rsMax.EOF Then
			GetLineNo = 1
		Else
			
			If isNull(rsMax("IssLineNo")) Then
				GetLineNo = 1
			Else
				GetLineNo = cint(rsMax("IssLineNo")) + 1
			End IF
		End IF
		
		'Close and release the connection
		rsMax.Close()
		Set rsMax = Nothing
		
		'Close and release the RecordSet
		'objConn.Close()
		'Set objConn = Nothing
		
End Function

'Initialization

Dim EntryID
Dim EntryIDWithCode
EntryID = ""
EntyIDWithCode=""
if Trim(Request.Form("D1"))<>"" or Request.Form("D1")<>2 then
	  strSQL = "SELECT Crltd_vlu, Crltd_nam FROM Codes WHERE Ccode_no='" & Request("D1") & "' AND crltfield='Y' AND Cdefcode='N' AND Crltd_nam='LGENTRAK'"
	  Response.Write strSQL
	  set rsTrackAction = Server.CreateObject ("ADODB.RecordSet")
	  rsTrackAction.Open strSQL, ConnFox
		  if rsTrackAction.BOF and rsTrackAction.EOF then 
			' NO track Entry to generate .
		  else
				set rsApplicationName = Server.CreateObject ("ADODB.RecordSet")
				rsApplicationName.Open "SELECT Capp_name From Aria_app Where Cbug_app='" & Request("App") & "'", ConnFox
				if rsApplicationName.BOF and rsApplicationName.EOF then
					appName=""
				else
					appName = rsApplicationName("Capp_name")
				end if
				
				
		  end if
		  set rsLetter=nothing
end if

'WMH [Start]
'	'Put the Code of the Action in the Issue Header
'	'- CAriaAct 
'		objOpenIssue.ARIAAction = Request("D1")
'		objOpenIssue.TrackingRef = EntryIDWithCode
'		objOpenIssue.CompletePercent=Request("txtpercent")
'		IF objOpenIssue.CompletePercent="100" Then
'			objOpenIssue.STATUS="C"
'		End IF
'		if Trim(Request("txtCompDate")) <> "" then
'			objOpenIssue.DueDate = Request("txtCompDate")
'		end if
'	
'	'add new Issue Detail in the OpenIssueObject ...
'		Dim objIssueDetail
'		Set objIssueDetail = objOpenIssue.ChildAddNew(1)
'		
'	'1- CIssueN
'		objIssueDetail.IssueNo = Request("IssueNo")
'	'2- cRespType
'		objIssueDetail.ResponseType="Y"
'	'3- cRespBy
'		objIssueDetail.ResponseBy = Session("AdmID")
'	'4- mRespActv
'		objIssueDetail.ResponseAction = Request("txtResponse") & " " & EntryID
'	'5- DRespDate
'		objIssueDetail.ResponseDate = Date()
'	'6- tRespTime
'		objIssueDetail.ResponseTime = Time()
'	'7- intLineNo 
'		objIssueDetail.LineNo	= GetLineNo(Request("IssueNo"))
'
'
'	'Update the child object
'		objOpenIssue.ChildSet 1,objIssueDetail
'	
'	'Save Object
'		set objIssueDetail = nothing
'		objOpenIssue.Save()
'		'Redirect to other page after saving ..
'		'Response.Write GerneratedTrack
'		Set objOpenIssue = Nothing
'		
'		if Trim(EntryIDWithCode)<>"" then
'			Response.Redirect "ARIAFinish.asp?TrackEntry=" & EntryIDWithCode
'		else
'			Response.Redirect "TrkIssue.asp?IssueNo=" & Request("IssueNo") & "&Type=" & Request("Type")
'		end if
	
'-----------------------------------------------------------------------------
'			Start of Save Action
'-----------------------------------------------------------------------------	
		'Put the Code of the Action in the Issue Header
		rsOpenIssueHdr("CAriaAct") = Request("D1")
		rsOpenIssueHdr("CTrackRef") = EntryIDWithCode		
		If Request("txtpercent")="" then 
			rsOpenIssueHdr("nIssPercnt") = "0"
		Else
			rsOpenIssueHdr("nIssPercnt") = Request("txtpercent")
		End If 	
		IF rsOpenIssueHdr("nIssPercnt")="100" Then
			rsOpenIssueHdr("CissStat")="C"
		End IF
		if Trim(Request("txtCompDate")) <> "" then
			rsOpenIssueHdr("DIssComp") = Request("txtCompDate")
		end if		

		'Save Issue Header
		rsOpenIssueHdr.Update()

		'add new Issue Detail in the rsOpenIssueDt Object ...
		rsOpenIssueDt.AddNew()
		rsOpenIssueDt("CIssueNo") = Request("IssueNo")
		rsOpenIssueDt("cRespType")="Y"
		rsOpenIssueDt("cRespBy") = Session("AdmID")
		rsOpenIssueDt("mRespAct") = Request("txtResponse") & " " & EntryID
		rsOpenIssueDt("DRespDate") = Date()
		rsOpenIssueDt("tRespTime") = Time()
		rsOpenIssueDt("intLineNo") = GetLineNo(Request("IssueNo"))			
		
		'Update the Issue Details
		rsOpenIssueDt.Update() 

		'Destroy Objects		
		set rsOpenIssueDt = nothing
		set rsOpenIssueHdr = nothing
		set cnnSql = nothing
		
		
		'Redirect to other page after saving ..
		if Trim(EntryIDWithCode)<>"" then
			Response.Redirect "ARIAFinish.asp?TrackEntry=" & EntryIDWithCode
		else
			Response.Redirect "TrkIssue.asp?IssueNo=" & Request("IssueNo") & "&Type=" & Request("Type")
		end if
'-----------------------------------------------------------------------------
'			End of Save Action
'-----------------------------------------------------------------------------	
'WMH [End]

%>
</body>
</HTML>
