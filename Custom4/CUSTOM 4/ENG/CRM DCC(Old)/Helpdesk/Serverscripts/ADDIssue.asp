<%@ Language=VBScript %>
<%
' ARD - 604499 change the field cwe_Mail with this one cEmail_Add
	Response.Buffer = true
	
	'Connection creation [Start]
	Dim Conn
	Set Conn = Server.CreateObject("ADODB.Connection")
	Conn.Open(Application("DataConnectionString"))

	Dim SysCon
	Set SysCon = server.CreateObject("ADODB.Connection")
	sysCon.Open Application("SystemConnectionString")
	'Connection creation [End]
	
'WMH [Start]
	'Dim objUIIssue
	'Set objUIIssue = Server.CreateObject("UIOpnIss.UIOpenIssue")
	'objUIIssue.ConParameter  = Application("SqlServer")
	
'SQl Server Connection Object
	Dim cnnSql, strSqlConnString
	Set cnnSql = Server.CreateObject("ADODB.Connection")
	strSqlConnString = Application("SqlServer")
	cnnSql.Open strSqlConnString
'WMH [End]	
''''*****************************************	
	'If issue type is Call Back
	Dim strSequence
	strSequence = ""
	

'WMH [Start]
'	If objUIIssue.Add() Then
'		objUIIssue.Application	= Request("lstAppl")
'		'objUIIssue.ARIAAction	= Saved By ARIA
'	
'		objUIIssue.Contact			= Request("lstReportedBy")
'		objUIIssue.CustomerID		= Session("ID")
'		objUIIssue.Details			= Request("txtDetail")
'		
'		'If the issue is a call back
'		'	its default status is Complete
'		'Else
'		'	Make it Open
'		'If Request("lstIsseType") = "L" Then
'		'	objUIIssue.Status				= "C"
'		'Else
'			objUIIssue.Status				=	"O"
'		'End If	
'		'objUIIssue.DueDate			= Saved By ARIA
'	
'		objUIIssue.eMail				= Request("txtEMail")
'
'		'objUIIssue.Extension		= Saved By ARIA
'
'		objUIIssue.Fax					= Request("txtFax")
'		Dim strIssueNo
'		
'		
'		strIssueNo							= GetSequence("CISSUENO")
'		objUIIssue.IssueNo			= strIssueNo
'		objUIIssue.IssueType		= Trim(Request("lstIsseType"))
'		objUIIssue.Module				= Request(Request("lstAppl"))
'		objUIIssue.Phone				= Request("txtPhone")
'		objUIIssue.Priority			= Request("lstPri")
'		objUIIssue.StartDate		=	Date()
'		objUIIssue.Subject			=	Request("txtSubject")
'				
'		'objUIIssue.Country			= strSrv
'		objUIIssue.Country 			= getCountryCode()
'		'Response.write "Country"&objUIIssue.Country&"<br>"
'		'Response.End
'		
'
'		'If Issue type is Call Back
'		'The tracking Ref. will be cIncdntID
'		If Len(Trim(strSequence)) > 0 Then
'			objUIIssue.TrackingRef	=	"L"&Trim(strSequence)
'		End IF
'
'		objUIIssue.Save()
'	End IF	
'	Set objUIIssue = Nothing



	Dim rsADDIssue
	Set rsADDIssue = Server.CreateObject("ADODB.RecordSet")
	rsADDIssue.LockType = 3
	rsADDIssue.CursorType = adOpenKeyset
	rsADDIssue.Open "Select * from SuIssHdr", cnnSql

	
	'Get Issue Num. 
	Dim strIssueNo
	strIssueNo = GetSequence("CISSUENO")
	
'	Add Issue Items
	rsADDIssue.AddNew()
	rsADDIssue("CIssueNo") = strIssueNo	
	rsADDIssue("CCust_Id")	= Session("ID")	
	rsADDIssue("DIssStart") =	Date()
	rsADDIssue("CIssType") = Trim(Request("lstIsseType"))
	rsADDIssue("Cbug_app")	= Request("lstAppl")
	rsADDIssue("CMod_Id") = Request(Request("lstAppl"))	
	rsADDIssue("CissStat") = "O"	
	rsADDIssue("CIssPrior") = Request("lstPri")
	rsADDIssue("CissSubjct") = Request("txtSubject")
	rsADDIssue("mIssDetail") = Request("txtDetail")
	rsADDIssue("Contact")	= Request("lstReportedBy")
	rsADDIssue("Cwe_mail")= Request("txtEMail")
	rsADDIssue("Phone") = Request("txtPhone")
	rsADDIssue("Fax") = Request("txtFax")
	rsADDIssue("cCont_Code") = ""'getCountryCode()

	'If Issue type is Call Back	'The tracking Ref. will be cIncdntID
	If Len(Trim(strSequence)) > 0 Then
		rsADDIssue("CTrackRef")	=	"L"&Trim(strSequence)
	End IF

	rsADDIssue.Update 

	'Destroy Objects
	set rsADDIssue = nothing
	set cnnSql = Nothing

	Response.Redirect("../Finish.asp?strIssueNo="&strIssueNo&"&strSequence="&strSequence)

'WMM [End]
%>
	


<%
	Function  getCountryCode()
		sqlgetCountryCode = "Select Ccont_Code from syccomp"
		set rsgetCountryCode= SysCon.execute(sqlgetCountryCode)
		getCountryCode=rsgetCountryCode("Ccont_Code")
	End Function
	
	Function GetSequence(strCode)
		'Dim objSeq
		'Set objSeq = Server.CreateObject("DBGenSeq.DBSequence")
		'Response.Write "GetSequence : "&strSeqType
		'GetSequence = objSeq.GetSequence("dsn=webtrack1;uid=aria;pwd=aria" , strSeqType)
		'Set objSeq = Nothing
		'********************************************************NEK
		set RSSeq = server.createobject("ADODB.RecordSet") 
		RSSeq.LockType = 2 
		RSSeq.CursorType = adOpenKeyset 
		strsql = "Select * from Sequence Where cseq_type+cseq_group = '" & strCode & "'"
		RSSeq.Open strsql,Conn,1,3
		If RSSeq.EOF Then 
			lnSequence = 1
			RSSeq.AddNew 
			RSSeq("cSeq_Type") = strCode 
			RSSeq("nSeq_No") = CLng(lnSequence) + 1 
			RSSeq("nFld_Wdth") = 6 
			RSSeq("cData_Typ") = "C" 
			RSSeq.Update 
		Else 
			lnSequence = CInt(RSSeq("nSeq_No")) 
			RSSeq("nSeq_No") = lnSequence + 1 
			RSSeq.Update 
		End If 
		RSSeq.Close 
		set RSSeq=Nothing 
		strSequence = String(6-Len(lnSequence),"0") + CSTR(lnSequence)
		GetSequence = strSequence
		'**************************************************************************	
	End Function


%>