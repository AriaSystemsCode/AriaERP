<%@ Language=VBScript %>
<%
'WMH [Start]
'' ARD - 604499 change the field cwe_Mail with this one cEmail_Add
'	Dim strCustomerID , strIssueNo , strRepBy
'
'	Dim objOpenIssue
'	strIssueNo		= Request("hdnIssueNo")
'	strCustomerID	= Request("hdnCustID")
'	strRepBy			= Request("hdnReportedBy")
'	'Collect issue information
'
'	Set objOpenIssue = Server.CreateObject("UIOpnIss.UIOpenIssue")
'	objOpenIssue.ConParameter  = Application("SqlServer")
'	If objOpenIssue.Load("WHERE cCust_Id='"& strCustomerID &"'AND CIssueNo='" & Trim(strIssueNo) & "'") Then
'		objOpenIssue.Status = Request("nxtstatue")
'		Dim objChild
'		Set objChild = objOpenIssue.ChildAddNew(1)
'		objChild.IssueNo				= strIssueNo
'		objChild.ResponseType		= "N"
'		objChild.ResponseBy			= Trim(Request("lstRespBy"))
'		objChild.ResponseAction = Request("txtRespDetail")
'		objChild.ResponseDate		= Date()
'		objChild.ResponseTime		= Time()
'		
'		objChild.LineNo					= GetLineNo(strIssueNo)
'		'Update the child object
'		objOpenIssue.ChildSet 1,objChild
'		
'		objOpenIssue.Phone			= Request("txtPhone")
'		objOpenIssue.Fax				= Request("txtFax")
'		objOpenIssue.eMail			= Request("txteMail")
'		objOpenIssue.Save()
'		set objChild = Nothing
'	End If
'	
'	Set objOpenIssue = Nothing
'

	
	Dim strCustomerID , strIssueNo , strRepBy
	
	strIssueNo		= Request("hdnIssueNo")
	strCustomerID	= Request("hdnCustID")
	strRepBy			= Request("hdnReportedBy")
	
	Dim cnnSql, strSqlConnString
	Set cnnSql = Server.CreateObject("ADODB.Connection")
	strSqlConnString = Application("SqlServer")
	cnnSql.Open strSqlConnString

	Dim rsOpenIssueHdr,rsOpenIssueDt,strMySQL

	'Alternate OojectOpenIssue
	Set rsOpenIssueHdr = Server.CreateObject("ADODB.RecordSet")
	rsOpenIssueHdr.LockType = 3
	rsOpenIssueHdr.CursorType = adOpenKeyset
	strMySQL = "Select * from SuIssHdr WHERE cCust_Id='"& strCustomerID &"'AND CIssueNo='" & Trim(strIssueNo) & "'" 			
	rsOpenIssueHdr.Open strMySQL, cnnSql

		
	'Alternate Child
	Set rsOpenIssueDt = Server.CreateObject("ADODB.RecordSet")
	rsOpenIssueDt.LockType = 3
	rsOpenIssueDt.CursorType = adOpenKeyset
	strMySQL = "Select * from SuIssDt WHERE CIssueNo='" & Request("IssueNo") & "'" 		
	rsOpenIssueDt.Open strMySQL, cnnSql


	'Put the Code of the Action in the Issue Header
	rsOpenIssueHdr("CissStat") = Request("nxtstatue")
	rsOpenIssueHdr("Phone") = Request("txtPhone")
	rsOpenIssueHdr("Fax") = Request("txtFax")
	rsOpenIssueHdr("Cwe_mail") = Request("txteMail")
		
	'Save Issue Header
	rsOpenIssueHdr.Update()

	'add new Issue Detail in the rsOpenIssueDt RecordSet Object ...
	rsOpenIssueDt.AddNew()
		

	rsOpenIssueDt("CIssueNo") = strIssueNo
	rsOpenIssueDt("cRespType")= "N"
	rsOpenIssueDt("cRespBy") = Trim(Request("lstRespBy"))
	rsOpenIssueDt("mRespAct") = Request("txtRespDetail")
		
	rsOpenIssueDt("DRespDate") = Date()
	rsOpenIssueDt("tRespTime") = Time()
	rsOpenIssueDt("intLineNo") = GetLineNo(strIssueNo)
		
	'Update the Issue Details
	rsOpenIssueDt.Update() 

	'Destroy Objects		
	set rsOpenIssueDt = nothing
	set rsOpenIssueHdr = nothing
	set cnnSql = nothing

'WMH [End]


	'Update Contacts File
	Session("ConnectionString") = Application("DataConnectionString")'"dsn=crm;uid=aria;pwd=aria;Deleted=Yes"
	Set Conn = Server.CreateObject("ADODB.Connection")
	Conn.Open(Session("ConnectionString"))

	Dim rsContacts
	Set rsContacts = createobject("ADODB.RecordSet")
	rsContacts.LockType = 3
	rsContacts.CursorType = adOpenKeyset



'WMH [Start]
	'rushmore Complated
	'rsContacts.Open "Select * from contact where Contact ='" & Trim(Request("hdnReportedBy"))&"' And cCont_ID='" & Session("ID") & "'", Conn,2,3,1
	rsContacts.Open "Select * from Contact where cconttype+ccont_id+store+contact Like 'C%' And ccont_id='" & Trim(Request("hdnReportedBy")) & "' And contact='" & Session("ID") & "'", Conn,2,3,1	
'WMH [End]	

	If Not rsContacts.EOF Then
		rsContacts.MoveFirst

		rsContacts("Phone")     = Request("txtPhone")
		' ARD - 604499 change the field cwe_Mail with this one cEmail_Add [Start]
		'rsContacts("cwe_mail")  = Request("txteMail")
		rsContacts("cEmail_Add")  = Request("txteMail")
		' ARD - 604499 change the field cwe_Mail with this one cEmail_Add [End]
		rsContacts("Fax")			  = Request("txtFax")
		rsContacts.Update
	End If

	
	'HDM why establish a new connection? Complated
	Function GetLineNo(strIssue)
		'Connect to the detail file and get the maximum lineno for the response
		'Dim strConn
		'strConn = Application("SqlServer")
		
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
				GetLineNo = cInt(rsMax("IssLineNo")) + 1
			End IF
		End IF
		
		'Close and release the connection
		rsMax.Close()
		Set rsMax = Nothing
		
		'Close and release the RecordSet
		'objConn.Close()
		Set objConn = Nothing
		
	End Function
	
	Response.Redirect("../Helpdesk.asp?Criteria=X")	
%>
