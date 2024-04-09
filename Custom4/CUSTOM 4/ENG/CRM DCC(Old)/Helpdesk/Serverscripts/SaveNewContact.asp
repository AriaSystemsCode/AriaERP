<%@ Language=VBScript %>
<%
	'Update Contacts File
	Session("ConnectionString") = Application("DataConnectionString")'"dsn=crm;uid=aria;pwd=aria;Deleted=Yes"
	Set Conn = Server.CreateObject("ADODB.Connection")
	Conn.Open(Session("ConnectionString"))

	Dim rsContacts
	Set rsContacts = createobject("ADODB.RecordSet")
	rsContacts.LockType = 3
	rsContacts.CursorType = adOpenKeyset

	
	rsContacts.Open "Select * from contact where cCont_ID='" & Request.QueryString("strCustID") & "'", Conn,2,3,1
	If rsContacts.EOF Then
		Response.Write("EOF")
	Else
		rsContacts.AddNew
		'Response.Write("Contact Updated")
		rsContacts("cContType")	= "C"
		rsContacts("cCont_ID")	= Request.QueryString("strCustID")
		rsContacts("Contact")		= Request("txtContact")
		rsContacts("Phone")     = Request("txtPhone")
		rsContacts("cWE_Mail")  = Request("txteMail")
		rsContacts("Fax")			  = Request("txtFax")
		rsContacts.Update
	End IF
	If Trim(Len(Request.QueryString("strIssue"))) > 0 Then
		Response.Redirect("../EditIssue.asp?IssNo=" & Request.QueryString("strIssue"))
	Else
		Response.Redirect("../Issue.asp")
	End If
%>
</HTML>

