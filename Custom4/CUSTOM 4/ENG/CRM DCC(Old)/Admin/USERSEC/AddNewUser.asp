<%@ Language=VBScript %>
<%Response.Buffer = true%>
<%
'*******************************************************************************
'Page Name:  AddNewUser.asp
'Date     :  03/08/2003
'Developer:  mms (Mai Maged)
'modified :  by wal 23/5/2005 to work for crm customer users >> issue 127795
'Purpose  :  Action page for users file.
'********************************************************************************
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<TITLE>CRM/Admin - Add New User</TITLE>

<BODY bgcolor=white leftmargin=0 topmargin=0>
<%	
'Request from variables
Dim strContact, strTitle, strID, strName, strPhone, strFax, strEmail, strPass, strLevel, strProfile

strID = Trim(Request.Form("txtID"))
strName = Trim(Request.Form("txtName"))

strEmail = Trim(Request.Form("txtEmail"))
strLevel = Trim(Request.Form("radLevel"))
strProfile = Trim(Request.Form("Profile"))

'Prepare the user password
Dim objPriv
set objPriv = Server.CreateObject("AriaWebSecurity.Privileges")
strPass = objPriv.GetPassword(Trim(Request.Form("txtPass")))

'Response.Write strContact & "<HR>"
'Response.Write strTitle & "<HR>"
'Response.Write strID & "<HR>"
'Response.Write strName & "<HR>"
'Response.Write strPhone & "<HR>"
'Response.Write strFax & "<HR>"
'Response.Write strEmail & "<HR>"
'Response.Write strPass & "<HR>"
'Response.Write strLevel & "<HR>"
'Response.Write strProfile & "<HR>"

'Response.End 
dim cnnSQL
set cnnSQL = server.CreateObject ("ADODB.Connection")
cnnSQL.Open Application("SqlServer") 
'Add user in syuuser table
Dim strSql, rsUser
set rsUser = Server.CreateObject("ADODB.RecordSet")
'mms - prevent user duplicates 20/8/2003 [start]
'strSql = "Select * From syuuser Where profile ='"& strProfile & "' ORDER BY cusr_name ASC"
strSql = "Select * From syuuser Where profile ='"& strProfile & "' and cuser_id ='" & Trim(strID) & "'"
rsUser.Open strSql,cnnSQL,1,4
'Response.Write rsUser.RecordCount 
if rsUser.RecordCount > 0 then
	Response.Write("<center><B><font color=red>!!</font>This user already exists.</b>")
	Response.End
Else
	rsUser.AddNew
	rsUser("cuser_id") = strID
	rsUser("cusr_name") = strName
	rsUser("cusr_pass") = strPass
	rsUser("cwe_mail") = strEmail
	rsUser("cusr_levl") = strLevel
	'add values of style profile and price code
	if Request.Form ("txtStyProfile") <> "" then
		rsUser("cStyleProfileCode") = trim(Request.Form ("txtStyProfile"))
	end if
	if Request.Form ("lstPrice") <> "" then
		rsUser("priccode") = trim(Request.Form ("lstPrice"))
	end if
	rsUser("profile") = strProfile
	rsUser.UpdateBatch
End if
'mms - prevent user duplicates 20/8/2003 [end]
if Request.Form ("txtWareCode") <> "" or Request.Form ("txtStyGroup") <> "" then
	'wal_add ware house code for all users/customer
	set rsWare  = server.CreateObject ("ADODB.Recordset")
	rsware.open"select * from customerwarecode where ccustid='"& trim(strProfile) &"'",cnnSQL,1,3
	 
	'add new record for the customer
	if rsware.EOF then'doesn't exists then add it 
		rsware.AddNew
			rsware("cwarecode") =  Request.Form ("txtWareCode")
			rsware("ccustid") = strProfile
			'wal_130731 save the style group selected per user
			if trim(Request.Form ("txtStyGroup")) <> "" then
				rsware("cstylegroup") =  Request.Form ("txtStyGroup")
			end if
			'rsware("cuserid") = strID
		rsware.Update 
		rsware.close
	end if
end if
'wal_130731 save the style group selected per user
'if trim(Request.Form ("txtStyGroup")) <> "" then
'	set rsStyGroup  = server.CreateObject ("ADODB.Recordset")
'	rsStyGroup.Open "select cuserid,ccustid,cstylegroup from from customerwarecode where ccustid='"& trim(strProfile) &"' and cuserid = ' "& Trim(strID) &" '",cnnSQL,1,3
'	if rsstygroup.EOF then'new user
'		rsstygroup.AddNew
'			rsstygroup("cstylegroup") =  Request.Form ("txtStyGroup")
'			rsstygroup("ccustid") = strProfile
'			rsstygroup("cuserid") = strID
'		rsstygroup.Update 
'		
'	else'exusting user update
'		rsstygroup("cstylegroup") =  Request.Form ("txtStyGroup")
'		rsstygroup.Update 
'	end if
'	rsstygroup.close
'end if
Response.Redirect "CustUsers.asp?UserID=" & strProfile &"&Mssg=" & Server.URLEncode(strMssg)
%>
</BODY>
</HTML>
