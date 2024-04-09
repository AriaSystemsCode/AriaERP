<%@ Language=VBScript %>
<%Response.Buffer = true%>
<%
'*******************************************************************************
'Page Name:  SaveProfile.asp
'Date     :  03/20/2006
'Developer:  wal
'Purpose  :  Action page to save the style profile
'********************************************************************************
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<TITLE>CRM/Admin - Add New Style Profile</TITLE>

<BODY bgcolor=white leftmargin=0 topmargin=0>
<%	
'Request from variables
Dim  strCode, strName
dim cnnSQL
set cnnSQL = server.CreateObject ("ADODB.Connection")
cnnSQL.Open Application("SqlServer") 
Dim strSql, rsProfileHdr, rsProfileDt
'check if i m coming from the edit page
if Request.QueryString ("code") = "" then'add new code
	strCode = Trim(Request.Form("txtCode"))
	strName = Trim(Request.Form("txtName"))

	
	'Add user in syuuser table
	
	set rsProfileHdr = Server.CreateObject("ADODB.RecordSet")
	set rsProfileDt = Server.CreateObject("ADODB.RecordSet")
	'check if the code exists before
	on error resume next
	strSql = "Select * From styleProfileHeader Where cStyleProfileCode ='"& strCode & "'"
	rsProfileHdr.Open strSql,cnnSQL,1,3
	'Response.Write rsUser.RecordCount 
	if not rsProfileHdr.eof then
		Response.Write("<center><font color=red><B>This Code already exists, cannot update!!</font></b>")
		Response.Write("<br><a href='javascript:window.history.back();'>Back</a>")
		Response.End
	Else
		rsProfileHdr.AddNew
			rsProfileHdr("cStyleProfileCode") =  Trim(Request.Form("txtCode"))
			rsProfileHdr("cStyleProfileName") =  Trim(Request.Form("txtName"))
		rsProfileHdr.Updatebatch
		'add detail info
		rsProfileDt.Open "select * from styleProfileDetail where 1=0",cnnSQL,1,3
		session("rsResult").movefirst()
		do while not session("rsResult").eof
			rsProfileDt.AddNew
				rsProfileDt("cStyleProfileCode")= Trim(Request.Form("txtCode"))
				rsProfileDt("cStyle")= trim(session("rsResult")("style"))
				rsProfileDt("cStyleGroup")= trim(session("rsResult")("cstygroup"))
			rsProfileDt.Updatebatch 
		session("rsResult").movenext
		loop
		session("rsResult") = nothing
	End if
else'i m updating existing code
	strCode = Trim(Request.Form("txtCode"))
	strName = Trim(Request.Form("txtName"))
	
	
	set rsProfileHdr = Server.CreateObject("ADODB.RecordSet")
	set rsProfileDt = Server.CreateObject("ADODB.RecordSet")
	'check if the code exists before
	on error resume next
	'update header
	strSql = "Select * From styleProfileHeader Where cStyleProfileCode ='"& strCode & "'"
	rsProfileHdr.Open strSql,cnnSQL,1,3
	'check if i m deleting this profile
	if Request.QueryString ("del") = "" then'not deleting
		rsProfileHdr("cStyleProfileName") =  Trim(Request.Form("txtName"))
		rsProfileHdr.Update
		'add detail info
		rsProfileDt.Open "select * from styleProfileDetail Where cStyleProfileCode ='"& strCode & "'",cnnSQL,1,3
		'delete existing recocrds
		do while not rsProfileDt.eof
			rsProfileDt.delete()
			rsProfileDt.update
		rsProfileDt.movenext
		loop
		session("rsResult").movefirst()
		do while not session("rsResult").eof'add all selected records
			rsProfileDt.AddNew
				rsProfileDt("cStyleProfileCode")= Trim(Request.Form("txtCode"))
				rsProfileDt("cStyle")= trim(session("rsResult")("style"))
				rsProfileDt("cStyleGroup")= trim(session("rsResult")("cstygroup"))
			rsProfileDt.Updatebatch 
		session("rsResult").movenext
		loop
		session("rsResult") = nothing

	else'deleting
		'check if this profile is not used by any user
		dim rsChk
		set rsChk = server.CreateObject ("adodb.recordset")
		
		rsChk.Open "select * from syuuser where cStyleProfileCode='" & Trim(Request.Form("txtCode")) &"'",cnnSQL,1,3
		if rsChk.EOF then'not exist then delet
			rsProfileHdr.delete
			rsProfileHdr.update%>
			<div=center>	
			<font face =arial size=2><b>The profile is deleted!</b>
			<br><a href="default.asp">Back to Main page</a></font>
		
		<%Response.End 
		else'exist cannot delete
			Response.Write("<center><font color=red><B>This Profile Code is used by one or more User cannot delete!!</font></b>")
			Response.Write("<br><a href='javascript:window.history.back();'>Back</a>")
			Response.End
		end if
	end if
end if
%>
<%'if err.Description <> "" then
	'Response.Write err.Description
	'Response.End 
%>
<%'else%>
<div=center>	
	<font face =arial size=2><b>Your Profile is saved!</b>
	<br><a href="default.asp">Back to Main page</a></font>
<%'end if%>
</BODY>
</HTML>
