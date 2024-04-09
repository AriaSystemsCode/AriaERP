<%@LANGUAGE="VBSCRIPT" CODEPAGE="1252"%>
<!--#include file="Connections/cnConn.asp" -->
<%
if Request.QueryString("strUserType") = "" then 
	strUserType= "C"
else 
	strUserType= Request.QueryString("strUserType")
end if
'wal_131300 get the price codes
dim rsPrice
set rsPrice  = server.CreateObject ("ADODB.recordset")
dim cnnDB
set cnnDB=server.CreateObject("ADODB.connection")
cnnDB.Open Application("DataConnectionString")

' Get the Currency of this Company
CompanyID = Session("CompanyID")

strgetCompanyCode = "select CcurrCode  from Syccomp where Ccomp_id = '" & Trim(CompanyID) & "'"
	
set rsgetCompanyCode = server.CreateObject("adodb.recordset")
set rsgetCurrency = server.CreateObject("adodb.recordset")
set rsgetCurrencyAlign = server.CreateObject("adodb.recordset")
set SysConn = server.CreateObject("adodb.connection")
SysConn.Open Application("SystemConnectionString")

rsgetCompanyCode.Open strgetCompanyCode , SysConn ,2 ,4 
if not rsgetCompanyCode.EOF then
	rsPrice.Open "select * from cstprich where cCurrcod='" &Trim(rsgetCompanyCode.Fields("CcurrCode").Value)& "' order by priccode",cnnDB,1,3
else
	rsPrice.Open "select * from cstprich order by priccode",cnnDB,1,3
end if

' *** Edit Operations: declare variables
Dim MM_editAction
Dim MM_abortEdit
Dim MM_editQuery
Dim MM_editCmd

Dim MM_editConnection
Dim MM_editTable
Dim MM_editRedirectUrl
Dim MM_editColumn
Dim MM_recordId

Dim MM_fieldsStr
Dim MM_columnsStr
Dim MM_fields
Dim MM_columns
Dim MM_typeArray
Dim MM_formVal
Dim MM_delim
Dim MM_altVal
Dim MM_emptyVal
Dim MM_i

Dim objPriv


'*****************************************************************************************************************		
if Request("cUserActivated") = "yes" then 'Activated then we will update only

	MM_editAction = CStr(Request.ServerVariables("SCRIPT_NAME"))
	If (Request.QueryString <> "") Then
	  MM_editAction = MM_editAction & "?" & Request.QueryString
	End If

	' boolean to abort record edit
	MM_abortEdit = false

	' query string to execute
	MM_editQuery = ""
	%>
	<%
	' *** Update Record: set variables

	If (CStr(Request("MM_update")) = "form1" And CStr(Request("MM_recordId")) <> "") Then

	  MM_editConnection = MM_cnConn_STRING
	  MM_editTable = "dbo.syuuser"
	  MM_editColumn = "cuser_id"
	  MM_recordId = "'" + Request.Form("MM_recordId") + "'"
	  
	  If Trim(Request("Password")) = "" Then
			MM_fieldsStr  = "User_ID|value|User_Name|value|Email|value|UsrLvl|value"
			MM_columnsStr = "cuser_id|',none,''|cusr_name|',none,''|cwe_mail|',none,''|cusr_levl|',none,''"
	  Else
			'prepare the password
			'Dim objPriv
			Set objPriv = Server.CreateObject("AriaWebSecurity.Privileges")
			strPWD = objPriv.GetPassword(ucase(Request("Password")))
				
			MM_fieldsStr  = "User_ID|value|User_Name|value|Email|value|Password|value|UsrLvl|value"
			MM_columnsStr = "cuser_id|',none,''|cusr_name|',none,''|cwe_mail|',none,''|cusr_pass|',"& strPWD &",''|cusr_levl|',none,''"
	  End If
	  'check if i m updating a customer user then add condtion on profile
	  if request("Profile") <> "" then
		 MM_fieldsStr  = MM_fieldsStr & "|profile|value" 
	     MM_columnsStr = MM_columnsStr & "|[profile]|', none,'"& request("Profile") &"' "
	  end if
	  'wal_131300 add new fields style profile and price code update case multi user login
	  'add values of style profile and price code
	  if strUserType = "C" and trim(session("CustomerLoginUsing")) = "User" then
		'if Request("txtStyProfile") <> "" then
			MM_fieldsStr  = MM_fieldsStr & "|cStyleProfileCode|value" 
			MM_columnsStr = MM_columnsStr & "|[cStyleProfileCode]|', none,'"& request("txtStyProfile") &"' "
		'end if
		'if Request("lstPrice") <> "" then
			MM_fieldsStr  = MM_fieldsStr & "|priccode|value" 
			MM_columnsStr = MM_columnsStr & "|[priccode]|', none,'"& request("lstPrice") &"' "
		'end if
		'add value for defualt store
		'MM_fieldsStr  = MM_fieldsStr & "|cStore|value" 
		'MM_columnsStr = MM_columnsStr & "|[cStore]|', none,'"& request("txtStore") &"' "
	  end if
	  ' create the MM_fields and MM_columns arrays
	  MM_fields = Split(MM_fieldsStr, "|")
	  MM_columns = Split(MM_columnsStr, "|")
		  
	  ' set the form values
	  For MM_i = LBound(MM_fields) To UBound(MM_fields) Step 2
	    MM_fields(MM_i+1) = CStr(Request.Form(MM_fields(MM_i)))
	  Next
	  
	  'check where i m coming from to return to the same page
	  if request("Profile") = "" then
		MM_editRedirectUrl = "Users.asp"
		' append the query string to the redirect URL
		If (MM_editRedirectUrl <> "" And Request.QueryString <> "") Then
		  If (InStr(1, MM_editRedirectUrl, "?", vbTextCompare) = 0 And Request.QueryString <> "") Then
		    MM_editRedirectUrl = MM_editRedirectUrl & "?" & Request.QueryString
		  Else
		    MM_editRedirectUrl = MM_editRedirectUrl & "&" & Request.QueryString
		  End If
		End If
	  else
		MM_editRedirectUrl = "CustUsers.asp?UserID=" &request("Profile")
	  end if
	  

	End If
	%>
	<%
	' *** Update Record: construct a sql update statement and execute it

	If (CStr(Request("MM_update")) <> "" And CStr(Request("MM_recordId")) <> "") Then

	  ' create the sql update statement
	  MM_editQuery = "update " & MM_editTable & " set "
	  For MM_i = LBound(MM_fields) To UBound(MM_fields) Step 2
	    MM_formVal = MM_fields(MM_i+1)
	    MM_typeArray = Split(MM_columns(MM_i+1),",")
	    MM_delim = MM_typeArray(0)
	    If (MM_delim = "none") Then MM_delim = ""
	    MM_altVal = MM_typeArray(1)
	    If (MM_altVal = "none") Then MM_altVal = ""
	    MM_emptyVal = MM_typeArray(2)
	    If (MM_emptyVal = "none") Then MM_emptyVal = ""
	    If (MM_formVal = "") Then
	      MM_formVal = MM_emptyVal
	    Else
	      If (MM_altVal <> "") Then
	        MM_formVal = MM_altVal
	      ElseIf (MM_delim = "'") Then  ' escape quotes
	        MM_formVal = "'" & Replace(MM_formVal,"'","''") & "'"
	      Else
	        MM_formVal = MM_delim + MM_formVal + MM_delim
	      End If
	    End If
	    If (MM_i <> LBound(MM_fields)) Then
	      MM_editQuery = MM_editQuery & ","
	    End If
	    MM_editQuery = MM_editQuery & MM_columns(MM_i) & " = " & MM_formVal
	  Next
	  'wal_check on profile condition
	  if request("profile") = "" then
		MM_editQuery = MM_editQuery & " where " & MM_editColumn & " = " & MM_recordId
	  else
		MM_editQuery = MM_editQuery & " where " & MM_editColumn & " = " & MM_recordId & " and profile='"&  trim(request("profile")) &"'"
	  end if
'Response.Write MM_editQuery
	  If (Not MM_abortEdit) Then
	    ' execute the update
	    Set MM_editCmd = Server.CreateObject("ADODB.Command")
	    MM_editCmd.ActiveConnection = MM_editConnection
	    MM_editCmd.CommandText = MM_editQuery
	    'Response.Write MM_editQuery
	    'Response.End 
	    MM_editCmd.Execute
	    MM_editCmd.ActiveConnection.Close
		'wal_check the warehouse code case customer user
		'wal_check to add warehouse case customer user
	  if strUserType = "C" then
		'if request("txtWareCode") <> ""  or Request.Form ("txtStyGroup") <> "" then
			strsql="select * from customerwarecode where "
			'check of login type to know the customer condition
			if request("profile") = "" then
				strsql = strsql & "ccustid='"& trim (Request("User_ID")) &"'"
			else
				strsql = strsql & "ccustid='"& trim (Request("profile")) &"'"
			end if
			set rsware = server.CreateObject ("ADODB.Recordset")
			rsware.source = strsql
			rsware.ActiveConnection = MM_editConnection
			rsware.CursorType = 1
			rsware.LockType  = 3
			rsware.open()
			if rsware.eof then'add new record for the customer
				if request("txtWareCode") <> ""  or Request.Form ("txtStyGroup") <> "" then
					rsware.AddNew
					rsware("cwarecode") = Request.Form ("txtWareCode")
					'wal_130731 save the style group selected per user
					if trim(Request.Form ("txtStyGroup")) <> "" then
						rsware("cstylegroup") =  Request.Form ("txtStyGroup")
					end if
					if request("profile") = "" then
						rsware("ccustid") = Request("User_ID")
					else
						rsware("ccustid") = Request("profile")
						rsware("cUserid") = Request("User_ID")
					end if
					rsware.Update 
					rsware.close
				end if
			else'update existing value
				rsware("cwarecode") =  Request.Form ("txtWareCode")
				'wal_130731 save the style group selected per user
				'if trim(Request.Form ("txtStyGroup")) <> "" then
				rsware("cstylegroup") =  Request.Form ("txtStyGroup")
				'end if
				rsware.Update 
				rsware.close
			end if
			
	end if
		'wal_130731 check style group [start]
		
		'wal_130731 check style group [end]
		'Response.End 
	    If (MM_editRedirectUrl <> "") Then
	      Response.Redirect(MM_editRedirectUrl)
	    End If
	  End If

	End If
	
'*****************************************************************************************************************	
else 'Not Activated then we will Add it  *************************************************************************

	MM_editAction = CStr(Request.ServerVariables("SCRIPT_NAME"))
	If (Request.QueryString <> "") Then
	  MM_editAction = MM_editAction & "?" & Request.QueryString
	End If

	' boolean to abort record edit
	MM_abortEdit = false

	' query string to execute
	MM_editQuery = ""
	%>
	<%
	' *** Redirect if username exists
	MM_flag="MM_insert"
	If (CStr(Request(MM_flag)) <> "") Then
	  MM_dupKeyRedirect="Users.asp"
	  MM_rsKeyConnection=MM_cnConn_STRING
	  MM_dupKeyUsernameValue = CStr(Request.Form("User_ID"))
	  MM_dupKeySQL="SELECT cuser_id FROM dbo.syuuser WHERE cuser_id='" & MM_dupKeyUsernameValue & "'"
	  MM_adodbRecordset="ADODB.Recordset"
	  set MM_rsKey=Server.CreateObject(MM_adodbRecordset)
	  MM_rsKey.ActiveConnection=MM_rsKeyConnection
	  MM_rsKey.Source=MM_dupKeySQL
	  MM_rsKey.CursorType=0
	  MM_rsKey.CursorLocation=2
	  MM_rsKey.LockType=3
	  MM_rsKey.Open
	  If Not MM_rsKey.EOF Or Not MM_rsKey.BOF Then 
	    ' the username was found - can not add the requested username
	    MM_qsChar = "?"
	    If (InStr(1,MM_dupKeyRedirect,"?") >= 1) Then MM_qsChar = "&"
	    MM_dupKeyRedirect = MM_dupKeyRedirect & MM_qsChar & "requsername=" & MM_dupKeyUsernameValue
	    Response.Redirect(MM_dupKeyRedirect)
	  End If
	  MM_rsKey.Close
	End If
	%>
	<%
	' *** Insert Record: set variables

	If (CStr(Request("MM_insert")) = "form1") Then

	  'prepare the password
	  'Dim objPriv
	  Set objPriv = Server.CreateObject("AriaWebSecurity.Privileges")
	  strPWD = objPriv.GetPassword(ucase(Request("Password")))

	  MM_editConnection = MM_cnConn_STRING
	  MM_editTable = "dbo.syuuser"
	  MM_editRedirectUrl = "Users.asp"
	  'MM_fieldsStr  = "User_ID|value|User_Name|value|Email|value|Password|value|UsrLvl|value"
	  'MM_columnsStr = "cuser_id|',none,''|cusr_name|',none,''|cwe_mail|',none,''|cusr_pass|',"& strPWD &",''|cusr_levl|',none,''"
	  MM_fieldsStr  = "User_ID|value|User_Name|value|Email|value|Password|value|UsrLvl|value|profile|value" 
	  MM_columnsStr = "cuser_id|',none,''|cusr_name|',none,''|cwe_mail|',none,''|cusr_pass|',"& strPWD &",''|cusr_levl|',none,'',''|[profile]|', none,'"& Request("strUserType") &"' "
	  
	  ' create the MM_fields and MM_columns arrays
	  MM_fields = Split(MM_fieldsStr, "|")
	  MM_columns = Split(MM_columnsStr, "|")
	  
	  ' set the form values
	  For MM_i = LBound(MM_fields) To UBound(MM_fields) Step 2
			MM_fields(MM_i+1) = CStr(Request.Form(MM_fields(MM_i)))
	  Next

	  ' append the query string to the redirect URL
	  If (MM_editRedirectUrl <> "" And Request.QueryString <> "") Then
	    If (InStr(1, MM_editRedirectUrl, "?", vbTextCompare) = 0 And Request.QueryString <> "") Then
	      MM_editRedirectUrl = MM_editRedirectUrl & "?" & Request.QueryString
	    Else
	      MM_editRedirectUrl = MM_editRedirectUrl & "&" & Request.QueryString
	    End If
	  End If

	End If
	%>
	<%
	  'wal_check to add warehouse case customer user
	  if strUserType = "C" then
		if Request.Form ("txtWareCode") <> "" or Request.Form ("txtStyGroup") <> "" then
			set rsWare  = server.CreateObject ("ADODB.Recordset")
			rsware.Source  = "select * from customerwarecode where 1=0"
			rsware.ActiveConnection = MM_editConnection
			rsware.CursorType = 1
			rsware.LockType  = 3
			rsware.Open 
			'add new record for the customer
			rsware.AddNew
				rsware("cwarecode") =  Request.Form ("txtWareCode")
				rsware("ccustid") = Request.Form ("User_ID")
				'wal_130731 save the style group selected per user
				if trim(Request.Form ("txtStyGroup")) <> "" then
					rsware("cstylegroup") =  Request.Form ("txtStyGroup")
				end if
			rsware.Update 
			rsware.close
		end if
	  end if
	%>
	<%
	' *** Insert Record: construct a sql insert statement and execute it

	Dim MM_tableValues
	Dim MM_dbValues

	If (CStr(Request("MM_insert")) <> "") Then

	  ' create the sql insert statement
	  MM_tableValues = ""
	  MM_dbValues = ""
	  For MM_i = LBound(MM_fields) To UBound(MM_fields) Step 2
	    MM_formVal = MM_fields(MM_i+1)
	    MM_typeArray = Split(MM_columns(MM_i+1),",")
	    MM_delim = MM_typeArray(0)
	    
	    If (MM_delim = "none") Then MM_delim = ""
	    MM_altVal = MM_typeArray(1)
	    
	    If (MM_altVal = "none") Then MM_altVal = ""
	    MM_emptyVal = MM_typeArray(2)
	    
	    If (MM_emptyVal = "none") Then MM_emptyVal = ""
	    If (MM_formVal = "") Then
	      MM_formVal = MM_emptyVal
	    Else
	      If (MM_altVal <> "") Then
	        MM_formVal = MM_altVal
	      ElseIf (MM_delim = "'") Then  ' escape quotes
	        MM_formVal = "'" & Replace(MM_formVal,"'","''") & "'"
	      Else
	        MM_formVal = MM_delim + MM_formVal + MM_delim
	      End If
	    End If
	    If (MM_i <> LBound(MM_fields)) Then
	      MM_tableValues = MM_tableValues & ","
	      MM_dbValues = MM_dbValues & ","
	    End If
	    MM_tableValues = MM_tableValues & MM_columns(MM_i)
	    MM_dbValues = MM_dbValues & MM_formVal
	  Next
	  MM_editQuery = "insert into " & MM_editTable & " (" & MM_tableValues & ") values (" & MM_dbValues & ")"

	  If (Not MM_abortEdit) Then
	    'execute the insert
	    Set MM_editCmd = Server.CreateObject("ADODB.Command")
	    MM_editCmd.ActiveConnection = MM_editConnection
	    MM_editCmd.CommandText = MM_editQuery
	    Response.Write(MM_editQuery)
'	    Response.End 	    
	    MM_editCmd.Execute
	    MM_editCmd.ActiveConnection.Close

	    If (MM_editRedirectUrl <> "") Then
	      Response.Redirect(MM_editRedirectUrl)
	    End If
	  End If

	End If

end if	
'*****************************************************************************************************************
%>
<%
Dim rsUser__MMColParam
rsUser__MMColParam = "1"
If (Request.QueryString("UserID") <> "") Then 
  rsUser__MMColParam = Request.QueryString("UserID")
End If
%>
<%
Dim rsUser
Dim rsUser_numRows

Set rsUser = Server.CreateObject("ADODB.Recordset")
rsUser.ActiveConnection = MM_cnConn_STRING

'rsUser.Source = "SELECT cuser_id, cusr_name, cusr_pass, cwe_mail, cusr_levl FROM dbo.syuuser WHERE cuser_id = '" + Replace(rsUser__MMColParam, "'", "''") + "'"
strSQL = ""
select case strUserType
case "C" 'Customers
	'wal_check to add warehouse if exists---also check on style group
	set rsWare   = server.CreateObject ("ADODB.Recordset")
	
	'check which login type i m using
	if trim(session("CustomerLoginUsing")) = "User" then
		strSql = "Select * From syuuser Where cuser_id ='"& Trim(request("UserID")) & "' and profile='"& trim(request("profile")) &"' ORDER BY cusr_name ASC"
		rsware.Source  = "select * from customerwarecode where ccustid='" & trim(request("profile")) & "'"
		'rsStyGrp.source = "select * from customerwarecode where ccustid='" & trim(request("profile")) & "' and cuserid ='"& Trim(request("UserID")) & "'"
	else
		strSQL = strSQL + "SELECT Fox.Account cuser_id, Fox.Btname cusr_name, syuuser.cwe_mail, syuuser.profile, syuuser.cusr_levl "
		strSQL = strSQL + "FROM dbo.syuuser "
		strSQL = strSQL + "RIGHT OUTER JOIN OPENROWSET('MSDASQL', '"
		strSQL = strSQL + Application("DataConnectionString")
		strSQL = strSQL + "', 'Select * From customer') as Fox ON syuuser.cuser_id = Fox.Account COLLATE Latin1_General_CI_AS "
		'strSQL = strSQL + "', 'Select * From customer') as Fox ON syuuser.cuser_id = Fox.Account "
		strSQL = strSQL + "WHERE (Fox.type = 'M') "
		strSQL = strSQL + "and (Fox.Account = '"& Replace(rsUser__MMColParam, "'", "''")  &"') "
		strSQL = strSQL + "ORDER BY Fox.Btname ASC"	
		rsware.Source  = "select * from customerwarecode where ccustid='" & Trim(request("UserID")) & "'"
		'rsStyGrp.source = "select * from customerwarecode where cuserid ='"& Trim(request("UserID")) & "'"
	end if
	
	rsware.ActiveConnection = MM_cnConn_STRING
	rsware.CursorType = 1
	rsware.LockType  = 3
	rsware.Open 
	if not rsware.eof then
		if not isnull(rsware("cwarecode"))then 
			strWareCode = rsware("cwarecode")
		end if
		'wal_130731 get style group too
		if not isnull(rsware("cStyleGroup")) then
			strStyGroup = rsware("cStyleGroup")
		end if
	end if
	rsware.close
	'wal_130731 get style group
	'rsStyGrp.ActiveConnection = MM_cnConn_STRING
	'rsStyGrp.CursorType = 1
	'rsStyGrp.LockType  = 3
	'rsStyGrp.Open 
	'if not rsStyGrp.eof then
	'	strStyGroup = rsStyGrp("cStyleGroup")
	'end if
	'rsStyGrp.close
case "S" 'Sales
	strSQL = strSQL + "SELECT Fox.cuser_id, Fox.cusr_name, dbo.syuuser.cwe_mail, profile, syuuser.cusr_levl "
	strSQL = strSQL + "FROM dbo.syuuser "
	strSQL = strSQL + "RIGHT OUTER JOIN OPENROWSET('MSDASQL', '"
	strSQL = strSQL + Application("SystemConnectionString")
	strSQL = strSQL + "', 'Select * From syuuser') as Fox ON dbo.syuuser.cuser_id = Fox.cuser_id  COLLATE Latin1_General_CI_AS "
	'strSQL = strSQL + "', 'Select * From syuuser') as Fox ON dbo.syuuser.cuser_id = Fox.cuser_id "
	strSQL = strSQL + "WHERE (Fox.cuser_id = '"& Replace(rsUser__MMColParam, "'", "''")  &"') "
	strSQL = strSQL + "ORDER BY Fox.cusr_name ASC"		
case "A" 'Admin
	strSQL = strSQL + "SELECT cuser_id, cusr_name, cwe_mail, syuuser.cusr_levl, profile FROM dbo.syuuser "
	strSQL = strSQL + "where cuser_id ='"& Replace(rsUser__MMColParam, "'", "''")  &"' "
	strSQL = strSQL + "ORDER BY cusr_name ASC"
end select
 
rsUser.Source = strSQL
'Response.Write strSQL & "<hr>"
'Response.End 
rsUser.CursorType = 0
rsUser.CursorLocation = 2
rsUser.LockType = 1
rsUser.Open()

rsUser_numRows = 0
%>
<html>
<head>
<title>Edit/Activate User</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<link href="../../Images/<%=session("theme")%>/Common.css" rel="stylesheet" type="text/css">
<script language="JavaScript" type="text/JavaScript">
<!--
function MM_findObj(n, d) { //v4.01
  var p,i,x;  if(!d) d=document; if((p=n.indexOf("?"))>0&&parent.frames.length) {
    d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}
  if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];
  for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document);
  if(!x && d.getElementById) x=d.getElementById(n); return x;
}

function MM_validateForm() { //v4.0
  var i,p,q,nm,test,num,min,max,errors='',args=MM_validateForm.arguments;
  for (i=0; i<(args.length-2); i+=3) { test=args[i+2]; val=MM_findObj(args[i]);
    if (val) { nm=val.name; if ((val=val.value)!="") {
      if (test.indexOf('isEmail')!=-1) { p=val.indexOf('@');
        if (p<1 || p==(val.length-1)) errors+='- '+nm+' must contain an e-mail address.\n';
      } else if (test!='R') { num = parseFloat(val);
        if (isNaN(val)) errors+='- '+nm+' must contain a number.\n';
        if (test.indexOf('inRange') != -1) { p=test.indexOf(':');
          min=test.substring(8,p); max=test.substring(p+1);
          if (num<min || max<num) errors+='- '+nm+' must contain a number between '+min+' and '+max+'.\n';
    } } } else if (test.charAt(0) == 'R') errors += '- '+nm+' is required.\n'; }
  } if (errors) alert('The following error(s) occurred:\n'+errors);
  document.MM_returnValue = (errors == '' && CheckPassWord());
}
function CheckPassWord()
{
	if(document.form1.Password.value == document.form1.Confirm_Password.value)
	{
		return true;
	}
	else
	{
		alert('Password not identical')
		return false;
	}
}
function openwindow(strFile) 
{  
	window.open(strFile,'','toolbar=no,status=no,scrollbars=no,location=no,resizable=yes,menubar=no,directories=no,top=' + ((window.screen.height-500)/2) + ',left=' + ((window.screen.width-800)/2) + ',width=800,height=500')
}
//-->
</script>
</head>

<body bgcolor="#3366CC" text="#FFFFF0">
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
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/DCC logo.jpg" WIDTH=60 HEIGHT=59></TD>
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

<table width="95%" border="0" align="center">
  <tr> 
    <td> 
    <%if trim(session("CustomerLoginUsing")) = "User" then%>
		<form action="<%=MM_editAction%>?Profile=<%=request("Profile")%>" method="POST" name="form1" onSubmit="MM_validateForm('User_ID','','R','Email','','NisEmail','Password','','R','Confirm_Password','','R');return document.MM_returnValue">
	<%else%>
		<form action="<%=MM_editAction%>" method="POST" name="form1" onSubmit="MM_validateForm('User_ID','','R','Email','','NisEmail','Password','','R','Confirm_Password','','R');return document.MM_returnValue">
	<%end if%>
        <table width="50%" border="0" align="center">
		  <tr> 
			<td colspan="3" class="Dark_Cell"><div align="right"><a href="Users.asp"><font color="#FFFFFF">
			Back to Users' Security</font></a></div></td>
		  </tr>
		
          <tr> 
             <% if isnull(rsUser.Fields.Item("cusr_levl").Value) then 'Not Activated%>
	            <td colspan="3" class='Title'>Activate User</td>
             <% else 'Activated%>	            
   	            <td colspan="3" class='Title'>Edit User</td>
             <% end if %>             
          </tr>
          <tr> 
            <td width="24%" class='Dark_Cell'>ID</td>
            <td colspan="2" class='Light_Cell'> <input name="User_ID" type="text" id="User_ID2" value="<%= Trim((rsUser.Fields.Item("cuser_id").Value)) %>" size="10" maxlength="10" Readonly>
              <% if isnull(rsUser.Fields.Item("cusr_levl").Value) then 'Not Activated%>
              <%else%>
			      <%If (CStr((rsUser.Fields.Item("cusr_levl").Value)) = CStr("O")) Then%>
					<a href="UserPriv.asp?Profile=<%=request("Profile")%>&lstUsers=<%= Trim((rsUser.Fields.Item("cuser_id").Value)) %>&strUserType=<%=strUserType%>">Privileges...</a>              
	              <%end if%>
              <%end if%>              
              </td>
          </tr>
          <tr> 
            <td class='Dark_Cell'>Name</td>
            <td colspan="2" class='Light_Cell'><input name="User_Name" type="text" id="User_Name" value="<%= Trim((rsUser.Fields.Item("cusr_name").Value)) %>" size="30" maxlength="35">*</td>
          </tr>
          <tr> 
            <td class='Dark_Cell'>Email</td>
            <td colspan="2" class='Light_Cell'><input name="Email" type="text" id="Email" value="<%= Trim((rsUser.Fields.Item("cwe_mail").Value)) %>" size="30" maxlength="200"></td>
          </tr>
          <!--wal_039385 add the filed for warehouse case customer-->
          <%if strUserType = "C" then%>
			<tr> 
			  <td class='Dark_Cell'>Ware house</td>
			  <td colspan="2" class='Light_Cell'>
					<input name="txtWareCode" type="text" id="txtWareCode" value="<%=trim(strWareCode)%>" size="20" maxlength="6">
					<input type=button value="Select" onclick="openwindow('getWareCode.asp');">
			  </td>
			</tr>
			<!--wal_130731 add the filed for style group-->
			<tr> 
			  <td class='Dark_Cell'>Style Group</td>
			  <td colspan="2" class='Light_Cell'>
			  	<input name="txtStyGroup" type="text" id="txtStyGroup" value="<%=strStyGroup%>" size="20" maxlength="6">
			  	<input type=button value="Select" onclick="openwindow('getStyGroup.asp');" id=button1 name=button1>
			  </td>
			</tr>
			<!--wal_131300 add the filed for style profile and price code case multi user login-->
		<%if trim(session("CustomerLoginUsing")) = "User" then %>
			<tr> 
			  <td class='Dark_Cell'>Style Profile</td>
			  <td colspan="2" class='Light_Cell'>
			  	<input name="txtStyProfile" type="text" id="txtStyProfile" value="<%=Trim((rsUser.Fields.Item("cStyleProfileCode").Value)) %>" size="20" maxlength="10">
			  	<input type=button value="Select" onclick="openwindow('getStyProfile.asp');" id=button1 name=button1>
			  </td>
			</tr>
			<tr> 
			  <td class='Dark_Cell'>Price Code</td>
			  <td colspan="2" class='Light_Cell'>
				<select name="lstPrice">
					<option value="">--None--
					<%if not rsPrice.EOF then
						do while not rsPrice.EOF%>
							<option value="<%=rsPrice("priccode")%>" <%if Trim((rsUser.Fields.Item("priccode").Value)) = trim(rsPrice("priccode")) then%>selected<%end if%> ><%=rsPrice("priccode")%>
					<%	rsPrice.MoveNext 
						loop
					 else%>
						<option value="">None
					<% end if%>
				</select>
			  	<!--input name="txtStyPrice" type="text" id="txtStyPrice" value="<%=Trim((rsUser.Fields.Item("priccode").Value)) %>" size="20" maxlength="6" readonly>
			  	<input type=button value="Select" onclick="openwindow('getStyPrice.asp');" id=button1 name=button1-->
			  </td>
			</tr>
		  <%end if
		  end if%>
          <tr> 
            <td class='Dark_Cell'>Password</td>
            <td colspan="2" class='Light_Cell'><input name="Password" type="password" id="Password" size="10" maxlength="8">*</td>
          </tr>
          <tr> 
            <td class='Dark_Cell'>Confirm Password</td>
            <td colspan="2" class='Light_Cell'><input name="Confirm_Password" type="password" id="Confirm_Password" size="10" maxlength="8">* 
            </td>
          </tr>
          <tr> 
            <td nowrap class='Dark_Cell'>Level</td>

            <% if isnull(rsUser.Fields.Item("cusr_levl").Value) then 'Not Activated%>
                  <td width="35%" nowrap class='Light_Cell'> <font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
				  <input type="radio" name="UsrLvl" value="A" checked>Administrator</font></td>
				<td width="41%" nowrap class='Light_Cell'> <font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
				  <input "UsrLvl" type="radio" name="UsrLvl" value="O" >
				  Operator</font></td>
			<%else 'activated%>
				<td width="35%" nowrap class='Light_Cell'> <font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 			
				<input <%If (CStr((rsUser.Fields.Item("cusr_levl").Value)) = CStr("A")) Then Response.Write("CHECKED") : Response.Write("")%> type="radio" name="UsrLvl" value="A">
				  Administrator</font></td>
				<td width="41%" nowrap class='Light_Cell'> <font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
				  <input <%If (CStr((rsUser.Fields.Item("cusr_levl").Value)) = CStr("O")) Then Response.Write("CHECKED") : Response.Write("")%> name="UsrLvl" type="radio" value="O" >
				  Operator</font></td>
			<%end if%>				  
          </tr>
          <tr> 
            <td colspan="3" class='Light_cell'>(*) Indicates Required Field</td></tr><tr>
              <td colspan="3"><div align="right">                  
                </div></td>
           </tr>          
            <td colspan="3"><div align="right"> 
                 <% if isnull(rsUser.Fields.Item("cusr_levl").Value) then 'Not Activated%>
	                <input name="Submit" type="submit" value="Activate">
                 <% else 'Activated %>
   	                <input name="Submit" type="submit" value="Submit">
                 <% end if %>
                <input type="reset" name="Submit2" value="Reset">
              </div></td>
          </tr>
        </table>
		<%if Request("cUserActivated") = "yes" then 'Activated then we will update only%>
			<input type="hidden" name="MM_update" value="form1">
		<%else%>
	        <input type="hidden" name="MM_insert" value="form1">
		<%end if%>					
		<input type="hidden" name="MM_recordId" value="<%= rsUser.Fields.Item("cuser_id").Value %>">		
        <input type="hidden" name="strUserType" value="<%If Request("strUserType") <>"" then Response.Write strUserType%>">
      </form></td>
  </tr>
</table>
</body>
</html>
<%
rsUser.Close()
Set rsUser = Nothing
%>
