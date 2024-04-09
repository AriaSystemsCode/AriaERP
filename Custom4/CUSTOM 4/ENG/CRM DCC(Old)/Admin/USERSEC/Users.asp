<%@LANGUAGE="VBSCRIPT" CODEPAGE="1252"%>

<%

'wma
If Session("DBType") = "ORACLE" then
	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=OraOLEDB.Oracle;Data Source="&Trim(session("SQLServer"))&";user id=" & Trim(Session("SqlUserName")) & ";password=" & Trim(Session("SqlPassWord")) & ""	
Else
	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog="&Trim(session("DBName"))&";Data Source="&Trim(session("SQLServer"))&";uid=" & Trim(Session("SqlUserName")) & ";pwd=" & Trim(Session("SqlPassWord")) & ""
End If
	
MM_cnConn_STRING = Application("SqlServer")	
	
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
  Dim objPriv
  Set objPriv = Server.CreateObject("AriaWebSecurity.Privileges")
  strPWD = objPriv.GetPassword(Request("Password"))
  


  MM_editConnection = MM_cnConn_STRING
  MM_editTable = "dbo.syuuser"
  MM_editRedirectUrl = "Users.asp"
  'MM_fieldsStr  = "User_ID|value|User_Name|value|Email|value|Password|value|UsrLvl|value"
  'MM_columnsStr = "cuser_id|',none,''|cusr_name|',none,''|cwe_mail|',none,''|cusr_pass|',"& strPWD &",''|cusr_levl|',none,''"
   MM_fieldsStr  = "User_ID|value|User_Name|value|Email|value|Password|value|UsrLvl|value|profile|value" 
   MM_columnsStr = "cuser_id|',none,''|cusr_name|',none,''|cwe_mail|',none,''|cusr_pass|',"& strPWD &",''|cusr_levl|',none,'',''|[profile]|', none,'A' "

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
    ' execute the insert
    'Response.Write(MM_editQuery)
    'Response.End 
    Set MM_editCmd = Server.CreateObject("ADODB.Command")
    MM_editCmd.ActiveConnection = MM_editConnection
    MM_editCmd.CommandText = MM_editQuery
    MM_editCmd.Execute
    MM_editCmd.ActiveConnection.Close

    If (MM_editRedirectUrl <> "") Then
      Response.Redirect(MM_editRedirectUrl)
    End If
  End If

End If

%>
<%
Dim rsUsers
Dim rsUsers_numRows

Set rsUsers = Server.CreateObject("ADODB.Recordset")
rsUsers.ActiveConnection = MM_cnConn_STRING 
if Request("strUserType") = "" then 
	strUserType = "C"
else 
	strUserType= Request("strUserType")
end if
'rsUsers.Source = "SELECT cuser_id, cusr_name, cwe_mail FROM dbo.syuuser where profile ='"& strUserType &"' ORDER BY cusr_name ASC"
strSQL = ""
select case strUserType
case "C" 'Customers
	'strSQL = strSQL + "SELECT Fox.Account cuser_id, Fox.Btname cusr_name, dbo.syuuser.cwe_mail, dbo.syuuser.profile "
	strSQL = strSQL + "SELECT Fox.Account cuser_id, Fox.Btname cusr_name, syuuser.cwe_mail, syuuser.profile, syuuser.cusr_levl "	
	strSQL = strSQL + "FROM dbo.syuuser "
	strSQL = strSQL + "RIGHT OUTER JOIN OPENROWSET('MSDASQL', '"
	strSQL = strSQL + Application("DataConnectionString")
	strSQL = strSQL + "', 'Select * From customer') as Fox ON dbo.syuuser.cuser_id = Fox.Account COLLATE Latin1_General_CI_AS "
	'strSQL = strSQL + "', 'Select * From customer') as Fox ON dbo.syuuser.cuser_id = Fox.Account "
	strSQL = strSQL + "WHERE(Fox.type = 'M') and (dbo.syuuser.profile='C' or dbo.syuuser.profile='' or dbo.syuuser.profile is null) "	
	'wal_127795 check if there is a search conditon
	if Request.QueryString ("Search") = "Y" then
		'Do we have an account to search
		If Trim(Request.Form ("txtID")) <> "" Then
			strSQL = strSQL + " And Upper(Fox.account) Like '" & Trim(Ucase(Request.Form ("txtID"))) &"%'"
		End If
		'Do we have a customer name?
		If Trim(Ucase(Request("txtName"))) <> "" Then
			strSQL = strSQL + " And Upper(Fox.stName) Like '" & Trim(Ucase(Request.Form ("txtName"))) &"%'"
		End If
		'Response.Write "<font size=3>"& strsql
			
	end if
	strSQL = strSQL + "ORDER BY Fox.Btname ASC"	
case "S" 'Sales
'	strSQL = strSQL + "SELECT Fox.cuser_id, Fox.cusr_name, dbo.syuuser.cwe_mail, dbo.syuuser.profile, syuuser.cusr_levl "
'	strSQL = strSQL + "FROM dbo.syuuser "
'	strSQL = strSQL + "RIGHT OUTER JOIN OPENROWSET('MSDASQL', '"
'	strSQL = strSQL + Application("SystemConnectionString")
'	strSQL = strSQL + "', 'Select * From syuuser') as Fox ON dbo.syuuser.cuser_id = Fox.cuser_id COLLATE Latin1_General_CI_AS  "
'	'strSQL = strSQL + "', 'Select * From syuuser') as Fox ON dbo.syuuser.cuser_id = Fox.cuser_id "
'	strSQL = strSQL + "ORDER BY Fox.cusr_name ASC"		
	
	
			
	strSQL = strSQL + "SELECT Fox.cuser_id, Fox.cusr_name, dbo.syuuser.cwe_mail, dbo.syuuser.profile, "
	strSQL = strSQL + "dbo.syuuser.cusr_levl, FoxSales.repcode "
	strSQL = strSQL + "FROM OPENROWSET('MSDASQL', '"
	strSQL = strSQL + Application("DataConnectionString")		
	strSQL = strSQL + "', 'Select * From salesrep') FoxSales INNER JOIN "
	strSQL = strSQL + "OPENROWSET('MSDASQL', '"
	strSQL = strSQL + Application("SystemConnectionString")	
	strSQL = strSQL + "', 'Select * From syuuser') Fox ON FoxSales.repcode = Fox.cuser_id LEFT OUTER JOIN "
	strSQL = strSQL + "dbo.syuuser ON Fox.cuser_id COLLATE Latin1_General_CI_AS = dbo.syuuser.cuser_id "
	strSQL = strSQL + "ORDER BY Fox.cusr_name "
	
	
	
case "A" 'Admin
	strSQL = strSQL + "SELECT cuser_id, cusr_name, cwe_mail, profile,cusr_levl FROM dbo.syuuser where profile ='A' ORDER BY cusr_name ASC"
end select 

'Response.Write strSQL & "<hr>"
'Response.End 

rsUsers.Source = strSQL
rsUsers.CursorType = 0
rsUsers.CursorLocation = 2
rsUsers.LockType = 1
rsUsers.Open()

rsUsers_numRows = 0
%>
<%
Dim Repeat1__numRows
Dim Repeat1__index

Repeat1__numRows = 20
Repeat1__index = 0
rsUsers_numRows = rsUsers_numRows + Repeat1__numRows
%>
<%
'  *** Recordset Stats, Move To Record, and Go To Record: declare stats variables

Dim rsUsers_total
Dim rsUsers_first
Dim rsUsers_last

' set the record count
rsUsers_total = rsUsers.RecordCount

' set the number of rows displayed on this page
If (rsUsers_numRows < 0) Then
  rsUsers_numRows = rsUsers_total
Elseif (rsUsers_numRows = 0) Then
  rsUsers_numRows = 1
End If

' set the first and last displayed record
rsUsers_first = 1
rsUsers_last  = rsUsers_first + rsUsers_numRows - 1

' if we have the correct record count, check the other stats
If (rsUsers_total <> -1) Then
  If (rsUsers_first > rsUsers_total) Then
    rsUsers_first = rsUsers_total
  End If
  If (rsUsers_last > rsUsers_total) Then
    rsUsers_last = rsUsers_total
  End If
  If (rsUsers_numRows > rsUsers_total) Then
    rsUsers_numRows = rsUsers_total
  End If
End If
%>
<%
Dim MM_paramName 
%>
<%
' *** Move To Record and Go To Record: declare variables

Dim MM_rs
Dim MM_rsCount
Dim MM_size
Dim MM_uniqueCol
Dim MM_offset
Dim MM_atTotal
Dim MM_paramIsDefined

Dim MM_param
Dim MM_index

Set MM_rs    = rsUsers
MM_rsCount   = rsUsers_total
MM_size      = rsUsers_numRows
MM_uniqueCol = ""
MM_paramName = ""
MM_offset = 0
MM_atTotal = false
MM_paramIsDefined = false
If (MM_paramName <> "") Then
  MM_paramIsDefined = (Request.QueryString(MM_paramName) <> "")
End If
%>
<%
' *** Move To Record: handle 'index' or 'offset' parameter

if (Not MM_paramIsDefined And MM_rsCount <> 0) then

  ' use index parameter if defined, otherwise use offset parameter
  MM_param = Request.QueryString("index")
  If (MM_param = "") Then
    MM_param = Request.QueryString("offset")
  End If
  If (MM_param <> "") Then
    MM_offset = Int(MM_param)
  End If

  ' if we have a record count, check if we are past the end of the recordset
  If (MM_rsCount <> -1) Then
    If (MM_offset >= MM_rsCount Or MM_offset = -1) Then  ' past end or move last
      If ((MM_rsCount Mod MM_size) > 0) Then         ' last page not a full repeat region
        MM_offset = MM_rsCount - (MM_rsCount Mod MM_size)
      Else
        MM_offset = MM_rsCount - MM_size
      End If
    End If
  End If

  ' move the cursor to the selected record
  MM_index = 0
  While ((Not MM_rs.EOF) And (MM_index < MM_offset Or MM_offset = -1))
    MM_rs.MoveNext
    MM_index = MM_index + 1
  Wend
  If (MM_rs.EOF) Then 
    MM_offset = MM_index  ' set MM_offset to the last possible record
  End If

End If
%>
<%
' *** Move To Record: if we dont know the record count, check the display range

If (MM_rsCount = -1) Then

  ' walk to the end of the display range for this page
  MM_index = MM_offset
  While (Not MM_rs.EOF And (MM_size < 0 Or MM_index < MM_offset + MM_size))
    MM_rs.MoveNext
    MM_index = MM_index + 1
  Wend

  ' if we walked off the end of the recordset, set MM_rsCount and MM_size
  If (MM_rs.EOF) Then
    MM_rsCount = MM_index
    If (MM_size < 0 Or MM_size > MM_rsCount) Then
      MM_size = MM_rsCount
    End If
  End If

  ' if we walked off the end, set the offset based on page size
  If (MM_rs.EOF And Not MM_paramIsDefined) Then
    If (MM_offset > MM_rsCount - MM_size Or MM_offset = -1) Then
      If ((MM_rsCount Mod MM_size) > 0) Then
        MM_offset = MM_rsCount - (MM_rsCount Mod MM_size)
      Else
        MM_offset = MM_rsCount - MM_size
      End If
    End If
  End If

  ' reset the cursor to the beginning
  If (MM_rs.CursorType > 0) Then
    MM_rs.MoveFirst
  Else
    MM_rs.Requery
  End If

  ' move the cursor to the selected record
  MM_index = 0
  While (Not MM_rs.EOF And MM_index < MM_offset)
    MM_rs.MoveNext
    MM_index = MM_index + 1
  Wend
End If
%>
<%
' *** Move To Record: update recordset stats

' set the first and last displayed record
rsUsers_first = MM_offset + 1
rsUsers_last  = MM_offset + MM_size

If (MM_rsCount <> -1) Then
  If (rsUsers_first > MM_rsCount) Then
    rsUsers_first = MM_rsCount
  End If
  If (rsUsers_last > MM_rsCount) Then
    rsUsers_last = MM_rsCount
  End If
End If

' set the boolean used by hide region to check if we are on the last record
MM_atTotal = (MM_rsCount <> -1 And MM_offset + MM_size >= MM_rsCount)
%>
<%
' *** Go To Record and Move To Record: create strings for maintaining URL and Form parameters

Dim MM_keepNone
Dim MM_keepURL
Dim MM_keepForm
Dim MM_keepBoth

Dim MM_removeList
Dim MM_item
Dim MM_nextItem

' create the list of parameters which should not be maintained
MM_removeList = "&index="
If (MM_paramName <> "") Then
  MM_removeList = MM_removeList & "&" & MM_paramName & "="
End If

MM_keepURL=""
MM_keepForm=""
MM_keepBoth=""
MM_keepNone=""

' add the URL parameters to the MM_keepURL string
For Each MM_item In Request.QueryString
  MM_nextItem = "&" & MM_item & "="
  If (InStr(1,MM_removeList,MM_nextItem,1) = 0) Then
    MM_keepURL = MM_keepURL & MM_nextItem & Server.URLencode(Request.QueryString(MM_item))
  End If
Next

' add the Form variables to the MM_keepForm string
For Each MM_item In Request.Form
  MM_nextItem = "&" & MM_item & "="
  If (InStr(1,MM_removeList,MM_nextItem,1) = 0) Then
    MM_keepForm = MM_keepForm & MM_nextItem & Server.URLencode(Request.Form(MM_item))
  End If
Next

' create the Form + URL string and remove the intial '&' from each of the strings
MM_keepBoth = MM_keepURL & MM_keepForm
If (MM_keepBoth <> "") Then 
  MM_keepBoth = Right(MM_keepBoth, Len(MM_keepBoth) - 1)
End If
If (MM_keepURL <> "")  Then
  MM_keepURL  = Right(MM_keepURL, Len(MM_keepURL) - 1)
End If
If (MM_keepForm <> "") Then
  MM_keepForm = Right(MM_keepForm, Len(MM_keepForm) - 1)
End If

' a utility function used for adding additional parameters to these strings
Function MM_joinChar(firstItem)
  If (firstItem <> "") Then
    MM_joinChar = "&"
  Else
    MM_joinChar = ""
  End If
End Function
%>
<%
' *** Move To Record: set the strings for the first, last, next, and previous links

Dim MM_keepMove
Dim MM_moveParam
Dim MM_moveFirst
Dim MM_moveLast
Dim MM_moveNext
Dim MM_movePrev

Dim MM_urlStr
Dim MM_paramList
Dim MM_paramIndex
Dim MM_nextParam

MM_keepMove = MM_keepBoth
MM_moveParam = "index"

' if the page has a repeated region, remove 'offset' from the maintained parameters
If (MM_size > 1) Then
  MM_moveParam = "offset"
  If (MM_keepMove <> "") Then
    MM_paramList = Split(MM_keepMove, "&")
    MM_keepMove = ""
    For MM_paramIndex = 0 To UBound(MM_paramList)
      MM_nextParam = Left(MM_paramList(MM_paramIndex), InStr(MM_paramList(MM_paramIndex),"=") - 1)
      If (StrComp(MM_nextParam,MM_moveParam,1) <> 0) Then
        MM_keepMove = MM_keepMove & "&" & MM_paramList(MM_paramIndex)
      End If
    Next
    If (MM_keepMove <> "") Then
      MM_keepMove = Right(MM_keepMove, Len(MM_keepMove) - 1)
    End If
  End If
End If

' set the strings for the move to links
If (MM_keepMove <> "") Then 
  MM_keepMove = MM_keepMove & "&"
End If

MM_urlStr = Request.ServerVariables("URL") & "?" & MM_keepMove & MM_moveParam & "="

MM_moveFirst = MM_urlStr & "0"
MM_moveLast  = MM_urlStr & "-1"
MM_moveNext  = MM_urlStr & CStr(MM_offset + MM_size)
If (MM_offset - MM_size < 0) Then
  MM_movePrev = MM_urlStr & "0"
Else
  MM_movePrev = MM_urlStr & CStr(MM_offset - MM_size)
End If
%>

<html>
<head>
<title>Users List</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
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
		return false;
	}
}


function GetUsersType(srcForm)
{
	srcForm.action = 'Users.asp'
	srcForm.submit()
}

//-->
</script>
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
</head>

<body bgcolor="#aecae6">
<!--TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
  <TR> 
	<td>
		<div border=1 id="Layer8" style="position:absolute; left:0; top:0; width:700; height:69; z-index:35; background-image: url(image/heder.JPG); layer-background-image: url(image/heder.JPG); border: 1px none #000000">
			<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"    codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" width="700" height="69" id=ShockwaveFlash1>
    <param name=movie value="../../banner.swf">
    <param name=quality value=high>
    <embed src="../../banner.swf" quality=high pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash" type="application/x-shockwave-flash" width="750" height="69">
    </embed> 
  </object>
		</div>
	</td>
  </TR>
</TABLE><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR-->
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
  <tr>
    <td>
    <form name=frmSelectType method=post>
    <table bordercolor="#111111"  align=center border="1" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
        <tr>
          <td colspan="5" class='Dark_Cell'><div align="right"><a href="Default.asp"><font color="#FFFFFF">Back 
              to users' security page</font></a> </div></td>
        </tr>
        <tr> 
          <td colspan="5" class='Title'><div align="center"><font face="Verdana, Arial, Helvetica, sans-serif"><strong>CRM 
              Users List</strong></font></div></td>
        </tr>
        <tr> 
          <td colspan="1" class='Dark_Cell'>Users Type</td>
          <td colspan="4" class='Light_Cell'>
			<select name="strUserType" id="strUserType" onChange="GetUsersType(this.form)">
					<option value="C" <%If Trim(Request("strUserType"))= "C" Then Response.Write("SELECTED")%>>Customers.</option>			
					<option value="S" <%If Trim(Request("strUserType"))= "S" Then Response.Write("SELECTED")%>>Sales Rep.</option>
					<option value="A" <%If Trim(Request("strUserType"))= "A" Then Response.Write("SELECTED")%>>Site Admin.</option>
			</select>
          </td>
        </tr>        
        <tr> 
          <td colspan="5"><div align="center"><font color="#FFCC66"> 
              <% If Trim(Request.QueryString("requsername"))<>"" Then %>
              <font color="#FF0000" face="Verdana, Arial, Helvetica, sans-serif">User 
              <strong><%= Request.QueryString("requsername") %></strong> already 
              exists</font> 
              <% End If %>
              </font></div></td>
        </tr>
        <!--wal_127795 add a search bar case customer user type-->
        <%if strUserType = "C" then%>
			  <tr> 
				<td class='Dark_Cell'><strong>
				<font size="2" face="Verdana, Arial, Helvetica, sans-serif">ID</font></strong>
				<input type=text name=txtID>
				</td>
				<td class='Dark_Cell'><strong>
				<font size="2" face="Verdana, Arial, Helvetica, sans-serif">Name</font></strong>
				<input type=text name=txtName>
				</td>
				<td colspan="3" class='Dark_Cell'><input type=button value="Search" onclick="javascript:document.frmSelectType.action='Users.asp?search=Y';document.frmSelectType.submit();"></td>
			  </tr>
        <%end if%>
        <tr> 
          <td class='Dark_Cell'><strong><font size="2" face="Verdana, Arial, Helvetica, sans-serif">ID</font></strong></td>
          <td class='Dark_Cell'><strong><font size="2" face="Verdana, Arial, Helvetica, sans-serif">Name</font></strong></td>
          <td colspan="2" class='Dark_Cell'>Email &nbsp;</td>
          <!--td colspan="1" class='Dark_Cell'>User Level &nbsp;</td-->
          <td colspan="1" class='Dark_Cell'>Activated &nbsp;</td>
        </tr>
        <%if rsUsers.EOF then%>
			 <tr> 
			  <td colspan="5"><div align="center"><font face="Verdana, Arial, Helvetica, sans-serif"><strong>
			      No result!</strong></font></div></td>
			 </tr>
        <%end if%>
        <% 
While ((Repeat1__numRows <> 0) AND (NOT rsUsers.EOF)) 
%>
        <tr> <!--wal_127795 check case customer type adn login=multi user change rediredt page-->
			  <%if strUserType="C" and trim(session("CustomerLoginUsing")) = "User" then %>
				<td class='Light_cell'><a href="CustUsers.asp?UserID=<%=trim(rsUsers.Fields.Item("cuser_id").Value)%>&cUserActivated=<%if not IsNull(rsUsers.Fields.Item("profile").Value) then Response.Write "yes" %>&strUserType=<%=strUserType%>"><%=(rsUsers.Fields.Item("cuser_id").Value)%></a></td>
			  <%else%>
				<td class='Light_cell'><a href="EditUser.asp?UserID=<%=trim(rsUsers.Fields.Item("cuser_id").Value)%>&cUserActivated=<%if not IsNull(rsUsers.Fields.Item("profile").Value) then Response.Write "yes" %>&strUserType=<%=strUserType%>"><%=(rsUsers.Fields.Item("cuser_id").Value)%></a></td>
			  <%end if%>
			  <td class='Light_cell'><%=(rsUsers.Fields.Item("cusr_name").Value)%></td>
			  <td colspan="2" class='Light_cell'> <%=(rsUsers.Fields.Item("cwe_mail").Value)%>&nbsp;</td>
			  <!--td colspan="1" class='Light_cell'><font color="#ff0000">
			  <%if not IsNull(rsUsers.Fields.Item("profile").Value) then 
					if rsUsers.Fields.Item("cusr_levl").Value = "A" then
						Response.Write "Administrator" 
					else
						Response.Write "Operator" 					
					end if
				end if
			  %>&nbsp;</font></td-->			  
			  <td colspan="1" class='Light_cell'><font color="#ff0000"><%if rsUsers.Fields.Item("profile").Value <> "" then Response.Write "Yes" %>&nbsp;</font></td>
        </tr>
        <% 
  Repeat1__index=Repeat1__index+1
  Repeat1__numRows=Repeat1__numRows-1
  rsUsers.MoveNext()
Wend
%>
        <tr> 
          <td colspan="5"> <div align="center"> 
              <table border="0" width="50%" align="center">
                <tr> 
                  <td width="23%" align="center"> <font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
                    <% If MM_offset <> 0 Then %>
                    <a href="<%=MM_moveFirst%>">First</a> 
                    <% End If ' end MM_offset <> 0 %>
                    </font></td>
                  <td width="31%" align="center"> <font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
                    <% If MM_offset <> 0 Then %>
                    <a href="<%=MM_movePrev%>">Previous</a> 
                    <% End If ' end MM_offset <> 0 %>
                    </font></td>
                  <td width="23%" align="center"> <font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
                    <% If Not MM_atTotal Then %>
                    <a href="<%=MM_moveNext%>">Next</a> 
                    <% End If ' end Not MM_atTotal %>
                    </font></td>
                  <td width="23%" align="center"> <font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
                    <% If Not MM_atTotal Then %>
                    <a href="<%=MM_moveLast%>">Last</a> 
                    <% End If ' end Not MM_atTotal %>
                    </font></td>
                </tr>
              </table>
            </div></td>
        </tr>
      </table>
      </form>
      </td>
  </tr>
  <tr>
    <td>
</td>
  </tr>
</table>
<% if strUserType = "A" then%>
	<div align="center">
	        <form action="<%=MM_editAction%>" method="POST" ID="form1" name="form1" onSubmit="MM_validateForm('User_ID','','R','Email','','NisEmail','Password','','R','Confirm_Password','','R');return document.MM_returnValue">
	          <table width="43%" border="0" align='ceenter'>
	            <tr> 
	              <td colspan="3" class='Title'><div align="center"><font size="2" face="Verdana, Arial, Helvetica, sans-serif"><strong>
	              Add New Site Admin.</strong></font></div></td>
	            </tr>
	            <tr> 
	              <td width="24%" class='Dark_cell'>ID</td>
	              <td colspan="2" class='Light_cell'> <input name="User_ID" type="text" id="User_ID" size="15" maxlength="10"></td>
	            </tr>
	            <tr> 
	              <td class='Dark_cell'>Name</td>
	              <td colspan="2" class='Light_cell'><input name="User_Name" type="text" id="User_Name" size="30" maxlength="35">*</td>
	            </tr>
	            <tr> 
	              <td class='Dark_cell'>Email</td>
	              <td colspan="2" class='Light_cell'><input name="Email" type="text" id="Email" size="30" maxlength="200">*</td>
	            </tr>
	            <tr> 
	              <td class='Dark_cell'>Password</td>
	              <td colspan="2" class='Light_cell'><input name="Password" type="password" id="Password" size="10" maxlength="8">*</td>
	            </tr>
	            <tr> 
	              <td class='Dark_cell'>Confirm Password</td>
	              <td colspan="2" class='Light_cell'><input name="Confirm_Password" type="password" id="Confirm_Password" size="10" maxlength="8">*</td>
	            </tr>
	            <tr> 
	              <td nowrap class='Dark_cell'>Level</td>
	              <td width="35%" nowrap class='Light_cell'> <font size="2" face="Verdana, Arial, Helvetica, sans-serif">
	                <input type="radio" name="UsrLvl" value="A" checked>
	                Administrator</font></td>
	              <td width="41%" nowrap class='Light_cell'> <font size="2" face="Verdana, Arial, Helvetica, sans-serif">
	                <input name="UsrLvl" type="radio" value="O">
	                Operator</font></td>
	            </tr>
	            <tr> 
	            <td colspan="3" class='Light_cell'>(*) Indicates Required Field</td></tr><tr>
	              <td colspan="3"><div align="right"> 
	                  <input name="Submit" type="submit" value="Submit">
	                  <input type="reset" name="Submit2" value="Reset">
	                </div></td>
	            </tr>
	          </table>
	          <input type="hidden" name="MM_insert" value="form1">
	        </form>
	</div>
<%end if%>
</body>
</html>
<%
rsUsers.Close()
Set rsUsers = Nothing
%>
