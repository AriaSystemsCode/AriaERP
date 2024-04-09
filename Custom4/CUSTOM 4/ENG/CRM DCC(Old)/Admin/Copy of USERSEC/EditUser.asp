<%@LANGUAGE="VBSCRIPT" CODEPAGE="1252"%>
<!--#include file="Connections/cnConn.asp" -->
<%

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
	  MM_editRedirectUrl = "Users.asp"
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
	  MM_editQuery = MM_editQuery & " where " & MM_editColumn & " = " & MM_recordId

	  If (Not MM_abortEdit) Then
	    ' execute the update
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
'	    Response.Write(MM_editQuery)
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
if Request.QueryString("strUserType") = "" then 
	strUserType= "C"
else 
	strUserType= Request.QueryString("strUserType")
end if
'rsUser.Source = "SELECT cuser_id, cusr_name, cusr_pass, cwe_mail, cusr_levl FROM dbo.syuuser WHERE cuser_id = '" + Replace(rsUser__MMColParam, "'", "''") + "'"
strSQL = ""
select case strUserType
case "C" 'Customers
	strSQL = strSQL + "SELECT Fox.Account cuser_id, Fox.Btname cusr_name, syuuser.cwe_mail, syuuser.profile, syuuser.cusr_levl "
	strSQL = strSQL + "FROM dbo.syuuser "
	strSQL = strSQL + "RIGHT OUTER JOIN OPENROWSET('MSDASQL', '"
	strSQL = strSQL + Application("DataConnectionString")
	strSQL = strSQL + "', 'Select * From customer') as Fox ON syuuser.cuser_id = Fox.Account COLLATE Latin1_General_CI_AS "
	strSQL = strSQL + "WHERE(Fox.type = 'M') "
	strSQL = strSQL + "and (Fox.Account = '"& Replace(rsUser__MMColParam, "'", "''")  &"') "
	strSQL = strSQL + "ORDER BY Fox.Btname ASC"	
case "S" 'Sales
	strSQL = strSQL + "SELECT Fox.cuser_id, Fox.cusr_name, dbo.syuuser.cwe_mail, profile, syuuser.cusr_levl "
	strSQL = strSQL + "FROM dbo.syuuser "
	strSQL = strSQL + "RIGHT OUTER JOIN OPENROWSET('MSDASQL', '"
	strSQL = strSQL + Application("SystemConnectionString")
	strSQL = strSQL + "', 'Select * From syuuser') as Fox ON dbo.syuuser.cuser_id = Fox.cuser_id  COLLATE Latin1_General_CI_AS "
	strSQL = strSQL + "ORDER BY Fox.cusr_name ASC"		
case "A" 'Admin
	strSQL = strSQL + "SELECT cuser_id, cusr_name, cwe_mail, syuuser.cusr_levl, profile FROM dbo.syuuser where profile ='A' ORDER BY cusr_name ASC"
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

<table width="95%" border="0" align="center">
  <tr> 
    <td> <form action="<%=MM_editAction%>" method="POST" name="form1" onSubmit="MM_validateForm('User_ID','','R','Email','','NisEmail','Password','','R','Confirm_Password','','R');return document.MM_returnValue">
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
            <td colspan="2" class='Light_Cell'> <input name="User_ID" type="text" id="User_ID2" value="<%= Trim((rsUser.Fields.Item("cuser_id").Value)) %>" size="15" maxlength="10" Readonly>
              <% if isnull(rsUser.Fields.Item("cusr_levl").Value) then 'Not Activated%>
              <%else%>
			      <%If (CStr((rsUser.Fields.Item("cusr_levl").Value)) = CStr("O")) Then%>
					<a href="UserPriv.asp?lstUsers=<%= Trim((rsUser.Fields.Item("cuser_id").Value)) %>&strUserType=<%=strUserType%>">Privileges...</a>              
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
