<%@ Language=VBScript %>
<%Response.Buffer = true
 Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>

<%

'wma
'first we check if the setup.txt found
'get values seved in the file 
Dim strAppPath
Dim strFilePath

strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))

If Right(strAppPath,1) = "\" Then
	strFilePath = "admin\crmsetup\setup\setup.txt"
Else
	strFilePath = "\admin\crmsetup\setup\setup.txt"
End If

'Response.Write(strAppPath & strFilePath)
Dim objFile
Set objFile = Server.CreateObject("Scripting.FileSystemObject")
on error resume next
Set objTxtFile = objFile.OpenTextFile(strAppPath & strFilePath,1)

'first time
If err.number<>0 then 
		response.write "You must setup CRM first."
		response.write "<br><a href=""CrmSetup\crmSetup.asp"">Setup CRM</a>"
		Response.End
else
	strFile = objTxtFile.ReadAll
	Dim strArSetups
		strArSetups = Split(strFile," AND ", -1 , 1)

		'Declare Vartiables To Hold the temporary key and values
		Dim strKey
		Dim strValue
		For intLoop = 0 To UBound(strArSetups)
			strArKeyValue = Split(strArSetups(intLoop) , "=" , -1 , 1)
			Application(strArKeyValue(0)) = strArKeyValue(1)
			Session(strArKeyValue(0)) = strArKeyValue(1)
		Next
		'Application("DataConnectionString") = "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("DataPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"
		'Application("SystemConnectionString") = "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("SystemPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"
		If  Application("SqlUserName")= "" and  Application("SqlPassWord")="" then
			session("AdminLogin") = "true"
			session("firstTimeLogin") = "true"
			Response.Redirect "default.asp"		
		end if
	objTxtFile.Close
	Set objTxtFile = Nothing
End If
On Error GoTo 0

	
If Request("txtuser")<> "" then

	'login using sql serve account
	If Ucase(Request("txtuser")) = Ucase(Application("SqlUserName")) and  Ucase(Request("txtpw")) = Ucase(Application("SqlPassWord")) then
		session("AdminLogin") = "true"
		session("firstTimeLogin") = "false"
		Response.Redirect "default.asp"				
	end if

	'login throw admin user
	Dim objUserPriv
	Set objUserPriv = Server.CreateObject("AriaWebSecurity.Privileges")
	If Session("DBType") = "ORACLE" then
		Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=OraOLEDB.Oracle;Data Source="&Trim(session("SQLServer"))&";user id=" & Trim(Session("SqlUserName")) & ";password=" & Trim(Session("SqlPassWord")) & ""	
	Else
		Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog="&Trim(session("DBName"))&";Data Source="&Trim(session("SQLServer"))&";uid=" & Trim(Session("SqlUserName")) & ";pwd=" & Trim(Session("SqlPassWord")) & ""
	End If
	objUserPriv.ConnString = Application("SqlServer")
	
	'wma
	'Check if the user is valid
	objUserPriv.UserID  = Ucase(Request("txtuser")) 'Session("RSCust").Fields("Account")
	objUserPriv.UserPWD = Trim(Ucase(Request("txtpw")))
	'reset this application variable
	Application(Trim(Ucase(Request("txtuser")))) = ""
	session("strAppUserVar") = Trim(Ucase(Request("txtuser")))
	If objUserPriv.ValidateUser() Then
		Application(Trim(Ucase(Request("txtuser"))) & "Lvl") = objUserPriv.UserLvl
		If objUserPriv.UserLvl = "O" Then
			objUserPriv.GetUserTokens()
			set objConn = Server.CreateObject("adodb.connection")
			objConn.Open(Application("SqlServer"))
			set rsSyuusr = objConn.Execute("select profile from syuuser where cuser_id = '"& Request("txtuser") &"' ")
			if rsSyuusr.eof or trim(rsSyuusr("profile")) <> "A"  then 'not administrator
				Response.Write("Invalid user ID or password.<br>")
				Response.Write ("<a href='javascript:window.history.back();'>Back</a>")
				Response.End 
			end if
			'Fill Session string with user tokens
			For intTokenLoop = 1 To objUserPriv.Tokens.Count
				Set objToken = objUserPriv.Tokens.Item(intTokenLoop)
				Application(Trim(Ucase(Request("txtuser")))) = Application(Ucase(Request("txtuser"))) & "|" & objToken.TokenID
			Next
		End If
	Else
		Response.Write("Invalid user ID or password.<br>")
		Response.Write ("<a href='javascript:window.history.back();'>Back</a>")
		Response.End 
	End If
	session("AdminLogin") = "true"
	session("firstTimeLogin") = "false"
	Response.Redirect "default.asp"
end If	
%>

<html>
<head>
<title>CRM - Login</title>
<SCRIPT LANGUAGE=javascript>
<!--
function foucus_id()
{
	document.form1.txtuser.focus();
}

//-->
</SCRIPT>
    <SCRIPT LANGUAGE=javascript>
		<!--
		function formvalid(frm)
		{
			if(frm.txtuser.value == "")
			{
				alert("Please Enter the UserName!");
				return false;
			}
			
			return true;
		}
		//-->
		</SCRIPT>


<meta http-equiv="Content-Type" content="text/html;">
<meta name="description" content="FW4 DW4 HTML">
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/Common.css">
<!-- Fireworks 4.0  Dreamweaver 4.0 target.  Created Thu Feb 08 20:17:18 GMT+0400 (Arabian Standard Time) 2001-->
</head>
<body onload="foucus_id()">
<!-- ImageReady Slices (Login1_Theme1.psd) -->
<TABLE WIDTH=100% BORDER=0 CELLPADDING=0 CELLSPACING=0>
  <TR> 
    <TD> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=8 HEIGHT=1></TD>
    <TD> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=39 HEIGHT=1></TD>
    <TD> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=33 HEIGHT=1></TD>
    <TD> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=60 HEIGHT=1></TD>
    <TD> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=10 HEIGHT=1></TD>
    <TD> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=47 HEIGHT=1></TD>
    <TD width="100%"> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=6 HEIGHT=1></TD>
    <TD> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=125 HEIGHT=1></TD>
    <TD> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=10 HEIGHT=1></TD>
    <TD> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=13 HEIGHT=1></TD>
    <TD> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=361 HEIGHT=1></TD>
    <TD> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=30 HEIGHT=1></TD>
    <TD> <IMG SRC="../images/<%=session("theme")%>/spacer.gif" WIDTH=8 HEIGHT=1></TD>
  </TR>
  <TR> 
    <TD COLSPAN=6> <IMG SRC="../images/<%=session("theme")%>/Login1_01.jpg" WIDTH=197 HEIGHT=8></TD>
    <TD background="../images/<%=session("theme")%>/Login1_02.jpg"> <IMG SRC="../images/<%=session("theme")%>/Login1_02.jpg" WIDTH=6 HEIGHT=8></TD>
    <TD COLSPAN=6> <IMG SRC="../images/<%=session("theme")%>/Login1_03.jpg" WIDTH=547 HEIGHT=8></TD>
  </TR>
  <TR> 
    <TD COLSPAN=3 ROWSPAN=2> <IMG SRC="../images/<%=session("theme")%>/Login1_04.jpg" WIDTH=80 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../images/<%=session("theme")%>/DCC logo.jpg" WIDTH=60 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../images/<%=session("theme")%>/Login1_06.jpg" WIDTH=10 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../images/<%=session("theme")%>/Login1_07.jpg" WIDTH=47 HEIGHT=59></TD>
    <TD ROWSPAN=2 background="../images/<%=session("theme")%>/Login1_08.jpg"> <IMG SRC="../images/<%=session("theme")%>/Login1_08.jpg" WIDTH=6 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../images/<%=session("theme")%>/Login1_09.jpg" WIDTH=125 HEIGHT=59></TD>
    <TD COLSPAN=5> <IMG SRC="../images/<%=session("theme")%>/Login1_10.jpg" WIDTH=422 HEIGHT=30></TD>
  </TR>
  <TR> 
    <TD COLSPAN=5> <IMG SRC="../images/<%=session("theme")%>/Login1_11.jpg" WIDTH=422 HEIGHT=29></TD>
  </TR>
  <TR> 
    <TD COLSPAN=6> <IMG SRC="../images/<%=session("theme")%>/Login1_12.jpg" WIDTH=197 HEIGHT=8></TD>
    <TD background="../images/<%=session("theme")%>/Login1_13.jpg"> <IMG SRC="../images/<%=session("theme")%>/Login1_13.jpg" WIDTH=6 HEIGHT=8></TD>
    <TD COLSPAN=6> <IMG SRC="../images/<%=session("theme")%>/Login1_14.jpg" WIDTH=547 HEIGHT=8></TD>
  </TR>
  <TR> 
    <TD ROWSPAN=4> <IMG SRC="../images/<%=session("theme")%>/Login1_15.jpg" WIDTH=8 HEIGHT=274></TD>
    <TD COLSPAN=5> <IMG SRC="../images/<%=session("theme")%>/Login1_16.jpg" WIDTH=189 HEIGHT=25></TD>
    <TD background="../images/<%=session("theme")%>/Login1_17.jpg"> <IMG SRC="../images/<%=session("theme")%>/Login1_17.jpg" WIDTH=6 HEIGHT=25></TD>
    <TD COLSPAN=2> <IMG SRC="../images/<%=session("theme")%>/Login1_18.jpg" WIDTH=135 HEIGHT=25></TD>
    <TD ROWSPAN=4> <IMG SRC="../images/<%=session("theme")%>/Login1_19.jpg" WIDTH=13 HEIGHT=274></TD>
    <TD WIDTH=361 HEIGHT=274 ROWSPAN=4 background="../images/<%=session("theme")%>/Login1_02.jpg"><font face="Arial" size="1" color="#000000">In 
      this time of revolutionary change in the business landscape, having a customer 
      centric strategy is key. That is why the integration of the Internet into 
      every customer interaction before, during and after the sale has become 
      a vital component to an organization's overall business strategy.Aria's 
      new Web Based CRM module allows apparel companies to integrate their back-end 
      ERP systems with the Internet to offer 24 hours on line customer service 
      to their retail customer stores. The system offers Retail stores and Sales-reps 
      features such as on line product Catalog, order entry/inquiry, Stock Availability, 
      Return Authorization, Customer statements and much more. The system also 
      integrates with Workflow to automatically manage customer on-line interactions. 
      Research indicates that customers today expect you to have an interactive 
      web-site to allow them to get online service.Aria's initial release of the 
      CRM module has built-in interfaces to Aria's ERP system but the system can 
      be easily integrated with any other back-end system.</font></TD>
    <TD ROWSPAN=4> <IMG SRC="../images/<%=session("theme")%>/Login1_21.jpg" WIDTH=30 HEIGHT=274></TD>
    <TD ROWSPAN=4> <IMG SRC="../images/<%=session("theme")%>/Login1_22.jpg" WIDTH=8 HEIGHT=274></TD>
  </TR>
  <TR> 
    <TD ROWSPAN=2> <IMG SRC="../images/<%=session("theme")%>/Login1_23.jpg" WIDTH=39 HEIGHT=229></TD>
    <TD COLSPAN=4> <IMG SRC="../images/<%=session("theme")%>/Login1_24.jpg" WIDTH=150 HEIGHT=137></TD>
    <TD ROWSPAN=2 background="../images/<%=session("theme")%>/Login1_25.jpg"> <IMG SRC="../images/<%=session("theme")%>/Login1_25.jpg" WIDTH=6 HEIGHT=229></TD>
    <TD COLSPAN=2 ROWSPAN=2 background="../images/<%=session("theme")%>/Login1_26.jpg" valign="top" align="center"> 
    <form method="POST" onsubmit="return formvalid(this)" name=form1 target="_parent">
      <p>
      <input type="text" name="txtuser" size="10" style="border-style: solid; border-width: 2; padding-top: 0" maxlength=16></p>
      <p>
      <input type="password" name="txtpw" size="10" style="border-style: solid; border-width: 2; padding-top: 0" maxlength=16>
      </p>
      <p></p>
      <p><input type="submit" value="Submit" name="B1"><input type="reset" value="Reset" name="B2"></p>
    </form>
&nbsp;</TD>
  </TR>
  <TR> 
    <TD COLSPAN=4> <IMG SRC="../images/<%=session("theme")%>/Login1_27.jpg" WIDTH=150 HEIGHT=92></TD>
  </TR>
  <TR> 
    <TD COLSPAN=5> <IMG SRC="../images/<%=session("theme")%>/Login1_28.jpg" WIDTH=189 HEIGHT=20></TD>
    <TD background="../images/<%=session("theme")%>/Login1_29.jpg"> <IMG SRC="../images/<%=session("theme")%>/Login1_29.jpg" WIDTH=6 HEIGHT=20></TD>
    <TD COLSPAN=2> <IMG SRC="../images/<%=session("theme")%>/Login1_30.jpg" WIDTH=135 HEIGHT=20></TD>
  </TR>
  <TR> 
    <TD COLSPAN=6> <IMG SRC="../images/<%=session("theme")%>/Login1_31.jpg" WIDTH=197 HEIGHT=38></TD>
    <TD background="../images/<%=session("theme")%>/Login1_32.jpg"> <IMG SRC="../images/<%=session("theme")%>/Login1_32.jpg" WIDTH=6 HEIGHT=38></TD>
    <TD COLSPAN=6> <IMG SRC="../images/<%=session("theme")%>/Login1_33.jpg" WIDTH=547 HEIGHT=38></TD>
  </TR>
  <tr> 
    <td height="1"><img height="1" width="8" src="../images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="39" src="../images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="33" src="../images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="60" src="../images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="10" src="../images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="47" src="../images/<%=session("theme")%>/spacer.gif"></td>
    <td></td>
    <td><img height="1" width="125" src="../images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="10" src="../images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="13" src="../images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="361" src="../images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="30" src="../images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="8" src="../images/<%=session("theme")%>/spacer.gif"></td>
  </tr>
</TABLE>
<!-- End ImageReady Slices -->
</body>
</html>