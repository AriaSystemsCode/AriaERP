<%@LANGUAGE="VBSCRIPT"%>
<HTML>
<Head>
<!--#include file="Connections/cnConn.asp" -->
<script language="JavaScript" type="text/JavaScript">
<!--
//wal_127795 function to open the get store page in a new window and check the Store link
function openwindow(strID) 
{  
	var strID,chkBox;

	chkBox = "document.frmPriv." + strID;
	
	eval(chkBox).checked = true;
	window.open('getStore.asp?first=T&CustID=<%=trim(request("Profile"))%>&UserID='+document.frmPriv.lstUsers.value,'','toolbar=no,status=no,scrollbars=no,location=no,resizable=yes,menubar=no,directories=no,top=' + ((window.screen.height-500)/2) + ',left=' + ((window.screen.width-800)/2) + ',width=800,height=500')
}

function MM_reloadPage(init) {  //reloads the window if Nav4 resized
  if (init==true) with (navigator) {if ((appName=="Netscape")&&(parseInt(appVersion)==4)) {
    document.MM_pgW=innerWidth; document.MM_pgH=innerHeight; onresize=MM_reloadPage; }}
  else if (innerWidth!=document.MM_pgW || innerHeight!=document.MM_pgH) location.reload();
}
MM_reloadPage(true);
//-->
</script>
<Title>CRM Users' Security</Title>
<%
'get stores saved for this user if exists
session("strStores") = ""
session("strRmvStr") = ""
'check if there are stores saved for the user in the db
dim rsUserStr
set rsUserStr = server.CreateObject ("ADODB.recordset")
rsUserStr.ActiveConnection = MM_cnConn_STRING
rsUserStr.Source = "select * from privileges where  cUser_ID='" & Trim(Request("lstUsers")) & "' and Profile='" & Request("Profile") & "' and cTokenID like 'STR%'"
rsUserStr.Open 
if not rsUserStr.EOF then
	do while not rsUserStr.EOF 
		session("strStores") = session("strStores") & ", " & rsUserStr("cTokenID")
	rsUserStr.MoveNext 
	loop
end if
Dim rsUsers
Dim rsUsers_numRows

Set rsUsers = Server.CreateObject("ADODB.Recordset")
rsUsers.ActiveConnection = MM_cnConn_STRING
'wma
'rsUsers.Source = "SELECT cuser_id, cusr_name FROM syuuser where  cusr_levl='O' ORDER BY cusr_name ASC"
'wal_ 127795 chevck if i ve a profile i.e the customer ID selected
if request("Profile") = "" then
	rsUsers.Source = "SELECT cuser_id, cusr_name, profile FROM syuuser where cusr_levl='O' ORDER BY cusr_name ASC"
else
	rsUsers.Source = "SELECT cuser_id, cusr_name, profile FROM syuuser where profile='"& trim(request("Profile")) &"' and cusr_levl='O' ORDER BY cusr_name ASC"
end if
rsUsers.CursorType = 0
rsUsers.CursorLocation = 2
rsUsers.LockType = 1
rsUsers.Open()
if rsUsers.EOF and rsUsers.BOF then
	Response.Write "No record(s) exist!<br>"
	Response.Write ("<a href=javascript:window.history.back();>Back</a>")
	Response.End 
end if
rsUsers_numRows = 0
%>
<%
Dim rsSecurity
Dim rsSecurity_numRows

Set rsSecurity = Server.CreateObject("ADODB.Recordset")
rsSecurity.ActiveConnection = MM_cnConn_STRING
rsSecurity.Source = "SELECT * FROM [Privileges]"
rsSecurity.CursorType = 0
rsSecurity.CursorLocation = 2
rsSecurity.LockType = 1
rsSecurity.Open()

rsSecurity_numRows = 0
%>

<%

	Function ReadSubGroup(objSecurity, intGroupCount,strParentGroup)
	  
	  For intGroup = 1 To objSecurity.SubGroups.Count
	    Dim objCurrentGroup
	    Set objCurrentGroup = objSecurity.SubGroups.Item(intGroup)
	    
	    'Response.Write (WriteLITag(objCurrentGroup.GroupID,"","")& WriteCheckBox(objCurrentGroup.GroupID," Group='" & strParentGroup & "'","Group") & objCurrentGroup.GroupID & "-" & objCurrentGroup.GroupName & Chr(10))
		'wal_127795 check manually case multi user login for the store link we added and add a link on it to open the page "getStore"	    
	    if objCurrentGroup.GroupID = "STORE" and trim(session("CustomerLoginUsing")) = "User" then
			Response.Write(WriteULTag(objCurrentGroup.GroupID,"Class='Expanded'"))
			if session("strStores") <> "" then
				strChecked = "Checked"
			Else
				strChecked = ""
			End If
			strChecked = strChecked & " Group='" & Trim(strParentGroup) & "'"
			
			Response.Write (WriteLITag(objCurrentGroup.GroupID,"","")& WriteCheckBox(objCurrentGroup.GroupID,strChecked,"Group") & "<a href=""javascript:openwindow('"& objCurrentGroup.GroupID &"')"">" & objCurrentGroup.GroupName & "</a>" & Chr(10))
			xxCall = ReadSubGroup(objCurrentGroup, intGroup,objCurrentGroup.GroupID)
		else
			Response.Write(WriteULTag(objCurrentGroup.GroupID,"Class='Expanded'"))
			Response.Write (WriteLITag(objCurrentGroup.GroupID,"","")& WriteCheckBox(objCurrentGroup.GroupID," Group='" & strParentGroup & "'","Group") & objCurrentGroup.GroupName & Chr(10))
			xxCall = ReadSubGroup(objCurrentGroup, intGroup,objCurrentGroup.GroupID)
		end if
	   
	  Next
	  Response.Write(WriteULTag(objSecurity.GroupID,"Class='Expanded'"))
	  ReadTokens objSecurity.Tokens,intGroup,strParentGroup
	  Response.Write("</UL>")
	  Response.Write("</UL>")
	End Function
	
	Function ReadTokens(objTkens,intGroupCount,strGroup)
	 
		For intTokenLoop = 1 To objTkens.Count
			Set objToken = objTkens.Item(intTokenLoop)
			If objToken.HasAccess Then
				strChecked = "Checked"
			Else
				strChecked = ""
			End If
			strChecked = strChecked & " Group='" & Trim(strGroup) & "'"
			'Response.Write(WriteLITag(objToken.TokenID,"PlainLI","onClick='CancelTokenBubble()'")& WriteCheckBox(objToken.TokenID,strChecked,"Token") & objToken.TokenID & "-" & objToken.TokenName & Chr(10))
			Response.Write(WriteLITag(objToken.TokenID,"PlainLI","onClick='CancelTokenBubble()'")& WriteCheckBox(objToken.TokenID,strChecked,"Token") & objToken.TokenName & Chr(10))
		Next
		
	End Function
	
	Function WriteULTag(strID,strAttribute)
'		WriteULTag = "<UL id='" & strID & "' Name='" & strID & "' onClick='doCheck(this)' style='display:''' " & strAttribute &">"
		WriteULTag = "<UL id='" & strID & "' Name='" & strID & "' style='display:''' " & strAttribute &">"
	End Function

	Function WriteLITag(strID,strClass,strAttribute)
		WriteLITag = "<Li id='" & strID & "' Name='" & strID & "' style='display:' Class='" & strClass & "' " & strAttribute & ">"
	End Function
	

	Function WriteCheckBox(strID,strAttribute,strName)
		WriteCheckBox = "<input type='CheckBox' id='" & strID & "' name='" & strName & "' value='"& strID &"' onClick='doCheckClick(this)' " & strAttribute &">"
		
	End Function	
%>

<style type="text/css">
<!--
.Expanded {
	list-style-image: url(Images/min.GIF);
}
.Collapsed {
	list-style-image: url(Images/plus.GIF);
}
.PlainLI{
list-style-type: none;
list-style-image: none;
}
TD.Title
{
    FONT-SIZE: 19pt;
    COLOR: lightskyblue;
    FONT-FAMILY: Impact, Arial;
    BACKGROUND-COLOR: #4166b7;
    TEXT-ALIGN: center
}
TD.Light_cell
{
    FONT-SIZE: 10pt;
    COLOR: navy;
    FONT-FAMILY: Arial;
    BACKGROUND-COLOR: #fffff0
}

-->
</style>

<SCRIPT LANGUAGE=javascript>
<!--

function doCheck(objSrc)
{
	window.event.cancelBubble = true;
	 
	var intLoop = 0;

	for (intLoop=0; intLoop < objSrc.all.length; intLoop++)
	{
		if (objSrc.all.item(intLoop).id==objSrc.id)
		{
		}
		else
		{
			if (objSrc.all.item(intLoop).style.display=='none')
			{
				objSrc.className = 'Expanded'
				objSrc.all.item(intLoop).style.display=''
			}
			else
			{
				objSrc.className = 'Collapsed'
				objSrc.all.item(intLoop).style.display='none'
			}
		}
	}
}

function CancelTokenBubble()
{
	window.event.cancelBubble = true;
}

function doCheckClick(objSrc)
{
	window.event.cancelBubble = true;
	var intLoop = 0;
	for (intLoop=0; intLoop < objSrc.parentElement.all.length; intLoop++)
	{
		objSrc.parentElement.all.item(intLoop).checked = objSrc.checked
	}
}

function GetUser(srcForm)
{
	srcForm.action = 'UserPriv.asp?Profile=<%=request("Profile")%>'
	srcForm.submit()
}

function CheckSave()
{

	if (document.frmPriv.lstUsers.value=='')
	{
		alert('Please Select User');
		return false;
	}
	//alert('Check');
	document.frmPriv.action='save.asp?CustID=<%=trim(request("Profile"))%>';
	document.frmPriv.submit();
}
//-->
</SCRIPT>

<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
</HEAD>


<BODY BGCOLOR="#aecae6" TOPMARGIN="0" LEFTMARGIN="0">

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

<form Name="frmPriv" method=POST>
<input type="hidden" name="strUserType" value="<%= Request.QueryString("strUserType")%>">

<table bordercolor="#111111" border="1" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
  <tr>
    <td bgcolor="#0099FF" width=100% colspan=2 valign="top" height=5% class='Dark_Cell' align='right'> 
			<A href='default.asp'><Font color="#FFFFF0">Back to Users' Security</font></A>
		</td>
	<tr>
	</tr>
    <td width=100% colspan=2 valign="top" height=5% class='Title'> 
			User Privileges
		</td>
  </tr>
  <tr>
    <td width="12%" valign="top" class='light_Cell' height=5%><font color="#FFFFF0" size="2" face="Verdana, Arial, Helvetica, sans-serif">Select 
      User</font>
    </td>
    <td width="88%" valign="top" height=5%> <font color="#FFFFF0" size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
      <select name="lstUsers" id="lstUsers" onChange="GetUser(this.form)">
      <option Value=''>-- Select User --
        <%
				While (NOT rsUsers.EOF)
					%>
					<option value="<%=(rsUsers.Fields.Item("cuser_id").Value)%>" <%If Trim(Request("lstUsers"))= Trim(rsUsers.Fields.Item("cuser_id").Value) Then Response.Write("SELECTED")%>><%=(rsUsers.Fields.Item("cuser_id").Value)%> - <%=(rsUsers.Fields.Item("cusr_name").Value)%></option>
					<%
					rsUsers.MoveNext()
				Wend
				If (rsUsers.CursorType > 0) Then
					rsUsers.MoveFirst
				Else
					rsUsers.Requery
				End If
				%>
      </select>
      <br>
      </font>
    </td>
  </tr>
	<tr> 
	<td colspan="2" valign="top">
<%

	Dim objSecurity
	Set objSecurity = Server.CreateObject("AriaWebSecurity.Privileges")
	objSecurity.ConnString = MM_cnConn_STRING
	If Request("lstUsers")="" Then
		strtUserID = "*"
	Else
		strtUserID = Request("lstUsers")
	End If
	objSecurity.UserID = strtUserID
	objSecurity.Profile = cstr(Trim(request("Profile")))
	'wma
	'objSecurity.Parent = "CRMCUST"	
	If Not rsUsers.EOF and Request("lstUsers") <>"" then
		rsUsers.Find "cuser_id = '"& Request("lstUsers") &"' "

		'Response.Write  rsUsers("cuser_id")
		select case  trim(rsUsers("profile"))
		case "S"	
			objSecurity.Parent = "CRMSALES"	
		case "A"	
			objSecurity.Parent = "CRMADMIN"	
		case else
			objSecurity.Parent = "CRMCUST"
		end select				
			
	End if
					

	If objSecurity.Load Then
	  For intLoop = 1 To objSecurity.SubGroups.Count
			Set objMainGroup = objSecurity.SubGroups.Item(intLoop)
			'Response.Write(WriteULTag(objMainGroup.GroupID,"Class='Expanded'") & WriteLITag(objMainGroup.GroupID,"","")& WriteCheckBox(objMainGroup.GroupID,"","Group") & objMainGroup.GroupID & "-" & objMainGroup.GroupName & Chr(10))
			Response.Write(WriteULTag(objMainGroup.GroupID,"Class='Expanded'") & WriteLITag(objMainGroup.GroupID,"","")& WriteCheckBox(objMainGroup.GroupID,"","Group") & objMainGroup.GroupName & Chr(10))
	    ReadSubGroup objMainGroup, intLoop,objSecurity.Parent
	    Response.Write("</ul>")
	  Next
	Else
	End If

%>
</td></tr>
<tr>
<td align='right' colspan=2 valign="top" height=5%>

<INPUT type="button" value="Save" onclick="javascript:CheckSave();"><INPUT type="reset" value="Reset" id=button2 name=button2>
</td>
</tr>
</table></form>
<p>

<%
rsUsers.Close()
Set rsUsers = Nothing
%>