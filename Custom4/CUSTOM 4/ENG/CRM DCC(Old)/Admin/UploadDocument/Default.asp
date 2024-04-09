<%@ LANGUAGE="VBSCRIPT" %>
<%Response.Buffer = true%>
<%
server.ScriptTimeout=600
Dim strAppPath
Dim strFilePath
strFlag= Request.QueryString("Flag")
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
	
If err.number<>0 then
		response.write "You must setup CRM first."
		response.write "<br><a href=""../CrmSetup\crmSetup.asp"">Setup CRM</a>"
		Response.End
End If
On Error GoTo 0
'Create filesystem object
Dim fsFiles
set fsFiles = Server.CreateObject("Scripting.FileSystemObject")

'Create folder for each issue to save uploaded files in it
Dim strFilesFolder , Folder
strFilesFolder = Server.MapPath("../../Documents")
if fsFiles.FolderExists(strFilesFolder) then
	set Folder = fsFiles.GetFolder(strFilesFolder)	
Else
	set Folder = fsFiles.CreateFolder(strFilesFolder)
End if

'Delete record from DB
if Request.QueryString ("Flag") = "D" then
	set UploadComp = server.CreateObject("aspSmartUpload.SmartUpload")
	on error resume next
	UploadComp.Upload
	'Response.Write UploadComp.Form("chkID")& 
	'Response.End 
	If (UploadComp.Form("chkID") <> "") Then 
	  if instr(1,UploadComp.Form("chkID"),",") then
	  	splitItem = split(trim(UploadComp.Form("chkID")),",")
		  
		for i=0 to UBOUND(splitItem)
			strFile = Server.MapPath ("../../Documents/" & splitItem(i))
			fsFiles.DeleteFile (strFile)
		next 
	  else
		strFile = Server.MapPath ("../../Documents/" & UploadComp.Form("chkID"))
		fsFiles.DeleteFile (strFile)	
	  end if
	End If
	if err.number > 0 then
		Response.Write err.Description
		
	end if
	Response.Redirect ("default.asp?flag=""")
end if


Function WriteFileName(strFileName)
	
	Dim intDotPos
	strReturn = strFileName
	intDotPos = InstrRev(strFileName,".")
	If intDotPos > 0 Then
		strReturn = Mid(strFileName,1,intDotPos-1)
	End If
	strPath = strVirtualPath & "/" & strFileName
	WriteFileName = "<A HREF=""javascript:openwindow ('"& strPath &"')"">" & strReturn &"</a>"

End Function
%>
<HTML>
<HEAD>
<TITLE>CRM - Admin</TITLE>
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
<SCRIPT LANGUAGE=javascript>
<!--
//Function to show the uploading message
function showMssg()
{
	try
	{
	divLoad.style.display = 'block';
	}
	catch(e){}
}

//Function to hide the uploading message
function hideMssg()
{
	try
	{
	divLoad.style.display = 'none';
	}
	catch(e){}
}
//-->
</SCRIPT>

<%
'Case action page
if strFlag = "S" then
%>
	<div id="divLoad" name="divLoad" style="display:none;color:red;"><center><b><font size=3>File being uploaded.Please stand by ...</b></center></div>
<%
	'Create upload object
	Dim objUpload 
	set objUpload = Server.CreateObject("aspSmartUpload.SmartUpload")
	
	'Setting deny list (files not allowed to be uploaded)
	objUpload.DeniedFilesList = "exe,dll,bat,com,asp"
   ' objUpload.AllowedFilesList = "pdf,txt,doc,xls"
	'Upload files 
	Dim intFileCount , strFileName
	%>
	<SCRIPT LANGUAGE=javascript>
	<!--
		showMssg();		
	//-->
	</SCRIPT>
	<%
	'mms - handling errors occured 28/8/2003 [start]
	On Error Resume Next
	objUpload.Upload
	If Err.Number <> 0 Then
		%>
		<SCRIPT LANGUAGE=javascript>
		<!--
			hideMssg();		
		//-->
		</SCRIPT>
		<%
		Response.Write "<font size=3>" & Err.Description & "<font size=3>An Error has occured on the server while attempting to upload your file. Please try again later."
		Response.End 
	End if
	'mms - handling errors occured 28/8/2003 [end]
	intFileCount = objUpload.Save("../../tempDocs")
	strFileName = objUpload.Files.Item("fAttach").FileName
	%>
	<SCRIPT LANGUAGE=javascript>
	<!--
		hideMssg();		
	//-->
	</SCRIPT>
	<%
	'Check if file already exists
	Dim bolExist
	bolExist = False
	for each x in Folder.Files
		if x.name = strFileName then
			bolExist = True
			exit for
		End if
	Next

	'Determine the source file & destination file
	Dim strSrcFile , strExtension , strDestFile
	strSrcFile = Server.MapPath("../../tempDocs/" & strFileName)
	
	strExtension = fsFiles.GetExtensionName("strSrcFile")
	strDestFile = Server.MapPath ("../../Documents/" & strFileName & "." & strExtension)

	'Move file from source to destination
	if bolExist = True then
	%>
	<SCRIPT LANGUAGE=javascript>
		hideMssg();
		if (confirm("This file already exists, do you want to overwrite it?"))
		{
			<%= fsFiles.DeleteFile(strDestFile) %> 
			showMssg();
			<%=	fsFiles.MoveFile(strSrcFile , strDestFile) %>
			hideMssg();
			document.writeln("<center><b><font size=3>File has been overwritten.</b></center>");
		}
		else
		{
			document.writeln("<center><b><font size=3>Action has been canceled.</b></center>");
		}
	</SCRIPT>
	<%
	Else
		on error resume next
		fsFiles.MoveFile strSrcFile , strDestFile
		If Err.Number <> 0 Then
			Response.Write "<font=3>"& err.Description	
		end if
		Response.Write ("<center><b><font size=3>File has been added successfully.</b></center>")
	End if
End if
'mms - make this page to submit on itself - 4/8/2003 [end]
%>

<script language="JavaScript" type="text/JavaScript">
<!--


function MM_reloadPage(init) {  //reloads the window if Nav4 resized
  if (init==true) with (navigator) {if ((appName=="Netscape")&&(parseInt(appVersion)==4)) {
    document.MM_pgW=innerWidth; document.MM_pgH=innerHeight; onresize=MM_reloadPage; }}
  else if (innerWidth!=document.MM_pgW || innerHeight!=document.MM_pgH) location.reload();
}
MM_reloadPage(true);
//-->

function checkFile(strFile)
{
	var strExt;
	if (strFile != "")
	{
		strExt = strFile.substring(strFile.lastIndexOf(".")+1);
	}
	else
	{
		alert ("Please choose file.");
		document.frmAttch.fAttach.focus();
		return false;
	}
	
	if ((strExt.toUpperCase() == "EXE") || (strExt.toUpperCase() == "DLL") || (strExt.toUpperCase() == "BAT") || (strExt.toUpperCase() == "COM") )
	{
		alert ("Sorry, You are not allowed to attach executable files."); 
		return false;
	}
	else
	{
		return true;
	}
}
</script>

<SCRIPT LANGUAGE=javascript>
<!--
function openwindow(filename) 
{  
	window.open(filename,'','toolbar=1,status=1,scrollbars=yes,resizable=yes,location=no,menubar=no,directories=no,top=0,left=0,width=700,height=500')
	
}

//-->
</SCRIPT>

</HEAD>
<BODY BGCOLOR="#aecae6" TOPMARGIN="0" LEFTMARGIN="0">

<!--TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
  <TR> 
	<td>
		<div border=1 id="Layer8" style="position:absolute; left:0; top:0; width:700; height:69; z-index:35; background-image: url(image/heder.JPG); layer-background-image: url(image/heder.JPG); border: 1px none #000000">
			<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"   codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" width="700" height="69" id=ShockwaveFlash1>
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
<TR>
    <TD align=right><a href="../Default.asp"><font size=2 face=arial>Back to 
      Admin Page</font></a></TD>
</TR>
</Table>
<p><p>



<%
strAppUserVar = session("strAppUserVar")
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"UPLOAD") <= 0 Then
%>
	<Table border=0 width=95% align=center>
		<tr>
		<td class="Title"><font color="Yellow">
			You're not authorized to view this page</font>
		</td>
		</tr>
	</Table>

	<%
	Response.End 
End If
%>        

<FORM METHOD="POST" ENCTYPE="multipart/form-data" action="default.asp?Flag=S" id=form1 name=form1>
<table bordercolor="#111111" border="1" align=center width=60% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<tr> 
	  <td class=light_cell colspan=2><div align="center"><font size="3"><strong>Upload Customer Document(s)</strong></font></div></td>
	</tr>
	<!--tr> 
		<td align = center> 
			
			   <br><br>
			   Style Code:<INPUT TYPE=TEXT NAME=stylecode SIZE="10" value="<%=Request.QueryString("sty")%>">&nbsp;<INPUT id=button5 name=button2  type="button" value="Get Style" onclick="return getstyle();">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<BR>
			   Image Path:<INPUT TYPE="FILE" NAME="ImagePath" SIZE="40"><BR>
			   <INPUT TYPE="SUBMIT" VALUE="Upload" id=SUBMIT1 name=SUBMIT1 size = 30>
			   <p><font size=-2> Only select .jpg files less than 200 K.B. </font>
			
		</td>	
	</tr-->
	<TR>
		<TD>Select your file:</TD>
		<TD><INPUT type=file name="fAttach" size=41 style="WIDTH: 403px; HEIGHT: 22px"></TD>
   </TR>
   <TR>
    <TD colspan=2>
      <P align=center><INPUT type=submit value="Add Document" name="sub1" onclick="return checkFile(document.frmAttch.fAttach.value);"></P>
    </TD>
   </TR>
	</td>
	</tr>
</table>
<BR><BR>
<%
'get the files under the folder documents if exists
'strDefaultPath = server.mappath(Request.ServerVariables("PATH_INFO"))
'intBSlashPos = InstrRev(strDefaultPath,"\")
'strDefaultPath = Mid(strDefaultPath,1,intBSlashPos - 1)&"\Documents"
strVirtualPath = "../../Documents"
strServerName = Trim(Request.ServerVariables("server_name"))
strAppPath = Trim(Request.ServerVariables("script_name"))


strpath=split(strAppPath,"/")
strDefaultPath = strFilesFolder'strServerName & "/" & strpath(1) & "/Documents"
Dim fso
Dim f
Dim f1
Dim s
Dim sf
Dim strPathtoFind

strPathtoFind = strFilesFolder'strDefaultPath

'If Right(strPathtoFind,1) <> "\" Then
'	strPathtoFind = strPathtoFind & "\"
'End if
'Response.Write("<font size=3>" & strPathtoFind)

Set fso = Server.CreateObject("Scripting.FileSystemObject")

Set f = fso.GetFolder(strPathtoFind)

Set sf = Folder.SubFolders
Set fc = Folder.Files
%>
<table border="0" cellpadding="0" cellspacing="3" width=60% align=center>
<%
if fc.count > 0 then%>
	<tr><td class=light_cell><div  align="center"><font size="3"><strong>Documents Attached</font></div></td></tr>
<%
	'Read Files
	for each f1 in fc
		Response.Write("<TR><TD align=left><input type=checkbox value='" & f1.name & "' name='chkID'>")
		Response.Write(WriteFileName(f1.name))
		Response.Write("<br></TD></TR>")
	next
%>
	<tr><td align="center"><br><input type=button name="btnDel" value="Delete Checked" onclick="document.form1.action='default.asp?Flag=D';document.form1.submit();"> </td></tr>
<%
else%>
	<tr><td align=center>No documents exist!</td></tr>
<%
end if
%>

</table>
</FORM>
</body>
</html>
<%'check if there are files exists
'strServerName = Trim(Request.ServerVariables("server_name"))
'strAppPath = Trim(Request.ServerVariables("script_name"))


'strpath=split(strAppPath,"/")
'strAppPath = "http://" & strServerName & "/" & strpath(1) & "/Documents"
'Response.Write "<font size=3>" & strAppPath
'Response.End 
'if Folder.Files.count > 0 then
%>
	<!--iframe src="<%=strAppPath%>" width=100% height=100% frameborder=0 marginwidth="0" marginheight="0"-->
<%'end if%>