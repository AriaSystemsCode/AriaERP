<%@ LANGUAGE="VBSCRIPT" %>
<%Response.Buffer = true%>
<%
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
	
	If err.number<>0 then
			response.write "You must setup CRM first."
			response.write "<br><a href=""../CrmSetup\crmSetup.asp"">Setup CRM</a>"
			Response.End
	End If
	On Error GoTo 0


	'IF request("slctDivision") <> "" Then
		'Session("Division") = request("slctDivision")
	'Else
	Session("Division") = Trim(Session("CatDivision"))
	'End If
%>
<HTML>
<HEAD>
<TITLE>CRM - Admin</TITLE>
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
<script language="JavaScript" type="text/JavaScript">
<!--


function MM_reloadPage(init) {  //reloads the window if Nav4 resized
  if (init==true) with (navigator) {if ((appName=="Netscape")&&(parseInt(appVersion)==4)) {
    document.MM_pgW=innerWidth; document.MM_pgH=innerHeight; onresize=MM_reloadPage; }}
  else if (innerWidth!=document.MM_pgW || innerHeight!=document.MM_pgH) location.reload();
}
MM_reloadPage(true);
//-->


function checkcontents(form){
	if (document.form1.stylecode.value==""){
		alert("Please Select Style");
		document.form1.stylecode.focus();
		return false;
	}
	
	if (document.form1.ImagePath.value==""){
		alert("Please Select Image to upload");
		document.form1.ImagePath.focus();
		return false;
	}	
	
}

function getstyle(){
	location.href = "findstyle.asp?LoginType=O" 
}
</script>



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
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"AUPLOAF") <= 0 Then
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


<table bordercolor="#111111" border="1" align=center width=60% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<tr> 
	  <td class=light_cell><div align="center"><font size="3"><strong>Upload Style Images</strong></font></div></td>
	</tr>
	<tr> 
		<td align = center> 
			<FORM METHOD="POST" ENCTYPE="multipart/form-data" id=form1 name=form1 onsubmit="return checkcontents(this);">
			   <br><br>
			   Style Code:<INPUT TYPE=TEXT NAME=stylecode SIZE="10" value="<%=Request.QueryString("sty")%>">&nbsp;<INPUT id=button5 name=button2  type="button" value="Get Style" onclick="return getstyle();">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<BR>
			   Image Path:<INPUT TYPE="FILE" NAME="ImagePath" SIZE="40"><BR>
			   <INPUT TYPE="SUBMIT" VALUE="Upload" id=SUBMIT1 name=SUBMIT1 size = 30>
			   <p><font size=-2> Only select .jpg files less than 200 K.B. </font>
			</FORM>
		</td>	
	</tr>
		<!--------------------------------------------------------------------------------->
<%		

'Variables
'*********
Dim mySmartUpload
Dim file
Dim intCount
intCount=0
        
'Object creation
'***************
 Set mySmartUpload = Server.CreateObject("aspSmartUpload.SmartUpload")

'restrictions
'Only allow txt or htm files
'***************************
 mySmartUpload.AllowedFilesList = "jpg,gif"

'Deny upload if the total fila size is greater than 200000 bytes
'***************************************************************
 mySmartUpload.TotalMaxFileSize = 200000

On Error Resume Next

'Upload
'******
mySmartUpload.Upload

if mySmartUpload.Form("stylecode") <>"" then
%>
	<tr>
		<td>
<%		
	'Select each file
	'****************
	For each file In mySmartUpload.Files
	'  Only if the file exist
	'  **********************
	   If not file.IsMissing Then
	   '  Save the files with his original names in a virtual path of the web server
	   '  **************************************************************************
	      file.SaveAs("../../styimg/" & mySmartUpload.Form("stylecode") & ".jpg")                                                  
	      intCount = intCount + 1
	   End If
	Next
	   
	'Trap errors
	'***********
	If Err Then
		IF Err.number = -2147220494 THEN
			Response.Write ("<b>Wrong selection:</b> Only jpg files can be uploaded.") & "<br>"
		elseif  Err.number = -2147467259 THEN
			Response.Write ("<b>Wrong selection:</b> File size should less than 200 K.B. ") & "<br>"					 
		END if
	   Response.Write("<b>Wrong selection : </b>" & Err.description) & "<hr>"
	   Response.Write("<b>Wrong selection : </b>" & Err.number)
	Else
	'  Display the number of files uploaded
	'  ************************************
	   Response.Write(intCount & " file(s) uploaded.")      
	End If
	 
end if
on error goto 0

%>
<!--------------------------------------------------------------------------------------------------->	

		</td>
	</tr>
</table>