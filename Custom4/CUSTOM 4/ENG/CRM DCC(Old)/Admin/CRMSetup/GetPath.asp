<%@ Language=VBScript %>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
<Title>CRM Setup</Title>
</HEAD>
<BODY BGCOLOR="#AECAE6" TOPMARGIN="0" LEFTMARGIN="0">

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

<p></p>

<center>
<%

Dim fso
Dim f
Dim f1
Dim s
Dim sf
Dim strPathtoFind

strPathtoFind = Trim(Request.QueryString("Path"))
If Right(strPathtoFind,1) <> "\" Then
	strPathtoFind = strPathtoFind & "\"
End if
'Response.Write("<strong><font size=2 color=#000080>" &strPathtoFind)

Set fso = Server.CreateObject("Scripting.FileSystemObject")

'Response.Write("<strong><font color=#000080>" & Request.QueryString("Path") & "\</font></strong>")

'response.write(strpathtofind)
'response.end
Set f = fso.GetFolder(strPathtoFind)

Set sf = f.SubFolders
Set fc = f.Files

intFolders = sf.count
intFiles = fc.count
intAll = sf.count + fc.count
intremain = intAll Mod 10
'Response.Write "<br><strong><font size=2 color=#000080>" &intFolders&"___"&intFiles&"___"&intAll
IF intremain = 0 Then
	intCols = intAll / 10
Else
	intCols = Cint((intAll / 10)) + 1
End IF

redim All(intAll,2)
Dim i
i = 1 
for each f1 in sf
	All(i,1) = f1.name
	i = i + 1
next

for each f1 in fc
	All(i,1) = f1.name
	All(i,2) = f1.type
	
	i = i + 1
next


Response.Write("<Center><br><Table border=0 width=""10%"">")

IF intAll > 10 Then
	asd = 10
Else
	asd = intAll
End IF

for i=1 to asd
	Response.Write("<TR>")
	y=i
	Response.Write("<TD nowrap>")
	IF y < intFolders + 1 Then
		Response.Write("<A href='GetPath.asp?Path=" & Request.QueryString("Path") & "\" & All(y,1) & "&id="& Request.QueryString("id") & "'>" & "<img src=../../images/" & Session("Theme") & "/folderb.gif width=32 height=28 border=0>" & All(y,1) & "</a>")	
	Else
		
		IF Ucase(Right(All(y,1),3)) = "GIF" Then
			Response.Write("<A href='SelectedPath.asp?file=" & All(y,1) & "&hdnPath=" & Request.QueryString("Path")& "\" & All(y,1) & "&id="& Request.QueryString("id") &"'>" & "<img src=../../images/" & Session("Theme") & "/fileb.gif width=31 height=37 border=0>" & All(y,1) & "</a>")	
		Else
			Response.Write("<img src=../../images/" & Session("Theme") & "/fileb.gif width=31 height=37 border=0>" & All(y,1))	
		End IF
		
	End IF
	Response.Write("</TD>")
	
	for x=2 to intCols
		IF (y) > intAll - 10 Then
		Else
			Response.Write("<TD nowrap>")
			y = y + 10
			IF y < intFolders + 1 Then
				Response.Write("<A href='GetPath.asp?Path=" & Request.QueryString("Path") & "\" & All(y,1) & "&id="& Request.QueryString("id") & "'>" & "<img src=../../images/" & Session("Theme")& "/folderb.gif width=32 height=28 border=0>" & All(y,1) & "</a>")	
			Else
				IF Ucase(Right(All(y,1),3)) = "GIF" Then
					Response.Write("<A href='SelectedPath.asp?file=" & All(y,1) & "&hdnPath=" & Request.QueryString("Path")& "\" & All(y,1) & "&id="& Request.QueryString("id") &"'>" & "<img src=../../images/fileb.gif width=31 height=37 border=0>" & All(y,1) & "</a>")	
				Else
					Response.Write("<img src=../../images/" & Session("Theme") & "/fileb.gif width=31 height=37 border=0>" & All(y,1))	
				End IF
				
			End IF
			
			
			Response.Write("</TD>")
		End IF
	next
	Response.Write("</TR>")
next
%>

</Table>
<FORM action=SelectedPath.asp?id=<%Response.Write(Request.QueryString("id"))%> method=POST id=form1 name=form1>
<input type="hidden" Name = "hdnPath" value='<%Response.Write(strPathtoFind)%>'>
<INPUT type="submit" value="Select" id=submit1 name=submit1>
</FORM>
</center>
</BODY>
</HTML>
