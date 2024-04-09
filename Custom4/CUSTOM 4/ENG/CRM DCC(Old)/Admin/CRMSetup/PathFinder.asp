<%@ Language=VBScript %>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
<Title>CRM - Select Drive</Title>
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


<div align="center">
  <center>
  <table border="0" width="100%" cellspacing="0" cellpadding="0">
    <tr>
      <td width="100%" align="middle">
          <p>&nbsp;
</p>
      </td>
    </tr>
    <tr>
      <td width="100%" align="middle">
      <P></P></td>
    </tr>
  </table>
  </center>
</div>

<%
'first update sesion variables
Session("CompanyID")= Request("txtCompID")
Session("DataPath")=Request("flDataFiles")
Session("SysPath")=Request("flSystemPath")
Session("LogoPath") = Request("LogoPath")
Session("SQLServer") = Request("txtServerName")
Session("UserCanEdit")=Request("lstEditProfile")
Session("NotifyEmail")=Request("txtNotifyAddress")
Session("ConfirmationAddress") = Request("lstconfirmadd")
Session("OTSSetup") = Trim(Request("lstOTS"))
Session("PasswordField") = Request("lstPassField")


Response.Write("<Center><Table border=0 width=""10%"">")

Dim objPath
Dim objDrives
Set objPath = Server.CreateObject("Scripting.FileSystemObject")
Set objDrives = objPath.Drives
For Each x in objDrives
	Response.Write("<TR height=20><TD valign=bottom width=40><img src=../../images/"&Session("Theme")&"/driveb.gif width=32 height=16></TD><TD valign=top>")
	Response.Write("<A Href='GetPath.asp?Path=" & x.DriveLetter & ":&id="& Request.QueryString("id") &"'>" & x.DriveLetter & ":\</A>")
	Response.Write("</TD></TR>")
Next
Set objPath = Nothing
%>
</TABLE>
<CENTER></CENTER>
<BR>
</BODY>
</HTML>
