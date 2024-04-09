<%@ Language=VBScript %>
<%
Response.CacheControl  = "no-cache"
Response.AddHeader  "Pragma", "no-cache"
Response.Expires = -1
'Response.Expires = 0
Response.Buffer = true
'Session.Abandon() 'you may un comment this line

'wma
'after abandon
if Request.QueryString("AdminLogin")<>"" then 
	session("AdminLogin") = Request.QueryString("AdminLogin")
	session("strAppUserVar") = Request.QueryString("userid")
end if	
'check if the admin not login in case of the sql servr connection ok.
If not session("AdminLogin") = "true" and Application("SqlServer")= "" then
	session("AdminLogin") = "true"
	session("firstTimeLogin") = "true"
end if	
'elseif not session("AdminLogin") = true and Application("SqlServer")<> "" then
if not session("AdminLogin") = "true" then
	Response.Redirect "Login.asp"
end if

%>
<HTML>
<HEAD>
<TITLE>CRM - Admin</TITLE>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/Common.css">
<script language="JavaScript" type="text/JavaScript">
<!--


function MM_reloadPage(init) {  //reloads the window if Nav4 resized
  if (init==true) with (navigator) {if ((appName=="Netscape")&&(parseInt(appVersion)==4)) {
    document.MM_pgW=innerWidth; document.MM_pgH=innerHeight; onresize=MM_reloadPage; }}
  else if (innerWidth!=document.MM_pgW || innerHeight!=document.MM_pgH) location.reload();
}
MM_reloadPage(true);
//-->
</script>
</HEAD>
<BODY BGCOLOR="#aecae6" TOPMARGIN="0" LEFTMARGIN="0">

<!--TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
  <TR> 
	<td>
		<div border=1 id="Layer8" style="position:absolute; left:0; top:0; width:700; height:69; z-index:35; background-image: url(image/heder.JPG); layer-background-image: url(image/heder.JPG); border: 1px none #000000">
			<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"  codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" width="700" height="69" id=ShockwaveFlash1>
    <param name=movie value="../banner.swf">
    <param name=quality value=high>
    <embed src="../banner.swf" quality=high pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash" type="application/x-shockwave-flash" width="750" height="69">
    </embed> 
  </object>
		</div>
	</td>
  </TR>
</TABLE><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR-->
<TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
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
    <TD COLSPAN=6> <IMG SRC="../images/<%=Session("Theme")%>/Login1_01.jpg" WIDTH=197 HEIGHT=8></TD>
    <TD background="../images/<%=Session("Theme")%>/Login1_02.jpg"> <IMG SRC="images/<%=Session("Theme")%>/Login1_02.jpg" WIDTH=6 HEIGHT=8></TD>
    <TD COLSPAN=6> <IMG SRC="../images/<%=Session("Theme")%>/Login1_03.jpg" WIDTH=547 HEIGHT=8></TD>
  </TR>
  <TR> 
    <TD COLSPAN=3 ROWSPAN=2> <IMG SRC="../images/<%=Session("Theme")%>/Login1_04.jpg" WIDTH=80 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../images/<%=Session("Theme")%>/DCC logo.jpg" WIDTH=60 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../images/<%=Session("Theme")%>/Login1_06.jpg" WIDTH=10 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../images/<%=Session("Theme")%>/Login1_07.jpg" WIDTH=47 HEIGHT=59></TD>
    <TD ROWSPAN=2 background="../images/<%=Session("Theme")%>/Login1_08.jpg"> <IMG SRC="images/<%=Session("Theme")%>/Login1_08.jpg" WIDTH=6 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../images/<%=Session("Theme")%>/Login1_09.jpg" WIDTH=125 HEIGHT=59></TD>
    <TD COLSPAN=5> <IMG SRC="../images/<%=Session("Theme")%>/Login1_10.jpg" WIDTH=422 HEIGHT=30></TD>
  </TR>
  <TR> 
    <TD COLSPAN=5> <IMG SRC="../images/<%=Session("Theme")%>/Login1_11.jpg" WIDTH=422 HEIGHT=29></TD>
  </TR>
  <TR> 
    <TD COLSPAN=6> <IMG SRC="../images/<%=Session("Theme")%>/Login1_12.jpg" WIDTH=197 HEIGHT=8></TD>
    <TD background="../images/<%=Session("Theme")%>/Login1_13.jpg"> <IMG SRC="images/<%=Session("Theme")%>/Login1_13.jpg" WIDTH=6 HEIGHT=8></TD>
    <TD COLSPAN=6> <IMG SRC="../images/<%=Session("Theme")%>/Login1_14.jpg" WIDTH=547 HEIGHT=8></TD>
  </TR>
</TABLE><BR><BR>

<Table width="95%" Align="Center">
 <TR>
	<th align=center><font color=black size=4><u>You must open a new browser, for your changes 
      to take effect.</u><br></th>
  </TR>
   </tr><td ><br></td></tr>
  <TR> 
    <td> <table bordercolor="#111111" border="1" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">

        
        <TR> 
          <TD nowrap> <li><B><A href="CrmSetup/crmSetup.asp"><font color="">CRM 
              Setup</font></a><font color=""><br>
              <font size="1" face="Arial, Helvetica, sans-serif">Adjust the 
              setup parameters of your CRM module.</font></font> <br>
              </B></li></TD>
        </TR>
        <TR> 
          <TD nowrap> <li><B><a href="layout/default.asp"><font color="">CRM 
              Layout &amp; Menu</font></a><font color=""><br>
              <font size="1" face="Arial, Helvetica, sans-serif">Select your 
              preferred theme. </font></font><br>
              </B></li></TD>
        </TR>
        <TR> 
          <TD nowrap> <li><B><a href="Menu/"><font color="">CRM 
              Menu Items</font></a><font color=""><br>
              <font size="1" face="Arial, Helvetica, sans-serif">Select your 
              Menu items to your sales rep. and customers. </font></font><br>
              </B></li></TD>
        </TR>        
        <TR> 
          <TD nowrap> <li><font color="#FF0000"><b><A href="AdmOpenIssues/Default.asp"><font color="">Manage 
              Helpdesk</font></a></b></font><font color=""><b><A href="AdmOpenIssues/Default.asp"> 
              </a><br>
              <font size="1" face="Arial, Helvetica, sans-serif">View open 
              Issues and submit your answers. </font></b></font><b><br>
              </b></li></TD>
        </TR>
        <TR> 
          <TD nowrap> <li><font color="#0000FF"><B><A href="TransLog/"><font color="">View 
              Customer Log</font></a></B></font><font color=""><B><br>
              <font size="1" face="Arial, Helvetica, sans-serif">View each 
              customer's actions and keep track of changes and modifications applied 
              by customers. </font></B></font><font color="#FF0000"><B><br>
              </B></font></li></TD>
        </TR>

        <TR> 
          <TD nowrap> <li><font color="#0000FF"><B><A href="USERSEC/">
          <font color="">Users and Security</font></a></B></font><font color=""><B><br>
              <font size="1" face="Arial, Helvetica, sans-serif">Add new users and assign privileges 
              </font></B></font><font color="#FF0000"><B><br>
              </B></font></li></TD>
        </TR>
        <!--wal_131300 add new option to add style profile-->
        <TR> 
          <TD nowrap> <li><font color="#0000FF"><B><A href="StyleProfile/">
          <font color="">Style Profile</font></a></B></font><font color=""><B><br>
              <font size="1" face="Arial, Helvetica, sans-serif">Add/Edit Style profile
              </font></B></font><font color="#FF0000"><B><br>
              </B></font></li></TD>
        </TR> 
        <TR> 
          <TD nowrap> <li><font color="#0000FF"><B><A href="UploadImages/">
          <font color="">Upload Style Images</font></a></B></font><font color=""><B><br>
              <font size="1" face="Arial, Helvetica, sans-serif">Upload your style images to appear in CRM Catalog
              </font></B></font><font color="#FF0000"><B><br>
              </B></font></li></TD>
        </TR> 
         <TR> 
          <TD nowrap> <li><font color="#0000FF"><B><A href="UploadDocument/">
          <font color="">Upload Document(s)</font></a></B></font><font color=""><B><br>
              <font size="1" face="Arial, Helvetica, sans-serif">Upload any documents as a customer reference
              </font></B></font><font color="#FF0000"><B><br>
              </B></font></li></TD>
        </TR>           
         <TR> 
          <TD nowrap> <li><B><A href="../login.asp" target=_blank  onClick="self.close(); return true;"><font color="">Run CRM</font></a><font color=""><br>
              <font size="1" face="Arial, Helvetica, sans-serif">
              After finished all modifications you may click this link.</font></font> <br>
              </B></li></TD>
        </TR>      
     </TABLE></td>
  </tr>
</Table>
</BODY>
</HTML>


