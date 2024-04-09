<%
if Trim(Session("ID")) = "" then
	Response.redirect "default.htm"
end if
%>

<html>

<head>
<meta http-equiv="Content-Type" content="text/html; charset=windows-1252">
<meta http-equiv="Content-Language" content="en-us">
<meta name="GENERATOR" content="Microsoft FrontPage 4.0">
<meta name="ProgId" content="FrontPage.Editor.Document">
<title>New Page 1</title>
</head>

<body leftmargin="0" topmargin="0" bgColor=#aecae6>
<CENTER><!-- URL's used in the movie-->
<A href="helpdesk.asp" ></A>
<A href="return.asp" ></A> 
<A href="catalog.asp" ></A> 
<A href="OTS.asp" ></A> 
<A href="order.asp" ></A> 
<A href="custprof.asp" ></A><!-- text used in the movie--><!--ustomer elationship anagement -->
<OBJECT classid=clsid:D27CDB6E-AE6D-11cf-96B8-444553540000 
codeBase=http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0 
height=140 id=ShockwaveFlash1 width=780><PARAM NAME="movie" VALUE="CustNav.swf"><PARAM NAME="loop" VALUE="false"><PARAM NAME="menu" VALUE="false"><PARAM NAME="quality" VALUE="medium"><PARAM NAME="wmode" VALUE="transparent"><PARAM NAME="bgcolor" VALUE="#AECAE6"> 
 <EMBED src="CustNav.swf" loop=false  
 menu=false quality=medium wmode=transparent bgcolor=#AECAE6  WIDTH=100% HEIGHT=172 TYPE="application/x-shockwave-flash" PLUGINSPAGE="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash"></EMBED></OBJECT>
</CENTER>

<p align="center"><u><font face="Arial" size="5">Sales Order Page</font></u></p>
<p><font face="Arial"><A href="ord1.asp">Remote Order Entry</A>  </font></p>
<p><font face="Arial"><A 
href="OrdStatus.asp">Check Order Status</A>  </font></p>

</body>

</html>
