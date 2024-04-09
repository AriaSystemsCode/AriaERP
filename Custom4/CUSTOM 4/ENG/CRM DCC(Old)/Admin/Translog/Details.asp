<%@ Language=VBScript%>
<%Response.Buffer = true%>
<HTML>
<HEAD>
<TITLE>CRM - Transaction Log Admin</TITLE>
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
</HEAD>

<BODY BGCOLOR="#AECAE6" TOPMARGIN="0" LEFTMARGIN="0">
<!--TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
  <TR> 
	<td>
		<div border=1 id="Layer8" style="position:absolute; left:0; top:0; width:700; height:69; z-index:35; background-image: url(image/heder.JPG); layer-background-image: url(image/heder.JPG); border: 1px none #000000">
			<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"       codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" width="700" height="69" id=ShockwaveFlash1>
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

<!--#INCLUDE FILE="../../glbscrpt.asp" -->

<%
strRec = Request.Form ("Memo")
if trim(strRec) <> "" then
''''''''''''''''''''''''''''''''''''''''''''''''
	Response.Write("<table bordercolor=""#111111"" border=""1"" align=center width=95% style=""border-collapse: collapse"" cellpadding=""0"" cellspacing=""0"">")
	strMemo = split(strRec,"!#!")
	select case strMemo(0)
		case "Add"
			Response.Write("<tr><TD><b>Add " & strMemo(1) & "</b></td><TD align=right><a href=default.asp>Back to view the log.</a></td></tr></table><br>")
		case "Edit"
			Response.Write("<tr><TD><b>Edit " & strMemo(1) & "</b></td><TD align=right><a href=""default.asp"">Back to view the log.</a></td></tr></table><br>")
		case "Delete"
			Response.Write("<tr><TD><b>Delete " & strMemo(1) & "</b></td><TD align=right><a href=default.asp>Back to view the log.</a></td></tr></table><br>")
	end select
	Response.Write("<table bordercolor=""#111111"" border=""1"" align=center width=95% style=""border-collapse: collapse"" cellpadding=""0"" cellspacing=""0"">")
	select case strMemo(0)
	
	''''''''''''''''''''''''''''''''''''''''''''		
	'''''''''''Add                     '''''''''
	''''''''''''''''''''''''''''''''''''''''''''
		case "Add"
			'Response.Write "<p align=left>Add "&strMemo(1)&"</p>"
			select case strMemo(1)
				case "Contact"
					'Response.Write " Contact"
					'for iCont=2 to ubound(strMemo)
					'	Response.Write strMemo(iCont)
					'next
					strHead = "<tr><td>&nbsp;</td><td>Ccont_id - " & fld_desc(Ucase("Ccont_id")) & "</td><td>Ccontttl - " & fld_desc(Ucase("Ccontttl")) & "</td><td>Phone - " & fld_desc(Ucase("phone")) & "</td><td>Fax - " & fld_desc(Ucase("fax")) & "</td><td>Cemail_add - " & fld_desc(Ucase("cemail_add")) & "</td></tr>"
					strContent1 = "<tr><td>New</td><td >"&strMemo(2)&"</td><td >"&strMemo(3)&"</td><td >"&strMemo(4)&"</td><td >"&strMemo(5)&"</td><td >"&strMemo(6)&"</td></tr>"
					strContent2 = "<tr><td>Old</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td></tr>"
					strContent3 = "<tr><td>Deleted</td><td >&nbsp;</td><td >&nbsp;</td><td>&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td></tr>"
				case "Customer"
					Response.Write " Customer"
				case "RA Request"
					strMemo(6)= getCode_desc("CDIVISION",Ucase(strMemo(6)))
					strMemo(5)= getCode_desc("REASON",Ucase(strMemo(5)))
					strHead = "<tr><td>&nbsp;</td><td>Account - " & fld_desc(Ucase("Account")) & "</td><td>Store - " & fld_desc(Ucase("store")) & "</td><td>Cwarecode - " & fld_desc(Ucase("Cwarecode")) & "</td><td>Reason - " & fld_desc(Ucase("reason")) & "</td><td>Cdivision - " & fld_desc(Ucase("Cdivision")) & "</td><td>Radate - " & fld_desc(Ucase("radate")) & "</td><td>Void - " & fld_desc(Ucase("void")) & "</td><td>Totqty - " & fld_desc(Ucase("totqty")) & "</td><td>Amount - " & fld_desc(Ucase("amount")) & "</td><td>Rano - " & fld_desc(Ucase("rano")) & "</td></tr>"
					strContent1 = "<tr><td>New</td><td >"&strMemo(2)&"</td><td >"&strMemo(3)&"</td><td >"&strMemo(4)&"</td><td >"&strMemo(5)&"</td><td >"&strMemo(6)&"</td><td>"&strMemo(7)&"</td><td>"&strMemo(8)&"</td><td >"&strMemo(9)&"</td><td >"&strMemo(10)&"</td><td >"&strMemo(11)&"</td></tr>"
					strContent2 = "<tr><td>Old</td><td >&nbsp;</td><td >&nbsp;</td><td>&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td>&nbsp;</td></tr>"
					strContent3 = "<tr><td>Deleted</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td></tr>"
				case "Remote Order"
					strMemo(5)= getCode_desc("CDIVISION",Ucase(strMemo(5)))
					strMemo(6)= getCode_desc("SEASON",Ucase(strMemo(6)))
					strHead = "<tr><td>&nbsp;</td><td>Account - " & fld_desc(Ucase("Account")) & "</td><td>Store - " & fld_desc(Ucase("store")) & "</td><td>Custpo - " & fld_desc(Ucase("Custpo")) & "</td><td>Cdivision - " & fld_desc(Ucase("cdivision")) & "</td><td>Season - " & fld_desc(Ucase("season")) & "</td><td>Start - " & fld_desc(Ucase("start")) & "</td><td>Complete - " & fld_desc(Ucase("Complete")) & "</td><td>Book - " & fld_desc(Ucase("book")) & "</td><td>Bookamt - " & fld_desc(Ucase("bookamt")) & "</td><td>Order - " & fld_desc(Ucase("order")) & "</td></tr>"
					strContent1 = "<tr><td>New</td><td >"&strMemo(2)&"</td><td >"&strMemo(3)&"</td><td >"&strMemo(4)&"</td><td >"&strMemo(5)&"</td><td >"&strMemo(6)&"</td><td >"&strMemo(7)&"</td><td >"&strMemo(8)&"</td><td >"&strMemo(9)&"</td><td >"&strMemo(10)&"</td><td >"&strMemo(11)&"</td></tr>"
					strContent2 = "<tr><td>Old</td><td  >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td></tr>"
					strContent3 = "<tr><td>Deleted</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td></tr>"
				case else
					Response.Write strMemo(1)
			end select
	''''''''''''''''''''''''''''''''''''''''''''		
	'''''''''''Edit                     '''''''''
	''''''''''''''''''''''''''''''''''''''''''''
		case "Edit"
			'Response.Write "<p align=left>Edit "&strMemo(1)&"</p>"
			select case strMemo(1)
				case "Contact"
					'Response.Write " Contact"
					strHead = "<tr><td>&nbsp;</td><td>Ccont_id - " & fld_desc(Ucase("ccont_id")) & "</td><td>Ccontttl - " & fld_desc(Ucase("ccontttl")) & "</td><td>Phone - " & fld_desc(Ucase("phone")) & "</td><td>Fax - " & fld_desc(Ucase("fax")) & "</td><td>Cemail_add - " & fld_desc(Ucase("cemail_add")) & "</td></tr>"
					strContent1 = "<tr><td>New</td><td >"&strMemo(9)&"</td><td>"&strMemo(10)&"</td><td>"&strMemo(11)&"</td><td>"&strMemo(12)&"</td><td >"&strMemo(13)&"</td></tr>"
					strContent2 = "<tr><td>Old</td><td >"&strMemo(3)&"</td><td>"&strMemo(4)&"</td><td>"&strMemo(5)&"</td><td>"&strMemo(6)&"</td><td >"&strMemo(7)&"</td></tr>"
					strContent3 = "<tr><td>Deleted</td><td >&nbsp;</td><td>&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td>&nbsp;</td></tr>"
				case "Customer"
					'Response.Write " Customer" & strRec
					strHead = "<tr><td>&nbsp;</td><td>Dba - " & fld_desc(Ucase("dba")) & "</td><td>Keeper - " & fld_desc(Ucase("keeper")) & "</td><td>Buyer - " & fld_desc(Ucase("buyer")) & "</td><td>Phone1 - " & fld_desc(Ucase("phone1")) & "</td><td>Phone2 - " & fld_desc(Ucase("phone2")) & "</td><td>Fax - " & fld_desc(Ucase("fax")) & "</td><td>Caddress1 - " & fld_desc(Ucase("caddress1")) & "</td><td>Caddress2 - " & fld_desc(Ucase("caddress2")) & "</td><td>Caddress3 - " & fld_desc(Ucase("caddress3")) & "</td><td>Caddress4 - " & fld_desc(Ucase("caddress4")) & "</td><td>Caddress5 - " & fld_desc(Ucase("caddress5")) & "</td><td>Caddress6 - " & fld_desc(Ucase("caddress6")) & "</td></tr>"
					strContent1 = "<tr><td>New</td><td >"&strMemo(16)&"</td><td>"&strMemo(18)&"</td><td>"&strMemo(19)&"</td><td>"&strMemo(17)&"</td><td >"&strMemo(20)&"</td><td >"&strMemo(21)&"</td><td >"&strMemo(22)&"</td><td >"&strMemo(23)&"</td><td >"&strMemo(24)&"</td><td >"&strMemo(25)&"</td><td >"&strMemo(26)&"</td><td >"&strMemo(27)&"</td></tr>"
					strContent2 = "<tr><td>Old</td><td>"&strMemo(3)&"</td><td>"&strMemo(5)&"</td><td >"&strMemo(6)&"</td><td>"&strMemo(4)&"</font></td><td ><font size=-1>"&strMemo(7)&"</font></td><td ><font size=-1>"&strMemo(8)&"</font></td><td ><font size=-1>"&strMemo(9)&"</font></td><td ><font size=-1>"&strMemo(10)&"</font></td><td ><font size=-1>"&strMemo(11)&"</font></td><td ><font size=-1>"&strMemo(12)&"</font></td><td ><font size=-1>"&strMemo(13)&"</font></td><td ><font size=-1>"&strMemo(14)&"</font></td></tr>"
					strContent3 = "<tr><td>Deleted</td><td >&nbsp;</td><td >&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td></tr>"
				case else
					Response.Write strMemo(1)
			end select
	''''''''''''''''''''''''''''''''''''''''''''		
	'''''''''''Del                     '''''''''
	''''''''''''''''''''''''''''''''''''''''''''
		case "Delete"
			'Response.Write "<p>Delete "&strMemo(1)&"</p>"
			select case strMemo(1)
				case "Contact"
					'Response.Write " Contact"
					strHead = "<tr><td>&nbsp;</td><td>Ccont_id - " & fld_desc(Ucase("ccont_id")) & "</td><td>Ccontttl - " & fld_desc(Ucase("ccontttl")) & "</td><td>Phone - " & fld_desc(Ucase("phone")) & "</td><td>Fax - " & fld_desc(Ucase("fax")) & "</td><td>Cemail_add - " & fld_desc(Ucase("cemail_add")) & "</td></tr>"
					strContent1 = "<tr><td>New</td><td  >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td></tr>"
					strContent2 = "<tr><td>Old</td><td  >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td><td >&nbsp;</td></tr>"
					strContent3 = "<tr><td>Deleted</td><td >"&strMemo(2)&"</td><td >"&strMemo(3)&"</td><td >"&strMemo(4)&"</td><td >"&strMemo(5)&"</td><td>"&strMemo(6)&"</td></tr>"
				case "Customer"
					Response.Write " Customer"
				case else
					Response.Write strMemo(1)
			end select
		end select
''''''''''''''''''''''''''''''''''''''''''''''''
end if
%>
<table bordercolor="#111111" border="1" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<%=strHead%>
	<%=strContent1%>
	<%=strContent2%>
	<%=strContent3%>

</table>

</BODY>

</HTML>