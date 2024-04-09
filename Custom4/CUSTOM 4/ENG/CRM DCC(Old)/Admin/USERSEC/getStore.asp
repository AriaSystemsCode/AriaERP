<%@ Language=VBScript %>
<%Response.Buffer = true%>
<%
'*******************************************************************************
'Page Name:  getStore.asp
'Date     :  05/23/2005
'Developer:  wal - 127795
'Purpose  :  get the stores for the selected customer
'********************************************************************************
%>
<%

'check if i m submmiting then get the last values selected into the session var and close window
if request("submit") = "T" then
	session("strRmvStr") = Request.Form ("hidStr")
	if Request.Form ("chkID") <> "" then
		arTokens = Split(Request.Form ("chkID"),", ")
		For intLoop = 0 To uBound(arTokens)
			'check if its checked before then don't add
			if instr(session("strStores"),Trim(arTokens(intLoop))) <=0 then
				if session("strStores") = "" then
					session("strStores") = Trim(arTokens(intLoop))
				else
					session("strStores") = session("strStores") & ", " & Trim(arTokens(intLoop))
				end if
			end if
		next
	
	end if	%>
	<script language="JavaScript" type="text/JavaScript">
	<!--
		window.close();
	//-->
	</script>
<%
end if	
'get customer stores
Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open Application("DataConnectionString")
Dim cnConnection 
Set cnConnection = server.CreateObject ("ADODB.Connection")
cnConnection.Open Application("SqlServer")	
Set rsStoreResult = Server.CreateObject("ADODB.RECORDSET")

strSql= "Select * from Customer where "
strSql = strSql & "type+account+Upper(store) like 'S" & request("CustID") & "%' order by store"
'Response.Write strsql
'Set records per page
Const NumPerPage = 15
	
'Retrive what page we are Currently on
Dim CurrPage
IF Request.QueryString("CurrPage") = "" Then
	CurrPage = 1
Else
	CurrPage = Request.QueryString("CurrPage")
End IF
	
'Set Cursor Location
rsStoreResult.CursorLocation = 2

'Set Cachsize to no. of records/page
rsStoreResult.CacheSize = NumPerPage
rsStoreResult.Open strSql,conn
If not rsStoreResult.eof and not rsStoreResult.bof Then
	rsStoreResult.PageSize = NumPerPage
	TotalPages = rsStoreResult.PageCount 
	rsStoreResult.AbsolutePage = CurrPage
	Dim Count 
	Count =0
else
	Response.Write "No stores exists!"
	Response.End 
end if
'save stores selected

'check if i m coming first time then clear the session 
if request("first") = "T" then
	'session("strStores") = ""
	'session("strRmvStr") = ""
	'check if there are stores saved for the user in the db
	'dim rsUserStr
	'set rsUserStr = server.CreateObject ("ADODB.recordset")
	'rsUserStr.Open "select * from privileges where  cUser_ID='" & Request("UserID") & "' and Profile='" & Request("CustID") & "' and cTokenID like 'STR%'",cnConnection
	'if not rsUserStr.EOF then
	'	do while not rsUserStr.EOF 
	'		session("strStores") = session("strStores") & ", " & rsUserStr("cTokenID")
	'	rsUserStr.MoveNext 
	'	loop
	'end if
else
	'request the values deleted if unchecked to delete them later from the table
	session("strRmvStr") = Request.Form ("hidStr")
	'check if there are values selected
	if Request.Form ("chkID") <> "" then
		'check if its checked before then don't add it 
		arTokens = Split(Request.Form ("chkID"),", ")
		For intLoop = 0 To uBound(arTokens)
			'check if its checked before then don't add
			if instr(session("strStores"),Trim(arTokens(intLoop))) <=0 then
				if session("strStores") = "" then
					session("strStores") = Trim(arTokens(intLoop))
				else
					session("strStores") = session("strStores") & ", " & Trim(arTokens(intLoop))
				end if
			end if
		next
		'if instr(session("strStores"),Request.Form ("chkID")) <=0 then
		'	if session("strStores") = "" then
		'		session("strStores") = Request.Form ("chkID")
		'	else
		'		session("strStores") = session("strStores") & ", " & Request.Form ("chkID")
		'	end if
		'end if
	end if	
end if	

%>
<HTML>
<HEAD>
<TITLE>CRM - Admin</TITLE>
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
<script language="JavaScript" type="text/JavaScript">
<!--
function chkStores()
{
	document.frmStr.action = "getStore.asp?submit=T";
	document.frmStr.submit ();
	//window.close();
}
function chk(val)
{
	var oldVal;
	if (val.checked == false)
	{
		oldVal = document.frmStr.hidStr.value;
		document.frmStr.hidStr.value  = oldVal + "," + val.value;
	}
	//alert(document.frmStr.hidStr.value);
}
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
       

<BR>
<form name=frmStr method=post>
<%'Response.Write"<font size=3>"& session("strStores")%>
<table bordercolor="#111111" border="1" align=center width=60% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<tr> 
	  <td colspan=3 class=dark_cell><div align="center"><font size="3"><strong>Assign Store(s)</strong></font></div></td>
	</tr>
	<TR>
		<TD class="dark_cell" width=5%>></td>
		<TD class="dark_cell" ><Strong>Store</Strong></TD>
		<TD class="dark_cell" ><strong>Store Name</strong></TD>
	</TR>
	<%Do While Not rsStoreResult.EOF And Count < rsStoreResult.PageSize %>
		<TR>
			<TD class="light_cell" ><input type=checkbox name="chkID" onclick="chk(this);"value="STR<%=trim(rsStoreResult("Store"))%>" <%if instr(session("strStores"),trim(rsStoreResult("Store"))) > 0 and instr(session("strRmvStr"),trim(rsStoreResult("Store"))) <= 0 then%>checked<%end if%>></td>
			<TD class="light_cell" ><%=trim(rsStoreResult("Store"))%></TD>
			<TD class="light_cell" ><%=trim(rsStoreResult("Stname"))%></TD>
		</TR>
	<%Count = Count + 1
	  rsStoreResult.MoveNext 
	  Loop%>
	<tr> <td colspan=3 class=dark_cell><div align="center"><font size="3">
	<%IF  CurrPage > 1 Then%>
		  
			<%Response.Write("<A href=""javascript:document.frmStr.action='getStore.asp?CustID="& request("CustID") &"&CurrPage=" & CurrPage - 1 & "';document.frmStr.submit();"">Prev</a> |  " )%>
	<%end if%>
	<%IF  Cint(CurrPage) <> Cint(TotalPages) Then%>
			<%Response.Write("<A href=""javascript:document.frmStr.action='getStore.asp?CustID="& request("CustID") &"&CurrPage=" & CurrPage + 1 & "';document.frmStr.submit();"">Next</a>")%>
		  
	<%end if%>
	</font></div></td></tr>
</table>
<input type=hidden name="hidStr" value="<%=session("strRmvStr")%>">
<br>
<div align=center><input type=button value="Select" onclick="chkStores()"></div>
</form>
</body>
</html>
<%

%>
