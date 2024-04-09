<%@ Language=VBScript %>
<!--#include file="../Connections/cnConn.asp" -->
<%Response.Buffer = true%>
<%
'*******************************************************************************
'Page Name:  EditProfile.asp
'Date     :  03/20/2006
'Developer:  wal
'Purpose  :  Action page to edit the style profile
'********************************************************************************
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<TITLE>CRM/Admin - Select Style Profile</TITLE>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<link href="../../Images/<%=session("theme")%>/Common.css" rel="stylesheet" type="text/css">
<BODY bgcolor=white leftmargin=0 topmargin=0>
<%	
dim cnnSQL
set cnnSQL = server.CreateObject ("ADODB.Connection")
cnnSQL.Open Application("SqlServer") 
'Add user in syuuser table
Dim strSql, rsProfileHdr
set rsProfileHdr = Server.CreateObject("ADODB.RecordSet")
set rsProfileDt = Server.CreateObject("ADODB.RecordSet")
'check if the code exists before
on error resume next
strSql = "Select * From styleProfileHeader"
Const NumPerPage  = 25

Dim CurPage
If Request.QueryString("CurPage") = "" then
    CurPage = 1 'We're on the first page
Else
    CurPage = Request.QueryString("CurPage")
End If
rsProfileHdr.CursorLocation = 2
rsProfileHdr.CacheSize = NumPerPage
rsProfileHdr.CursorType = 3
rsProfileHdr.Open strSql,cnnSQL,1,3
'Response.Write rsUser.RecordCount 
if rsProfileHdr.eof then
	Response.Write("<center><font color=red size=2><B>No codes exists!!</b>")
	Response.Write("<br><a href='javascript:window.history.back();'>Back</font></a>")
	Response.End
Else
	rsProfileHdr.PageSize = NumPerPage
	TotalPages = rsProfileHdr.PageCount 
	rsProfileHdr.AbsolutePage = CurPage

	Dim Count
	Count = 0
%>
<script language="JavaScript" type="text/JavaScript">
<!--
function setValue(val)
{
	opener.document.form1.action = "Addprofile.asp?selCode="+val;
	opener.document.form1.submit();
	window.close();
}
//-->
</script>
<body bgcolor="#3366CC" text="#FFFFF0">
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
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/DCC logo.jpg" WIDTH=60 HEIGHT=59></TD>
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
<form name=form1 method=post>
 <Table  align=center width=80%>
	<tr> 
	  <td class="Dark_Cell"><div align="right"><a href="Default.asp"><font color="#FFFFFF">
	  Back to Style Profile</font></a></div></td>
	</tr>
	<TR>
	<TD class=Title>Select Style Profile</TD>
	</TR>
</TAble>
 <table border="1" width="80%" align=center cellpadding="0" cellspacing="0" "#111111">
	<TR>
		<TD width=22% class="dark_cell">Profile Code</TD>
		<TD Width=45% class="dark_cell">Name</TD>
	</TR>
  <%
	Do While Not rsProfileHdr.EOF And Count < NumPerPage	
  %>
		<TR>
			<TD class="light_cell" >
			<%'check if i m coming to select then set the value and close the window
			  if Request.QueryString ("select") = "T" then
			%>	<a href="javascript:setValue('<%=trim(rsProfileHdr("cStyleProfileCode"))%>');"><%=trim(rsProfileHdr("cStyleProfileCode"))%></a>
			<%else%>
				<a href="Addprofile.asp?code=<%=trim(rsProfileHdr("cStyleProfileCode"))%>"><%=trim(rsProfileHdr("cStyleProfileCode"))%></a>
			<%end if%>
			</TD>
			<TD class="light_cell" >&nbsp<%=trim(rsProfileHdr("cStyleProfileName"))%></TD>
		</TR>
  <%
	rsProfileHdr.MoveNext	
	Count = Count + 1	
	Loop
  %>
</table>
<Table border=0 width=95% align=center><TR><TD>
<%IF count > 0 Then
		Response.Write("<Center><font face=Arial size=2>Page " & CurPage & " of " & TotalPages & "</font></center>")
		Response.Write "<table align=center border=0><tr><td>"
		if CurPage > 1 then
				'We are not at the beginning, show the back button%>
				<a href="javascript:document.form1.action='editprofile.asp?curpage=<%=curpage - 1%>';document.form1.submit();"><img border=0 src="../../images/<%=Session("theme")%>/back.gif"></a>
		<%End If

			if CInt(CurPage) <> CInt(TotalPages) then
		    'We are not at the end, show a next button%>
				<a href="javascript:document.form1.action='editprofile.asp?curpage=<%=curpage+ 1%>';document.form1.submit();"><img border=0 src="../../images/<%=Session("theme")%>/next.gif"></a>
		<%End If
	End IF
%>
</TD>
</TR>
</Table>
</form>
<%	
End if
%>

</BODY>
</HTML>
