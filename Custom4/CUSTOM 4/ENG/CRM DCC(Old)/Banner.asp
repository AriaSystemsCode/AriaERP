<HTML>
<HEAD>
<TITLE>Login1_Theme1</TITLE>
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=iso-8859-1">
<LINK rel="stylesheet" type="text/css" href="images/<%=Session("THEME")%>/common.css">
<SCRIPT LANGUAGE=javascript>

		//alert('<%=trim(session("ID"))%>');

</SCRIPT>
</HEAD>
<%
'wal_load the customer logo
'check if the file exists
Dim objFile
Set objFile = Server.CreateObject("Scripting.FileSystemObject")
strfile = server.MapPath("CustomerLogo/" & trim(session("ID")) & ".jpg") 
%>
<body topmargin="0" leftmargin="0" marginheight="0" marginwidth="0">
<TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
  <tr>
	<td width=70% bgcolor="#4166B7" ><img src="DCC.jpg" width=35% height=70></td>
	<%if objFile.FileExists (strfile) then%>
		<td bgcolor="#4166B7" ><img src="CustomerLogo/<%=trim(session("ID"))%>.jpg" width=100% height=70></td>
	<%else%>
		<td bgcolor="#4166B7" ></td>
	<%end if%>
</tr>
</TABLE>
</BODY>
</HTML>