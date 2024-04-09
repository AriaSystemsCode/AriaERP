<html>
<head>
<title>CRM - Catalog</title>
<meta name="GENERATOR" content="Microsoft FrontPage 4.0">
<meta name="ProgId" content="FrontPage.Editor.Document">
</head>
<Body leftmargin=0 topmargin=0 bgcolor="#aecae6">
<%
Dim FoxCon
Set FoxCon = server.CreateObject("ADODB.Connection")
FoxCon.Open Application("DataConnectionString")

Const NumPerPage= 5
Dim CurPage

If Request.QueryString("CurPage")= "" Then
	CurPage = 1
Else
	CurPage = Request.QueryString("CurPage")
End IF

Dim RSGroups
Set RSGroups = Server.CreateObject("ADODB.Recordset")
strsql = "select DISTINCT style.cstymajor, codes.cdiscrep from style, codes where style.cstygroup='" & Trim(request("grp")) & "' and codes.ccode_no='" & Trim(request("grp")) & "'"

RSGroups.CursorLocation = 3
RSGroups.CacheSize = NumPerPage

RSGroups.Open strsql,FoxCon 

RSGroups.PageSize = NumPerPage
TotalPages = RSGroups.PageCount 
IF not (RSGroups.Eof And RSGroups.Bof) Then
	RSGroups.AbsolutePage = CurPage
End IF

Dim Count 
Count = 0
%>
	<font face=arial>
	<table border="0" width="750">
	  <tr>
	    <td width="16%" valign=top align=center><%
	    IF nOt (RSGroups.Eof And RSGroups.BOF )		 Then
				Response.Write("<B>" & RSGroups.Fields("cdiscrep").Value & "</B>")
	    End IF
%></td>
	    
<%
Do while Not RSGroups.EOF  And count < NumPerPage
	%>
	    <td width="20%"><a target="_parent" href="catstyle.asp?style=<%=RSGroups.Fields("cstymajor").Value%>"><img src="../styimg/<%=Trim(RSGroups.Fields("cstymajor").Value)%>.jpg" width=83 height=109 border=0></a></td>
	<%
	Count = Count + 1
	RSGroups.MoveNext 
Loop
%>
	  </tr>
	  <tr>
	    <td width="16%"></td>
	    <td width="16%"></td>
	    <td width="17%"></td>
	    <td width="17%"></td>
	    <td width="17%"></td>
	    <td width="17%"></td>
	  </tr>
	</table>
	
<%
IF CurPage > 1 Then
%>
	<a href="show_group.asp?grp=<%=Trim(request("grp"))%>&CurPage=<%=CurPage - 1%>"> Previous </a>
<%
End IF
IF Not(RSGroups.EOF And RSGroups.BOF) Then
	IF cint(CurPage) <> Cint(TotalPages) Then
	%>
		<a href="show_group.asp?grp=<%=Trim(request("grp"))%>&CurPage=<%=CurPage + 1%>"> Next </a>
	<%
	End IF
End IF


%>


</font>

</Body>
</Html>
