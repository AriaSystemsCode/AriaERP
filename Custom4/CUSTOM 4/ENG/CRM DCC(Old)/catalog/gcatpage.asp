<%@ Language=VBScript %>
<%
Response.Buffer=True
Response.Expires=-1

If Trim(Session("ID")) = "" And Session("rep")= "" Then
'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
	</script>	
<%End if

'dim arrGroups(1,2)
arrFound= split(session("showcatalogval"),",")
redim arrGroups(ubound(arrFound),2)
' redim arrGroups(ubound(arrGroups),2)

For intcount =0 to ubound(arrFound)
	arrGroups(intcount,0) = arrFound(intcount)
Next

%>
<html>
<head>

<title>CRM - Catalog</title>
<meta name="GENERATOR" content="Microsoft FrontPage 4.0">
<meta name="ProgId" content="FrontPage.Editor.Document">
<base target="_parent">
</head>

<%
' ARD - This part is to find which group has Styles and which is not and prevent those are empty from showing [Start]
Dim FoxCon
Set FoxCon = server.CreateObject("ADODB.Connection")
FoxCon.Open Application("DataConnectionString")

Dim RSGroups
Set RSGroups = Server.CreateObject("ADODB.Recordset")
For intcount =0 to ubound(arrGroups,1)
	strsql = "select * from style where style.cstygroup='" & Trim(arrGroups(intcount,0)) & "'"
	RSGroups.Open strsql,FoxCon
	IF Not(RSGroups.EOF and RSGroups.BOF) Then
		if strGroupFound = "" Then
			strGroupFound = RSGroups.Fields("cstygroup").Value 
		Else
			strGroupFound = strGroupFound & "," & RSGroups.Fields("cstygroup").Value 
		End IF
	End IF
	RSGroups.Close 
Next
strvalidGroup = Split(strGroupFound,",")
strTemp = ""
intresult = int(100/(ubound(strvalidGroup)+1))
For intcount =0 to ubound(strvalidGroup)
	
	IF strTemp = "" Then
		strTemp = intresult & "%"
	Else
		strTemp = strTemp & "," & intresult & "%"
	End IF
Next

' ARD - This part is to find which group has Styles and which is not and prevent those are empty from showing [End]
%>

<frameset rows="160,130,130,*<%'=strTemp%>" framespacing="0" border="0" frameborder="0" height="1000">
  <!--frame name="top"  target="_parent" src="menu.asp"-->
  <frame name="top"  target="_parent" scrolling="no" noresize src="../Custmenu.htm">
  <%
  Const NumPerPage = 2
  Dim CurPage
  Dim intUpLimit
  Dim intLowLimit
  
  IF Request.QueryString("CurPage") = "" Then
		CurPage = 1
  Else
		CurPage = Request.QueryString("CurPage")
  End IF
  
  intLowLimit = (CurPage * NumPerPage) - NumPerPage
  intUpLimit  = intLowLimit + NumPerPage-1
  IF intUpLimit > ubound(strvalidGroup)  Then
		intUpLimit = ubound(strvalidGroup) 
  End IF
  
  For intcount = intLowLimit to intUpLimit
		Response.Write("<frame name=""" & strvalidGroup(intcount) & """ scrolling=""no"" noresize target=""_parent"" src=""show_group.asp?grp=" & strvalidGroup(intcount) & """>")
	Next
  
  %>
  
	<frame name="bot" scrolling="no" noresize target="_parent" src="next_prev.asp?Curpage=<%=CurPage%>&UpLimit=<%=ubound(strvalidGroup)%>"> 
	<%
	    IF cint(CurPage) = Cint(ubound(strvalidGroup)) Then
  %><frame name="bot" scrolling="no" noresize target="_parent" src="blank.htm"> 
    <%End IF%>

  <noframes>
  <body leftmargin=0 topmargin=0 bgcolor="#aecae6">

  <p>This page uses frames, but your browser doesn't support them.</p>

  </body>
  </noframes>
</frameset>

</html>
