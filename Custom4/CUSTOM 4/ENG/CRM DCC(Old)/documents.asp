<%@ Language=VBScript %>
<%Response.Buffer = true
Response.CacheControl = "no-cache" 
Response.AddHeader "Pragma", "no-cache" 
Response.Expires = -1 %>
<%
if session("ID") = "" then
	Session("ID") = Application("ID")
	Dim Conn
	Set Conn = server.CreateObject("ADODB.Connection")
	Conn.Open Application("DataConnectionString")

	Set Session("RSCust") = Server.CreateObject("ADODB.RecordSet")
	strsql = "Select * from customer where type+account+store like'M" & Ucase(trim(Session("ID"))) & "%'"

	'WMA change Session("RSCust") to be updatable [Start]
	'Session("RSCust").open strsql,Conn
	Session("RSCust").open  strSql,conn , 2, 4
	'Session("ID")  = trim(Session("RSCust")("account"))
	'Response.Write "<font size=2>" & Session("ID")

end if
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
%>
	<script language="javascript">
	parent.location.href="login.asp"
	</script>
<%End if
IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
	strSalesRep = Ucase(Session("RSCust").fields("salesrep"))
END IF
	
IF Len(Trim(Session("rep")))>0 Then
 
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
	strSalesRep = Ucase(Trim(Session("rep")))
End IF
Function WriteFileName(strFileName)
	
	Dim intDotPos
	strReturn = strFileName
	intDotPos = InstrRev(strFileName,".")
	If intDotPos > 0 Then
		strReturn = Mid(strFileName,1,intDotPos-1)
	End If
	strPath = strVirtualPath & "/" & strFileName
	WriteFileName = "<A HREF=""javascript:openwindow ('"& strPath &"')"">" & strReturn &"</a>"

End Function
%>

<html>
<head>
<title>CRM - Documents</title>
<SCRIPT LANGUAGE=javascript>
<!--
function openwindow(filename) 
{  
	window.open(filename,'','toolbar=1,status=1,scrollbars=yes,resizable=yes,location=no,menubar=no,directories=no,top=0,left=0,width=700,height=500')
	
}

//-->
</SCRIPT>
<LINK rel="stylesheet" type="text/css" href="images/<%=Session("THEME")%>/Common.css">
</head>
<body>
<Div align='center'>
<%IF strFile = "cust" Then%>
	<SCRIPT LANGUAGE="JavaScript1.2"
	        SRC="HM_Loader_Cust.js"
	        TYPE='text/javascript'>
	</SCRIPT>
	<p><BR><BR><br></p>
<%Else%>
	<SCRIPT LANGUAGE="JavaScript1.2"
	        SRC="HM_Loader_Sales.js"
	        TYPE='text/javascript'>
	</SCRIPT>
	<p><br><br><br></p>
	<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
	<TR>

		<!-- ARD -->
		<TD colspan=13>
		<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=session("rscust")("btname")%></P>
		</TD>
		<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></a></TD>
		<!-- ARD -->
	</TR>
	</table>
<%End IF%>
 
<Table width=95% height=50 align=center border=1>
<TR>
<TD class=title>Documents</TD>
</TR>
</Table>

<BR><BR>
<%
'get the files under the folder documents if exists
strDefaultPath = server.mappath(Request.ServerVariables("PATH_INFO"))
intBSlashPos = InstrRev(strDefaultPath,"\")
strDefaultPath = Mid(strDefaultPath,1,intBSlashPos - 1)&"\Documents"
strVirtualPath = "Documents"

Dim fso
Dim f
Dim f1
Dim s
Dim sf
Dim strPathtoFind

strPathtoFind = strDefaultPath

If Right(strPathtoFind,1) <> "\" Then
	strPathtoFind = strPathtoFind & "\"
End if
'Response.Write(strPathtoFind)

Set fso = Server.CreateObject("Scripting.FileSystemObject")

Set f = fso.GetFolder(strPathtoFind)

Set sf = f.SubFolders
Set fc = f.Files
%>
<table border="0" cellpadding="0" cellspacing="3" width="80%" align=center>
<%
if fc.count > 0 then
	'Read Files
	for each f1 in fc
		Response.Write("<TR><TD width='3%'><img src=images/File3.png></TD><TD align=left>")
		Response.Write(WriteFileName(f1.name))
		Response.Write("<br></TD></TR>")
	next
else%>
	<tr><td align=center>No documents exist!</td></tr>
<%
end if
%>

</table>
</BODY>
</html>