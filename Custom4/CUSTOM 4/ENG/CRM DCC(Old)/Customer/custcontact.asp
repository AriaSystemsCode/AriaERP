<%@ Language=VBScript %>
<html>
<head>
<meta NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
</head>
<body bgcolor="#aecae6">
<%
'account=Request.form("text1")
account=Session("ID")
set conn=server.CreateObject("ADODB.connection")
set rs=server.CreateObject("ADODB.recordset")
conn.Open "dsn=CRM;uid=aria;pwd=aria"
sqlstat="select * from contact where  ccont_id='" & account & "'"
rs.open sqlstat,conn ',1,3%>
<center>
<a href="custgeneral.asp"><img border="0" height="23" src="images/general.jpg" width="134"></a>
<a href="custpending.asp"><img border="0" height="23" src="images/pending.jpg" width="134"></a><img src="images/contacts%20copy.jpg" WIDTH="134" HEIGHT="23">
</center>
<center><font color="navy" size="5"><b>Contacts</b></font></center>
<br>
<table width="100%" border="1" bgcolor="#6495d0" bordercolor="#aecae6">
  <tr>
    
    <%
     	Dim strTemp2' as string
if Not rs.EOF or not rs.BOF then
			if len(trim(rs("contact")))<> 0 then
				strTemp2= "<td align=""center"" width=""20%""><b><font color=""Navy"" size=""2"" face=""arial"">"
  			Response.Write(strTemp2)
				Response.Write("Contact")
				strTemp2= "</font></b></td>"
  			Response.Write(strTemp2)
			end if
		end if
		%>
    
    <%
     	Dim strTemp4' as string
if Not rs.EOF or not rs.BOF then
			if len(trim(rs("ccontttl")))<> 0 then
				strTemp4= "<td align=""center"" width=""20%""><b><font color=""Navy"" size=""2"" face=""arial"">"
  			Response.Write(strTemp4)
				Response.Write("Title")
				strTemp4= "</font></b></td>"
  			Response.Write(strTemp4)
			end if
		end if
		%>
		
<%
     	Dim strTemp5' as string
if Not rs.EOF or not rs.BOF then
			if len(trim(rs("phone")))<> 0 then
				strTemp5= "<td align=""center"" width=""20%""><b><font color=""Navy"" size=""2"" face=""arial"">"
  			Response.Write(strTemp5)
				Response.Write("Phone")
				strTemp5= "</font></b></td>"
  			Response.Write(strTemp5)
			end if
		end if
		%>    
    <%
     	Dim strTemp3' as string
if Not rs.EOF or not rs.BOF then
			'if len(trim(rs("fax")))<> 0 then
				strTemp3= "<td align=""center"" width=""20%""><b><font color=""Navy"" size=""2"" face=""arial"">"
  			Response.Write(strTemp3)
				Response.Write("Fax")
				strTemp3= "</font></b></td>"
  			Response.Write(strTemp3)
			'end if
		end if
		%>
    
    
    <%
     	Dim strTemp6' as string
if Not rs.EOF or not rs.BOF then
			if len(trim(rs("ccontsalut")))<> 0 then
				strTemp6= "<td align=""center"" width=""20%""><b><font color=""Navy"" size=""2"" face=""arial"">"
  			Response.Write(strTemp6)
				Response.Write("Salutation")
				strTemp6= "</font></b></td>"
  			Response.Write(strTemp6)
			end if
		end if
		%>
		
  </tr>
</table>

<table width="100%" border="1" bgcolor="#6495d0" >

<%do while not rs.eof%>
  <tr>
    <td align="center" valign="center" width="20%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("contact")%></font></td>
    <td align="center" valign="center" width="20%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("ccontttl")%></font></td>
    <td align="center" valign="center" width="20%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("phone")%></font></td>
    <td align="center" valign="center" width="20%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("fax")%></font></td>
    <td align="center" valign="center" width="20%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("ccontsalut")%></font></td>
  </tr>
<%rs.MoveNext
loop%>
</table>
<%rs.Close
conn.Close
set rs=nothing
set conn=nothing
%>
</body>
</html>
