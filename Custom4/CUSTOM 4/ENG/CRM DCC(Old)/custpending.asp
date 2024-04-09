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
conn.Open "dsn=CRMSYS;uid=aria;pwd=aria"
sqlstat="select  * from syschdul where ccont_id='"&account&"' AND ccompleted='N'"
rs.open sqlstat,conn ',1,3%>
<center>
<A href="custgeneral.asp"><IMG border=0 height=23 src="images/general.jpg" width=134></a>
<A href="custpending.asp"><IMG border=0 height=23 src="images/pending%20copy.jpg" style="LEFT: 307px; TOP: 15px" width=134 ></a>
<A href="custcontact.asp"><IMG border=0 height=23 src="images/contacts.jpg" style="LEFT: 447px; TOP: 15px" width=134 ></a>
</center>
<center><h2><font color="navy" face="arial"><b>Pending</b></font><h2></center>
<table width="100%" border="1" bgcolor="#6495d0" bordercolor="ivory">
  <tr>
    <%'if Not rs.EOF or not rs.BOF then
		'Response.Write "No records were found."%>
    <%
     	Dim strTemp3' as string
			if Not rs.EOF or not rs.BOF then
			if len(trim(rs("ctrantype")))<> 0 then
				strTemp3= "<td Align=""center"" width=""14%"" ><font color=""navy"" size=""2"" face=""arial""><b>"
  			Response.Write(strTemp3)
				Response.Write("Activity")
				strTemp3= "</font></b></td>"
  			Response.Write(strTemp3)
			end if
		end if
		%>
    
     <%
     	Dim strTemp4' as string
			if Not rs.EOF or not rs.BOF then
			if len(trim(rs("dtrandate")))<> 0 then
				strTemp4= "<td Align=""center"" width=""14%""><b><font color=""navy"" size=""2"" face=""arial"">"
  			Response.Write(strTemp4)
				Response.Write("Date")
				strTemp4= "</font></b></td>"
  			Response.Write(strTemp4)
			end if
		end if
		%>
   
     <%
     	Dim strTemp5' as string
			if Not rs.EOF or not rs.BOF then
			if len(trim(rs("ctrantime")))<> 0 then
				strTemp5= "<td Align=""center"" width=""14%""><b><font color=""navy"" size=""2"" face=""arial"">"
  			Response.Write(strTemp5)
				Response.Write("Time")
				strTemp5= "</font></b></td>"
  			Response.Write(strTemp5)
			end if
		end if
		%>
    
     <%
     	Dim strTemp6' as string
			if Not rs.EOF or not rs.BOF then
			if len(trim(rs("cuser_id")))<> 0 then
				strTemp6= "<td Align=""center"" width=""14%""><b><font color=""navy"" size=""2"" face=""arial"">"
  			Response.Write(strTemp6)
				Response.Write("User")
				strTemp6= "</font></b></td>"
  			Response.Write(strTemp6)
			end if
		end if
		%>

     <%
     	Dim strTemp7' as string
			if Not rs.EOF or not rs.BOF then
			if len(trim(rs("contact")))<> 0 then
				strTemp7= "<td Align=""center"" width=""14%""><b><font color=""navy"" size=""2"" face=""arial"">"
  			Response.Write(strTemp7)
				Response.Write("Contact")
				strTemp7= "</font></b><td>"
  			Response.Write(strTemp7)
			end if
		end if
		%>
    
     <%
     	Dim strTemp8' as string
			if Not rs.EOF or not rs.BOF then
			if len(trim(rs("ctranreson")))<> 0 then
				strTemp8= "<td Align=""center"" width=""14%""><b><font color=""navy"" size=""2"" face=""arial"">"
  			Response.Write(strTemp8)
				Response.Write("Reason")
				strTemp8= "</font></b></td>"
  			Response.Write(strTemp8)
			end if
		end if
		%>
    
    
    <%
     	Dim strTemp9' as string
			if Not rs.EOF or not rs.BOF then
			if len(trim(rs("csubject")))<> 0 then
				strTemp9= "<td Align=""center"" width=""14%""><b><font color=""navy"" size=""2"" face=""arial"">"
  			Response.Write(strTemp9)
				Response.Write("Subject")
				strTemp9= "</font></b></td>"
  			Response.Write(strTemp9)
			end if
		end if
		%>
  </tr>
</table>
<table width="100%" border="1" bgcolor="#aecae6">
<%do while not rs.eof%>
  <tr>
    <td width="14%" Align="center" valign="center" bgcolor="ivory"><font color="navy" size="2" face="Arial"><b><%
    if rs("ctrantype")="C" then 
    Response.Write "Call"
   end if
    if rs("ctrantype")="A" then 
    Response.Write "Appointment"
   end if
    if rs("ctrantype")="T" then 
    Response.Write "To do"
   end if
    %></b></font></td>
		<td width="14%" Align="center" valign="center" bgcolor="ivory"><font color="navy" size="2" face="Arial"><b><%Response.Write rs("dtrandate")%></b></font></td>
    <td width="14%" Align="center" valign="center" bgcolor="ivory"><font color="navy" size="2" face="Arial"><b><%Response.Write rs("ctrantime")%></b></font></td>
    <td width="14%" Align="center" valign="center" bgcolor="ivory"><font color="navy" size="2" face="Arial"><b><%Response.Write rs("cuser_id")%></b></font></td>
    <td width="14%" Align="center" valign="center" bgcolor="ivory"><font color="navy" size="2" face="Arial"><b><%Response.Write rs("contact")%></b></font></td>
    <td width="15%" Align="center" valign="center" bgcolor="ivory"><font color="navy" size="2" face="Arial"><b>
    
    <%'set dbconn=server.CreateObject("ADODB.connection")
'set dbrs=server.CreateObject("ADODB.recordset")
'dbconn.Open "dsn=CRM;uid=aria;pwd=aria"
'sqls="select  * from syschdul where codes where cfld_name='CTRANRESON' AND CCODE_NO='"&rs("ctranreson")&"'AND CDEFCODE='N'"
'dbrs.open sqls,dbconn

%>
<%'do while not dbrs.EOF%>
    <%'Response.Write dbrs("cdiscrep")%>
    <%'dbrs.MoveNext
    'loop%>
    <%
    set dbconn=server.CreateObject("ADODB.connection")
    dbconn.Open "dsn=CRM;uid=aria;pwd=aria"
    
    set dbrs1=server.CreateObject("ADODB.recordset")
    sqls="select distinct * from codes where cfld_name='CTRANRESON' AND CCODE_NO='"&rs("ctranreson")&"'AND CDEFCODE='N'"
			dbrs1.open sqls,dbconn
    	do while not dbrs1.EOF
      Response.Write dbrs1("Cdiscrep")
    	dbrs1.MoveNext
    	loop
    	dbrs1.Close
    	dbconn.Close
		set dbrs1=nothing
		set dbconn=nothing%>
    </font></B></td>
    <td Align="center" valign="center" width="15%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><b><%Response.Write rs("csubject")%></b></font></td>
  </tr>
<%rs.MoveNext
loop%>

 </table>
<%rs.Close
'dbrs1.Close
conn.Close
'dbconn.Close
set rs=nothing
'set dbrs1=nothing
set conn=nothing
'set dbconn=nothing%>


</body>
</html>
