<%@ Language=VBScript %>
<%
Response.Buffer=True
Response.Expires=-1%>
<%
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	Response.redirect "default.htm"
End if

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	strFile= "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF
%>

<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<LINK rel="stylesheet" type="text/css" href="images/<%=Session("THEME")%>/common.css">
<Title>CRM - Customer Selected</Title>

<SCRIPT LANGUAGE=javascript>
<!--
function SubmitNext()
{
	document.FORM2.action = 'orderhsave.asp';
	document.FORM2.submit()
}
//-->
</SCRIPT>

</head>
<body>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>

<p><br><br><br><br></p>

<%'Response.Write "<font size=3>Customer ID"&Request("CustID")&"111</font>"
'Response.End

Session("customerid")= Request("CustID")
session("ID") = Request("CustID")
application("ID") = Request("CustID")
set conn=server.CreateObject("ADODB.connection")
set Session("RSCust")=server.CreateObject("ADODB.recordset")
conn.Open Application("DataConnectionString")
Dim strsql ' as string
strsql="select * from customer where account='"& ucase(Request("CustID")) &"' AND type='M'"
Session("RSCust").open strsql,conn,1,3 
'wal_039385 get defualt warehouse for customer if exists
	set rsware = server.CreateObject ("ADODB.Recordset")
	rsware.Source = "select * from customerwarecode where ccustid='"& trim(session("ID")) &"'"
	rsware.ActiveConnection = Application("SqlServer")
	rsware.Open 
	if not rsware.EOF then
		application("WareCode") = rsware("cwarecode")
	end if
	rsware.Close
%>
<div align="center">
  <center>

 <table border="0" width=95% cellspacing="0" cellpadding="0" style="border-collapse: collapse">
    <tr>
      <td width="75%">Your currently selected <%=session("CustField")%> is <%=Session("RSCust").fields("Account")%> -
         <%=Session("RSCust").fields("stname")%></td>
      <TD width=25% align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>"></TD>
    </tr>
  </table>
  </center>
</div>
<p> </p>
</BODY>
</HTML>