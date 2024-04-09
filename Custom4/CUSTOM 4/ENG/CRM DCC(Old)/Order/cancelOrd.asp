<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %> 
<%Response.Buffer = true%>

<%
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<Title>CRM - Remote Order</Title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</head>
<body>
<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>
<link REL="stylesheet" HREF="../images/<%=session("Theme")%>/order.css" TYPE="text/css">
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>

</table>
<%End IF%>
<%
' To handle the case of the rep.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF
'Variable definition
Dim strsql ' as string
Dim strOrder ' as string
Dim intCount, intOrdWidth ' as int

'Creat the Connection
set conne=server.CreateObject("ADODB.connection")
conne.Open Application("DataConnectionString")

conne.Execute ("update Ordhdr set status='X' WHERE ordhdr.cordtype+ordhdr.order = '" & trim(session("Type"))& trim(session("OrdNo")) &  "'")
'Dim rsHdrInfo
'Set rsHdrInfo = server.CreateObject("ADODB.recordset")
'rsHdrInfo.Open "Select status FROM Ordhdr WHERE ordhdr.account+ordhdr.cordtype+ordhdr.order like '" & CustID & session("Type")& session("OrdNo") &  "'",conne,1,3
'Response.Write"<font size=2>"& rsHdrInfo.RecordCount  & "<br>" & rsHdrInfo.Source & "<hr>"

'rsHdrInfo("status") = "X"
'rsHdrInfo.Update ()
%>
  <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr> <!--td width="100%" class=MessageFont>Your order has been successfully saved in the system under this number: <strong><%=Request("order")%></strong></td-->
      <td width="100%" class=MessageFont>Your order has been cancelled!</td></tr>
      <%if (Session("PNPCase")= "DepositRefund" or Session("PNPCase")="DepositCancel") and Session("PNPFinalStatus") = "success" and Session("PNPcard-amount")<>0 then  %>
      	<script language="javascript">
		window.alert("You have been Refunded $ <%=Session("PNPcard-amount")%>")
		</script>	      
      <%end if%>
      <%Session("PNPcard-amount")=0 %>
  </table>
</body>
</html>