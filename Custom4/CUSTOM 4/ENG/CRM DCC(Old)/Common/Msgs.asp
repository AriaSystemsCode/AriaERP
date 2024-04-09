<%
Response.Buffer=true
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'	Response.redirect "../default.asp"%>
<script language="javascript">
	parent.location.href = "../login.asp"
</script>
<%End if
IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF
%>
<HTML>
<HEAD>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/common.css">
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
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
<%end if%>
<p><br><br><br><br></p>
<p><br><br><br><br></p>
		<P ALIGN=CENTER><font size=3>Sorry..Your Customer Account is not ACTIVE..</font></P>		
</body>
</html>

