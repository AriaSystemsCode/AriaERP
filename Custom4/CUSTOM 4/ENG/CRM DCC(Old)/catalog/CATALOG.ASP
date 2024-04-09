<%

Response.CacheControl = "no-cache" 
Response.AddHeader "Pragma", "no-cache" 
Response.Expires = -1 
response.Buffer =true
'Response.Write "<font size=2>" &trim(Session("RSCust")("account"))
'Response.End 
'Response.Write IsObject(Session("RSLine")) & session("ID")
'Response.End 
'WMA change Session("RSCust") to be updatable [end]
''WMA anwanted redirect[Start]
'Session("Store") = ""
'Session("StoreID")="MAIN"
'Set conn=server.CreateObject("ADODB.connection")
'conn.Open Application("DataConnectionString")

'Set RSSetups = Server.CreateObject("ADODB.RecordSet")
'RSSetups.Open "SELECT Mdata_def FROM setups where Capp_id = 'SO' AND Cfld_name = 'M_COMPDATE'",Conn

'intCompDate = RSSetups("Mdata_def")
'Session("Start") =date()
'Session("Completed") = date() + intCompDate
'Response.Redirect  "CatSearch.asp"
''WMA anwanted redirect[End]



'WMA avoid empty RSLine in every click cataloge in the menu [start]
'Session("PO")=""
'Session("ordQty")=""
'Session("ordAmount")=""
'WMA avoid empty RSLine in every click cataloge in the menu [end]


If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
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

<html>
<head>
<Title>CRM - Catalog</Title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/Catalog.css" TYPE="text/css" >
<SCRIPT LANGUAGE=javascript>
<!--
function hideloadingmsg() 
{
	document.all.loadingmsg.style.display = 'none';
	document.all.loadingmsg.style.visibility = 'hidden';
}
//-->
</SCRIPT>

</head>

<body onLoad="hideloadingmsg();">
<!--<div name='loadingmsg' id='loadingmsg' align=center style='display:inline;visibility:visible'>-->
<p align=center>
<A align=center style="font-family=verdana;font-size=12px;font-weight=bold;"><font color=red>Please standby, ..Loading data from server.</font></A>
</p>
</DIV>

<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><BR><BR><BR></p>
<%Else%>

<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><BR></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected customer is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></P>
	</TD>
	<TD align=right><a href="repcust.asp">Get Customer</a></TD>
	<!-- ARD -->
</TR>
</table>
 <%End IF%>
<Table width=95% height=50 align=center border=1>
<TR>
<TD class=title>Catalog</TD>
</TR>
</Table>
<%IF compWork = "Y" Then%>

<Table border=0 width=95% align=center>
<TR>
	<TD>
		<b>Define your search criteria to browse our catalog.</b>
	</TD>
</TR>
</table>


<%'Response.Write "<font size=5>compWork =="&compWork&"<font><br>"
'Response.End %>
<p>
<FORM action="catlines.asp?PageID=<%=Request.QueryString("PageID")%>" id=FORM1 method=post name=FORM1>
<P>
<TABLE border=1  width=95% bgcolor="#6495D0" bordercolor="#AECAE6" align=center>
  <TR>
    <TD width="30%"><font face="Arial" size=2 color="#000080"><strong>Season</strong><font></TD>
    <TD width="70%">
		<%
		
		Dim RSSeason,RSDSeason ' as ADODB.RECORDSET
		Dim RSSetups 'as ADODB.RECORDSET

		Dim strTemp,ID ' as string
		Dim intCompDate ' as integer
		Dim RShdr 'as Recordset
		
		Session("ConnectionString") = Application("DataConnectionString")
		Set Conn = Server.CreateObject("ADODB.Connection")
		Conn.Open(Session("ConnectionString"))

		Set RSSetups = Server.CreateObject("ADODB.RecordSet") ' GET THE COMPLETETION DATE
		RSSetups.Open "SELECT Mdata_def FROM setups where Cfld_name = 'M_COMPDATE' AND Capp_id = 'SO' ",Conn

		set connSQL= server.CreateObject ("ADODB.Connection")
		
		'HDM is this condidtion goes vise versa
		'If we attempt to connect to SQL why we pass an Oracle Connection string????
		'Comment all this section out and fix
		'also why the UID and PWD are hard coded????
		'finally we have an application variable that holds the connection information why not use it???
		'if Application("DBType") = "SQL" then
		'	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=OraOLEDB.Oracle;Data Source=DB99;user id=ARIA;password=ARIAADMN"
		'else
		'	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source="&Trim(session("SQLServer"))&";uid="&Trim(Session("SqlUserName"))&";pwd="&Trim(Session("SqlPassWord"))
		'end if
		'Response.Write(Application("SqlServer"))
		'Response.End 		
		'hdm [End]
		'Response.Write("Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source="&session("SQLServer")&";uid=" & Session("SqlUserName") & ";pwd=" & Session("SqlPassWord") & "")
		'Response.End 
		connSQL.Open "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source="&session("SQLServer")&";uid=" & Session("SqlUserName") & ";pwd=" & Session("SqlPassWord") & ""

		'Get packs ID from SQl Server tables
		set rsPacks = Server.CreateObject ("ADODB.Recordset")
		strSQL= "select CustGroup.PackID from CustClassification ,CustGroup  "
		strSQL = strSQL & " where CustClassification.CustGroup=CustGroup.GroupID "
		strSQL = strSQL & " AND CustClassification.CustID='" & Trim(CustID) & "'"
		rsPacks.Open strSQL,connSQL,1,3



		'GET the Pack_id Information.
		IF Not (rsPacks.EOF And rsPacks.BOF) Then
			strsql = "Select PACK_ID, ACCOUNT, SEASON, CDIVISION from spck_hdr where "
			strsql = strsql & " type+account+pack_id = 'P*****" & Trim(rsPacks.Fields("PackID").Value) & "'"
		Else
			strsql = "Select PACK_ID, ACCOUNT, SEASON, CDIVISION from spck_hdr where "
			strsql = strsql & " type+account+pack_id like 'P" & Trim(CustID) & "WEB%" & "'"
		End IF

		Set RShdr = Server.CreateObject("ADODB.RECORDSET")
		RShdr.Open strSql,Conn,2,4
		IF Not(RShdr.EOF And RShdr.BOF) Then

			IF Trim(RShdr.Fields("SEASON").Value) = "*" Then ' CASE OF ALL SEASONS
				Set RSSeason = Server.CreateObject("ADODB.RecordSet")
				RSSeason.Open "SELECT * FROM codes where cdefcode+crltfield+cfld_name = 'NNSEASON'",Conn

				Set RSDSeason = Server.CreateObject("ADODB.RecordSet")
				RSDSeason.Open "SELECT * FROM codes where cdefcode+crltfield+cfld_name = 'DNSEASON'",Conn
				%>
				<SELECT id=slctSeason name=slctSeason style="HEIGHT: 22px"> 
				<OPTION value="All">All
				<%If Not(RSSeason.EOF And RSSeason.BOF) Then%> 
					<%RSDSeason.MoveFirst %>
					<OPTION SELECTED value=<%=RSDSeason("Ccode_no")%> ><%=RSDSeason("Cdiscrep")%>
					<%Do While Not RSSeason.EOF %>
					<%
						If RSDSeason("Cdiscrep") = RSSeason("Cdiscrep") Then
							RSSeason.MoveNext
						Else
						strTemp = "<OPTION value=" & RSSeason("Ccode_no") & ">" & RSSeason("Cdiscrep")
						Response.Write(strTemp)
						RSSeason.MoveNext		
						End IF
					%>
					<%loop%>
					</SELECT>
				<%End if%>
				<%				
				Session("Division") = Trim(RShdr.Fields("CDIVISION").Value) 
			Else
				Session("Season") = Trim(RShdr.Fields("SEASON").Value)
				Session("Division") = Trim(RShdr.Fields("CDIVISION").Value) 
				Response.Write(Session("Division") & "|")
				Session("Store") = ""
				'Session("StoreID")="MAIN"
				Response.Redirect("catlines.asp?PageID="&Request.QueryString("PageID"))
			End IF
		Else
			'Response.Write "ok"
			'Response.End 
			Response.Write("<Table width=""95%"" align=center><tr><td>No Packs assigned for this customer.</td></tr></table>")
			Response.Redirect("catlines.asp?PageID="&Request.QueryString("PageID"))
			'Response.End 
		End IF
		%>
    </TD>
  </TR>
      <%
      intCompDate = RSSetups("Mdata_def") 
      Session("Start") =date()
			Session("Completed") = date() + intCompDate
			 %>
  </Table>
<table width=95% align=center> 
    <tr>
        <td width="50%">
        </td>
			<td width="50%">
			<p align="right">
			<input type=submit name=submit1 value="Next"></P></TD>
  </TR>
</TABLE>

</FORM>
<P></P>
<%

End IF
%>



</body>
</html>
