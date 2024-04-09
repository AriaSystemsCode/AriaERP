<%@ Language=VBScript %>
<%
Response.Buffer=True
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
	parent.location.href ="../default.asp"
	</script>
<%End if

' To handle the case of the rep.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF


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



'set records per page
Const NumPerPage = 25

'Retrive what page we are currently on
Dim CurrPage

IF Request.QueryString("CurrPage")="" Then
	CurrPage = 1		'First Page
Else
	CurrPage = Request.QueryString("CurrPage")	
END IF


set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
set rsOrdersMatch = server.CreateObject("ADODB.RecordSet")
dim bDay, bMonth,bYear,eDay,eMonth,eYear
if Trim(Ucase(Request.Form("txtBeginDate")))<>"" then
 bDay=day(Trim(Ucase(Request.Form("txtBeginDate"))))
 if bDay<10 then
	bDay = "0"&bDay
 end if
 bMonth=month(Trim(Ucase(Request.Form("txtBeginDate"))))
 if bMonth<10 then
	bMonth = "0"&bMonth
 end if
 bYear = year(Trim(Ucase(Request.Form("txtBeginDate"))))	
 Session("BeginDate")=bYear&bMonth&bDay
 'Session("BeginDate") = (Trim(Ucase(Request.Form("txtBeginDate"))))
End IF

if Trim(Ucase(Request.Form("txtEndDate")))<>"" then
 eDay=day(Trim(Ucase(Request.Form("txtEndDate"))))
 if eDay<10 then
	eDay = "0"&eDay
 end if
 eMonth=month(Trim(Ucase(Request.Form("txtEndDate"))))
 if eMonth<10 then
	eMonth = "0"&eMonth
 end if
eYear=year(Trim(Ucase(Request.Form("txtEndDate"))))
Session("EndDate")  =eYear&eMonth&eDay
'Session("EndDate")  = (Trim(Ucase(Request.Form("txtEndDate"))))
End IF

IF Request.Form("selectStatus")<>"" Then
	Session("Status") = Request.Form("selectStatus")
End IF
strSQL = strSQL & " order by order"
''''''''''''''''''''''''''''''''''''''''''''
'Set Cursor Location
rsOrdersMatch.CursorLocation = 2'adUseClient

'Set Cachesize to no. of records/page
rsOrdersMatch.CacheSize = NumPerPage

''''''''''''''''''''''''''''''''''''''''''''


'Build the query according to entered values ..
strSQL = "SELECT Ordhdr.Order, Ordhdr.complete, Ordhdr.Status, Ordhdr.Entered, Ordhdr.Store, ordhdr.cordtype,"
strSQL = strSQL & " Customer.Stname FROM Ordhdr, Customer WHERE "
strSQL = strSQL & " Customer.Store=Ordhdr.Store "
strSQL = strSQL & " AND Customer.Account ='" & CustID & "'"
'ARD - Optimization [Start]
strSQL = strSQL & " And ordhdr.account+ordhdr.cordtype+ordhdr.order like '"&custID&"%"&Request.Form("txtOrderNo")&"%' "
'ARD - Optimization [End]

IF Trim(Request.Form("txtOrderNo"))<>"" then
	testexistSQL = "SELECT Order,Status,Entered,Start, Complete, Store, Season,Custpo, Cdivision, Book, Ship, Cancel, Open, Bookamt, Shipamt,Cancelamt, Openamt,cordtype FROM Ordhdr WHERE Account+cordtype+order like'" & CustID & "%" &Trim(Request.Form("txtOrderNo"))&"'"
	'rsOrdersMatch.Open testexistSQL, conn
	Set rsorders = server.CreateObject("ADODB.RecordSet")
	rsorders.Open testexistSQL, conn

	IF rsorders.BOF AND rsorders.EOF then
		strSQL=strSQL & " AND Order LIKE '" & Request.Form("txtOrderNo") & "%'"
	Else
		Response.Redirect ("ordconfrpt.asp?OrderNo=" & Request.Form("txtOrderNo")& "&Type=" & rsorders.Fields("cordtype").Value)
	End IF
End IF
 

IF Session("BeginDate") <> "" then
	strSQL = strSQL & " AND dtos(Entered) >='" & Session("BeginDate") & "'"
end if 
if Session("EndDate") <>"" then
	strSQL = strSQL & " AND dtos(Entered) <='" & Session("EndDate") & "'"
end if
if Session("Status") <> "ALL" then
	strSQL = strSQL & " AND Ordhdr.Status='" & Session("Status") & "'"
end if
'Response.Write "<font size=3>"&strSQL&"</font>"

rsOrdersMatch.Open strSQL, conn

IF Not (rsOrdersMatch.EOF And rsOrdersMatch.BOF) Then
rsOrdersMatch.PageSize = NumPerPage
TotalPages = rsOrdersMatch.PageCount 
rsOrdersMatch.AbsolutePage = CurrPage
End IF

%>


<HTML>
<HEAD>
<SCRIPT LANGUAGE=javascript>
<!--

function hideloadingmsg() 

{
	document.all.loadingmsg.style.display = 'none';
    document.all.loadingmsg.style.visibility = 'hidden';

}

//-->

</SCRIPT>

<meta http-equiv="Content-Type" content="text/html; charset=windows-1256">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<Title>CRM - Check Order Status</Title>
<LINK rel="stylesheet" type="text/css" href="../images/<%=session("Theme")%>/order.css">
</HEAD>
<body onLoad="hideloadingmsg()">
<div name='loadingmsg' id='loadingmsg' align=center style='display:inline;visibility:visible'>
	<p align=center>
		<A align=center style="font-family=verdana;font-size=12px;font-weight=bold;"><font color=red>Please standby, ..Loading data from server.</font></A>
	</p>
</DIV>

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
<p><br><br><br><br></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
<!--	
	<TD colspan=14 background="../images/bground.gif">

	<font size="2" face="Arial">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
		<%set connTemp=server.createobject("ADODB.Connection")
			set RSTemp=server.createobject("ADODB.Recordset")
			connTemp.open Application("DataConnectionString")
			sqlTemp="select * from customer where account='" & session("customerid") & "'"
			'RSTemp.open sqlTemp,connTemp,1,3
			%>
              Your currently selected customer is <%=Session("customerid")%>
             <%response.write " - "%><%=session("rscust")("btname")%>%></font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="repcust.asp" ><font color=white size="2" face="Arial"><b>Get Customer</b></font></a>
	-->
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected <%=session("CustField")%> is <b><%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></b></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>
 <%
End IF%>
<Table width=95% align=center height=50 border=1>
<TR>
<TD class=title>Order Confirmation</TD>
</TR>
</Table>


<%IF compWork = "Y" Then%>

<br>
  <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%">
      <% 
	if rsOrdersMatch.EOF AND rsOrdersMatch.BOF then%>	
<table border=0 >
<tr><td>
		<b>No orders match your criteria. </b>
</td></tr>
<tr><td>
	<A HREF="ord_Conf.asp">back</A>
</td></tr>
</table>
	<%
	else%>
      <strong>Order(s) matching your criteria:</strong>
      
</td>
    </tr>
  </table>

<div align="center">
  <center>

<table border="1" width="95%" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
	<tr>
        <td width="76" class="dark_cell">
            <strong>
			<%=session("CustField")%>
            </strong>
		</td>
        <td colspan=4 width="514" class="light_cell">
			
			<%=Session("RSCust").fields("account")%> - <%=Session("RSCust").fields("btname")%>
			
		</td>
    </tr>
	<tr>
        <td width="76" class="dark_cell">Order #</td>
        <td width="86" class="dark_cell">Status</td>
        <td width="90" class="dark_cell" align=center>Entered</td>
        <td width="97" class="dark_cell" align=center>Completed</td>
        <td width="367" class="dark_cell"><%=session("StoreField")%></td>
    </tr>	

	
	<%
		
 'Set Count to Zero
  Dim Count
  Count = 0


		Do while not rsOrdersMatch.EOF And Count < rsOrdersMatch.PageSize%>
			<tr>
				<td width="76" class="light_cell">
					<A HREF="ordconfrpt.asp?OrderNo=<%= rsOrdersMatch("Order")%>&Type=<%=rsOrdersMatch.Fields("cordtype").Value %>">
                    <%= rsOrdersMatch("Order")%>
                  </A>
				&nbsp;</td>
				<td width="86" class="light_cell">
					<%Select case rsOrdersMatch("Status")
						case "O":
							Response.Write "Open"
						case "C":
							Response.Write "Complete"
						case "H":
							Response.Write "Hold"
						case "B":
							Response.Write "Reported"
						case "X":
							Response.Write "Canceled"
					end select%> &nbsp;</td>
				<td width="90" class="light_cell" align=center><%If rsOrdersMatch("Entered")<>"12:00:00 AM" then response.write rsOrdersMatch("Entered") Else response.write "N/A"%> &nbsp;</td>
				<td width="97" class="light_cell" align=center>&nbsp;<%If rsOrdersMatch.fields("complete")<>"12:00:00 AM" then response.write rsOrdersMatch.fields("complete") Else response.write "N/A"%>
				</td>
				<td width="367" class="light_cell">
					<% if Trim(rsOrdersMatch("Store"))= "" then
							Response.Write "Main" & " - " & rsOrdersMatch("Stname")
						else
							Response.Write rsOrdersMatch("Store") & " - " & rsOrdersMatch("Stname")
						
						end if%> &nbsp;</td>
			</tr>	
			<%
			count = count + 1
			rsOrdersMatch.MoveNext()
		Loop
	end if 
%>
</table>
  </center>
</div>
<BR>
<%	End IF %>

<Table width=95% align=center><TR><TD align=center>

<%
IF Not (rsOrdersMatch.EOF And rsOrdersMatch.BOF) Then
Response.Write "<strong>"&("Page " & Currpage & "of " & rsOrdersMatch.PageCount & "<br>")
End IF

'Display next/prev 
IF CurrPage > 1 THEN
	'show prev 
	Response.Write("<A href=""ord_conf_resp.asp?Currpage=" & Currpage-1 & """><img border=0 src=""../images/" & Session("Theme") & "/back.gif""></A>&nbsp;")
END IF
'Show next Button
IF rsOrdersMatch.EOF And rsOrdersMatch.Bof THen
Else
	IF Cint(CurrPage)<> Cint(rsOrdersMatch.PageCount) Then
		Response.Write("<A href=""ord_conf_resp.asp?Currpage=" & Currpage+1 & """><img border=0 src=""../images/" & Session("Theme") & "/next.gif""></A>")
	End IF
End IF

%>
</TD></TR></Table>
<br>
<br>
</BODY>
</HTML>