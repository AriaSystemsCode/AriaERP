<%@ Language=VBScript %>
<%Response.Buffer=true

If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.htm"%>
	<script language="javascript">
	parent.location.href="../login.asp"
	</script>
<%END IF

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	custid = trim(session("ID"))
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	else
		custid = Session("customerid")
	END IF
	strFile = "reb"
End IF

%>


<HTML>
<HEAD><LINK REL=stylesheet HREF="../images/<%=Session("Theme")%>/invoice.css" TYPE="text/css">
<meta http-equiv="Content-Type" content="text/html; charset=windows-1256">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<Title>CRM - Invoice Status</Title>
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
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected <%=Session("custField")%> is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>
 <%End IF%>
<Table width=95% align=center height=50 border=1>
<TR>
<TD class=title>Check Invoice Status</TD>
</TR>
</Table>

<%
Set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
Set rsInvoiceMatch = server.CreateObject("ADODB.RecordSet")

'Set records per page
Const NumPerPage = 25

'Retrive what page we are currently on
Dim CurrPage
IF  Request.QueryString("CurrPage") = "" Then
	CurrPage = 1
Else
	CurrPage = Request.QueryString("CurrPage")
End IF

'Set cursor location
rsInvoiceMatch.CursorLocation = 2

'Set CachSize
rsInvoiceMatch.CacheSize = NumPerPage
dim bDay,bMonth,bYear,eDay,eMonth,eYear
 
IF Trim(Ucase(Request.Form("txtBeginDate")))<>"" Then
	bDay = day(Trim(Ucase(Request.Form("txtBeginDate"))))
	if bDay<10 then
		bDay = "0"&bDay
	end if
	bMonth=month(Trim(Ucase(Request.Form("txtBeginDate"))))
	if bMonth<10 then
		bMonth = "0"&bMonth
	end if
	bYear = year(Trim(Ucase(Request.Form("txtBeginDate"))))
	Session("BeginDate") =bYear&bMonth&bDay
	'Session("BeginDate") = (Trim(Ucase(Request.Form("txtBeginDate"))))
End IF

IF Trim(Ucase(Request.Form("txtEndDate")))<>"" Then
	eDay = day(Trim(Ucase(Request.Form("txtEndDate"))))
	if eDay<10 then
		eDay = "0"&eDay
	end if
	eMonth=month(Trim(Ucase(Request.Form("txtEndDate"))))
	if eMonth<10 then
		eMonth = "0"&eMonth
	end if
	eYear = year(Trim(Ucase(Request.Form("txtEndDate"))))
	Session("EndDate") =eYear&eMonth&eDay

'	Session("EndDate")  = (Trim(Ucase(Request.Form("txtEndDate"))))
End IF

IF Request.Form("selectStatus") = "ALL" Then
	Session("Status") = "*"
Else
	Session("Status") = Request.Form("selectStatus")
End IF


IF Len(trim(session("rep")))>0 Then
	STRSQL = "SELECT * FROM CUSTOMER WHERE Type+ACCOUNT+Store like 'M" & session("customerid") & "%'"
	Set RSTemp = Server.CreateObject("ADODB.Recordset")
	RSTemp.Open strsql,conn

	IF RSTemp.EOF And RSTemp.Bof Then
		Response.Redirect "repfindcustomer.asp?CUST=" & session("customerid")
	Else
		strSQL = "SELECT distinct Invhdr.Invoice, Invhdr.Order, Invhdr.Status,Invhdr.ShipAmt, Invhdr.Ship, Invhdr.Invdate, Invhdr.Shipdate, Invhdr.Store, "
		strSQL = strSQL & " Customer.Stname FROM Invhdr, Customer WHERE "
		strSQL = strSQL & " (customer.type+customer.account+customer.Store like 'M" & custid& "%' or customer.type+customer.account+customer.Store like 'S" & custid& "%') "
		strSQL = strSQL & " and (Customer.Store=Invhdr.Store or ALLTRIM(Invhdr.Store)='MAIN')"
		'check if to add check on sales rep or he's admin
		if Session("Authority") <> "Full" then
			strSQL = strSQL & " and ( (Invhdr.rep1='"& session("rep") &"' and Invhdr.rep1=customer.salesrep) or (Invhdr.rep2='"& session("rep") &"'"
			strSQL = strSQL & " and Invhdr.rep1=customer.rep2) )  "
		end if
		strSQL = strSQL & " and Invhdr.account+Invhdr.Invoice like '" & custid& "%' "

	END IF
ELSE ' not Rept
'  strSQL = "SELECT distinct Invhdr.Invoice, Invhdr.Order, Invhdr.Status, Invhdr.Invdate, Invhdr.Store, Customer.Stname FROM Invhdr, Customer WHERE Customer.Store=Invhdr.Store  and Invhdr.account='"&session("id")&"' and customer.account=invhdr.account"	
  strSQL = "SELECT distinct Invhdr.Invoice, Invhdr.Order, Invhdr.Status, Invhdr.ShipAmt, Invhdr.Ship, Invhdr.Invdate, Invhdr.Shipdate,  "
  strSQL = strSQL & " Invhdr.Store, Customer.Stname FROM Invhdr, Customer WHERE "
  strSQL = strSQL & " Invhdr.account+Invhdr.invoice like '"&custid&"%' "
  strSQL = strSQL & " and (customer.type+customer.account+customer.store like 'M"&session("id")&"%' "
  strSQL = strSQL & " or customer.type+customer.account+customer.store like 'S"&session("id")&"%') "				
  strSQL = strSQL & " and (Customer.Store=Invhdr.Store or ALLTRIM(Invhdr.Store)='MAIN' )  "
  'strSQL = strSQL & " order by invoice desc"

END IF

IF Trim(Request.Form("txtInvoiceNo"))<>"" Then
	testexistSQL = "SELECT distinct Invoice, Order,Status,Invdate,Shipdate, Duedate, Store, Season,Custpo, Cdivision FROM Invhdr where account+Invoice like '" &custid & Trim(Request.Form("txtInvoiceNo")) & "%'"
	Set RSInv = Server.CreateObject("ADODB.RecordSet")
	RSInv.Open testexistSQL, conn
	IF RSInv.BOF AND RSInv.EOF Then
		'strSQL=strSQL & " AND Invoice LIKE '" & Request.Form("txtInvoiceNo") & "%'"
	Else
		RSInv.close()
		Response.Redirect ("invoicedetail.asp?InvNo=" & Request.Form("txtInvoiceNo"))
	END IF
END IF

IF Session("BeginDate") <> "" Then
	strSQL = strSQL & " AND dtos(Invdate) >='" & Session("BeginDate") & "'"
END IF
IF Session("EndDate") <> "" Then
	strSQL = strSQL & " AND dtos(Invdate) <='" & Session("EndDate") & "'"
END IF
'strSQL = strSQL & " order by invoice desc"
strSQL = strSQL & " order by Invdate desc, invoice desc"

'Response.Write("<font size=3>"&strSQL& "</font>")
rsInvoiceMatch.Open strSQL, conn
'Response.Write ("<font size=3>"&rsInvoiceMatch.ActiveConnection& "</font>")



IF Len(trim(session("rep")))>0 Then 
	'session("customerid")=request("txtcustid")
	Set rs=server.CreateObject("ADODB.recordset")
	sqls="Select * from  customer where type+account+store='M"&session("customerid")&"'"
	rs.Open sqls,conn,1,3
END IF
%>

<br>
  <table width=95%  align=center>
    <tr>
      <td width="100%">
  
  <% IF rsInvoiceMatch.EOF AND rsInvoiceMatch.BOF Then%>	
		<b>There are no records matching your selected criteria </b>
		<BR>
		<%id=session("customerid")%>
		<%If trim(Session("ID"))<>"" Then%>
		<A HREF="invoice.asp">back</A>
			<%Else%>
		<A HREF="repinvoice.asp?id=<%=id%>">back</A>	
		<%End If%>
	<%Else
	rsInvoiceMatch.PageSize = NumPerPage
	TotalPages = rsInvoiceMatch.PageCount 
	rsInvoiceMatch.AbsolutePage = CurrPage
	%>
    </tr>
  </table>
<div align="center">
  <center>
  <table width=95% border="0" cellpadding="0" cellspacing="0">
  		<tr>
			<td ><strong>The following invoices match your selected criteria:</strong></td>
		</tr>
  </table>
<table width=95% border="1" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
	<tr>
        <td class="dark_cell">
			<strong><%=session("CustField")%>
		</td>
        <td colspan=6 class="light_cell">
			
			<%IF Len(trim(session("rep")))>0 Then%>
			<%=rs("account")%> - <%=rs("btname")%>
			<%Else%>
			<%=Session("RSCust").fields("account")%> - <%=Session("RSCust").fields("btname")%>
			<%END IF%>
			
		</td>
    </tr>
	<tr>
        <td class="dark_cell">Invoice #</td>
        <td class="dark_cell">Order #</td>
        <td class="dark_cell" align=center>Invoice Date</td>
        <td class="dark_cell" align=center>Ship Date</td>
        <td class="dark_cell" align=right>Shipped Qty.</td>
        <td class="dark_cell" align=right>Shipped Amount&nbsp;</td>
        
        <td class="dark_cell"><%=session("StoreField")%></td>
    </tr>
    
<%
		Dim Count
		Count = 0
		
		Do While Not rsInvoiceMatch.EOF And Count < rsInvoiceMatch.PageSize %>
			<tr>
				<td class="light_cell">
					<A HREF="invoicedetail.asp?InvNo=<%= rsInvoiceMatch("Invoice")%>"><%= rsInvoiceMatch("Invoice")%></A> &nbsp;</td>
				<!--td class="light_cell">
					<%Select case rsInvoiceMatch("Status")
						Case "O":
							Response.Write "Open"
						Case "C":
							Response.Write "Complete"
						Case "V":
							Response.Write "Void"
					END SELECT%> &nbsp;</td-->
				<td class="light_cell"><%=rsInvoiceMatch("Order")%> &nbsp;</td>
				<td class="light_cell" align=center><%=rsInvoiceMatch("Invdate")%></td>
				<td class="light_cell" align=center><%=rsInvoiceMatch("Shipdate")%></td>
				<td class="light_cell" align=right><%=rsInvoiceMatch("Ship")%>&nbsp;</td>
				<td class="light_cell" align=right>
				<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
					<%=Session("Currency") & rsInvoiceMatch("ShipAmt")%>
				<%else%>
					<%=rsInvoiceMatch("ShipAmt") & Session("Currency")%>
				<%end if %>
				<%'="$"&rsInvoiceMatch("ShipAmt")%>&nbsp;</td>
				
				<td class="light_cell" align=left>
					<% IF Trim(rsInvoiceMatch("Store"))= "" Then
							Response.Write "Main" & " - " & rsInvoiceMatch("Stname")
						Else
							Response.Write rsInvoiceMatch("Store") & " - " & rsInvoiceMatch("Stname")
						END IF%> &nbsp;</td>
			</tr>	
			<%
			Count = Count + 1
			rsInvoiceMatch.MoveNext()
		Loop
	
%>
</table>

  </center>
</div>

<Table width=95% align=center><TR><TD align=center>
<%

Response.Write ("<br>")
Response.Write("page " & Currpage & "of " & rsInvoiceMatch.PageCount & "<br>")
Response.Write ("<br>")

IF  CurrPage > 1 Then
	Response.Write("<A href=""invoiceresponse.asp?CurrPage=" & CurrPage - 1 & """><img border=0 src=""../images/"&Session("Theme")&"/back.gif""> </a>" )
End IF
IF rsInvoiceMatch.EOF And rsInvoiceMatch.Bof THen
Else
	IF Cint(CurrPage) <> Cint(TotalPages) Then
		Response.Write("<A href=""invoiceresponse.asp?CurrPage=" & CurrPage + 1 & """><img border=0 src=""../images/"&Session("Theme")&"/next.gif""></a>")
    end if
End IF
%>  

<BR>
<%
	conn.Close
	Set rsInvoiceMatch = nothing
END IF ' for if rsInvoiceMatch.BOF and EOF
%>
</TD></TR></Table>
</BODY>
</HTML>