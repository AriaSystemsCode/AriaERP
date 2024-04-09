<%@ Language=VBScript %>
<%Response.Buffer=true

If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.htm"%>
	<script language="javascript">
	parent.location.href="../default.asp"
	</script>
<%END IF

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
	
Session("chkCust")= request("chkCust")			
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" and request("chkCust") = "" and Request.QueryString("chkCust") = "" Then
		Response.Redirect("../repcust.asp") 
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
	if request("chkCust") <> "" Then
		custid = ""
		Session("customerid") = ""
	end if
End IF


%>


<HTML>
<HEAD><LINK REL=stylesheet HREF="../images/<%=Session("Theme")%>/invoice.css" TYPE="text/css">
<meta http-equiv="Content-Type" content="text/html; charset=windows-1256">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<Title>CRM - Invoice Payment</Title>
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
	<%IF Trim(Session("customerid")) <> ""  Then%>
		<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=session("rscust").fields("btname").value%></P>
	<%else%>
		&nbsp
	<%end if%>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>
 <%End IF%>
<Table width=95% align=center height=50 border=1>
<TR>
<TD class=title>Check Invoice Payment</TD>
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
		strSQL = "SELECT distinct Invhdr.Invoice, Invhdr.Order, Invhdr.Status,Invhdr.ShipAmt,Invhdr.Totalchg , Invhdr.Ship, Invhdr.Invdate, Invhdr.Shipdate, Invhdr.Store, "
		strSQL = strSQL & " Customer.Btname FROM Invhdr, Customer WHERE "
		'strSQL = strSQL & " (customer.type+customer.account+customer.Store like 'M" & custid& "%' or customer.type+customer.account+customer.Store like 'S" & custid& "%') "
		'strSQL = strSQL & " and (Customer.Store=Invhdr.Store or ALLTRIM(Invhdr.Store)='MAIN')"
		strSQL = strSQL & "Customer.Account=Invhdr.Account and customer.type='M'"
		'and ALLTRIM(Customer.Store)='MAIN'
		'check if to add check on sales rep or he's admin
		if Session("Authority") <> "Full" then
			strSQL = strSQL & " and ( (Invhdr.rep1='"& session("rep") &"' and Invhdr.rep1=customer.salesrep) or (Invhdr.rep2='"& session("rep") &"'"
			strSQL = strSQL & " and Invhdr.rep1=customer.rep2) )  "
		end if
		strSQL = strSQL & " and Invhdr.account+Invhdr.Invoice like '" & CustID& "%' "

	END IF
END IF

IF Trim(Request.Form("txtInvoiceNo"))<>"" Then
	testexistSQL = "SELECT distinct Invoice, Order,Status,Invdate,Shipdate, Duedate, Store, Season,Custpo, Cdivision FROM Invhdr where account+Invoice like '" &custid & Trim(Request.Form("txtInvoiceNo")) & "%'"
	Set RSInv = Server.CreateObject("ADODB.RecordSet")
	RSInv.Open testexistSQL, conn
	IF RSInv.BOF AND RSInv.EOF Then
		'strSQL=strSQL & " AND Invoice LIKE '" & Request.Form("txtInvoiceNo") & "%'"
	Else
		RSInv.close()
		
		'Response.Redirect ("../PnpCom/paymentform.asp?InvNo="& Request.Form("txtInvoiceNo") &"& PNPCase=Invoice")
	%>	
		<script language="javascript">
			//parent.location.href ="../PnpCom/paymentform.asp?InvNo=<%=Request.Form("txtInvoiceNo")%>&PNPCase=Invoice"
			window.location.href ="PaymentDetails.asp?InvNo=<%=Request.Form("txtInvoiceNo")%>"
		</script>	

	<%
	END IF
END IF

IF Session("BeginDate") <> "" Then
	strSQL = strSQL & " AND dtos(Invdate) >='" & Session("BeginDate") & "'"
END IF
IF Session("EndDate") <> "" Then
	strSQL = strSQL & " AND dtos(Invdate) <='" & Session("EndDate") & "'"
END IF

'Status
strSQL = strSQL & " AND invhdr.Status <>'V'"                                                                                                                                                                                                                                                                                                                                                                                             
'WMA Add Invoice Flag Payment
'strSQL = strSQL & " AND Npaied =.F."                                                                                                                                                                                                                                                                                                                                                                                             
strSQL = strSQL & " order by invoice desc"


'Response.Write("<font size=3>"&strSQL& "</font>")
'Response.End 
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
		<A HREF="InvoicePayment.asp">back</A>
			<%Else%>
		<A HREF="InvoicePayment.asp?id=<%=id%>">back</A>	
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
	<%IF Trim(Session("customerid")) <> ""  Then%>
		<tr>
		    <td class="dark_cell" height="19">
				<%=session("CustField")%>
			</td>
		    <td colspan=8 class="light_cell" height="19">
				
				<%=Session("RSCust").fields("account")%> - <%=Session("RSCust").fields("btname")%>
				
			</td>
		</tr>
	<%end if%>
	<tr>
        <td class="dark_cell">Invoice #</td>
        <td class="dark_cell">Status</td>
        <td class="dark_cell">Order #</td>
        <td class="dark_cell" align=center>Invoice Date</td>
        <td class="dark_cell" align=center>Ship Date</td>
        <td class="dark_cell" align=right>Shipped Qty.</td>
        <td class="dark_cell" align=right>Shipped Amount&nbsp;</td>
        <td class="dark_cell" align=right>Invoice Amount&nbsp;</td>
        
        <!--td class="dark_cell"><%=session("StoreField")%></td-->
        <td class="dark_cell">Customer</td>
    </tr>
    
<%
		Dim Count
		Count = 0
		
		Do While Not rsInvoiceMatch.EOF And Count < rsInvoiceMatch.PageSize %>
			<tr>
				<td class="light_cell">
					<!--A HREF="../PnpCom/paymentform.asp?InvNo=<%= rsInvoiceMatch("Invoice")%>&PNPCase=Invoice" target="_top"><%= rsInvoiceMatch("Invoice")%></A> &nbsp;</td-->
					<A HREF="PaymentDetails.asp?InvNo=<%= rsInvoiceMatch("Invoice")%>"><%= rsInvoiceMatch("Invoice")%></A> &nbsp;</td>
				<td class="light_cell">
					<%Select case rsInvoiceMatch("Status")
						Case "O":
							Response.Write "Open"
						Case "C":
							Response.Write "Complete"
						Case "V":
							Response.Write "Void"
					END SELECT%> &nbsp;</td>
				<td class="light_cell"><%=rsInvoiceMatch("Order")%> &nbsp;</td>					
				<td class="light_cell" align=center><%=rsInvoiceMatch("Invdate")%></td>
				<td class="light_cell" align=center><%=rsInvoiceMatch("Shipdate")%></td>
				<td class="light_cell" align=right><%=rsInvoiceMatch("Ship")%>&nbsp;</td>
				<td class="light_cell" align=right><%="$"&rsInvoiceMatch("ShipAmt")%>&nbsp;</td>
				<td class="light_cell" align=right><%="$"&rsInvoiceMatch("Totalchg")%>&nbsp;</td>				
				<td class="light_cell" align=left><%= rsInvoiceMatch("Btname")%> &nbsp;</td>
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
	Response.Write("<A href=""InvoicePaymentResponse.asp?chkCust="&request("chkCust")&"&CurrPage=" & CurrPage - 1 & """><img border=0 src=""../images/"&Session("Theme")&"/back.gif""> </a>" )
End IF
IF rsInvoiceMatch.EOF And rsInvoiceMatch.Bof THen
Else
	IF Cint(CurrPage) <> Cint(TotalPages) Then
		Response.Write("<A href=""InvoicePaymentResponse.asp?chkCust="&request("chkCust")&"&CurrPage=" & CurrPage + 1 & """><img border=0 src=""../images/"&Session("Theme")&"/next.gif""></a>")
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


<%
'Successfull Messege
'Response.Write "<font size=5>" & Request("chkCust")
if Request.QueryString("InvSuccNo")<>"" then
%>
	<script language="javascript">
		window.alert ("Your Invoice Pyment # <%=Request.QueryString("InvSuccNo")%> successfully submited");
	</script>	
<%	
end if
%>