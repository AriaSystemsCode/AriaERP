<%@ Language=VBScript %>
<%'Response.Buffer=true
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 


IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	CurCust = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	CurCust = Session("CustomerID")
End IF

Set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
'RecordSets
Set rsInvoiceMatch = server.CreateObject("ADODB.RecordSet")
'strSQL = "SELECT	Invhdr.Account,Invhdr.Shipamt,Invhdr.Invoice, Invhdr.Store,Invhdr.Custpo,Invhdr.Rep1,"
strSQL = "SELECT	Invhdr.* , CodesShipvia.Cdiscrep AS ShipviaDisc,"
strSQL = strSQL & " CodesTerms.Cdiscrep AS TermsDisc,"
strSQL = strSQL & " Invline.Pack_id,Customer.Btname,Customer.Caddress1,Customer.Caddress3,Customer.Caddress4,"
strSQL = strSQL & " Customer.Caddress5,Customer.Caddress6,ShiptoCustomer.Stname AS ShiptoStname,"
strSQL = strSQL & " ShiptoCustomer.Caddress1 AS ShiptoAddress1,ShiptoCustomer.Caddress3 AS ShiptoAddress3,"
strSQL = strSQL & " ShiptoCustomer.Caddress4 AS ShiptoAddress4,ShiptoCustomer.Caddress5 AS ShiptoAddress5,"
strSQL = strSQL & " ShiptoCustomer.Caddress6 AS ShiptoAddress6 "
strSQL = strSQL & " FROM	Invhdr,Invline,Customer, Customer AS ShiptoCustomer,Codes AS CodesSeason,"
strSQL = strSQL & " Codes AS CodesDivision,Codes AS CodesShipvia,Codes AS CodesTerms where"

strSQL = strSQL & " Invhdr.Account+Invhdr.Invoice='"&CurCust & Request("InvNo") & "'"

'strSQL = strSQL & " AND (CodesSeason.Cdefcode+CodesSeason.Crltfield+CodesSeason.Cfld_name='NNSEASON'"
'strSQL = strSQL & " OR  CodesSeason.Cdefcode+CodesSeason.Crltfield+CodesSeason.Cfld_name like 'NNALL%')"
strSQL = strSQL & " AND CodesSeason.Ccode_no=Invhdr.Season"

strSQL = strSQL & " AND CodesDivision.Cdefcode+CodesDivision.Crltfield+CodesDivision.Cfld_name='NNCDIVISION'"
strSQL = strSQL & " AND CodesDivision.Ccode_no = Invhdr.Cdivision "

strSQL = strSQL & " AND CodesShipvia.Cdefcode+CodesShipvia.Crltfield+CodesShipvia.Cfld_name='NNSHIPVIA'"
strSQL = strSQL & " AND CodesShipvia.Ccode_no = Invhdr.Shipvia"

strSQL = strSQL & " AND CodesTerms.Cdefcode+CodesTerms.Crltfield+CodesTerms.Cfld_name='NNCTERMCODE'"
strSQL = strSQL & " AND (Invhdr.Ctermcode = '' or CodesTerms.Ccode_no=Invhdr.Ctermcode)"

strSQL = strSQL & " AND Invline.Invoice=Invhdr.Invoice "
strSQL = strSQL & " AND Invline.invoice+STR(Invline.lineno,6) like '"&Request("InvNo")&"%'"
strSQL = strSQL & " AND  ShiptoCustomer.Account=Invhdr.Account "
strSQL = strSQL & " AND Customer.Type+Customer.Account+Customer.Store like 'M" & CurCust& "%'"
'strSQL = strSQL & " AND ShiptoCustomer.Store=Invhdr.Store "
strSQL = strSQL & " AND (ShipToCustomer.Store=Invhdr.Store or ALLTRIM(Invhdr.Store)='MAIN' )"
strSQL = strSQL & " "
'Response.Write("<br><br><br><font size=3>" & strsql &"<br></font>" )
rsInvoiceMatch.Open strSQL, conn
'Response.Write(rsInvoiceMatch.RecordCount )
'Response.End 
%>

<HTML>
<HEAD><LINK REL=stylesheet HREF="../images/<%=Session("Theme")%>/invoice.css" TYPE="text/css">
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<Title>CRM - Invoice Status - Inv. # <%=Request("InvNo")%></Title>
</head>
<body >

<SCRIPT LANGUAGE=javascript>
<!--
function GoReport()
{
	var flg = navigator.appVersion  ;
	
	if(flg.indexOf("MSIE") != -1)
		{
		//	document.location.href = "InvoiceReport.asp?InvNo=<%=Request.QueryString("InvNo")%>&viewer=ActiveX";
			document.location.href = "InvReport.asp?InvNo=<%=Request.QueryString("InvNo")%>&viewer=ActiveX";
		}
	else
		{
			//document.location.href = "InvoiceReport.asp?InvNo=<%=Request.QueryString("InvNo")%>&viewer=Java-Plug-in";
			document.location.href = "InvReport.asp?InvNo=<%=Request.QueryString("InvNo")%>&viewer=Java-Plug-in";
		}
}
//-->
</SCRIPT>

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
	<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>
 <%End IF%>

<Table width=95% height=50 align=center border=1>
<TR>
<TD class=Title>Check Invoice</TD>
</TR>
</Table>

<%

if rsInvoiceMatch.EOF AND rsInvoiceMatch.BOF then %>
	<BR>
	<table  width=95% cellspacing="0" cellpadding="0" align=center>
	<tr><td><b>No details exist for this invoice number</b>
	</td></tr>
	<tr><td>	
	<%If trim(request("BACK"))="ORDER" Then%>
			<A HREF="../order/OrdStatusDetail.asp?OrderNo=<%=request("OrderNo")%>">back</A>
	<%Else%>
			<%If trim(Session("ID"))="" Then%>
					<A HREF="repinvoice.asp">Back</A>
			<%Else%>
					<A HREF="Invoice.asp">Back</A>
			<%End If%>
	<%End IF%>
	</td></tr>
	</table><BR>
	
<%
else
Set rsInvoiceDetail = Server.CreateObject("ADODB.RecordSet")
strSQL = "SELECT Invline.Style, Invline.Desc1, Invline.Price, Invline.Qty1, Invline.Qty2, Invline.Qty3,"
strSQL = strSQL & " Invline.Qty4, Invline.Qty5, Invline.Qty6, Invline.Qty7, Invline.Qty8, Invline.Totqty,"
strSQL = strSQL & " (Invline.Totqty * Invline.Price) AS Amount, Scale.Sz1, Scale.Sz2,  Scale.Sz3,"
strSQL = strSQL & " Scale.Sz4, Scale.Sz5, Scale.Sz6, Scale.Sz7, Scale.Sz8 FROM Invline,Style,Scale WHERE"
strSQL = strSQL & " InvLine.invoice+STR(InvLine.lineno,6) like '"&request("InvNo")&"%'"
strSQL = strSQL & " AND Style.Style=Invline.Style and style.status+style.cstygroup like 'A%'"
strSQL = strSQL & " AND Scale.Type+Scale.Scale+Scale.prepak = 'S'+Style.Scale"
strSQL = strSQL & " AND Invline.order+STR(Invline.lineno,6)+Invline.invoice like '" & rsInvoiceMatch("Order") & "%"&request("InvNo")&"'"
strSQL = strSQL & " Order By Invline.Lineno"

rsInvoiceDetail.Open strSQL, conn
'Response.Write("<br><br>"& rsInvoiceDetail.RecordCount )
 

'check to see if there is detail for theis Order
IF rsInvoiceDetail.EOF And rsInvoiceDetail.BOF Then %>
	<BR>
		<table width=95% cellspacing="0" cellpadding="0" align=center>
	<tr><td><b>No Detail Exist for this Invoice number</b></td></tr>
	<tr><td><BR>
			<%If trim(Session("ID"))="" Then%>
					<A HREF="repinvoice.asp">back</A>
			<%Else%>
					<A HREF="Invoice.asp">Back</A>
			<%End If%>
	
	
	</table>
<%
Else
%>

<BR>
<Table width=95% align=center><TR><TD align=center>
<strong>Invoice # <%=rsInvoiceMatch("Invoice")%> <strong>
<BR>
<a href="javascript:GoReport();">Printer Friendly Report</a>
</TD></TR></Table>
<BR>
<table border="0"  width="95%" align=center>
<tr>
	<td width=33%>
		<STRONG>Sold to</STRONG>
	</td>
	<td>
	</td>
	<td width=33%>
		<STRONG>Ship to</STRONG>
	</td>
</tr>
<tr>
	<td>
		<table border="1" width=100% style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
			<tr>
				<td class="dark_cell"><%=rsInvoiceMatch("Btname")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsInvoiceMatch("Caddress1")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsInvoiceMatch("Caddress3")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsInvoiceMatch("Caddress4")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsInvoiceMatch("Caddress5")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsInvoiceMatch("Caddress6")%>&nbsp;</td>
			</tr>
		</table>
	</td>
	<td width=30%>
	</td>
	<td >
	<%	
	'WMA get order Alternate shipment info [start]
	Set rsOrderMatch = server.CreateObject("ADODB.RecordSet")
	strSQL = "SELECT * from Ordhdr where Order='"& rsInvoiceMatch("Order") &"'"
	rsOrderMatch.Open strSQL, conn
	
	'if rsOrderMatch.EOF AND rsOrderMatch.BOF then 
	if trim(rsOrderMatch("Alt_ShpTo")) = "True" then
	dim myarr
	myarr = split(rsOrderMatch("caddress3"),",") 
	
	'Response.Write("<br><font size=3>" & ubound(myarr) &"<br></font>" )
	%>
			<table border="1" width=100% style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
				<tr>
					<td class="dark_cell"><%=rsOrderMatch("stname")%>&nbsp;</td>
				</tr>
				<tr>
					<td class="dark_cell"><%=rsOrderMatch("caddress1")%>&nbsp;</td>
				</tr>
				<tr>
					<td class="dark_cell"><%if ubound(myarr)>=0 then Response.Write myarr(0)%>&nbsp;</td>
				</tr>
				<tr>
					<td class="dark_cell"><%if ubound(myarr)>=1 then Response.Write  myarr(1)%>&nbsp;</td>
				</tr>
				<tr>
					<td class="dark_cell"><%if ubound(myarr)>=2 then Response.Write myarr(2)%>&nbsp;</td>
				</tr>
				<tr>
					<td class="dark_cell"><%=rsOrderMatch("caddress4")%>&nbsp;</td>
				</tr>
			</table>		
	<%else%>
			<table border="1" width=100% style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
				<tr>
					<td class="dark_cell"><%=rsInvoiceMatch("ShiptoStname")%>&nbsp;</td>
				</tr>
				<tr>
					<td class="dark_cell"><%=rsInvoiceMatch("ShiptoAddress1")%>&nbsp;</td>
				</tr>
				<tr>
					<td class="dark_cell"><%=rsInvoiceMatch("ShiptoAddress3")%>&nbsp;</td>
				</tr>
				<tr>
					<td class="dark_cell"><%=rsInvoiceMatch("ShiptoAddress4")%>&nbsp;</td>
				</tr>
				<tr>
					<td class="dark_cell"><%=rsInvoiceMatch("ShiptoAddress5")%>&nbsp;</td>
				</tr>
				<tr>
					<td class="dark_cell"><%=rsInvoiceMatch("ShiptoAddress6")%>&nbsp;</td>
				</tr>
			</table>		
	<%end if
	'WMA get order Alternate shipment info [end]		
	%>
	</td>
</tr>
</table>
<BR>

<div align="center">
  <center>

<table border="1"  width="95%" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
	<tr>
		<td class="dark_cell">
			<STRONG>Ship Via</STRONG>		</td>
		<td class="dark_cell">
			<STRONG><%=session("CustField")%></STRONG>
		</td>
		
		<td class="dark_cell">
			<STRONG>Order</STRONG>
		</td>
			
		<td colspan=2 class="dark_cell">
			<STRONG>Sales Representatives</STRONG>
		</td>
		<td class="dark_cell" align=center><STRONG>Invoice Date</td>
		<td class="dark_cell" align=center><STRONG>Ship Date</td>
    </tr>

    <tr>
		<td class="light_cell">
			<%=rsInvoiceMatch("ShipviaDisc")%> &nbsp;</td>
		<td class="light_cell">
			<%=rsInvoiceMatch("Account")%> &nbsp;</td>
	
		<td class="light_cell">
			<%=rsInvoiceMatch("Order")%> &nbsp;</td>
		<td  class="light_cell">
			<%=rsInvoiceMatch("Rep1")%> &nbsp;</td>
		<td class="light_cell">
			<%=rsInvoiceMatch("Rep2") %> &nbsp;</td>
		<td class="light_cell" align=center> <%=rsInvoiceMatch("Invdate")%> &nbsp;</td>
	
		<td class="light_cell" align=center>	<%=rsInvoiceMatch("Shipdate")%> &nbsp;</td>
		
    </tr>
    <tr>
		<td class="dark_cell">
			<STRONG>Terms</STRONG>
		</td>
		<td class="dark_cell">
			<STRONG><%=session("StoreField")%></STRONG>
		</td>
		<td class="dark_cell">
			<STRONG>Department #</STRONG>
		</td>
		<td colspan=2 class="dark_cell">
			<STRONG>Customer Purchase Order</STRONG>
		</td>
		<td colspan=2 class="dark_cell"><STRONG>Packing List #</td>
    </tr>
    <Tr>
		<td class="light_cell">
			<%=rsInvoiceMatch("TermsDisc")%> &nbsp;</td>
		<td class="light_cell">
			<%=rsInvoiceMatch("Store")%> &nbsp;</td>
		<td class="light_cell">
			<%=rsInvoiceMatch("Dept")%> &nbsp;</td>
		<td colspan=2 class="light_cell">
			<%=rsInvoiceMatch("Custpo")%> &nbsp;</td>
		<td colspan=2 class="light_cell"><%=rsInvoiceMatch("Pack_id")%> &nbsp;</td>
    </tr>
</table>
  </center>
</div>
<BR>

<div align="center">
  <center>

<table border="1"  width="95%" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
<%
rsInvoiceDetail.MoveFirst()
Do While Not rsInvoiceDetail.EOF
%> 

	<tr>
		<td colspan=1 class="dark_cell">Style</td>
		<TD colspan=4 class="dark_cell"><%=rsInvoiceDetail("Style")%>&nbsp;</TD>
		<td colspan=7 class="dark_cell"><%=rsInvoiceDetail("Desc1")%>&nbsp;</td>
	</tr>

	<tr>
		<td width=12% class="light_cell">
			<STRONG>Size</STRONG>
		</td>				
		<td align=right class="light_cell"><%=rsInvoiceDetail("Sz1")%></td>
		<td align=right class="light_cell"><%=rsInvoiceDetail("Sz2")%></td>
		<td align=right class="light_cell"><%=rsInvoiceDetail("Sz3")%></td>
		<td align=right class="light_cell"><%=rsInvoiceDetail("Sz4")%></td>
		<td align=right class="light_cell"><%=rsInvoiceDetail("Sz5")%></td>
		<td align=right class="light_cell"><%=rsInvoiceDetail("Sz6")%></td>
		<td align=right class="light_cell"><%=rsInvoiceDetail("Sz7")%></td>
		<td align=right class="light_cell"><%=rsInvoiceDetail("Sz8")%></td>
		
		<td align=right class="light_cell">Tot. Qty.</td>
		<td class="light_cell" align=right>&nbsp;Price</td>
		
		<td align=right class="light_cell">Amount</td>
    </tr>
    <tr>
		<td class="light_cell">
			<STRONG>Qty.</STRONG>
		</td>	
		<td align=right valign=top  class="light_cell" >
			<%if Trim(rsInvoiceDetail("Qty1")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsInvoiceDetail("Qty1"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsInvoiceDetail("Qty2")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsInvoiceDetail("Qty2"))%>
			<%end if%>
		
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsInvoiceDetail("Qty3")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsInvoiceDetail("Qty3"))%>
			<%end if%>
		
	    </td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsInvoiceDetail("Qty4")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsInvoiceDetail("Qty4"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsInvoiceDetail("Qty5")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsInvoiceDetail("Qty5"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsInvoiceDetail("Qty6")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsInvoiceDetail("Qty6"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsInvoiceDetail("Qty7")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsInvoiceDetail("Qty7"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsInvoiceDetail("Qty8")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsInvoiceDetail("Qty8"))%>
			<%end if%>
		</td>
		
		<td align=right class="light_cell">	<%=rsInvoiceDetail("Totqty")%></td>
		<td align=right class="light_cell">
		<%if Session("CurrencyAlign")="LEFT" then%>
			<%=Session("Currency") & FormatNumber(rsInvoiceDetail("Price"))%>
		<%else%>	
			<%=FormatNumber(rsInvoiceDetail("Price")) & Session("Currency")%>
		<%end if %>
			</td>
		
		<td align=right class="light_cell">
		<%if Session("CurrencyAlign")="LEFT" then%>
			<%=Session("Currency") & FormatNumber(rsInvoiceDetail("Amount"))%>
		<%else%>
			<%=FormatNumber(rsInvoiceDetail("Amount")) & Session("Currency")%>
		<%end if %>
		</td>
    </tr>
	

<%
	rsInvoiceDetail.MoveNext()
Loop
%>
</Table>
  </center>
</div>
<BR>

<div align="center">
  <center>

<!--table border="1"  width="95%" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
	<tr>
		<td class="dark_cell">Packing List #</td>
		<td class="dark_cell" align=center>Invoice Date</td>
		<td class="dark_cell" align=center>Ship Date</td>
		<td align=right class="dark_cell" >No. of Cartons</td>
		<td align=right class="dark_cell">Weight</td>
		<td align=right class="dark_cell">Shipped Amount</td>
		<td align=right class="dark_cell">Tax</td>
		<td align=right class="dark_cell">Amount to Pay</td>
    </tr>
    <tr>
		<td class="light_cell"><%=rsInvoiceMatch("Pack_id")%> &nbsp;</td>
		<td class="light_cell" align=center> <%=rsInvoiceMatch("Invdate")%> &nbsp;</td>
	
		<td class="light_cell" align=center>	<%=rsInvoiceMatch("Shipdate")%> &nbsp;</td>
		<td align=right class="light_cell"><%=rsInvoiceMatch("Cartons")%></td>
		
				<td align=right class="light_cell">
			<%=rsInvoiceMatch("Weight")%></td>
		<td align=right class="light_cell">
		<%if Session("CurrencyAlign")="LEFT" then%>
			<%=Session("Currency") & FormatNumber(rsInvoiceMatch("Shipamt"))%>
		<%else%>
			<%=FormatNumber(rsInvoiceMatch("Shipamt")) & Session("Currency")%>
		<%end if%>
			</td>
				
		<td align=right class="light_cell">
		<%if Session("CurrencyAlign")="LEFT" then%>

		<%else%>
			<%=FormatNumber(rsInvoiceMatch("Tax_amt")) & Session("Currency")%>
		<%end if%>
		</td>
		
		<td align=right class="light_cell">
		<%
		if Session("CurrencyAlign")="LEFT" then%>
			<%=Session("Currency") & FormatNumber(cdbl(rsInvoiceMatch("Totalchg")))%>
		<%else%>
			<%=FormatNumber(cdbl(rsInvoiceMatch("Totalchg"))) & Session("Currency")%>
		<%end if%>
		
    </tr>
</table-->
  </center>
</div>
<BR>
<BR><br>
	<table border="1"  width="95%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0" align=center>
	<TR>
		<TD colspan=4   valign=center><p><b><font size=3>Invoice Charges</font></b></p></TD>
	</TR>

	<TR>
		<TD class="dark_cell">Total Weight</TD>
		<TD class="light_cell" align=right>
			<%=round(cdbl(trim(rsInvoiceMatch("Weight"))),2)%>
		</TD>

		<TD class="dark_cell">Shipped Amount</TD>
		<TD class="light_cell" align=right>
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & FormatNumber(rsInvoiceMatch("Shipamt"))%>
			<%else%>
				<%=FormatNumber(rsInvoiceMatch("Shipamt")) & Session("Currency")%>
			<%end if %>
		</TD>
	</TR>
	<TR>
		<TD  class="dark_cell">No. of Cartons</TD>
		<TD class="light_cell" align=right>
			<%=round(trim(rsInvoiceMatch("cartons")),2)%>
		</TD>
		<TD class="dark_cell">Freight</TD>
		<TD class="light_cell" align=right>
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(cdbl(rsInvoiceMatch("freight")),2),2)%>
			<%else%>
				<%=formatnumber(round(cdbl(rsInvoiceMatch("freight")),2),2) & Session("Currency")%>
			<%end if %>
		</TD>
		
		
	</TR>

	<TR>
		<TD  class="dark_cell"></TD>
		<TD class="light_cell" align=right></TD>
		<TD class="dark_cell">UPS Insur</TD>
		<TD class="light_cell" align=right>
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(cdbl(rsInvoiceMatch("Insur")),2),2)%>
			<%else%>
				<%=formatnumber(round(cdbl(rsInvoiceMatch("Insur")),2),2) & Session("Currency")%>
			<%end if %>
		</TD>
	</TR>
	<%if cdbl(rsInvoiceMatch("Cod_amt")) <> 0 then%>
		<TR>
			<TD  class="dark_cell"></TD>
			<TD class="light_cell" align=right></TD>
			<TD class="dark_cell">C.O.D Charge</TD>
			<TD class="light_cell" align=right>
				<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
					<%=Session("Currency") & round(cdbl(rsInvoiceMatch("Cod_amt")),2)%>
				<%else%>
					<%=round(cdbl(rsInvoiceMatch("Cod_amt")),2) & Session("Currency")%>
				<%end if %>
			</TD>
		</TR>
	<%else%>
	<%end if%>
	
	<%if cdbl(rsInvoiceMatch("Discount")) <> 0 then%>
	<TR>
		
		<TD  class="dark_cell"></TD>
		<TD class="light_cell" align=right></TD>
		<TD class="dark_cell">Discount Amount</TD>
		<TD class="light_cell" align=right>
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(cdbl(rsInvoiceMatch("Discount")),2)%>
			<%else%>
				<%=formatnumber(cdbl(rsInvoiceMatch("Discount")),2) & Session("Currency")%>
			<%end if %>
		</TD>
	</TR>
	<%end if%>
	<%if cdbl(rsInvoiceMatch("tax_rate")) <> 0 then%>
	<TR>
		<TD class="dark_cell">Tax Rate</TD>
		<TD class="light_cell" align=right>
			<%=FormatNumber(cdbl(rsInvoiceMatch("tax_rate")),3)%>%
		</TD>
		<TD class="dark_cell">Tax Amount</TD>
		<TD class="light_cell" align=right>
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(cdbl(rsInvoiceMatch("tax_amt")),2),2)%>
			<%else%>
				<%=formatnumber(round(cdbl(rsInvoiceMatch("tax_amt")),2),2) & Session("Currency")%>
			<%end if %>
		</TD>
	</TR>
	<%end if%>

	
	<TR>
		<TD colspan=4 class="light_cell" height=1></TD>
	</TR>
	
	<TR>
		<TD colspan=2 class="dark_cell" align=right></TD>
		<TD class="dark_cell">Total Shipped Amount</TD>
		<TD class="light_cell" align=right>
			
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & FormatNumber(cdbl(rsInvoiceMatch("Totalchg")))%>
			<%else%>
				<%=FormatNumber(cdbl(rsInvoiceMatch("Totalchg"))) & Session("Currency")%>
			<%end if %>
		</TD>
	</TR>
	</table>

	
	
<%
END IF ' for no detail exist for the record ...
END IF ' for if the No is valid ...
%>
<p>
</BODY>
</HTML>