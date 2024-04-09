<%@ Language=VBScript %>
<%
Response.CacheControl  = "no-cache"
Response.AddHeader  "Pragma", "no-cache"
Response.Expires = -1
'Response.Expires = 0
Response.Buffer = true

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
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" and request("chkCust") = "" Then
		'Response.Redirect("../repcust.asp")
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
<meta http-equiv="Content-Type" content="textahtml; charset=windows-1256">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<Title>CRM - Invoice Payment Details</Title>
<SCRIPT LANGUAGE=javascript>
<!--
function doSubmit()
{
		if (document.all("card-amount").value==0)
		{
			alert("Please enter numbers greater than 0.00 !");
			document.all("card-amount").focus();
			return false;
		}

		if (isNaN(document.all("card-amount").value))
		{
			alert("Please enter numbers only!");
			document.all("card-amount").focus();
			return false;
		}

	/*
		depAmtVal = document.all("card-amount").value;
		var re;
		re = /,/g;
		depAmtVal = depAmtVal.replace(re,'');
		depAmtVal = parseFloat(depAmtVal);
		if (isNaN(depAmtVal))
		{
			alert("Please enter numbers only!");
			document.all("card-amount").focus();
			return false;
		}
	*/
	
		//not greater than the rest amount
		if (parseFloat(document.all("card-amount").value) > parseFloat(document.all("maxAmountToPay").value))
		{
			alert("Please enter values not greater than rest of invoice amount!");
			document.all("card-amount").focus();
			return false;
		}
		else
		{
	    	document.form1.target = "_top";
	    	document.form1.action="../PnpCom/paymentform.asp?InvNo=<%=Request("InvNo")%>&PNPCase=Invoice";
	    	document.form1.submit();
  			return true;
		}
}	

function doCancel()
{
	window.location.href="InvoicePaymentResponse.asp?chkCust=<%=Session("chkCust")%>";
	//window.history.go(-1);
}	
-->	
</script>		

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
<TD class=title>Invoice Payment Details</TD>
</TR>
</Table>

<%
Set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

Set rsInvoiceMatch = server.CreateObject("ADODB.RecordSet")
strSQL = "SELECT distinct Invhdr.Invoice, Invhdr.Order, Invhdr.Status,Invhdr.ShipAmt,Invhdr.Totalchg , Invhdr.Ship, Invhdr.Invdate, Invhdr.Shipdate, Invhdr.Store, "
strSQL = strSQL & " Customer.Btname FROM Invhdr, Customer WHERE "
strSQL = strSQL & "Customer.Account=Invhdr.Account and customer.type='M'"
strSQL = strSQL & " and Invhdr.Invoice='"& trim(Request("InvNo")) &"'"
strSQL = strSQL & " and Invhdr.account+Invhdr.Invoice like '" & CustID& "%' "
'Response.Write("<font size=3>"&strSQL& "</font>")
'Response.End 
rsInvoiceMatch.Open strSQL, conn


%>
	<div align="center">
	  <center>
	  <table width=85% border="0" cellpadding="0" cellspacing="0">
	  		<tr>
				<td ><strong>Invoice Details:</strong></td>
			</tr>
	  </table>
	  
	<table width=85% border="1" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
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
	    
		<%
		dim dblInvoiceAmount 'as doubl
		dim dblDepositAmount 'as doubl
		dim dblTotalChargeAmount 'as doubl
		dim blnFoundInvoice 'as Boolean
		dim blnFoundCharge 'as Boolean
		if not rsInvoiceMatch.EOF then 
		%>
			<tr>
			<td class="dark_cell" align=left>Invoice #</td>
				<td class="light_cell" align=left>
					<!--A HREF="../PnpCom/paymentform.asp?InvNo=<%= rsInvoiceMatch("Invoice")%>&PNPCase=Invoice" target="_top"><%= rsInvoiceMatch("Invoice")%></A> &nbsp;</td-->
					<%= rsInvoiceMatch("Invoice")%> &nbsp;</td>
				
				<td class="dark_cell" align=left>Invoice Date</td>
				<td class="light_cell" align=left><%=rsInvoiceMatch("Shipdate")%></td>
			</tr>	
			<tr>
				<td class="dark_cell" align=left>Shipped Amount&nbsp;</td>
				<td class="light_cell" align=left><%="$"&FormatNumber(rsInvoiceMatch("ShipAmt"))%>&nbsp;</td>
				<td class="dark_cell" align=left>Invoice Amount&nbsp;</td>
				<td class="light_cell" align=left><%="$"&FormatNumber(rsInvoiceMatch("Totalchg"))%>&nbsp;</td>				
			</tr>
			
			<tr>
		        <td class="dark_cell" align=left>Order #</td>
		        <td class="light_cell" align=left><%=rsInvoiceMatch("Order")%> &nbsp;</td>					
      			<td class="dark_cell" align=left>Ship Date</td>
				<td class="light_cell" align=left><%=rsInvoiceMatch("Invdate")%></td>
			</tr>	
			<%
			dblInvoiceAmount = cdbl(rsInvoiceMatch("Totalchg")) '/2
			
			'Order Charge RecordSets
			Set rsOrdCharg = server.CreateObject("ADODB.RecordSet")
			'strSQL = "SELECT Ndeposit from Ordcharg "
			strSQL = "SELECT Ndeposit, Ntotchg from Ordcharg "
			strSQL = strSQL & "WHERE COrder='"& trim(rsInvoiceMatch("Order")) &"' "
			rsOrdCharg.Open strSQL, conn
			
			'Get Deposit Amount
			if not rsOrdCharg.EOF then
				dblDepositAmount = cdbl(rsOrdCharg("Ndeposit"))
				dblTotalChargeAmount = cdbl(rsOrdCharg("Ntotchg"))
				blnFoundCharge = true	
			%>
			<tr>
		        <td class="dark_cell" align=left>Total Order Amount</td>
		        <td class="light_cell" align=left><%="$"&FormatNumber(dblTotalChargeAmount)%> &nbsp;</td>					
      			<td class="dark_cell" align=left>&nbsp;</td>
      			<td class="light_cell" align=left>&nbsp;</td>
			</tr>	
			<%	
			else
				dblDepositAmount = 0
				dblTotalChargeAmount = 0
			end if
	
	
			'PNPTrans RecordSets
			Set rsPNPTrans = server.CreateObject("ADODB.RecordSet")
			strSQL = "SELECT * from Pnptrans "
			strSQL = strSQL & "WHERE Order='"& trim(rsInvoiceMatch("Order")) &"'"
			strSQL = strSQL & "Order by Trandate, cTrantime"	
			rsPNPTrans.Open strSQL, conn
			
			
			
		end if
	%>
	</table><br><br>

		<%if not rsPNPTrans.EOF then %>
			<!--Transaction table-->
			  <center><table width=85% border="0" cellpadding="0" cellspacing="0">
			  	<tr>
					<td ><strong>Previous Payments:</strong></td>
				</tr></table></center>
			<table width=85% border="1" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">   
		<%end if%>
		<table width=85% border="1" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
		<tr>
				<td class="dark_cell" align=left>Transaction Id #</td>
				<td class="dark_cell" align=left>Amount</td>		
				<td class="dark_cell" align=left>Type</td>						
				<td class="dark_cell" align=left>Date</td>		
				<td class="dark_cell" align=left>Time</td>		
		</tr>
			
		<%
		dim dblPaidAmount 'as double
		dim dblAmountToPay 'as double
		dim dblInvoiceHistory 'as doubl
		do while not rsPNPTrans.EOF %>
			<tr>
		        <td class="light_cell" align=left><%=rsPNPTrans("cTransid")%></td>
		        <td class="light_cell" align=left><%="$" & FormatNumber(cdbl(rsPNPTrans("Amount"))*-1)%></td>
		        <%if trim(rsPNPTrans("ctrn_type"))="Invoice" then %>
					<td class="light_cell" align=left><%="Invoice #" & rsPNPTrans("Invoice")%></td>		        
					<%
					if trim(rsInvoiceMatch("Invoice")) = trim(rsPNPTrans("Invoice")) then 
						dblInvoiceHistory = dblInvoiceHistory + (cdbl(rsPNPTrans("Amount"))* -1)
						blnFoundInvoice=true 
					end if	
					%>
				<%else%>
					<td class="light_cell" align=left><%=rsPNPTrans("ctrn_type")%></td>		        
				<%end if%>				
		        <td class="light_cell" align=left><%=rsPNPTrans("Trandate")%></td>
		        <td class="light_cell" align=left><%=rsPNPTrans("cTrantime")%></td>
			</tr>	
			<%
			'current invoice avoided
			if trim(rsPNPTrans("ctrn_type"))<> "Invoice" then
				dblPaidAmount = dblPaidAmount + (cdbl(rsPNPTrans("Amount")) * -1)
			end if
			rsPNPTrans.MoveNext 
		loop
		'Amount to pay formula
		'dblAmountToPay = round((1- dblDepositAmount/dblInvoiceAmount) * dblInvoiceAmount ,2)
		 if dblTotalChargeAmount = 0 then
			 Response.Write "Invoice havn't related ordcharg record"		 
			 dblAmountToPay = 0
		 else 
			'dblAmountToPay = round((1- dblDepositAmount/dblTotalChargeAmount) * dblInvoiceAmount ,2)
			dblAmountToPay = round(((1- dblDepositAmount/dblTotalChargeAmount) * dblInvoiceAmount)-dblInvoiceHistory ,2)
			if Round(dblAmountToPay ,1) = 0 then  dblAmountToPay = 0 'avoid .01 values
		 end if
	%>
	</table><br><br>

		<!--Messages table-->
		<%if not blnFoundCharge then %>
			  <center><table width=85% border="0" cellpadding="0" cellspacing="0">
			  	<tr><td align=center ><strong>This order coming from outside the CRM so you can't submit payment.</strong></td></tr>
			</table></center>			
			<%elseif blnFoundInvoice=true and round(dblAmountToPay,1)=0 then %>
			  <center><table width=85% border="0" cellpadding="0" cellspacing="0">
			  	<tr><td align=center ><strong>This invoice has been already paid.</strong></td></tr>
			</table></center>			
		<%elseif dblAmountToPay=0 and dblTotalChargeAmount <> 0 then %>			
			  <center><table width=85% border="0" cellpadding="0" cellspacing="0">
		  	<tr><td align=center ><strong>The customer has already paid the full amount while ordering</strong></td></tr>
			</table></center>		
		<%end if%>
		
	<form method = post  name=form1>		
	<table width=85% border="1" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
		<tr>
				<input name=maxAmountToPay type=hidden value = <%=dblAmountToPay%>>
				<td class="dark_cell" align=left>Order Paid Amount</td>
		        <td class="light_cell" align=left width=100><%="$"& FormatNumber(Cdbl(dblPaidAmount))%></td>
				<td class="dark_cell" align=left>Amount to pay</td>	
				<%'if blnFoundInvoice=true then %>
			        <!--td class="light_cell" align=left > $ <input type=text name=card-amount size=8 value="<%="0.00" %>"></td-->	
				<%'else%>
			        <td class="light_cell" align=left > $ <input type=text name=card-amount size=8 value="<%= FormatNumber(dblAmountToPay,2,,,0) %>" <%if round(dblAmountToPay,1) = 0 then Response.Write "disabled" %> ></td>	
				<%'end if%>
				<td class="dark_cell" align=center><input type=button value="Cancel" onClick ="return doCancel()" id=submit1 name=submit1> <input type=submit value="submit payment" onClick ="return doSubmit()" <%if round(dblAmountToPay,1) = 0 then Response.Write "disabled" %>></td>		
		</tr>
	</table>
	</form>
	

  </center>
</div>

<Table width=95% align=center><TR><TD align=center>

<BR>
<%

	Set rsInvoiceMatch = nothing
	Set rsOrdCharg = nothing
	Set rsPNPTrans = nothing
	conn.Close
%>
</TD></TR></Table>
</BODY>
</HTML>


<%
'Successfull Messege
if Request.QueryString("InvSuccNo")<>"" then
%>
	<script language="javascript">
		window.alert ("Your Invoice Pyment # <%=Request.QueryString("InvSuccNo")%> successfully submited");
	</script>	
<%	
end if
%>