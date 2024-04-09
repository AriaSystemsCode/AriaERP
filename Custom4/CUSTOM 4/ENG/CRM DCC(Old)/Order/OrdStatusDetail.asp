<%@ Language=VBScript %>
<%Response.Buffer=true
if Trim(Session("rep")) = "" and trim(Session("ID"))= "" then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
	parent.location.href="../login.asp"
	</script>
<%end if
'Response.Write "<font size=3> Currecy=" & Session("Currency") & "</font>"
'setup var to the metohd of payment
Session("COD") = "F"
IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	'IF Trim(Session("customerid")) = "" Then
	'	Response.Redirect("../repcust.asp")
	'END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF

if trim (Session("rep")) = "" then
	CurCust = Session("ID")
else 
	CurCust = Session("CustomerID")
end if

set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
set rsOrdersMatch = server.CreateObject("ADODB.RecordSet")

 
BeginDate = (Trim(Request.Form("txtBeginDate")))
EndDate  = (Trim(Request.Form("txtEndDate")))
'NEK[Start]1/6/2003
OrderNoFrom=(Trim(Request("OrderNoStart")))
OrderNoTo =(Trim(Request("OrderNoEnd")))
'Response.Write "Order From ==" & OrderNoFrom & " TO == "&OrderNoTO
'Response.End 
'NEK[End]1/6/2003
Dim rsHdrInfo
Set rsHdrInfo = server.CreateObject("ADODB.recordset")
'get header info[start]
strSQL = "SELECT Ordhdr.order,Ordhdr.Book,Rep1,Ordhdr.Ship,Ordhdr.Cancel,Ordhdr.Open, Ordhdr.Account,"
strSQL = strSQL & " Ordhdr.status,Ordhdr.priority,OrdHdr.shipvia, Ordhdr.Start, Ordhdr.Complete,"
strSQL = strSQL & " Ordhdr.custpo,Ordhdr.entered,customer.btname,customer.stname AS CustStname, Alt_ShpTo,"
strSQL = strSQL & " Ordhdr.stname,Ordhdr.caddress1,Ordhdr.caddress2,Ordhdr.caddress3,Ordhdr.caddress4,Ordhdr.caddress5,"
'wal_127343 add 2 notes fields
'
strSQL = strSQL & " Ordhdr.note1,Ordhdr.note2,ordhdr.cContRef"
strSQL = strSQL & " FROM Ordhdr,Customer WHERE"
IF Trim(CurCust) <> ""  Then
	strSQL = strSQL & " ordhdr.account+ordhdr.cordtype+ordhdr.order like '" &CurCust& request("Type")& request("OrderNo") &  "' AND "
	strSQL = strSQL & " customer.type+customer.account+customer.store like 'M"&CurCust&"%'"
Else	
	strSQL = strSQL & " ordhdr.order = '" & request("OrderNo") &  "' AND "
	strSQL = strSQL & " customer.type+customer.account+customer.store like 'M%' and customer.account = ordhdr.account "
end if
'Response.Write "<font size=2>"&strSQL
rsHdrInfo.Open strSQL, conn
'get header info[end]
strSQL = "SELECT Scale.sz1,  Scale.sz2, Scale.sz3,  Scale.sz4, Scale.sz5, Scale.sz6, Scale.sz7, Scale.sz8, Ordline.*,(Ordline.Totqty * Ordline.Price) AS Diff FROM Ordline, scale WHERE"
'ARD - Optimization [start]
strSQL = strSQL & " Ordline.cordtype+ordline.order+STR(ordline.lineno,6) like '"& request("Type") & request("OrderNo")&"%' "
strSQL = strSQL & " AND scale.type='S'  and scale.scale = ordLine.scale order by lineno"
'ARD - Optimization [End] 
rsOrdersMatch.Open strSQL, conn

%>

<HTML>
<HEAD>
<Title>CRM - Check Order Status - Order # <%=request("orderNo")%></Title>
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">

</head>
<body>

<SCRIPT LANGUAGE=javascript>
<!--
function GoReport()
{
	var flg = navigator.appVersion  ;
	
	if(flg.indexOf("MSIE") != -1)
		{
			
			document.location.href = "OrdReport.asp?OrderNo=<%=Request.QueryString("OrderNo")%>&viewer=ActiveX&Type=<%=request("Type")%>";
		}
	else
		{
			
			document.location.href = "OrdReport.asp?OrderNo=<%=Request.QueryString("OrderNo")%>&viewer=Java-Plug-in&Type=<%=request("Type")%>";
		}
}
//-->
</SCRIPT>
<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<P><br><BR><BR></p>

<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
<!--	
	<TD colspan=14 background="../images/bground.gif">

	<font size="2" face="Arial">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
		<%set connTemp=server.createobject("ADODB.Connection")
			set RSTemp=server.createobject("ADODB.Recordset")
			connTemp.open Application("DataConnectionString")
			sqlTemp="select * from customer where account='" & session("customerid") & "'"
			RSTemp.open sqlTemp,connTemp,1,3

			%>
              Your currently selected customer is <%=Session("customerid")%>
             <%'response.write " - "&RSTemp("btname")%></font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="repcust.asp" ><font color=white size="2" face="Arial"><b>Get Customer</b></font></a>
	-->
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


<Table width=95% border=1 align=center height="50">
<TR>
<TD class="title">Check Order Status</TD>
</TR>
</Table>

<%
if rsHdrInfo.EOF AND rsHdrInfo.BOF then %>
	<table width=95% align=center>
		<tr>
			<td>
			<strong>
			There are no records matching your entered selection criteria<BR>
			<!--<A HREF="OrdStatus.asp"><font face="Arial" size="2" color="#000080"><strong>back</font></strong></A>-->
			</strong>
			</td>
		</tr>
	</table>
<%
else


'check to see if there is detail for theis Order
if rsOrdersMatch.EOF And rsOrdersMatch.BOF then %>
	<BR><b>No Detail Exist for this order number<BR>
 </b>
	<A HREF="OrdStatus.asp">Back</A>
<%
else

rsOrdersMatch.MoveFirst()
%>
<br>
	<Table width=95% border=0 align=center> 
		<TR>
			<TD align=center>
				Order # <strong><%=rsOrdersMatch("Order")%></strong>
				<BR><a href="javascript:GoReport();">Printer Friendly Report</a> 
			</TD>
		</TR>
	</Table>
<BR>
<br>

<div align="center">
  <center>

<table border="1"  width=95% bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<tr>
		<td class="dark_cell"><%=session("CustField")%></td>
		<td colspan=3 class="light_cell"><%=rsHdrInfo("Account")%> - <%=rsHdrInfo("btname")%></td>
		<td class="dark_cell">Sales Rep.</td>
		<td colspan=7 class="light_cell"><%=rsHdrInfo("Rep1")%></td>
	</tr>
	<tr>
		<td class="dark_cell">Order #</td>
		<td class="light_cell"><%=rsHdrInfo("Order")%>&nbsp;</td>
		<!--<td><font size="2" color="#000080" face="Arial"><STRONG>Account#</STRONG></font></td>
		<td bgcolor="Ivory"><font size="2" color="#000080" face="Arial"><%=rsOrdersMatch("Account")%></font></td>-->
		<td class="dark_cell">Status</td>
		<td class="light_cell"><%Select case Trim(rsHdrInfo("Status"))
									case "O"
										Response.Write "Open"
									case "H"
										Response.Write "Hold"
									case "B"
										Response.Write "Reported"
									case "C"
										Response.Write "Completed"
									case "X"
										Response.Write "Canceled"
								end select
								%> &nbsp;</td>
		<%
			Dim RSCodes
			Set RSCodes = server.CreateObject("ADODB.Recordset")
			Strsql = "select * from codes where cfld_name='SHIPVIA' and crltfield='N' and cdefcode='N' and ccode_no='" & rsHdrInfo("shipvia") & "'"
			RSCodes.Open strsql,conn
	
			IF Not(RSCodes.EOF and RSCodes.BOF) Then
				strShipVia = RSCodes.Fields("cdiscrep").Value 
			End IF
			RSCodes.Close 
		%>
		<%if session("CustPO") = "T" and strFile = "cust" then%>
			<td class="dark_cell"><STRONG>Cust. PO</STRONG></td>
			<td width=60 class="light_cell"><%=rsHdrInfo("Custpo")%>&nbsp;</td>
		<%else%>
			<td class="dark_cell"><STRONG></STRONG></td>
			<td width=60 class="light_cell">&nbsp;</td>
		<%end if%>
		<td class="dark_cell">Ship Via</td>
		<td width=110 class="light_cell"><%=strShipVia%>
		<td class="dark_cell">Start Ship</td>
		<td width=60 class="light_cell" align=center><%if rsHdrInfo("Start")<> "12:00:00 AM" then Response.Write rsHdrInfo("Start") end if %>&nbsp;</td>
		<td class="dark_cell">Expected Ship</td>
		<td width=60 class="light_cell" align=center><%if rsHdrInfo("Complete")<>"12:00:00 AM" then Response.Write rsHdrInfo("Complete") end if%>&nbsp;</td>
	</tr>
	<tr>
		<!--wal_128815 add Contract ID if setup=yes -->
		<%if session("ContID") = "T" then%>
			<td class="dark_cell"><STRONG><%=trim(Session("ContractField"))%></STRONG></td>
			<td class="light_cell"><%=rsHdrInfo("cContRef")%>&nbsp;</td>
		<%end if%>
		<td class="dark_cell">Booked Qty.</td>
		<td class="light_cell" align=right><%=Trim(rsHdrInfo("Book"))%></td>
		<td class="dark_cell">Shipped Qty.</td>
		<td class="light_cell" align=right><%=Trim(rsHdrInfo("Ship"))%></td>
		<td class="dark_cell">Canceled Qty.</td>
		<td class="light_cell" align=right><%=Trim(rsHdrInfo("Cancel"))%></td>
		<td class="dark_cell">Open Qty.</td>
		<td class="light_cell"  align=right><%=Trim(rsHdrInfo("Open"))%></td>
		<td class="light_cell" colspan=4>&nbsp;</td>
		
	</tr>
	<TR>
	<%'ARD Add Invoices to the report
		'WAL read from hdr not lines
		Set RSInvHdr = Server.CreateObject("ADODB.RecordSet")
		strsql = "select * from invhdr where account+invoice like '" & rsHdrInfo("Account") & "%' and order = '"& Request("orderNo") & "' and status <> 'V' order by invoice"
		'Response.Write "<font size=3>" & strsql
		'RSInvHdr.Open strsql,conn
		RSInvHdr.Open strsql,conn,2,2
		totInvAmt = 0
		'Set RSInvLine = Server.CreateObject("ADODB.RecordSet")
		'strsql = "select * from invline where order+str(lineno,6)+invoice like '" & Request("orderNo") & "%' order by invoice"
		'RSInvLine.Open strsql,conn
	%>
		<td class="dark_cell">Invoice(s)</td>
		<td colspan=11 class="light_cell">
		<%'IF RSInvLine.Eof And RSInvLine.BOF Then
			IF RSInvHdr.Eof And RSInvHdr.BOF Then
				Response.Write("No Invoices found for this order.")
			Else
				Dim strTemp ' as string
				Do While Not RSInvHdr.Eof%>
				<a href="../invoice/invoicedetail.asp?InvNo=<%=RSInvHdr.Fields("invoice").Value%>&BACK=ORDER&OrderNo=<%=request("OrderNo")%>">
			   <%IF strTemp = "" Then
					strTemp = RSInvHdr.Fields("invoice").Value 
					'WAL_check if this the last record to not display the comma[start]
					RSInvHdr.MoveNext 
					if RSInvHdr.Eof then
						RSInvHdr.MovePrevious 
						Response.Write(RSInvHdr.Fields("invoice").Value & "</a>")
					else
						RSInvHdr.MovePrevious 
						Response.Write(RSInvHdr.Fields("invoice").Value & "</a>, ")
					end if
					'WAL_check if this the last record to not display the comma[end]
				Else
					IF Not(instr(1,strTemp,RSInvHdr.Fields("invoice").Value) > 0) Then
						strTemp = strTemp & "," & RSInvHdr.Fields("invoice").Value
						'WAL_check if this the last record to not display the comma[start]
						RSInvHdr.MoveNext 
						if RSInvHdr.Eof then
							RSInvHdr.MovePrevious
							Response.Write(RSInvHdr.Fields("invoice").Value & "</a>")
						else
							RSInvHdr.MovePrevious
							Response.Write(RSInvHdr.Fields("invoice").Value & "</a>, ")
						end if
						'WAL_check if this the last record to not display the comma[end] 
					End IF
				End IF
				totInvAmt = totInvAmt+cdbl(RSInvHdr("Totalchg"))
				RSInvHdr.MoveNext 
				Loop
			End IF
		%><%=totAmt%>&nbsp;</td>
	</TR>
	<tr>
		<td rowspan=2 valign=top class="dark_cell">Notes</td>
		<td class="light_cell" colspan=11><%=Trim(rsHdrInfo("note1"))%></td>
		
	</tr>
	<tr>
		<td class="light_cell" colspan=11><%=Trim(rsHdrInfo("note2"))%></td>
	</tr>
</table>
<table border="1"  width=95% bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<tr>
		<td colspan=4 class="dark_cell">Sold To</td>
		<td colspan=6 class="dark_cell">Ship To</td>
	</tr>
	<%IF Trim(session("ID")) <> ""  Then%>
	<%'check if ship address chnged
	  if trim(rsHdrInfo("Alt_ShpTo")) = "True" then
	%>
		 <TR>
			<TD colspan=4 class="light_cell">
				&nbsp<%=session("RSCust")("btname")%>
			</TD>
			<TD colspan=6 class="light_cell">
				&nbsp<%=trim(rsHdrInfo("stname"))%>
			</TD>
		</TR>
		<TR>
			<TD colspan=4 class="light_cell">
				&nbsp<%=session("RSCust")("caddress12")%>
			</TD>
			<TD colspan=6 class="light_cell">
				&nbsp<%=trim(rsHdrInfo("caddress1"))%>
				
			</TD>
		</TR>
		<TR>
			<TD colspan=4 class="light_cell">
				&nbsp<%=session("RSCust")("caddress22")%>
			</TD>
			<TD colspan=6 class="light_cell">
				&nbsp<%=trim(rsHdrInfo("caddress2"))%>
				
			</TD>
		</TR>
		<TR>
			<TD colspan=4 class="light_cell">
				&nbsp<%=session("RSCust")("caddress32")%>, <%=session("RSCust")("caddress42")%>, <%=session("RSCust")("caddress52")%>
			</TD>
			<TD colspan=6 class="light_cell">
				&nbsp<%=trim(rsHdrInfo("caddress3"))%>
				
			</TD>
		</TR>
		<TR>
			<TD colspan=4 class="light_cell">
				&nbsp<%=session("RSCust")("caddress62")%>
			</TD>
			<TD colspan=6 class="light_cell">
				&nbsp<%=trim(rsHdrInfo("caddress4"))%>
				
			</TD>
		</TR>
	<%else%>
		<TR>
		<TD colspan=4 class="light_cell">
			&nbsp<%=session("RSCust")("btname")%>
		</TD>
		<TD colspan=6 class="light_cell">
			&nbsp<%=session("RSCust")("btname")%>
		</TD>
		</TR>
		<TR>
			<TD colspan=4 class="light_cell">
				&nbsp<%=session("RSCust")("caddress12")%>
			</TD>
			<TD colspan=6 class="light_cell">
				&nbsp<%=session("RSCust")("caddress1")%>
				
			</TD>
		</TR>
		<TR>
			<TD colspan=4 class="light_cell">
				&nbsp<%=session("RSCust")("caddress22")%>
			</TD>
			<TD colspan=6 class="light_cell">
				&nbsp<%=session("RSCust")("caddress2")%>
				
			</TD>
		</TR>
		<TR>
			<TD colspan=4 class="light_cell">
				&nbsp<%=session("RSCust")("caddress32")%>, <%=session("RSCust")("caddress42")%>, <%=session("RSCust")("caddress52")%>
			</TD>
			<TD colspan=6 class="light_cell">
				&nbsp<%=session("RSCust")("caddress3")%>, <%=session("RSCust")("caddress4")%>, <%=session("RSCust")("caddress5")%>
				
			</TD>
		</TR>
		<TR>
			<TD colspan=4 class="light_cell">
				&nbsp<%=session("RSCust")("caddress62")%>
			</TD>
			<TD colspan=6 class="light_cell">
				&nbsp<%=trim(session("RSCust")("caddress6"))%>
				
			</TD>
		</TR>
	<%end if%>
	<%end if%>
</table>
<br><br>
<table border="1"  width="95%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0" align=center>
<tr><td class="dark_cell">
<%
totShipAmt = 0
if not(rsOrdersMatch.eof and rsOrdersMatch.bof) then
rsOrdersMatch.MoveFirst()
end if
do while not rsOrdersMatch.EOF
Set RSScale=Server.CreateObject("ADODB.Recordset")
sqlscale="select * from scale,style where scale.type+scale.scale='S'+style.scale AND  Style.style='" & rsOrdersmatch("style")&"' And scale.scale=style.scale  "
'Response.Write sqlscale
'Response.End
'on error resume next
'RSScale.open sqlscale,conn,1,3
'Response.Write RSScale.RecordCount
'Response.End 
'strStyle= Replace(rsOrdersmatch("style")," ","&nbsp;")
'Response.Write(strStyle)
'if (RSScale.eof and RSScale.bof) then
 ' response.write("Your Order has no Lines.")
 ' response.end
'end if
%> <tr>
		
		<td width='15%' class="dark_cell">Style</td>
		<td width='5%' class="dark_cell" align=right dir=rtl><%=rsOrdersMatch("sz1")%></td>
		<td width='5%' class="dark_cell" align=right dir=rtl><%=rsOrdersMatch("sz2")%></td>
		<td width='5%' class="dark_cell" align=right dir=rtl><%=rsOrdersMatch("sz3")%></td>
		<td width='5%' class="dark_cell" align=right dir=rtl><%=rsOrdersMatch("sz4")%></td>
		<td width='5%' class="dark_cell" align=right dir=rtl><%=rsOrdersMatch("sz5")%></td>
		<td width='5%' class="dark_cell" align=right dir=rtl><%=rsOrdersMatch("sz6")%></td>
		<td width='5%' class="dark_cell" align=right dir=rtl><%=rsOrdersMatch("sz7")%></td>
		<td width='5%' class="dark_cell" align=right dir=rtl><%=rsOrdersMatch("sz8")%></td>
		<%'on error goto 0%>
	    <!--WMA Delete Commetion Field [Start]-->    
		<!--td width='5%' class="dark_cell" align="right" >Comm</td-->
	    <!--WMA Delete Commetion Field [End]-->  
	    <!--WMA-->
	    <%if CDbl(rsOrdersMatch("Disc_pcnt"))>0 then%>  
			<td width='5%' class="dark_cell" align="right" >Group</td>
			<td  nowrap class="dark_cell" align="right">Gross Price</td>
			<td  nowrap class="dark_cell" align="right">Disc</td>
		<%else%>
			<!--td  nowrap class="dark_cell" align="right" colspan=2>Gross Price</td-->
			<td width='15%' class="dark_cell" align="right" colspan=3>Group</td>
		<%end if%>
		<td  nowrap class="dark_cell" align="right">Price</td>
		<!--WMA Delete Commetion Field [Start]-->    
		<!--td width='5%' class="dark_cell" align="right" >Comm</td-->
		<td width='10%' class="dark_cell" align="center" nowrap>Expected Ship</td>
	    <!--WMA Delete Commetion Field [End]-->    
		<td width='7%' class="dark_cell" align="right" >Total Qty</td>
		<td width='7%' class="dark_cell" align="right" colspan=2>Amount</td>
	</tr>
	<tr>
	<%
	Dim strStyle
	strStyle= Replace(rsOrdersmatch("style")," ","&nbsp;")

	%>
		<td width="25%" valign=top class="light_cell"><%=strStyle%><br><%=rsOrdersmatch("Desc1")%></td>
		<td align=right valign=top  class="light_cell" >
			<%if Trim(rsOrdersMatch("Qty1")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrdersMatch("Qty1"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrdersMatch("Qty2")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrdersMatch("Qty2"))%>
			<%end if%>
		
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrdersMatch("Qty3")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrdersMatch("Qty3"))%>
			<%end if%>
		
	    </td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrdersMatch("Qty4")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrdersMatch("Qty4"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrdersMatch("Qty5")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrdersMatch("Qty5"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrdersMatch("Qty6")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrdersMatch("Qty6"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrdersMatch("Qty7")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrdersMatch("Qty7"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrdersMatch("Qty8")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrdersMatch("Qty8"))%>
			<%end if%>
		</td>
		
	    <!--WMA Delete Commetion Field [Start]-->    
		<!--td align=right valign=top class="light_cell" >
			<%if len(Trim(rsOrdersMatch("Comm1"))) = 0 then%>
				&nbsp
			<%else%>
				<%=FormatNumber(Trim(rsOrdersMatch("Comm1")))%>%
			<%end if%>
		</td-->
	    <!--WMA Delete Commetion Field [End]-->    
		<!--WMA-->
		<%if CDbl(rsOrdersMatch("Disc_pcnt"))>0 then%>
			<td align=right valign=top class="light_cell" width=5>
				<%if len(Trim(rsOrdersMatch("Group"))) = 0 then%>
					&nbsp
				<%else%>
					<%=Trim(rsOrdersMatch("Group"))%>
				<%end if%>
			</td>
			<td align=right valign=top class="light_cell" ><%if Session("CurrencyAlign")="LEFT" then %><%=Session("Currency")%><%=FormatNumber(rsOrdersMatch("Gros_Price"))%><%else%><%=FormatNumber(rsOrdersMatch("Gros_Price"))%><%=Session("Currency")%><% end if %></td>
			<td align=right valign=top class="light_cell" ><%=FormatNumber(rsOrdersMatch("Disc_pcnt"))%>%</td>
		<%else%>
			<!--td align=right valign=top class="light_cell" colspan=2><%if Session("CurrencyAlign")="LEFT" then %><%=Session("Currency")%><%=FormatNumber(rsOrdersMatch("Gros_Price"))%><%else%><%=FormatNumber(rsOrdersMatch("Gros_Price"))%><%=Session("Currency")%><% end if %></td-->			
			<td align=right valign=top class="light_cell" colspan=3 width=15>
			<%if len(Trim(rsOrdersMatch("Group"))) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrdersMatch("Group"))%>
			<%end if%>
		</td>
		<%end if%>
		<td align=right valign=top class="light_cell" ><%if Session("CurrencyAlign")="LEFT" then %><%=Session("Currency")%><%=FormatNumber(rsOrdersMatch("Price"))%><%else%><%=FormatNumber(rsOrdersMatch("Price"))%><%=Session("Currency")%><% end if %></td>		 
		<td align=center valign=top class="light_cell" >
			<%if cstr(rsOrdersMatch("complete"))<> "" and cstr(rsOrdersMatch("complete")) <> "12:00:00 AM" then %>
				<%=cstr(rsOrdersMatch("complete"))%>
			<%else%>
				<%= "N/A" %>
			<%end if%>
		</td>
	    <td align=right valign=top class="light_cell" >
			<%if Trim(rsOrdersMatch("Totqty")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrdersMatch("Totqty"))%>
			<%end if%>
		</td>
		
		
		<td align="Right" valign=top  class="light_cell" colspan=2>
			<%if Trim(rsOrdersMatch("Diff")) = 0 then%>
				&nbsp
			<%else%>
				<%if Session("CurrencyAlign")="LEFT" then %><%=Session("Currency")%><%=Trim(FormatNumber(rsOrdersMatch("Diff")))%><%else%><%=Trim(FormatNumber(rsOrdersMatch("Diff")))%><%=Session("Currency")%><%end if %>
			<%end if%>
		</td>
	</tr>
	
	<%'The next Lines to show the ship qty%>
	<%'The next lines to show the ship date
	
		Set RSship=Server.CreateObject("ADODB.Recordset")
		'HDM B606737,3 -12/9/2002- [Start] Icorrect where condition searching the invoice instead of the order
		'sqlship = "select * from invline where invoice+STR(lineno,6)  like'" & Request("orderNo") & "%' AND style+invoice+STR(lineno,6) like'"&rsOrdersMatch("style")&"%'"
			
		'HDM E302076,2 [Start] modify the ship recordset to collect invoice header information needed
		'sqlship = "select invLine from invline,invhdr where order+STR(lineno,6)+invoice like'" & Request("orderNo") & "%' AND style+invoice+STR(lineno,6) like'"&rsOrdersMatch("style")&"%'"
		sqlship = "select invLine.*, invhdr.shipdate, invhdr.piktkt,Invhdr.Totalchg from invline,invhdr where invline.order+STR(invline.lineno,6)+invline.invoice like '" & Request("orderNo") &"%' AND invline.style+invline.invoice+STR(invline.lineno,6) like'"&rsOrdersMatch("style")&"%' AND invhdr.status <> 'V' AND invline.invoice=invhdr.invoice and invline.lineno=" &cint(Trim(rsOrdersMatch("lineno")))& ""
		'sqlship = "select invLine.*, invhdr.shipdate, invhdr.piktkt, invhdr.tax_rate as hdrTax_rate, invhdr.tax_amt as hdrTax_amt  from invline,invhdr where invline.order+STR(invline.lineno,6)+invline.invoice like '" & Request("orderNo") &"%' AND invline.style+invline.invoice+STR(invline.lineno,6) like'"&rsOrdersMatch("style")&"%' AND invline.invoice=invhdr.invoice and invline.lineno=" &cint(Trim(rsOrdersMatch("lineno")))& ""
			
		'Response.Write "<font size=3>" & sqlship
			
		RSShip.open sqlship,conn,1,3
	%>
	<%
		'HDM E302076,2 Put this section in a loop to force displaying each ship line
		Do while not rsShip.EOF
		
	%>
	<TR>
		<td width="22%" valign=top class="light_cell">
		<b>Inv# <a href="../invoice/invoicedetail.asp?InvNo=<%=rsShip("invoice").Value%>&BACK=ORDER&OrderNo=<%=request("OrderNo")%>"><%=rsShip("invoice").Value%></a></b>
		<b>Ship Date</b> <%=Trim(RSShip("invdate"))%>
		</td>
		<%'RSInvLine.Filter = "style='" & Trim(rsOrdersMatch("Style")) & "'"  
			qty1=0
			qty2=0
			qty3=0
			qty4=0
			qty5=0
			qty6=0
			qty7=0
			qty8=0
			'RSInvLine.MoveFirst 
			'HDM E302076,2 No Need for this loop
			
			strPkTkt = ""
			qty1 = qty1 + cdbl(rsShip("qty1"))
			qty2 = qty2 + cdbl(rsShip("qty2"))
			qty3 = qty3 + cdbl(rsShip("qty3"))
			qty4 = qty4 + cdbl(rsShip("qty4"))
			qty5 = qty5 + cdbl(rsShip("qty5"))
			qty6 = qty6 + cdbl(rsShip("qty6"))
			qty7 = qty7 + cdbl(rsShip("qty7"))
			qty8 = qty8 + cdbl(rsShip("qty8"))
			strPkTkt = rsShip("piktkt")
			totQty = qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8
		%>
		<td align=right valign=top  class="light_cell" >
			<%if Trim(qty1) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(qty1)%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(qty2) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(qty2)%>
			<%end if%>
		
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(qty3) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(qty3)%>
			<%end if%>
		
	    </td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(qty4) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(qty4)%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(qty5)  = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(qty5)%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(qty6) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(qty6)%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(qty7) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(qty7)%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(qty8) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(qty8)%>
			<%end if%>
		</td>
		<td align=left valign=top colspan=5 class="light_cell"><b>Piktkt</b> <%=Trim(strPkTkt)%></td>
		
		<td align=right valign=top  class="light_cell"><%=totQty%></td>
		
		<%
			dim dblShipAmnt
			dim totAmt
			dblShipAmnt = 0
			
			'If not RSShip.EOF and not RSShip.BOF Then
			'	RSShip.MoveFirst 
			'	Do While Not RSShip.EOF
					dblShipAmnt = dblShipAmnt + cdbl(RSShip("neqvamnt"))
			'		RSShip.MoveNext 
			'	Loop
			'	RSShip.MoveFirst
			'End If
		
		%>
		
		<%If not RSShip.EOF and not RSShip.BOF Then%>
			<td align=right valign=top  class="light_cell" >
			<%if Session("CurrencyAlign")="LEFT" then %>
				<%=Session("Currency")%><%=FormatNumber(dblShipAmnt)%>
			<%else%>
				<%=FormatNumber(dblShipAmnt)%><%=Session("Currency")%>
			<%end if %></td>
		<%End If%>
	</TR>
	<%	totShipAmt = totShipAmt+dblShipAmnt
		
		rsShip.MoveNext
		Loop
	%>
<%
	rsOrdersMatch.MoveNext()
Loop
rsOrdersMatch.MoveFirst()
set rsOrderSums = server.CreateObject("ADODB.RecordSet")
strSQL = "SELECT SUM(Ordline.Qty1) AS Qty1Sum, SUM(Ordline.Qty2) AS Qty2Sum, SUM(Ordline.Qty3) AS Qty3Sum, "
strSQL = strSQL & " SUM(Ordline.Qty4) AS Qty4Sum, SUM(Ordline.Qty5) AS Qty5Sum, SUM(Ordline.Qty6) AS Qty6Sum, "
strSQL = strSQL & " SUM(Ordline.Qty7) AS Qty7Sum, SUM(Ordline.Qty8) AS Qty8Sum,Sum(Ordline.Totqty) AS TotqtySum, "
strSQL = strSQL & " SUM((Ordline.Totqty * Ordline.Price)) AS DiffSum FROM Ordline WHERE "
'Ard - Optimization [Start]
strSQL = strSQL & " ordline.cordtype+ordline.order+STR(ordline.lineno,6) like '"& request("Type")&Request("OrderNo")& "%'"
'Ard - Optimization [End]

rsOrderSums.Open strSQL, conn
%>
<!--tr>
		
	
		<td align=right width="154">
          <p align="left">Totals</p>
        </td>
		<td align=right valign=top  class="light_cell" >
			<%if Trim(rsOrderSums("Qty1Sum")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrderSums("Qty1Sum"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrderSums("Qty2Sum")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrderSums("Qty2Sum"))%>
			<%end if%>
		
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrderSums("Qty3Sum")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrderSums("Qty3Sum"))%>
			<%end if%>
		
	    </td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrderSums("Qty4Sum")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrderSums("Qty4Sum"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrderSums("Qty5Sum")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrderSums("Qty5Sum"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrderSums("Qty6Sum")) = 0then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrderSums("Qty6Sum"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrderSums("Qty7Sum")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrderSums("Qty7Sum"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsOrderSums("Qty8Sum")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsOrderSums("Qty8Sum"))%>
			<%end if%>
		</td>
	
		<td class="dark_cell">&nbsp;</td>
		<td align=right valign=top  class="dark_cell">&nbsp;</td>
		<td align=right valign=top  class="dark_cell">&nbsp;</td>
		<td align=right valign=top  class="dark_cell">&nbsp;</td>
		<td align=right valign=top  class="dark_cell">&nbsp;</td>
		<td align=right valign=top class="light_cell"><%=Trim(rsOrderSums("TotqtySum"))%></td>
	
		<td align=right valign=top class="light_cell" >
		<%if Session("CurrencyAlign")="LEFT" then %>
			<%=Session("Currency")%><%=Trim(FormatNumber(cdbl(rsOrderSums("DiffSum"))+cdbl(totShipAmt)))%>
		<%else%>
			<%=Trim(FormatNumber(cdbl(rsOrderSums("DiffSum"))+cdbl(totShipAmt)))%><%=Session("Currency")%>
		<%end if %></td>
	</tr-->
</table>
  </center>
</div>
<%
'get order charge info
dim rsCharge
set rsCharge= server.CreateObject ("ADODB.Recordset")
rsCharge.Open "select * from ordcharg where corder = '" &Request("orderNo")& "'",conn
if not rsCharge.EOF then
%>
	<BR><br>
	<table border="1"  width="95%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0" align=center>
	<TR>
		<TD colspan=4  valign=center><p><b><font size=3>Order Charges</font></b></p></TD>
	</TR>
	<%if Session("COD") = "T" then%>
		<TR>
			<TD width=20% class="dark_cell">Method of Payment</TD>
			<TD colspan=3 class="light_cell">
				<input type=radio name=radPay value="C">Cash on Delivary
				<input type=radio name=radPay value="D">Credit Card
			</TD>
			
		</TR>
		<TR>
			<TD colspan=4 class="light_cell" height=1></TD>
		</TR>
	<%end if%>

	<TR>
		<TD class="dark_cell">Total Weight</TD>
		<TD class="light_cell" align=right>
			<%=round(cdbl(trim(rsCharge("nweight"))),2)%>
		</TD>
		<%totAmt = cdbl(rsOrderSums("DiffSum"))+cdbl(totShipAmt)%>
		<TD class="dark_cell">Merchandise Amount</TD>
		<TD class="light_cell" align=right>
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(cdbl(totAmt),2),2)%>
			<%else%>
				<%=formatnumber(round(cdbl(totAmt),2),2) & Session("Currency")%>
			<%end if %>
		</TD>
	</TR>
	<TR>
		<TD  class="dark_cell">No. of Cartons</TD>
		<TD class="light_cell" align=right>
			<%=round(trim(rsCharge("n_cartons")),2)%>
		</TD>
		<TD class="dark_cell">Freight</TD>
		<TD class="light_cell" align=right>
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(cdbl(rsCharge("nfreight")),2),2)%>
			<%else%>
				<%=formatnumber(round(cdbl(rsCharge("nfreight")),2),2) & Session("Currency")%>
			<%end if %>
		</TD>
		
		
	</TR>
	<%if rsCharge("lPriChg") = true then%>
		<TR>
			<TD  class="dark_cell"></TD>
			<TD class="light_cell" align=right></TD>
			<TD class="dark_cell">Priority Shipping</TD>
			<TD class="light_cell" align=right>
				<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
					<%=Session("Currency") & formatnumber(cdbl(rsCharge("nPriChg")),2)%>
				<%else%>
					<%=formatnumber(cdbl(rsCharge("nPriChg")),2) & Session("Currency")%>
				<%end if %>
			</TD>
		</TR>
	<%else%>
	<%end if%>
	<%if rsCharge("lDropChg") = true then%>
		<TR>
			<TD  class="dark_cell"></TD>
			<TD class="light_cell" align=right></TD>
			<TD class="dark_cell">Drop Ship</TD>
			<TD class="light_cell" align=right>
				<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
					<%=Session("Currency") & formatnumber(cdbl(rsCharge("nDropChg")),2)%>
				<%else%>
					<%=formatnumber(cdbl(rsCharge("nDropChg")),2) & Session("Currency")%>
				<%end if %>
			</TD>
		</TR>
	<%else%>
	<%end if%>
	<TR>
		<TD  class="dark_cell"></TD>
		<TD class="light_cell" align=right></TD>
		<TD class="dark_cell">UPS Insur</TD>
		<TD class="light_cell" align=right>
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(cdbl(rsCharge("nInsur")),2),2)%>
			<%else%>
				<%=formatnumber(round(cdbl(rsCharge("nInsur")),2),2) & Session("Currency")%>
			<%end if %>
		</TD>
	</TR>
	<%if Session("COD") = "T" then%>
		<TR>
			<TD  class="dark_cell"></TD>
			<TD class="light_cell" align=right></TD>
			<TD class="dark_cell">C.O.D Charge</TD>
			<TD class="light_cell" align=right>
				<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
					<%=Session("Currency") & round(cdbl(rsCharge("nCod")),2)%>
				<%else%>
					<%=round(cdbl(rsCharge("nCod")),2) & Session("Currency")%>
				<%end if %>
			</TD>
		</TR>
	<%else%>
	<%end if%>
	
	<%if cdbl(rsCharge("nDiscount"))>0 then%>
	<TR>
		
		<TD class="dark_cell">Discount</TD>
		<TD class="light_cell" align=right>
			<%if trim(rsCharge("nDiscount")) <> "" then%> 
					<%=formatnumber(cdbl(trim(rsCharge("nDiscount"))),2)%>
			<%else%> 
				<%=FormatNumber(0)%> 
			<%end if%>%
		</TD>
		<%intDisc = cdbl(totAmt) * (cdbl(trim(rsCharge("nDiscount")))/100)%>
		<TD class="dark_cell">Discount Amount</TD>
		<TD class="light_cell" align=right>
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(intDisc,2),2)%>
			<%else%>
				<%=formatnumber(round(intDisc,2),2) & Session("Currency")%>
			<%end if %>
		</TD>
	</TR>
	<%end if%>
	<%if cdbl(rsCharge("ntax_amt")) > 0 then%>
	<TR>
		<TD class="dark_cell">Tax Rate</TD>
		<TD class="light_cell" align=right>
			<%=FormatNumber(cdbl(rsCharge("ntax_rate"))*100,3)%>%
		</TD>
		<TD class="dark_cell">Tax Amount</TD>
		<TD class="light_cell" align=right>
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(cdbl(rsCharge("ntax_amt")),2),2)%>
			<%else%>
				<%=formatnumber(round(cdbl(rsCharge("ntax_amt")),2),2) & Session("Currency")%>
			<%end if %>
		</TD>
	</TR>
	<%end if%>

	
	<TR>
		<TD colspan=4 class="light_cell" height=1></TD>
	</TR>
	
	<TR>
		<TD colspan=2 class="dark_cell" align=right></TD>
		<TD class="dark_cell">Total Order Amount</TD>
		<TD class="light_cell" align=right>
			
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(cdbl(rsCharge("nTotChg")),2))%>
			<%else%>
				<%=formatnumber(round(cdbl(rsCharge("nTotChg")),2)) & Session("Currency")%>
			<%end if %>
		</TD>
	</TR>
	<TR>
		<TD colspan=2 class="dark_cell" align=right></TD>
		<TD class="dark_cell">Deposit Amount</TD>
		<TD class="light_cell" align=right>
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(cdbl(rsCharge("ndeposit")),2),2)%>
			<%else%>
				<%=formatnumber(round(cdbl(rsCharge("ndeposit")),2),2) & Session("Currency")%>
			<%end if %>
		</TD>
	</TR>
	
	
	<!-- WMA change shippment values[start] -->
	<%
	'only show Ship amount data in case we have invoice record 
	IF not(RSInvHdr.Eof And RSInvHdr.BOF) Then
		RSInvHdr.MoveFirst 
	%>
		<TR><TD colspan=4 class="light_cell" height=1></TD></TR>
		<!--TR>
			<TD colspan=2 class="dark_cell" align=right></TD>
			<TD class="dark_cell">Ship Amount</TD>
			<TD class="light_cell" align=right>
				<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
					<%=Session("Currency") & formatnumber(round(cdbl(totShipAmt),2),2)%>
				<%else%>
					<%=formatnumber(round(cdbl(totShipAmt),2),2) & Session("Currency")%>
				<%end if %>
			</TD>
		</TR-->	

		<%'if cdbl(RSInvHdr("Tax_amt")) > 0 then  %>
		<!--TR>
			<TD class="dark_cell">Tax Rate</TD>
			<TD class="light_cell" align=right>
				<%=FormatNumber(cdbl(RSInvHdr("Tax_rate")),3)%>%
			</TD>
			<TD class="dark_cell">Tax Amount</TD>
			<TD class="light_cell" align=right>
				<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
					<%=Session("Currency") & formatnumber(round(cdbl(RSInvHdr("tax_amt")),2),2)%>
				<%else%>
					<%=formatnumber(round(cdbl(RSInvHdr("tax_amt")),2),2) & Session("Currency")%>
				<%end if %>
			</TD>
		</TR-->
		<%'end if%>
	
		<TR>
			<TD colspan=2 class="dark_cell" align=right></TD>
			<TD class="dark_cell">Total Ship Amount</TD>
			<TD class="light_cell" align=right>
				<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
					<%=Session("Currency") & formatnumber(round(cdbl(totInvAmt),2),2)%>
				<%else%>
					<%=formatnumber(round(cdbl(totInvAmt),2),2) & Session("Currency")%>
				<%end if %>
			</TD>
		</TR>
	<%
	end if
	%>
		<TR><TD colspan=4 class="light_cell" height=1></TD></TR>
	<!-- WMA change shippment values [end]-->
	<TR>
		<TD colspan=2 class="dark_cell" align=right></TD>
		<TD class="dark_cell">Balance to Pay</TD>
		<%intToPay = round(cdbl(rsCharge("nTotChg")),2) - round(cdbl(rsCharge("ndeposit")),2)%>
		<TD class="light_cell" align=right>
			
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(cdbl(intToPay),2))%>
			<%else%>
				<%=formatnumber(round(cdbl(intToPay),2)) & Session("Currency")%>
			<%end if %>
		</TD>
	</TR>
	</TABLE>
<%
end if'
end if ' for no detail exist for the record ...
end if ' for if the No is valid ...

%>
<p>
</BODY>
</HTML>