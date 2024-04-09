<%@ Language=VBScript %>
<%
Response.Buffer=true
IF Session("ID")="" And Session("rep")="" Then
	'Response.Redirect("../default.asp")%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>
<%End IF

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF

%>

<html>
<head>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
<title>CRM - Check Return Authorization Status - R.A. #<%=Request("RanoNo")%></title>
</head>
<body>

<SCRIPT LANGUAGE=javascript>
<!--
function GoReport()
{
	var flg = navigator.appVersion  ;
	
	if(flg.indexOf("MSIE") != -1)
		{
			document.location.href = "RetAuthRep.asp?RanoNo=<%=Request.QueryString("RanoNo")%>&viewer=ActiveX";
		}
	else
		{
			document.location.href = "RetAuthRep.asp?RanoNo=<%=Request.QueryString("RanoNo")%>&viewer=Java-Plug-in";
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
	<TD colspan=13>
	<P>Your currently selected <%=session("CustField")%> is <b><%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></b></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>
 <%End IF%>

<Table width=95% height=50 align=center border=1>
<TR>
<TD class=Title>Check RA Status</TD>
</TR>
</Table>
<Br>
<%IF compWork = "Y" Then%>


<%
set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")


'RecordSets
set rsRanoMatch = server.CreateObject("ADODB.RecordSet")

 
BeginDate = (Trim(Request.Form("txtBeginDate")))
EndDate  = (Trim(Request.Form("txtEndDate")))

if len(trim(session("rep")))>0 then
strSQL = "SELECT Retauth.Rano,Retauth.Radate,Retauth.Void,Retauth.order,"
strSQL = strSQL & " Retauth.invoice,Retauth.Auth,Retauth.Account,Retauth.Store,"
strSQL = strSQL & " Retauth.Custpo,Retauth.Cartons,CodesReason.Cdiscrep AS easonDisc, "
strSQL = strSQL & " Customer.Stname,Customer.Caddress1,Customer.Caddress2,Customer.Caddress3,"
strSQL = strSQL & " Customer.Caddress4,Customer.Caddress5,Customer.Caddress6, Customer.phone1,"
strSQL = strSQL & " Warehous.cwarecode,Warehous.cdesc AS WCdesc,Warehous.Caddress1 AS RCaddress1,"
strSQL = strSQL & " Warehous.Caddress2 AS RCaddress2,Warehous.Caddress3 AS RCaddress3,"
strSQL = strSQL & " Warehous.Caddress4 AS RCaddress4,Warehous.Caddress5 AS RCaddress5,"
strSQL = strSQL & " Warehous.Caddress6 AS RCaddress6,Warehous.Cphone AS RCphone,"
strSQL = strSQL & " Retauth.cretnote1,Retauth.cretnote2,Retauth.cretnote3,Retauth.cretnote4,"

strSQL = strSQL & " Retauth.Rano FROM "
strSQL = strSQL & " Retauth,Codes AS CodesReason,Codes AS CodesDivision,customer,warehous WHERE "

strSQL = strSQL & " CodesReason.cdefcode+CodesReason.crltfield+CodesReason.cfld_name = 'NNREASON' and"
strSQL = strSQL & " CodesReason.Ccode_no = Retauth.Reason AND "
strSQL = strSQL & " CodesDivision.cdefcode+CodesDivision.crltfield+CodesDivision.cfld_name = 'NNCDIVISION' and "
strSQL = strSQL & " CodesDivision.Ccode_no = Retauth.Cdivision AND "
strSQL = strSQL & " Retauth.Account+Retauth.Rano='" & Session("customerID") & Request("RanoNo")& "' AND "
strSQL = strSQL & " Retauth.store = Customer.store AND "
strSQL = strSQL & " (Customer.type+Customer.account+Customer.store like 'M" & Session("customerID") & "%' or "
strSQL = strSQL & "  Customer.type+Customer.account+Customer.store like 'S" & Session("customerID") &"%') and "
strSQL = strSQL & " ALLTRIM(Retauth.Cwarecode) = ALLTRIM(Warehous.Cwarecode)"
'Response.Write(strSQL)
else
strSQL = "SELECT Retauth.Rano,Retauth.Radate,Retauth.Void,Retauth.order,"
strSQL = strSQL & " Retauth.invoice,Retauth.Auth,Retauth.Account,Retauth.Store,"
strSQL = strSQL & " Retauth.Custpo,Retauth.Cartons, "
strSQL = strSQL & " Customer.Stname,Customer.Caddress1,Customer.Caddress2,Customer.Caddress3,"
strSQL = strSQL & " Customer.Caddress4,Customer.Caddress5,Customer.Caddress6, Customer.phone1,"
strSQL = strSQL & " Warehous.cwarecode,Warehous.cdesc AS WCdesc,Warehous.Caddress1 AS RCaddress1,"
strSQL = strSQL & " Warehous.Caddress2 AS RCaddress2,Warehous.Caddress3 AS RCaddress3,"
strSQL = strSQL & " Warehous.Caddress4 AS RCaddress4,Warehous.Caddress5 AS RCaddress5,"
strSQL = strSQL & " Warehous.Caddress6 AS RCaddress6,Warehous.Cphone AS RCphone,"
strSQL = strSQL & " Retauth.cretnote1,Retauth.cretnote2,Retauth.cretnote3,Retauth.cretnote4,"

strSQL = strSQL & " Retauth.Rano FROM "
strSQL = strSQL & " Retauth,Codes AS CodesDivision,customer,warehous WHERE "

'strSQL = strSQL & " CodesReason.cdefcode+CodesReason.crltfield+CodesReason.cfld_name = 'NNREASON' and"
'strSQL = strSQL & " CodesReason.Ccode_no = Retauth.Reason AND "
strSQL = strSQL & " CodesDivision.cdefcode+CodesDivision.crltfield+CodesDivision.cfld_name = 'NNCDIVISION' and "
strSQL = strSQL & " CodesDivision.Ccode_no = Retauth.Cdivision AND "
'HDM 11/24/2002 [Start]Use the Session("ID") variable as it's the correct one that holds the customer ID
'strSQL = strSQL & " (Customer.type+Customer.account+Customer.store like 'M" & Session("customerID") & "%' or "
'strSQL = strSQL & "  Customer.type+Customer.account+Customer.store like 'S" & Session("customerID") &"%') and "
strSQL = strSQL & " (Customer.type+Customer.account+Customer.store like 'M" & Session("ID") & "%' or "
strSQL = strSQL & "  Customer.type+Customer.account+Customer.store like 'S" & Session("ID") &"%') and "
'HDM 11/24/2002 [End]
strSQL = strSQL & " Retauth.Account+Retauth.Rano='" & Session("ID") & Request("RanoNo")& "' AND "
'strSQL = strSQL & " Retauth.Account='" & Session("ID") & "' AND"
'strSQL = strSQL & " Retauth.Rano = '" & Request("RanoNo") & "' AND"
strSQL = strSQL & " Retauth.store = Customer.store AND "
strSQL = strSQL & " ALLTRIM(Retauth.Cwarecode) = ALLTRIM(Warehous.Cwarecode)"
end if

'Response.Write "<font size=3>"&strSQL
'Response.End 
rsRanoMatch.Open strSQL, conn

if rsRanoMatch.EOF AND rsRanoMatch.BOF then %>
	<br>&nbsp;&nbsp;
	<Table width=95% align=center><TR><TD>

	No Return Authorizations match your criteria.
	<br><a HREF="ReturnStatus.asp">back</a>	
	</TD></TR></Table> 
<%
else

rsRanoMatch.MoveFirst()

set rsRanoDetail = Server.CreateObject("ADODB.RecordSet")
if len(trim(session("rep")))>0 then
	strSQL = "SELECT Raline.Reason, CodesReason.Cdiscrep AS reasonDisc, Raline.Style, Style.Desc1, Raline.Price, Raline.Qty1, Raline.Qty2, Raline.Qty3, Raline.Qty4, Raline.Qty5, Raline.Qty6, Raline.Qty7, Raline.Qty8, Raline.Totqty, (Raline.Totqty * Raline.Price) AS Amount, Raline.Nopnqty1,Raline.Nopnqty2,Raline.Nopnqty3,Raline.Nopnqty4,Raline.Nopnqty5,Raline.Nopnqty6, Raline.Nopnqty7,Raline.Nopnqty8, Raline.Ntotopnqty, (Raline.Ntotopnqty * Raline.Price) As OpnAmount ,  Scale.Sz1, Scale.Sz2,  Scale.Sz3,  Scale.Sz4, Scale.Sz5, Scale.Sz6, Scale.Sz7, Scale.Sz8 FROM Raline,Style,Codes AS CodesReason,Scale WHERE Style.Style=Raline.Style AND Scale.Scale=Style.Scale AND Scale.Type='S' AND Raline.Rano='" & rsRanoMatch("Rano") & "' AND Raline.Account='" & Session("customerID") & "' "
else
	strSQL = "SELECT Raline.Reason, CodesReason.Cdiscrep AS reasonDisc, Raline.Style, Style.Desc1, Raline.Price, Raline.Qty1, Raline.Qty2, Raline.Qty3, Raline.Qty4, Raline.Qty5, Raline.Qty6, Raline.Qty7, Raline.Qty8, Raline.Totqty, (Raline.Totqty * Raline.Price) AS Amount, Raline.Nopnqty1,Raline.Nopnqty2,Raline.Nopnqty3,Raline.Nopnqty4,Raline.Nopnqty5,Raline.Nopnqty6, Raline.Nopnqty7,Raline.Nopnqty8, Raline.Ntotopnqty, (Raline.Ntotopnqty * Raline.Price) As OpnAmount ,  Scale.Sz1, Scale.Sz2,  Scale.Sz3,  Scale.Sz4, Scale.Sz5, Scale.Sz6, Scale.Sz7, Scale.Sz8 FROM Raline,Style,Codes AS CodesReason,Scale WHERE Style.Style=Raline.Style AND Scale.Scale=Style.Scale AND Scale.Type='S' AND Raline.Rano='" & rsRanoMatch("Rano") & "' AND Raline.Account='" & Session("ID") & "' "
end if
strSQL = strSQL & " AND CodesReason.cdefcode+CodesReason.crltfield+CodesReason.cfld_name = 'NNREASON' and"
strSQL = strSQL & " CodesReason.Ccode_no = Raline.Reason"
strSQL = strSQL & " Order By Raline.Cra_linno"
rsRanoDetail.Open strSQL, conn

'check to see if there is detail for theis Order
if rsRanoDetail.EOF And rsRanoDetail.BOF then %>
	<br> 
  <table border="0" width=95% cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td >
	<b>No details exist for this Return Authorization number.</b>
	</td></tr>
	<tr><td>
	<a HREF="ReturnStatus.asp">	back</a>
	</td>	</tr></table>
<%
else
%>
<br>
<Table width=95% align=center><TR><TD align=center>
Return Authorization # <strong><%=rsRanoMatch("Rano")%></strong>
<bR>
<a href="javascript:GoReport();">Print Friendly Report</a>
</TD></TR></Table>

<br>

<div align="center">
  <center>

<table border="1" width="95%" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
	<tr>
		<td class="dark_cell">
			<strong>Date</strong>
		</td>
		
		<td class="light_cell">
			<%=rsRanoMatch("Radate")%> &nbsp;</td>
	
		<td class="dark_cell">
			<strong>Void</strong>
		</td>
			
		<td class="light_cell">
			<%=rsRanoMatch("Void")%> &nbsp;</td>
		<td class="dark_cell">
			<strong>Order</strong>
		</td>
		<td width=60 class="light_cell">
			<%=rsRanoMatch("order")%> &nbsp;</td>
	
		<td class="dark_cell">
			<strong>Invoice</strong>
		</td>
		<td width=60 class="light_cell">
			<%=rsRanoMatch("invoice")%> &nbsp;</td>
		<td class="dark_cell">
			<strong>Total Pcs.</strong>
		</td>
		<td class="light_cell">
			<%=rsRanoMatch("Auth")%> &nbsp;</td>
	</tr>

   <tr>
		<td class="dark_cell">
			<strong><%=session("CustField")%></strong>
		</td>
		
		<td class="light_cell">
			<%=rsRanoMatch("Account")%> &nbsp;</td>
	
		<td class="dark_cell">
			<strong>Ship From</strong>
		</td>
			
		<td class="light_cell">
			<%=rsRanoMatch("Store")%> &nbsp;</td>
		<td class="dark_cell">
			<strong>Cust PO</strong>
		</td>
		<td class="light_cell">
			<%=rsRanoMatch("Custpo")%> &nbsp;</td>
	
		<td class="dark_cell">
			<strong>Cartons</strong>
		</td>
		<td colspan=3 class="light_cell">
			<%=rsRanoMatch("Cartons")%> &nbsp;</td>
	
	</tr>
</table>
  </center>
</div>
<br>

<table border="0" width="95%" align=center>
<tr>
	<td width="33%">
		<strong>Sold to</strong>
	</td>
	<td>
	</td>
	<td width="33%">
		<strong>Returned To</strong>
	</td>
</tr>
<tr>
	<td>
		<table border="1" width="100%" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("Stname")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("Caddress1")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("Caddress2")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("Caddress3")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("Caddress4")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("Caddress5")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("Caddress6")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("phone1")%>&nbsp;</td>
			</tr>
			
		</table>
	</td>
	<td width="30%">
	</td>
	<td>
		<table border="1" width="100%" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("cwarecode")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("WCdesc")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("RCaddress1")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("RCaddress2")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("RCaddress3")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("RCaddress4")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("RCaddress5")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("RCaddress6")%>&nbsp;</td>
			</tr>
			<tr>
				<td class="dark_cell"><%=rsRanoMatch("RCphone")%>&nbsp;</td>
			</tr>
		</table>
	</td>
</tr>
</table>

<br>
<div align="center">
  <center>
<table border="1" width="95%" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
	<tr>
		<td colspan="4"  class="dark_cell">Style</td>
		<td colspan="4"  class="dark_cell">Description</td>
		<td colspan="4"  class="dark_cell">Reason</td>
		<td width="65" class="dark_cell" align=right>Tot. Qty.</td>
	</tr>
<%
rsRanoDetail.MoveFirst()
do while not rsRanoDetail.EOF
%> 
	<tr>
		<td colspan="4"  class="light_cell"><%=rsRanoDetail("Style")%> &nbsp;</td>
		<td colspan="4" class="light_cell"><%=rsRanoDetail("Desc1")%> &nbsp;</td>
		<td colspan="5"  class="light_cell"><%=rsRanoDetail("reasonDisc")%>&nbsp;</td>
	</tr>
	<tr>
		<td width="158" colspan="4" class="dark_cell">Size</td>				
		<td width="55" align="right" class="dark_cell" dir=rtl><%=rsRanoDetail("Sz1")%></td>
		<td width="55" align="right" class="dark_cell" dir=rtl><%=rsRanoDetail("Sz2")%></td>
		<td width="55" align="right" class="dark_cell" dir=rtl><%=rsRanoDetail("Sz3")%></td>
		<td width="55" align="right" class="dark_cell" dir=rtl><%=rsRanoDetail("Sz4")%></td>
		<td width="55" align="right" class="dark_cell" dir=rtl><%=rsRanoDetail("Sz5")%></td>
		<td width="55" align="right" class="dark_cell" dir=rtl><%=rsRanoDetail("Sz6")%></td>
		<td width="55" align="right" class="dark_cell" dir=rtl><%=rsRanoDetail("Sz7")%></td>
		<td width="55" align="right" class="dark_cell" dir=rtl><%=rsRanoDetail("Sz8")%></td>
		
		<td width="65" class="dark_cell">
			
		</td>
    </tr>
    <tr>
		<td colspan="4" width="158" class="dark_cell">
			<strong>Authorized</strong>
		</td>
		<td align=right valign=top  class="light_cell" >
			<%if Trim(rsRanoDetail("Qty1")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsRanoDetail("Qty1"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsRanoDetail("Qty2")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsRanoDetail("Qty2"))%>
			<%end if%>
		
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsRanoDetail("Qty3")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsRanoDetail("Qty3"))%>
			<%end if%>
		
	    </td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsRanoDetail("Qty4")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsRanoDetail("Qty4"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsRanoDetail("Qty5")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsRanoDetail("Qty5"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsRanoDetail("Qty6")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsRanoDetail("Qty6"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsRanoDetail("Qty7")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsRanoDetail("Qty7"))%>
			<%end if%>
		</td>
		<td align=right valign=top class="light_cell" >
			<%if Trim(rsRanoDetail("Qty8")) = 0 then%>
				&nbsp
			<%else%>
				<%=Trim(rsRanoDetail("Qty8"))%>
			<%end if%>
		</td>	
	
	-
		<td align="right" width="65" class="light_cell" dir=rtl><%=rsRanoDetail("Totqty")%></td>
    </tr>
   

<%
	rsRanoDetail.MoveNext()
Loop
%>
</table>
  </center>
</div>
<br>
<%
end if ' for no detail exist for the record ...
end if ' for if the No is valid ...
End IF
%>
</body>
</html>