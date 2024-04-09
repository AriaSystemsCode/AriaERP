<%@ Language=VBScript %>
<%
Response.Buffer=true
IF Session("ID")="" And Session("rep")="" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
	</script>	
<%END IF 

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
set connTemp=server.createobject("ADODB.Connection")
set RSTemp=server.createobject("ADODB.Recordset")
connTemp.open Application("DataConnectionString")
sqlTemp="select * from customer where account='"&session("customerid")&"'"
RSTemp.open sqlTemp,connTemp,1,3

%>

<html>
<head>
<LINK REL=stylesheet HREF="../images/<%=Session("Theme")%>/RA.css" TYPE="text/css">
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
             <%response.write " - "&RSTemp("btname")%></font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="repcust.asp" ><font color=white size="2" face="Arial"><b>Get Customer</b></font></a>
	-->
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected customer is <b><%=Session("customerid")%> - <%=RSTemp.fields("btname").value%></b></P>
	</TD>
	<TD align=right><a href="repcust.asp" style="text-decoration: none">Get Customer</a></TD>
	<!-- ARD -->
</TR>
</table>
 <%Set RSTemp=nothing
  ConnTemp.close
  Set ConnTemp=nothing


End IF%>

<Table width=95% height=50 align=center border=1>
<TR>
<TD class=Title>Check R/A Status</TD>
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
strSQL = "SELECT Retauth.Rano,Retauth.Radate,Retauth.Void,Retauth.order,Retauth.invoice,Retauth.Auth,Retauth.Account,Retauth.Store,Retauth.Custpo,Retauth.Cartons,CodesReason.Cdiscrep AS easonDisc, Customer.Stname,Customer.Caddress1,Customer.Caddress2,Customer.Caddress3,Customer.Caddress4,Customer.Caddress5,Customer.Caddress6, Customer.phone1,Warehous.cwarecode,Warehous.cdesc AS WCdesc,Warehous.Caddress1 AS RCaddress1,Warehous.Caddress2 AS RCaddress2,Warehous.Caddress3 AS RCaddress3,Warehous.Caddress4 AS RCaddress4,Warehous.Caddress5 AS RCaddress5,Warehous.Caddress6 AS RCaddress6,Warehous.Cphone AS RCphone,Retauth.cretnote1,Retauth.cretnote2,Retauth.cretnote3,Retauth.cretnote4,Retauth.Rano FROM Retauth,Codes AS CodesReason,Codes AS CodesDivision,customer,warehous WHERE CodesReason.Cdefcode='N' AND 	CodesReason.Crltfield='N' AND CodesReason.Ccode_no = Retauth.Reason AND     CodesReason.Cfld_name = 'REASON' AND CodesDivision.Cdefcode='N' AND	CodesDivision.Crltfield='N' AND    CodesDivision.Cfld_name='CDIVISION' AND CodesDivision.Ccode_no = Retauth.Cdivision AND Retauth.Account='" & Session("customerID") & "' AND Retauth.Rano = '" & Request("RanoNo") & "' AND Retauth.store = Customer.store AND Customer.account = '"& Session("customerID") & "' AND Retauth.Cwarecode = Warehous.Cwarecode"
else
strSQL = "SELECT Retauth.Rano,Retauth.Radate,Retauth.Void,Retauth.order,Retauth.invoice,Retauth.Auth,Retauth.Account,Retauth.Store,Retauth.Custpo,Retauth.Cartons,CodesReason.Cdiscrep AS easonDisc, Customer.Stname,Customer.Caddress1,Customer.Caddress2,Customer.Caddress3,Customer.Caddress4,Customer.Caddress5,Customer.Caddress6, Customer.phone1,Warehous.cwarecode,Warehous.cdesc AS WCdesc,Warehous.Caddress1 AS RCaddress1,Warehous.Caddress2 AS RCaddress2,Warehous.Caddress3 AS RCaddress3,Warehous.Caddress4 AS RCaddress4,Warehous.Caddress5 AS RCaddress5,Warehous.Caddress6 AS RCaddress6,Warehous.Cphone AS RCphone,Retauth.cretnote1,Retauth.cretnote2,Retauth.cretnote3,Retauth.cretnote4,Retauth.Rano FROM Retauth,Codes AS CodesReason,Codes AS CodesDivision,customer,warehous WHERE CodesReason.Cdefcode='N' AND 	CodesReason.Crltfield='N' AND CodesReason.Ccode_no = Retauth.Reason AND     CodesReason.Cfld_name = 'REASON' AND CodesDivision.Cdefcode='N' AND	CodesDivision.Crltfield='N' AND    CodesDivision.Cfld_name='CDIVISION' AND CodesDivision.Ccode_no = Retauth.Cdivision AND Retauth.Account='" & Session("ID") & "' AND Retauth.Rano = '" & Request("RanoNo") & "' AND Retauth.store = Customer.store AND Customer.account = '"& Session("ID") & "' AND Retauth.Cwarecode = Warehous.Cwarecode"
end if
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
strSQL = "SELECT Raline.Reason, Raline.Style, Style.Desc1, Raline.Price, Raline.Qty1, Raline.Qty2, Raline.Qty3, Raline.Qty4, Raline.Qty5, Raline.Qty6, Raline.Qty7, Raline.Qty8, Raline.Totqty, (Raline.Totqty * Raline.Price) AS Amount, Raline.Nopnqty1,Raline.Nopnqty2,Raline.Nopnqty3,Raline.Nopnqty4,Raline.Nopnqty5,Raline.Nopnqty6, Raline.Nopnqty7,Raline.Nopnqty8, Raline.Ntotopnqty, (Raline.Ntotopnqty * Raline.Price) As OpnAmount ,  Scale.Sz1, Scale.Sz2,  Scale.Sz3,  Scale.Sz4, Scale.Sz5, Scale.Sz6, Scale.Sz7, Scale.Sz8 FROM Raline,Style,Scale WHERE Style.Style=Raline.Style AND Scale.Scale=Style.Scale AND Scale.Type='S' AND Raline.Rano='" & rsRanoMatch("Rano") & "' AND Raline.Account='" & Session("customerID") & "' Order By Raline.Cra_linno"
else
strSQL = "SELECT Raline.Reason, Raline.Style, Style.Desc1, Raline.Price, Raline.Qty1, Raline.Qty2, Raline.Qty3, Raline.Qty4, Raline.Qty5, Raline.Qty6, Raline.Qty7, Raline.Qty8, Raline.Totqty, (Raline.Totqty * Raline.Price) AS Amount, Raline.Nopnqty1,Raline.Nopnqty2,Raline.Nopnqty3,Raline.Nopnqty4,Raline.Nopnqty5,Raline.Nopnqty6, Raline.Nopnqty7,Raline.Nopnqty8, Raline.Ntotopnqty, (Raline.Ntotopnqty * Raline.Price) As OpnAmount ,  Scale.Sz1, Scale.Sz2,  Scale.Sz3,  Scale.Sz4, Scale.Sz5, Scale.Sz6, Scale.Sz7, Scale.Sz8 FROM Raline,Style,Scale WHERE Style.Style=Raline.Style AND Scale.Scale=Style.Scale AND Scale.Type='S' AND Raline.Rano='" & rsRanoMatch("Rano") & "' AND Raline.Account='" & Session("ID") & "' Order By Raline.Cra_linno"
end if
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
<strong>Return Authorization # <%=rsRanoMatch("Rano")%><strong>
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
			<strong>Account</strong>
		</td>
		
		<td class="light_cell">
			<%=rsRanoMatch("Account")%> &nbsp;</td>
	
		<td class="dark_cell">
			<strong>Store</strong>
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
		<td class="light_cell">
			<%=rsRanoMatch("Cartons")%> &nbsp;</td>
		<td class="dark_cell">
			<strong>Reason</strong>
		</td>
		<td class="light_cell">
			<%=rsRanoMatch("easonDisc")%> &nbsp;</td>
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
		<td colspan="4" width="158" class="dark_cell"><strong>Style</strong></td>
		<td colspan="8" width="505" class="dark_cell"><strong>Description</strong></td>
		<td width="65" class="dark_cell"><strong>Tot. Qty.</strong></td>
	</tr>
<%
rsRanoDetail.MoveFirst()
do while not rsRanoDetail.EOF
%> 

	<tr>
		<td colspan="4" width="158" class="light_cell">
		 <%=rsRanoDetail("Style")%> &nbsp;</td>
		<td colspan="9" width="576" class="light_cell">
		<%=rsRanoDetail("Desc1")%> &nbsp;</td>
		
	</tr>
	<tr>
		<td width="158" colspan="4" class="dark_cell">
			
        <strong>Size</strong>
			
		</td>				
		<td width="55" align="right" class="dark_cell">
			<%=rsRanoDetail("Sz1")%> &nbsp;</td>
		<td width="55" align="right" class="dark_cell">
			<%=rsRanoDetail("Sz2")%> &nbsp;</td>
		<td width="55" align="right" class="dark_cell">
			<%=rsRanoDetail("Sz3")%> &nbsp;</td>
		<td width="55" align="right" class="dark_cell">
			<%=rsRanoDetail("Sz4")%> &nbsp;</td>
		<td width="55" align="right" class="dark_cell">
			<%=rsRanoDetail("Sz5")%> &nbsp;</td>
		<td width="55" align="right" class="dark_cell">
			<%=rsRanoDetail("Sz6")%> &nbsp;</td>
		<td width="55" align="right" class="dark_cell">
			<%=rsRanoDetail("Sz7")%> &nbsp;</td>
		<td width="55" align="right" class="dark_cell">
			<%=rsRanoDetail("Sz8")%> &nbsp;</td>
		
		<td width="65" class="dark_cell">
			
		</td>
    </tr>
    <tr>
		<td colspan="4" width="158" class="dark_cell">
			<strong>Authorized</strong>
		</td>	
		<td align="right" width="55" class="light_cell">
			<%=rsRanoDetail("Qty1")%> &nbsp;</td>
		<td align="right" width="55" class="light_cell">
			<%=rsRanoDetail("Qty2")%> &nbsp;</td>
		<td align="right" width="55" class="light_cell">
			<%=rsRanoDetail("Qty3")%> &nbsp;</td>
		<td align="right" width="55" class="light_cell">
			<%=rsRanoDetail("Qty4")%> &nbsp;</td>
		<td align="right" width="55" class="light_cell">
			<%=rsRanoDetail("Qty5")%> &nbsp;</td>
		<td align="right" width="55" class="light_cell">
			<%=rsRanoDetail("Qty6")%> &nbsp;</td>
		<td align="right" width="55" class="light_cell">
			<%=rsRanoDetail("Qty7")%> &nbsp;</td>
		<td align="right" width="55" class="light_cell">
			<%=rsRanoDetail("Qty8")%> &nbsp;</td>
	
	
		<td align="right" width="65" class="light_cell">
			<%=rsRanoDetail("Totqty")%> &nbsp;</td>
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