<%@ Language=VBScript %>
<%
Response.CacheControl  = "no-cache"
Response.AddHeader  "Pragma", "no-cache"
Response.Expires = -1
Response.Buffer = true
%>
<%If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then%>
	<script language="javascript">
	parent.location.href ="../login.asp"
	</script>
<%End if

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
	strSalesRep = Ucase(Session("RSCust").fields("salesrep"))
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	'IF Trim(Session("customerid")) = "" Then
	'	Response.Redirect("../repcust.asp")
	'END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
	strSalesRep = Ucase(Trim(Session("rep")))
End IF

if Session("Season")="All" or Session("Season")="*" then
	Session("Season") = "NONE"
end if
'get order detail[start]
set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
Dim rsHdrInfo
Set rsHdrInfo = server.CreateObject("ADODB.recordset")

if trim(request("OrderNo")) <> "" then
	session("OrdNo") = trim(request("OrderNo"))
end if
if trim(request("Type")) <> "" then
	session("Type") = trim(request("Type"))
end if
'get header info[start]
strSQL = "SELECT Ordhdr.order,Ordhdr.Book,Ordhdr.Ship,Ordhdr.Cancel,Ordhdr.Open, Ordhdr.Account,Ordhdr.Disc,"
strSQL = strSQL & " Ordhdr.status,Ordhdr.priority,OrdHdr.shipvia, Ordhdr.Start, Ordhdr.Complete,"
strSQL = strSQL & " Ordhdr.custpo,Ordhdr.entered,customer.stname AS CustStname, Alt_ShpTo,"
strSQL = strSQL & " Ordhdr.stname,Ordhdr.caddress1,Ordhdr.caddress2,Ordhdr.caddress3,Ordhdr.caddress4,Ordhdr.caddress5"
strSQL = strSQL & " FROM Ordhdr,Customer WHERE"
IF Trim(CustID) <> ""  Then
	strSQL = strSQL & " ordhdr.account+ordhdr.cordtype+ordhdr.order like '" &CustID& session("Type")& session("OrdNo") &  "' AND "
	strSQL = strSQL & " customer.type+customer.account+customer.store like 'M"&CustID&"%'"
Else	
	strSQL = strSQL & " ordhdr.order = '" & session("OrdNo") &  "' AND "
	strSQL = strSQL & " customer.type+customer.account+customer.store like 'M%' and customer.account = ordhdr.account "
end if
'strSQL = strSQL & " ordhdr.account+ordhdr.cordtype+ordhdr.order like '" & CustID & session("Type")& session("OrdNo") &  "' and "
'strSQL = strSQL & " customer.type+customer.account+customer.store like 'M"&CurCust&"%'"
'Response.Write "<br><br><font size=2>"&strSQL
rsHdrInfo.Open strSQL, conn
'get header info[end]

'Get Deposit amount
Set rsOrdCharge = conn.Execute("select Ndeposit from ordcharg where corder='"& session("OrdNo") &"'")
if rsOrdCharge.eof then
	Session("Ndeposit") = 0
else
	Session("Ndeposit") = rsOrdCharge("Ndeposit")
end if

if Request.QueryString ("Come") <> "U" then
	Set Session("RSLine") = server.CreateObject("ADODB.recordset")
	if not Session("RSLine") is nothing then
		if Session("RSLine").state <> 0 then
			Session("RSLine").close
		end if
	end if
	Session("RSLine").Open "select Ordline.cordtype,prepak, nsugretpri, Ordline.order,Ordline.account,Ordline.lineno,Ordline.cwarecode,Ordline.store,Ordline.style,Ordline.season,Ordline.desc1,Ordline.scale,Ordline.group,Ordline.price,Ordline.gros_price,Ordline.disc_pcnt,Ordline.start,Ordline.complete,Ordline.qty1,Ordline.qty2,Ordline.qty3,Ordline.qty4,Ordline.qty5,Ordline.qty8,Ordline.qty7,Ordline.qty6,Ordline.totqty,Ordline.comm1,Ordline.comm2,Ordline.po,Ordline.cedit_user,Ordline.dedit_date,Ordline.cedit_time FROM Ordline WHERE Ordline.cordtype+ordline.order+STR(ordline.lineno,6) like '"& trim(session("Type")) & trim(session("OrdNo"))&"%' order by lineno", conn,2,4
	Session("RSLine").movelast()
	Session("LineNo") = cint(Session("RSLine")("lineno"))+1
	'Response.Write Session("RSLine")("season")
	'Response.End 
	Session("RSLine").MoveFirst()
	do while not Session("RSLine").eof
		if Session("ordQty")="" then
			ordqty = 0
		Else
			ordqty = Session("ordQty")
		End IF
		if Session("ordAmount")="" then
			ordAmount = 0
		Else
			ordAmount = Session("ordAmount")'formatnumber(round(Session("ordAmount"),2),2)
		End IF
		Session("ordQty") = Cdbl(ordQty) + cdbl(Session("RSLine").fields("totqty"))
		Session("ordAmount") = cdbl(ordAmount) + (cdbl(Session("RSLine").fields("totqty")) * cdbl(Session("RSLine").fields("price")))
	Session("RSLine").movenext()		
	loop
end if
'get order detail[end]
'WAL_ check if there are records to be deleted[start]
if Request.QueryString("Mode") = "D" and trim(Request.Form ("chkLines")) <> "" then
	set RSStyle=server.CreateObject("ADODB.recordset")
	RSStyle.open "select * from style",conn 

	Dim arrID
	arrID = Split(trim(Request.Form ("chkLines")),",")
	Session("RSLine").MoveFirst()
	strSql = ""
	For i = 0 to UBound(arrID)
		
		strSql = "lineNo='" &Trim(arrID(i))& "'"
		Session("RSLine").Filter = strSql
		if not Session("RSLine").eof then
			Session("ordQty") = Cdbl(Session("ordQty")) - cdbl(Session("RSLine").fields("totqty"))
			RSStyle.Filter = "style='" & Ucase(Session("RSLine")("style")) & "'"
			Session("ordAmount") = Cdbl(Session("ordAmount")) - formatnumber(round((cdbl(Session("RSLine").fields("totqty")) * cdbl(Session("RSLine").fields("price"))),2),2)
			Session("RSLine").Delete()
			
			RSStyle.Filter = ""
		end if
		Session("RSLine").Filter = ""
	Next					
	
end if
'WAL_ check if there are records to be deleted[end] 
%>

<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<Title>CRM - Remote Order</Title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</head>
<body>

<SCRIPT language=JavaScript>

function go(which) 
{
  n = which.selectedIndex;
  str = which.title

  if (n != 0)
   {
    which.form.submit();
   }
}
function Up_lines(objInput)
{
//	//Avoid empty update lines
//	//objInput.Typeline.value = "U";
	if (eval("objInput") !=null) 
		objInput.Typeline.value = "U";
	else
	   alert("Please select style first");
}



function RmvLines() 
{
	if (document.form3.chkLines.checked == false)
	{
		alert("Please select a line!")
		return false;
	}
	else
	{
		document.form3.action = 'modifyOrder.asp?Come=U&Mode=D&From=M';
		document.form3.submit();
	}
}

function FrontPage_Form2_Validator(theForm)
{

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord1.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit numbers in the quantity field!");
    theForm.txtord1.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
 
  if (chkVal != "" && prsVal <= 0)
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord1.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord2.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit numbers in the quantity field!");
    theForm.txtord2.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && prsVal <= 0)
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord2.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord3.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit numbers in the quantity field!");
    theForm.txtord3.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && prsVal <= 0)
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord3.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord4.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit numbers in the quantity field!");
    theForm.txtord4.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && prsVal <= 0)
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord4.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord5.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit numbers in the quantity field!");
    theForm.txtord5.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && prsVal <= 0)
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord5.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord6.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit numbers in the quantity field!");
    theForm.txtord6.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && prsVal <= 0)
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord6.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord7.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit numbers in the quantity field!");
    theForm.txtord7.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && prsVal <= 0)
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord7.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord8.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit numbers in the quantity field!");
    theForm.txtord8.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && prsVal <= 0)
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord8.focus();
    return (false);
  }
   var QtysSum;
  if (theForm.Typeline.value=="U")
  { 
	QtysSum = theForm.txtord1.value + theForm.txtord2.value + theForm.txtord3.value + theForm.txtord4.value + theForm.txtord5.value + theForm.txtord6.value + theForm.txtord7.value + theForm.txtord8.value ;

	if (QtysSum == 0)
	{	
		alert ("The number of items must be greater than zero!");
		return (false);
	}
  }
  if (document.FORM2.txtDisc.value > 100)
	{
		alert("Value cannot exceed 100%");
		return false;
	}
  if (theForm.Typeline.value=="R")
  {
	if (FORM2.StyleChoosed.value=="F")
	{
		alert("Please select the item you want to delete from the lower table and click “Remove Line”!");
		return false;
	}
  }
  return (true);
}

function enable(val)
{
	var val;
	if (val == 'A' || '<%=Session("Type")%>' == 'A')
	{
		document.FORM1.txtAdd1.disabled = false;
		document.FORM1.txtAdd2.disabled = false;
		document.FORM1.txtAdd3.disabled = false;
		document.FORM1.txtAdd4.disabled = false;
		document.FORM1.txtAdd5.disabled = false;
	}
	else 
	{
		document.FORM1.txtAdd1.disabled = true;
		document.FORM1.txtAdd2.disabled = true;
		document.FORM1.txtAdd3.disabled = true;
		document.FORM1.txtAdd4.disabled = true;
		document.FORM1.txtAdd5.disabled = true;

	}
}

function chgAdd(val)
{
	document.FORM1.action='modifyorder.asp?From=<%=request.querystring("From")%>&val='+val;
	document.FORM1.submit ();
}
function checkformlines()
 {
	if (document.form3.islinesempty.value=="Y" )
	{
		alert ("Sales Order form is empty; can’t save!");
		return false;
	}
	if ('<%=Request.QueryString ("From")%>' == 'Cat')
	{
		alert("Please confirm order header information");
		document.form3.action='sorderh.asp?From=Ord';
		document.form3.submit();
	}
	return true;
 }
function openwindow(filename) 
{  
	window.open(filename,'','toolbar=no,status=no,scrollbars=no,location=no,resizable=yes,menubar=no,directories=no,top=' + ((window.screen.height-500)/2) + ',left=' + ((window.screen.width-800)/2) + ',width=800,height=500')
}
function cancelOrd()
{
	if (confirm("Are you sure you want to cancel this order?"))
	{
		// WMA PNP Payment Cancel [Start]
		//document.FORM1.action = 'cancelOrd.asp';
		//document.FORM1.submit ();
    	document.FORM1.target = "_top"
		document.FORM1.action="../PnpCom/paymentform.asp?PNPCase=DepositCancel";														
		document.FORM1.submit ();
		// WMA PNP Payment Cancel [End]
	}
	else
		return false;
}
</SCRIPT>

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
	<TD class="title">Modify Order</TD>
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
	'next 
	Session("tempCustomer") = rsHdrInfo("Account")
	
'	Session("RSLine").MoveFirst()
	Dim rs, RSStymaj, RSStruct ' as ADODB.RECORDSET

	set RSStruct=server.CreateObject("ADODB.recordset")
	strSql="select ciseghead from icistru where citemrecty+cisegno='U1' "
	RSStruct.open strSql,conn 

%>

<FORM action="redirect.asp?Type=M" id=FORM1 method=post name=FORM1>
<input type="hidden" name="card-amount" value=<%=Session("Ndeposit")%>>
<div align="center">
<center>
   <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%"><b>Order Header Info.:</b></td>
    </tr>
   </table>
   <table border="1"  width=95% bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<tr>
		<td class="dark_cell"><%=session("CustField")%></td>
		<td colspan=9 class="light_cell"><%=rsHdrInfo("Account")%> - <%=rsHdrInfo("CustStname")%></td>
	</tr>
	<tr>
		<td class="dark_cell">Order #</td>
		<td class="light_cell"><%=rsHdrInfo("Order")%>&nbsp;</td>
		<td class="dark_cell">Status</td>
		<td class="light_cell">
		<%Select case Trim(rsHdrInfo("Status"))
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
		%>&nbsp;</td>
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
		<td class="dark_cell">Ship Via</td>
		<td width=110 class="light_cell"><%=strShipVia%>
		<td class="dark_cell">Start Ship</td>
		<td width=60 class="light_cell" align=center><%if rsHdrInfo("Start")<> "12:00:00 AM" then Response.Write rsHdrInfo("Start") end if %>&nbsp;</td>
		<td class="dark_cell">Expected Ship</td>
		<td width=60 class="light_cell" align=center><%if rsHdrInfo("Complete")<>"12:00:00 AM" then Response.Write rsHdrInfo("Complete") end if%>&nbsp;</td>
	</tr>
	<tr>
		<td class="dark_cell">Booked Qty.</td>
		<td class="light_cell" align=right><%=Trim(rsHdrInfo("Book"))%></td>
		<td class="dark_cell">Shipped Qty.</td>
		<td class="light_cell" align=right><%=Trim(rsHdrInfo("Ship"))%></td>
		<td class="dark_cell">Canceled Qty.</td>
		<td class="light_cell" align=right><%=Trim(rsHdrInfo("Cancel"))%></td>
		<td class="dark_cell">Open Qty.</td>
		<td class="light_cell"  align=right><%=Trim(rsHdrInfo("Open"))%></td><td class="light_cell" colspan=2>&nbsp;</td>
		
	</tr>
   </table><br>
   <table border="1" width="95%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<TR>
		<TD class="dark_cell"  width=12%>Sales Rep</TD>
		<TD class="light_cell" width=60%><p><%=Session("rep")%></p></TD>
		<TD class="dark_cell" align=right>Total Qty.</TD>
		<TD class="dark_cell" align=right>Total Amount</TD>
	</TR>
	<TR>
		<TD colspan=2 class="light_cell" align=right></TD>
		<TD class="light_cell" align=right>
			<INPUT id=text17 name=ordQty size="5" readonly value="<%=Session("ordQty")%>">
		</TD>
		<TD  class="light_cell" align=right>
		
			<%' Response.Write "Currency Align " & Session("CurrencyAlign")
			if Session("ordAmount")="" then
				Session("ordAmount") ="0"
			end if %>
			<INPUT id=text17 name=ordAmount size="6" readonly  <%
			if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				value="<%=Session("Currency") & formatnumber(round(Session("ordAmount"),2),2)%>"
			<%else%>
				value="<%=formatnumber(round(Session("ordAmount"),2),2) & Session("Currency")%>"
			<%end if %>	>
		</TD>
	</TR>
	</table>
	<table width="95%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<TR>
		<TD align=right><input type=button value="Cancel Order" onclick="cancelOrd();"></TD>
	</TR>
	</table>
	<table border="1" width="95%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<TR>
	<%
		IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
		Else
			strTemp = "<TD class=light_cell colspan=13>"
			Response.Write(strTemp)
			Session("RSStyStruct").MoveFirst
			DO While Not Session("RSStyStruct").Eof
				IF Len(Session("getstyle"))=0 Then
					strValue = ""
				Else
					strValue = Session(Trim(Session("RSStyStruct").fields("cisegsdes")))
				End IF
				strTemp ="<b> " & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</b>" & "<INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & strValue & ">"
				strTemp = strTemp & " " & Trim(Session("RSStyStruct").fields("cisegsepr")) & " "
				Response.Write(strTemp)

				Session("RSStyStruct").MoveNext
			Loop
		End IF
	%>
	    <INPUT id=button5 name=button2  type="submit" value="Get Style">
		</TD>
	</TR>
	<TR>
	<TD colSpan=1 width=5% class="dark_cell">
				
		<strong>Description</strong>
	</TD>
	<TD colSpan=9 width=80% class="light_cell">
		  <%=Session("LongDesc")%> </TD>
	<TD align=right class="dark_cell">
		<strong>Price</strong>
	</TD>
	<TD  colSpan=2 align=right  class="light_cell">
		<%' Response.Write "Currency Align " & Session("CurrencyAlign")
		if Session("price")="" then
			Session("price") ="0"
		end if %>
		   <%
		if Trim(Session("CurrencyAlign")) = "LEFT" then%>
			<%=Session("Currency") & FormatNumber(Session("price"))%>
		<%else%>
			<%=FormatNumber(Session("price")) & Session("Currency")%>
		<%end if %>	
	</TD>
	</tr>
</FORM>

<%
if len(trim(Session("getstyle"))) >0 then '<!--Moghazy Jan 23 2004 END-->
Set RSOTS = server.CreateObject("ADODB.recordset")
strSql="select * from style where style='" & Session("getstyle") & "'"
RSOTS.open strSql,conn 
Dim intOTS ' as integer
If RSOTS.EOF then
	'Response.Write("<Font Size=3>EOF Here" & Session("getstyle")&"</font>")
End If
IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
	strComm =  rsOts("Commission")
	Set RSScale = server.CreateObject("ADODB.recordset")
	strSql="select * from scale where type='S' And scale='" & RSOTS("scale") & "'"
	RSScale.open strSql,conn 
	
	'wma get line discount [start]
	'IF (Not RSOTS.EOF OR NOT RSOTS.BOF) and (Not Session("RSLine").EOF OR NOT Session("RSLine").BOF)  Then
	'	Session("RSLine").movefirst
	'	do while not Session("RSLine").eof
	'		if RSOTS("style") = Session("RSLine")("style") then
	'			session("Curr_disc")= Session("RSLine")("disc_pcnt")
	'			Session("RSLine").movefirst
	'			exit do
	'		end if				
	'		Session("RSLine").movenext
	'		session("Curr_disc")=0
	'	loop
	'else
		'session("Curr_disc")=0
	'end if		
	'wma get line discount [end]

End IF
%>

<%intScaleCnt = cdbl("0")%>
<FORM action="updateline.asp?From=M" id=FORM2 method=post name=FORM2 onsubmit="return FrontPage_Form2_Validator(this)">
  <tr>
    <TD width="10%" class="dark_cell" ></TD>
    <TD width="10%" class="dark_cell">
      <P align=right>
      <%IF RSOTS.recordcount >0 Then 'Not RSOTS.EOF OR NOT RSOTS.BOF
					Response.Write(RSScale("sz1"))
					intScaleCnt = cdbl(RSScale("cnt"))
				Else
					Response.Write("Size1")
				End IF
			%>
	</P></TD>
    <TD width="10%" class="dark_cell">
      <P align=right>
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz2"))
				Else
					Response.Write("Size2")
				End IF
			%>
      </P></TD>
    <TD width="10%" class="dark_cell">
      <P align=right><strong>
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz3"))
				Else
					Response.Write("Size3")
				End IF
			%>
      </strong></P></TD>
    <TD width="10%" class="dark_cell">
      <P align=right >
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz4"))
				Else
					Response.Write("Size4")
				End IF
			%>
      </P></TD>
    <TD width="10%" class="dark_cell">
      <P align=right>
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz5"))
				Else
					Response.Write("Size5")
				End IF
			%>
      </P></TD>
    <TD width="10%" class="dark_cell">
      <P align=right>
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz6"))
				Else
					Response.Write("Size6")
				End IF
			%>
      </P></TD>
    <TD width="10%" class="dark_cell">
      <P align=right>
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz7"))
				Else
					Response.Write("Size7")
				End IF
			%>
      </P></TD>
    <TD width="10%" class="dark_cell">
      <P align=right>
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz8"))
				Else
					Response.Write("Size8")
				End IF
    	%>
      </P></TD>
	<%if request("Display") <> "" then
		strTemp = "Disabled"
	  else
	   strTemp = ""
	  end if%>
   	<TD width="5%" class="dark_cell" align=right>Disc%</TD>
    <TD width="5%" class="dark_cell" align=right>Group</TD>
    <TD width="5%" class="dark_cell" align=right>Total</TD>
  </tr>
  <tr>
    <TD width="10%" class="dark_cell">
      <p align="left">Order</p>
    </TD>
    <TD width="10%" class="light_cell">
      <P align=right>&nbsp
      <%
      IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz1")) <> "" and not isnull(RSScale("sz1"))then%>
			<INPUT  name=txtord1 size="5" maxlength=5 value="<%=Session("text1")%>" 
			<%IF intScaleCnt = 0 or trim(Session("text1")) = "" Then Response.Write(strTemp) End IF%> >
	  <%else%>
			<input type=hidden name=txtord1>
	  <%end if
	  end if%></P></TD>
    <TD width="10%" class="light_cell" >
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz2")) <> "" and not isnull(RSScale("sz2")) then%>
      <INPUT  name=txtord2 size="5" maxlength=5  value="<%=Session("text2")%>" <%IF intScaleCnt<2 or trim(Session("text2")) = ""  Then Response.Write(strTemp) End IF%>>
      <%else%>
			<input type=hidden name=txtord2>
	  <%end if
	  end if%></P></TD>
    <TD width="10%" class="light_cell" >
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz3")) <> "" and not isnull(RSScale("sz3")) then%>
      <INPUT  name=txtord3 size="5" maxlength=5 value="<%=Session("text3")%>" <%IF intScaleCnt<3 or trim(Session("text3")) = ""Then Response.Write(strTemp) End IF%>>
      <%else%>
			<input type=hidden name=txtord3>
	  <%end if
	  end if%></P></TD>
    <TD width=10% class="light_cell">
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz4")) <> "" and not isnull(RSScale("sz4")) then%>
      <INPUT  name=txtord4  value="<%=Session("text4")%>" <%IF intScaleCnt<4 or trim(Session("text4")) = ""Then Response.Write(strTemp) End IF%>
      size="5" maxlength=5 >
      <%else%>
			<input type=hidden name=txtord4>
	  <%end if
	  end if%></P></TD>
    <TD width=10% class="light_cell">
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz5")) <> "" and not isnull(RSScale("sz5")) then%>
      <INPUT name=txtord5  value="<%=Session("text5")%>" <%IF intScaleCnt<5 or trim(Session("text5")) = ""Then Response.Write(strTemp) End IF%>
      size="5" maxlength=5>
     <%else%>
			<input type=hidden name=txtord5>
	  <%end if
	  end if%></P></TD>
    <TD width="10%" class="light_cell">
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz6")) <> "" and not isnull(RSScale("sz6")) then%>
      <INPUT name=txtord6  value="<%=Session("text6")%>" <%IF intScaleCnt<6 or trim(Session("text6")) = ""Then Response.Write(strTemp) End IF%>
      size="5" maxlength=5>
      <%else%>
			<input type=hidden name=txtord6>
	  <%end if
	  end if%></P></TD>
    <TD width=10% class="light_cell">
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz7")) <> "" and not isnull(RSScale("sz7")) then%>
		<INPUT  name=txtord7  value="<%=Session("text7")%>" <%IF intScaleCnt<7 or trim(Session("text7")) = ""Then Response.Write(strTemp) End IF%>
		size="5" maxlength=5></P>
	  <%else%>
			<input type=hidden name=txtord7>
	  <%end if
	  end if%></TD>
    <TD width="10%" class="light_cell">
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz8")) <> "" and not isnull(RSScale("sz8")) then%>
		<INPUT  name=txtord8  value="<%=Session("text8")%>" <%IF intScaleCnt<8 or trim(Session("text8")) = ""Then Response.Write(strTemp) End IF%>
		size="5" maxlength=5>
	  <%else%>
			<input type=hidden name=txtord8>
	  <%end if
	  end if%></P></TD>
    
    <TD class="light_cell" align=right>
		<input type=text name="txtDisc" value="<%=formatnumber(session("Curr_disc"))%>" size=3>
		<!--input type=text name="txtDisc" <%'if Session("foundDisc")=true then%>value="<%'=formatnumber(Session("RSLine").fields("Disc_Pcnt"))%>"<%'else%>value="0.00"<%'end if%>size=3-->				
	</TD>
	
    <!--WMA Delete Commetion Field End-->
    <TD class="light_cell" align=right><input type=text name="txtGrp" maxlength=1 value="<%=session("Grp")%>"size=3></TD>
    <TD  class="dark_cell" ></TD>
  </tr>

  
 <%
    'WAL_calc of ots display[start]
    Dim rsOtsChk
    set rsOtsChk = server.CreateObject ("ADODB.Recordset")
    
	strSQl = "Select (style.stk1 - sum(ordline.qty1)) as sum1, (style.stk2 - sum(ordline.qty2)) as sum2, "& _
			 "(style.stk3 - sum(ordline.qty3)) as sum3, (style.stk4 - sum(ordline.qty4)) as sum4, "& _ 
			 "(style.stk5 - sum(ordline.qty5)) as sum5, (style.stk6 - sum(ordline.qty6)) as sum6, "& _
			 "(style.stk7 - sum(ordline.qty7)) as sum7, (style.stk8 - sum(ordline.qty8)) as sum8, "& _
			 "(style.stk1 + style.wip1 - sum(ordline.qty1)) as sum11, (style.stk2 + style.wip2 - sum(ordline.qty2)) as sum22, "& _
			 "(style.stk3 + style.wip3 - sum(ordline.qty3)) as sum33, (style.stk4 + style.wip4 - sum(ordline.qty4)) as sum44, "& _ 
			 "(style.stk5 + style.wip5 - sum(ordline.qty5)) as sum55, (style.stk6 + style.wip6 - sum(ordline.qty6)) as sum66, "& _
			 "(style.stk7 + style.wip7 - sum(ordline.qty7)) as sum77, (style.stk8 + style.wip8 - sum(ordline.qty8)) as sum88, "& _
			 "style.make from ordline, ordHdr, style where ordline.style+DTOS(ordline.complete)+ordline.cordtype+ordline.order+ordline.store+STR(ordline.lineno,6) like '" &Session("getstyle")& "%' "& _
			 "and ordHdr.status <> 'C' and ordHdr.Status <> 'X' and ordline.order = ordHdr.order and ordline.style = style.style"
	rsOtsChk.Open strSQL, conn

	if rsOtsChk.EOF then'no orders for this style then get value in stock
		rsOtsChk.Close ()
		strSQl = "Select style.stk1 as sum1, style.stk2 as sum2, "& _
				 "style.stk3 as sum3, style.stk4 as sum4, "& _ 
				 "style.stk5 as sum5, style.stk6 as sum6, "& _
				 "style.stk7 as sum7, style.stk8 as sum8, "& _
				 "sum(style.stk1 + style.wip1) as sum11, sum(style.stk2 + style.wip2) as sum22, "& _
				 "sum(style.stk3 + style.wip3) as sum33, sum(style.stk4 + style.wip4) as sum44, "& _ 
				 "sum(style.stk5 + style.wip5) as sum55, sum(style.stk6 + style.wip6) as sum66, "& _
				 "sum(style.stk7 + style.wip7) as sum77, sum(style.stk8 + style.wip8) as sum88, "& _
				 "style.make from style where style = '" &Session("getstyle")& "' "
		
		rsOtsChk.Open strSQL, conn
	end if

	if not rsOtsChk.eof and trim(Session("getstyle")) <> "" then
		Session("Make") = rsOtsChk("make")
	%>	

		<%If Session("ShowOTS")= "F" then'wma%>	
	
			<tr>
			<TD class="dark_cell">
			Availability
			</TD>
    			
			<%if cdbl(rsOtsChk("sum1")) > 0 then%>
				<TD class="light_cell" style="background-color: green">
				<input type=hidden name=txtType1 value="green">
			<%elseif cdbl(rsOtsChk("sum11")) > 0 then%>
				<TD class="light_cell" style="background-color: orange">
				<input type=hidden name=txtType1 value="orange">
			<%else
			 	IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
			 		if trim(RSScale("sz1")) <> "" and not isnull(RSScale("sz1")) then%>
			 			<TD class="light_cell" align=center style="background-color: red">
			 			<!--font color=white>Waiting List</font-->&nbsp;
			 		<%else%>
			 			<TD class="light_cell">
			 		<%end if%>
			 	<%else%>
			 				<TD class="light_cell">
			 	<%end if%>
			<%end if%>
		  
		</TD>
		  
		   <%if cdbl(rsOtsChk("sum2")) > 0 then%>
				<TD class="light_cell" style="background-color: green">
				<input type=hidden name=txtType2 value="green">
			   <%elseif cdbl(rsOtsChk("sum22")) > 0 then%>
					<TD class="light_cell" style="background-color: orange">
					<input type=hidden name=txtType2 value="orange">
			  <%else
			 	IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
			 		if trim(RSScale("sz2")) <> "" and not isnull(RSScale("sz2")) then%>
			 			<TD class="light_cell" align=center style="background-color: red">
			 			<!--font color=white>Waiting List</font-->&nbsp;
			 		<%else%>
			 			<TD class="light_cell">
			 		<%end if%>
			 	<%else%>
			 				<TD class="light_cell">
			 	<%end if%>
			<%end if%>
			   
			   
		   
		</TD>
				
				<%if cdbl(rsOtsChk("sum3")) > 0 then%>
					<TD class="light_cell" style="background-color: green">
					<input type=hidden name=txtType3 value="green">
				<%elseif cdbl(rsOtsChk("sum33")) > 0 then%>
					<TD class="light_cell" style="background-color: orange">
					<input type=hidden name=txtType3 value="orange">
				<%else
			 	IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
			 		if trim(RSScale("sz3")) <> "" and not isnull(RSScale("sz3")) then%>
			 			<TD class="light_cell" align=center style="background-color: red">
			 			<!--font color=white>Waiting List</font-->&nbsp;
			 		<%else%>
			 			<TD class="light_cell">
			 		<%end if%>
			 	<%else%>
			 				<TD class="light_cell">
			 	<%end if%>
			<%end if%>
				
		</TD>
				
				<%if cdbl(rsOtsChk("sum4")) > 0 then%>
					<TD class="light_cell" style="background-color: green">
					<input type=hidden name=txtType4 value="green">
				<%elseif cdbl(rsOtsChk("sum44")) > 0 then%>
					<TD class="light_cell" style="background-color: orange">
					<input type=hidden name=txtType4 value="orange">
				<%else
			 	IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
			 		if trim(RSScale("sz4")) <> "" and not isnull(RSScale("sz4")) then%>
			 			<TD class="light_cell" align=center style="background-color: red">
			 			<!--font color=white>Waiting List</font-->&nbsp;
			 		<%else%>
			 			<TD class="light_cell">
			 		<%end if%>
			 	<%else%>
			 				<TD class="light_cell">
			 	<%end if%>
			<%end if%>
				
		</TD>
				
				<%if cdbl(rsOtsChk("sum5")) > 0 then%>
					<TD class="light_cell" style="background-color: green">
					<input type=hidden name=txtType5 value="green">
				<%elseif cdbl(rsOtsChk("sum55")) > 0 then%>
					<TD class="light_cell" style="background-color: orange">
					<input type=hidden name=txtType5 value="orange">
				<%else
			 	IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
			 		if trim(RSScale("sz5")) <> "" and not isnull(RSScale("sz5")) then%>
			 			<TD class="light_cell" align=center style="background-color: red">
			 			<!--font color=white>Waiting List</font-->&nbsp;
			 		<%else%>
			 			<TD class="light_cell">
			 		<%end if%>
			 	<%else%>
			 				<TD class="light_cell">
			 	<%end if%>
			<%end if%>
		</TD>
				
				<%if cdbl(rsOtsChk("sum6")) > 0 then%>
					<TD class="light_cell" style="background-color: green">
					<input type=hidden name=txtType6 value="green">
				<%elseif cdbl(rsOtsChk("sum66")) > 0 then%>
					<TD class="light_cell" style="background-color: orange">
					<input type=hidden name=txtType6 value="orange">
				<%else
			 	IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
			 		if trim(RSScale("sz6")) <> "" and not isnull(RSScale("sz6")) then%>
			 			<TD class="light_cell" align=center style="background-color: red">
			 			<!--font color=white>Waiting List</font-->&nbsp;
			 		<%else%>
			 			<TD class="light_cell">
			 		<%end if%>
			 	<%else%>
			 				<TD class="light_cell">
			 	<%end if%>
			<%end if%>
				
		</TD>
				
				<%if cdbl(rsOtsChk("sum7")) > 0 then%>
					<TD class="light_cell" style="background-color: green">
					<input type=hidden name=txtType7 value="green">
				<%elseif cdbl(rsOtsChk("sum77")) > 0 then%>
					<TD class="light_cell" style="background-color: orange">
					<input type=hidden name=txtType7 value="orange">
				<%else
			 	IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
			 		if trim(RSScale("sz7")) <> "" and not isnull(RSScale("sz7")) then%>
			 			<TD class="light_cell" align=center style="background-color: red">
			 			<!--font color=white>Waiting List</font-->&nbsp;
			 		<%else%>
			 			<TD class="light_cell">
			 		<%end if%>
			 	<%else%>
			 				<TD class="light_cell">
			 	<%end if%>
			<%end if%>
				
		</TD>
				
				<%if cdbl(rsOtsChk("sum8")) > 0 then%>
					<TD class="light_cell" style="background-color: green">
					<input type=hidden name=txtType8 value="green">
				<%elseif cdbl(rsOtsChk("sum88")) > 0 then%>
					<TD class="light_cell" style="background-color: orange">
					<input type=hidden name=txtType8 value="orange">
				<%else
			 	IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
			 		if trim(RSScale("sz8")) <> "" and not isnull(RSScale("sz8")) then%>
			 			<TD class="light_cell" align=center style="background-color: red">
			 			<!--font color=white>Waiting List</font-->&nbsp;
			 		<%else%>
			 			<TD class="light_cell">
			 		<%end if%>
			 	<%else%>
			 				<TD class="light_cell">
			 	<%end if%>
			<%end if%>
		
		</TD>
		 <TD  class="dark_cell" >
		  <P></P></TD><TD  class="dark_cell" >
		  <P></P></TD>
		  <TD  class="dark_cell" >
		  <P></P></TD>
		<%End if'wma%>	  
	<%end if%>
  </tr>



	
<%If Session("ShowOTS")= "T" then'wma%>	
  <tr>
    <TD class="dark_cell">
      OTS
    </TD>
    
    <TD class="light_cell">
	<%if trim(RSScale("sz1")) <> "" and not isnull(RSScale("sz1"))then%>            
      <P align=right><INPUT id=text3 name=txtots1 
      <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else		
				intOTS = (cdbl(RSOTS("stk1")) + cdbl(RSOTS("wip1"))) - cdbl(RSOTS("ord1"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>  
      size="5" disabled>
      </P>
	<%end if%>              
    </TD>
      
    <TD class="light_cell">
	<%if trim(RSScale("sz2")) <> "" and not isnull(RSScale("sz2"))then%>            
      <P align=right><INPUT id=text3 name=txtots2 
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
	  Else	
			intOTS = (cdbl(RSOTS("stk2")) + cdbl(RSOTS("wip2"))) - cdbl(RSOTS("ord2"))
			strTemp = "value =" & intOTS
			Response.Write(strTemp)
      END IF
      %>
      size="5" disabled></P>
	<%end if%>              
    </TD>
      
    <TD class="light_cell">
	<%if trim(RSScale("sz3")) <> "" and not isnull(RSScale("sz3"))then%>            
      <P align=right><INPUT id=text3 name=txtots3 
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else		
				intOTS = (cdbl(RSOTS("stk3")) + cdbl(RSOTS("wip3"))) - cdbl(RSOTS("ord3"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>
      size="5" disabled></P>
	<%end if%>              
    </TD>
      
    <TD class="light_cell">
	<%if trim(RSScale("sz4")) <> "" and not isnull(RSScale("sz4"))then%>            
      <P align=right><INPUT id=text3 name=txtots4 
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else		
				intOTS = (cdbl(RSOTS("stk4")) + cdbl(RSOTS("wip4"))) - cdbl(RSOTS("ord4"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>
       size="5" disabled></P>
	<%end if%>               
    </TD>
    
    <TD class="light_cell">
	<%if trim(RSScale("sz5")) <> "" and not isnull(RSScale("sz5"))then%>            
      <P align=right><INPUT id=text3 name=txtots5 
            <%		
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else	
				intOTS = (cdbl(RSOTS("stk5")) + cdbl(RSOTS("wip5"))) - cdbl(RSOTS("ord5"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>
     size="5" disabled></P>
	<%end if%>             
     </TD>
     
    <TD class="light_cell">
	<%if trim(RSScale("sz6")) <> "" and not isnull(RSScale("sz6"))then%>        
      <P align=right><INPUT id=text3 name=txtots6 
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else	
				intOTS = (cdbl(RSOTS("stk6")) + cdbl(RSOTS("wip6"))) - cdbl(RSOTS("ord6"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>
      size="5" disabled></P>
	<%end if%>          
     </TD>
     
    <TD class="light_cell">
	<%if trim(RSScale("sz7")) <> "" and not isnull(RSScale("sz7"))then%>        
      <P align=right><INPUT id=text3 name=txtots7 
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else		
				intOTS = (cdbl(RSOTS("stk7")) + cdbl(RSOTS("wip7"))) - cdbl(RSOTS("ord7"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>

      size="5" disabled></P>
	<%end if%>          
     </TD>
      
	        
    <TD class="light_cell">
	<%if trim(RSScale("sz8")) <> "" and not isnull(RSScale("sz8"))then%>    
      <P align=right><INPUT id=text3 name=txtots8 
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else				
				intOTS = (cdbl(RSOTS("stk8")) + cdbl(RSOTS("wip8"))) - cdbl(RSOTS("ord8"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>
      size="5" disabled></P>
	<%end if%>      
	</TD>  
      
      
      
     <TD  class="dark_cell" >
      <P></P></TD><TD  class="dark_cell" >
      <P></P></TD>

    <TD class="light_cell">
      <p align=right><INPUT id=text12 name=txtots 
            <%
		
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else
				
				intOTS = (cdbl(RSOTS("Totstk")) + cdbl(RSOTS("Totwip"))) - cdbl(RSOTS("Totord"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>

      size="5" disabled
     ></p></TD>
  </tr>
<%End If%>



<%end if%> <!--Moghazy Jan 23 2004 END-->
</table>

</center>
</div>

<TABLE border=0 cellPadding=0 cellSpacing=0  width="95%" align=center>
  <TR>
    <TD width="20%" style="WIDTH: 20%">
		<!--WMA Lenged Table-->
		<Table width="95%" align=center border="0" cellpadding=0 cellspacing=0>
			<TR>
				<TD width="100%">
				<%If Session("ShowOTS")= "F" then'wma%>				
					<table border=1 bordercolor="#000000" align=left Width=270  cellpadding=0 cellspacing=0 style="border-collapse: collapse"> 
						<tr>
							<TD width="90"  class="light_cell" style="background-color: green" align=center><font color=white>Available</font></TD>
							<TD width="90"  class="light_cell" style="background-color: orange" align=center><font color=white>In Progress</font></TD>
							<TD width="90"  class="light_cell" style="background-color: red" align=center><font color=white>Waiting List</font></TD>
						</tr>	
					</table>		
				<%End If'wma%>					
				</TD>
			</TR>
		</Table>
	</TD>
    <TD width="25%" colSpan=2 style="WIDTH: 25%"></TD>
    <TD width="15%"></TD>
    <TD width="20%" colSpan=2>
      <P align=right>
		<%if Trim(Session("getstyle")) = "" then
			styleischoosed = "F"
		  else
			styleischoosed = "T"
		end if%>
		<input type="hidden" id=StyleChoosed name=StyleChoosed value="<%=styleischoosed%>">
				
		<INPUT id=button3 name=button3 style="HEIGHT: 24px; WIDTH: 96px" 
		type="submit" value="Update Line" onclick="Up_lines(this.form)">&nbsp;
				
	   </P>
	</TD>
  
  </TR>
</TABLE>
<TABLE border=0 cellPadding=1 cellSpacing=1 width="95%" align=center>
  
  <TR>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
   </TR>
</TABLE>

    <input type="hidden" name="Typeline" value="">
    <input type="hidden" name="remline" value="">
    <input type="hidden" name="SlctColor" value="<%=ucase(Session("getstyle"))%>">
</form>
    
<form id=form3 name=form3 action="modCharge.asp?ordNo=<%=rsHdrInfo("Order")%>&save=T" method="POST" onsubmit="return checkformlines()">
 <%if Session("RSLine").BOF and Session("RSLine").EOF then
	  emptylinesflag="Y"
	else
	  emptylinesflag="N"
	end if%>
<INPUT type=hidden value="<%=emptylinesflag%>" name=islinesempty>
<TABLE border=0 cellPadding=1 cellSpacing=1  width="95%" align=center>
  <TR>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD></TD>
    <TD style="WIDTH: 40%" width="40%" align=right>
		<INPUT name=btnCharge type="button" value="View Order Charges" onclick="javascript:openwindow('modCharge.asp?ordNo=<%=rsHdrInfo("Order")%>');">

		<INPUT id=button4 name=button3 style="HEIGHT: 24px; WIDTH: 96px" type="submit" value="Save Order" >
	</TD>
   </TR>
   
</TABLE>
<div align="center">
<center>
<table border="1" width="95%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
        
<TR>
	<TD class="dark_cell" align=center width=1%>></TD>
	<TD width="12%" class="dark_cell"><%=RSStruct("ciseghead")%>&nbsp;</TD>
	<TD width="25%" class="dark_cell">Description</TD>	
	<%
	Session("foundDisc") = false	         
    IF Not Session("RSLine").EOF OR Not Session("RSLine").BOF   Then    
	   Session("RSLine").movefirst
    	Do While Not Session("RSLine").EOF    	   	
		'Response.Write "<Font size=3>" & Session("RSLine").fields("Disc_Pcnt") & "<br>"
		'Response.Write "<Font size=3>" & "<br>" & Session("foundDisc")
			if CDbl(Session("RSLine").fields("Disc_Pcnt"))>0 then
					Session("foundDisc") = true
					exit do
			end if
			Session("RSLine").movenext
		loop
	end if 
	%>	
	<%if Session("foundDisc") = true then%>
		<TD width="5%" class="dark_cell" align=right>Group</TD>
		<TD width="8%" class="dark_cell" align=right>Gross Price</TD>
		<TD width="5%" class="dark_cell" align=right>Disc%</TD>
	<%else%>
		<TD width="18%" class="dark_cell" align=right colspan=3>Group</TD>
	<%end if%>
	<TD width="8%" class="dark_cell" align=right>Price</TD>
	<%if trim(Session("SizePerLine")) = "T" then%>
		<TD width="8%" class="dark_cell" align=right>Qty.</TD>
		<TD width="8%" class="dark_cell" align=right>Size</TD>
	<%else%>
		<TD width="8%" colspan=2 class="dark_cell" align=right>Qty.</TD>
	<%end if%>
	<TD width=10% class="dark_cell" align=center>Expected Ship</TD>

	<TD width="10%" class="dark_cell" align=right>Amount</TD>       
</TR>
<%
IF Not Session("RSLine").EOF OR Not Session("RSLine").BOF   Then
   Session("RSLine").movefirst      
	strTemp = "<input type=hidden name=save value=""T"">"
	Response.Write(strTemp)
	Do While Not Session("RSLine").EOF
		'check wich size # to display
		Set rsSty = server.CreateObject("ADODB.recordset")
		strSql="select * from style where style='" & Session("RSLine").fields("style") & "'"
		rsSty.open strSql,conn 
		IF Not rsSty.EOF OR NOT rsSty.BOF Then
			Set rsScl = server.CreateObject("ADODB.recordset")
			strSql="select * from scale where type='S' And scale='" & trim(rsSty("scale")) & "'"
			rsScl.open strSql,conn 
		End IF
		for i=1 to 8
			strQty = "qty"&i
			if cdbl(Session("RSLine").fields(strQty)) <> 0 then
				IF Not rsSty.EOF OR NOT rsSty.BOF Then
					strSize = rsScl("sz"&i)
				Else
					strSize = "Size"&i
				End IF 
				strSizeNo = "qty"&i
			end if
		next
		
		strTemp = "<TR>"
		Response.Write(strTemp)

		strTemp = "<TD Class=light_cell width=0><input type=checkbox name=chkLines  value='" & Session("RSLine").fields("lineNo") & "'></TD><TD class=""light_cell""><A HREF=""Dispordline.asp?Type=M&From="&Request.QueryString ("From")&"&LineNo=" &Session("RSLine").fields("lineno")& "&Size=" &strSizeNo& "&Style=" & Session("RSLine").fields("style") &  """>"& Session("RSLine").fields("style") & "</A></TD>"'" & "<A HREF=""Dispordline.asp?Style=" & Session("RSLine").fields("style") &  """>" & Session("RSLine").fields("style") & "</A></TD>"
		Response.Write(strTemp)

		strTemp = "<TD class=""light_cell"">" & Session("RSLine")("desc1") & "</TD>"
		Response.Write(strTemp)
	
		if Session("foundDisc")=true then 'just check for colspan 
			strTemp = "<TD  class=""light_cell"" align=right>" & Session("RSLine")("group") & "</TD>"
			Response.Write(strTemp)
		else
			strTemp = "<TD  colspan=3 class=""light_cell"" align=right>" & Session("RSLine")("group") & "</TD>"
			Response.Write(strTemp)
		end if
		
		if Session("foundDisc")=true then
			if Trim(Session("CurrencyAlign"))="LEFT" then
					strTemp = "<TD  class=""light_cell"" align=right>" & Session("Currency") & FormatNumber(Session("RSLine").fields("gros_price")) & "</TD>"
			else
					strTemp = "<TD  class=""light_cell"" align=right>" & FormatNumber(Session("RSLine").fields("gros_price")) & Session("Currency") & "</TD>"							
			end if 	
			Response.Write(strTemp)
		end if			
		
		if Session("foundDisc")=true then
			strTemp = "<TD class=""light_cell"" align=right>" & FormatNumber(Session("RSLine").fields("Disc_Pcnt")) & "%</TD>"
			Response.Write(strTemp)
		end if
		
		if Trim(Session("CurrencyAlign"))="LEFT" then
			strTemp = "<TD  class=""light_cell"" align=right>" & Session("Currency") & FormatNumber(Session("RSLine").fields("price")) & "</TD>"
		else	
			strTemp = "<TD  class=""light_cell"" align=right>" & FormatNumber(Session("RSLine").fields("price")) & Session("Currency") & "</TD>"							
		end if 	
		Response.Write(strTemp)
		
		'WAL_05/18/04 check if to display the size column or not according to the setup var trim(Session("SizePerLine"))[start]
		if trim(Session("SizePerLine")) = "T" then
		
			strTemp = "<TD class=""light_cell"" align=right>" & Session("RSLine").fields("totqty") & "</TD>"
			Response.Write(strTemp)
		
			strTemp = "<TD  class=""light_cell"" align=right>"& strSize &"</TD>"
			Response.Write(strTemp)
		else
			strTemp = "<TD class=""light_cell"" colspan=2 align=right>" & Session("RSLine").fields("totqty") & "</TD>"
			Response.Write(strTemp)
		end if
		'WAL_05/18/04 check if to display the size column or not according to the setup var trim(Session("SizePerLine"))[end]
	
		
		if cstr(Session("RSLine").fields("complete"))<> "" and cstr(Session("RSLine").fields("complete"))<> "12:00:00 AM" then 
			strTemp = "<TD class=""light_cell"" align=center>" & cstr(Session("RSLine").fields("complete")) & "</TD>"
		else
			strTemp = "<TD class=""light_cell"" align=center>" & "N/A" & "</TD>"
		end if
		Response.Write(strTemp)


		intAmount = cdbl(Session("RSLine").fields("totqty")) * cdbl(Session("RSLine").fields("price"))'
		if Trim(Session("CurrencyAlign"))="LEFT" then
			strTemp = "<TD  class=""light_cell"" align=right>" & Session("Currency") & formatnumber(round(intAmount,2),2)   & "</font></TD>"
		else
			strTemp = "<TD  class=""light_cell"" align=right>" & formatnumber(round(intAmount,2),2) & Session("Currency") & "</font></TD>"
		end if 
		Response.Write(strTemp)
							
		Session("RSLine").movenext

	Loop
	Session("RSLine").MoveFirst()%>
	</table>
	</center>
						
	<table cellspacing="0" width=95% style="border-collapse: collapse" bordercolor="#111111" cellpadding="0">
	<tr>
		<br><td align=left><input type=button value="Remove checked Line(s)" onclick="return RmvLines();"></td>
	</tr>
	</table>
	</div>
<%
End IF
%>
</Form>
</TABLE>
</center>
</div>
<P>&nbsp;</P>
<%End IF%>


<%if Request.QueryString("FinalStatus")= "badcard" OR Request.QueryString("FinalStatus")= "problem" then%>
	<script language="javascript">
		alert('The Payment Procedures were not completed successfully, Because of <%="(( "& Request.QueryString("MErrMsg") &" ))" %> , please try again');
	</script>	
<%end if%>
		
</BODY>
</HTML>