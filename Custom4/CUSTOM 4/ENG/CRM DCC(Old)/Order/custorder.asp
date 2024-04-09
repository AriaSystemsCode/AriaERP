<%@ Language=VBScript %>
<%
'Response.CacheControl  = "no-cache"

'Response.AddHeader  "Pragma", "no-cache"

Response.Expires = -1
Response.Buffer = true
%>
<%
'Response.Write "<font size=5>" & Session("Season") & "<hr>"
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'	Response.redirect "../default.asp"%>
<script language="javascript">
parent.location.href ="../login.asp"
</script>
<%End if
'Response.Write "<font size=2 color=red>"&Session("StoreID")
Set connt = server.CreateObject("ADODB.connection")
connt.Open Application("DataConnectionString")

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
	strSalesRep = Ucase(Session("RSCust").fields("account"))
END IF
	
IF Len(Trim(Session("rep")))>0 Then
 
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
	strSalesRep = Ucase(Trim(Session("rep")))
End IF

if Session("Season")="All" or Session("Season")="*" then
	'Session("Season") = "NONE" 'wma multiple seasons
	strSeason = "NONE" 
end if
'if Request.QueryString ("val") = "S" then
	'Session("RSCust").fields("shipvia") = Request.Form ("selShip")
'end if


''WMA temp Sol.
''Session("RSLine").fields("price") =  1000
''WMA temperary sol.
''Response.Write  Session("RSLine").fields("totqty")
''Response.End 
'if CDbl(Session("RSLine").fields("price")) = 0  then
'	strSQL = "select * from style where style ='"& Session("RSLine").fields("style") &"' "
'	set RSTempStyle=server.CreateObject("ADODB.recordset")
'	set connObj=server.CreateObject("ADODB.connection")
'	connObj.Open Application("DataConnectionString")
'	RSTempStyle.open strSql,connObj
'	if not RSTempStyle.EOF then
'		Session("RSLine").fields("price") = RSTempStyle("pricec")
'		Session("RSLine").fields("gros_price") = RSTempStyle("pricec") 					 
'		Session("ordAmount") = CDbl(Session("ordAmount")) + (CDbl(RSTempStyle("pricec")) * CDbl(Session("RSLine").fields("totqty")))
'	end if
'	RSTempStyle.Close
'	set RSTempStyle = nothing
'	connObj.Close
'	set connObj = nothing
'end if

'Response.Write  Session("ordAmount")
'Response.End 






'WAL_ check if there are records to be deleted[start]
if Request.QueryString("Mode") = "D" and trim(Request.Form ("chkLines")) <> "" then
	set RSStyle=server.CreateObject("ADODB.recordset")
	RSStyle.open "select * from style",connt 

	Dim arrID
	arrID = Split(trim(Request.Form ("chkLines")),",")
	Session("RSLine").MoveFirst()
	strSql = ""
	For i = 0 to UBound(arrID)
		
		strSql = "lineNo='" &Trim(arrID(i))& "'"
		Session("RSLine").Filter = strSql
		if not Session("RSLine").eof then
			RSStyle.Filter = "style='" & Ucase(Session("RSLine")("style")) & "'"
			Session("ordQty") = Cdbl(Session("ordQty")) - cdbl(Session("RSLine").fields("totqty"))
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
<Title>CRM - Order Entry</Title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</head>
<body>

<%
Dim strSql ' as string

If Len(Trim(Session("OrderFlag"))) = 0 Then
	Set Session("RSLine") = server.CreateObject("ADODB.recordset")
	Set RSLine1 = server.CreateObject("ADODB.recordset")
	
	strSql = "select * from ordline where .f."
	Session("RSLine").open  strSql, connt, 2, 4
End IF




%>

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
	//alert(document.form3.chkLines.checked)
	if (document.form3.chkLines.checked == false)
	{
		alert("Please select a line!")
		return false;
	}
	else
	{
		document.form3.action = 'custorder.asp?Mode=D&From=<%=request.querystring("From")%>';
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
function chgShipVia()
{
	
}
//function Up_lines(objInput)
//{
	
	//if (objInput.txtord1.value <= 0 && objInput.txtord2.value <= 0 && objInput.txtord3.value <= 0 && objInput.txtord4.value <= 0 && objInput.txtord5.value <= 0 && objInput.txtord6.value <= 0 && objInput.txtord7.value <= 0 && objInput.txtord8.value <= 0 )
		//{
			//alert("The number of items must be greater than zero!")
		//	return false;
	//	}
	//else
		//{
		//	objInput.Typeline.value = "U";
	//	}
	//return true;
//}

//function Re_lines(objInput) 
//{
	//objInput.Typeline.value = "R";
  //if (objInput.Typeline.value=="R")
  //{
	//if (document.FORM2.StyleChoosed.value=="F")
//	{
		//alert("Please select the item you want to delete from the lower table and click “Remove Line”!");
		//return false;
	//}
  //}
  //return true;
//}
function chgAdd(val)
{
	document.FORM1.action='custorder.asp?From=<%=request.querystring("From")%>&val='+val;
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
	<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>
 <%End IF%>

<Table width=95% align=center height="50" border="1" >
<TR>
<TD class="title">Order Entry</TD>
</TR>
</Table>
<%IF compWork = "Y" Then%>


<%
Dim rs, RSColor, RSStymaj, RSStruct ' as ADODB.RECORDSET
'Dim strSql ' as string

set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
set RSColor=server.CreateObject("ADODB.recordset")
strSql="select distinct style from style where cstymajor='" & request("menu1") & "'"
'RSColor.open strSql,conn 
set RSStruct=server.CreateObject("ADODB.recordset")

'strSql="select ciseghead from icistru where citemrecty+cisegno='U1'  And lsegendmaj=.T."
strSql="select ciseghead from icistru where citemrecty+cisegno='U1' "
RSStruct.open strSql,conn 




%>
<FORM action="
	<%IF Request.QueryString ("From") = "Cat" then 
		if Session("M_STYVIEW") = "P" Then	
			Response.Write("../catalog/catpage.asp")
		Else	
			Response.Write("../catalog/catSearch.asp?search=Y&group=ALL")
		End IF
	  Else
		Response.Write("redirect.asp")
	  End if%>" id=FORM1 method=post name=FORM1>
<!--FORM action="redirect.asp" id=FORM1 method=post name=FORM1-->
<div align="center">
  <center>
   <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%"><b>Enter Order Details:</b></td>
    </tr>
  </table>
 
<table border="1" width="95%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
  <TR>
		<TD width=20% class="dark_cell"><p><strong><%=session("CustField")%></strong></p>
		</TD>
		<TD colspan=3  class="light_cell"><p><%=Session("RSCust").fields("account")%> - <%=Session("RSCust").fields("btname")%></p>
		</TD>
  </TR>
  <TR>
		<%if strFile="cust" then%>
			<TD class="dark_cell"></TD>
			<TD class="light_cell" width=50%><p></p></TD>
		<%else%>
			<TD class="dark_cell">Sales Rep</TD>
			<TD class="light_cell" width=50%><p><%=Session("rep")%></p></TD>
		<%end if%>
		<!--TD width=8% class="dark_cell"><%=session("StoreField")%></TD>
		<TD width=8% class="dark_cell">
			<P >P.O.#</P>
		</TD>
		<TD width=12% class="dark_cell">Division</TD>
		<TD width=8% class="dark_cell">Season</TD>
		<TD width=8% class="dark_cell" align=center>Start Date</TD>
		<TD width=12% class="dark_cell" align=center>Complete Date</TD-->
		<TD class="dark_cell" align=right>Total Qty.</TD>
		<TD class="dark_cell" align=right>Total Amount</TD>
  </TR>
  <TR>
		<TD colspan=2 class="light_cell" align=right></TD>

		<TD class="light_cell" align=right>
			<INPUT id=text17 name=ordQty size="5" style='TEXT-ALIGN: right'readonly value="<%=Session("ordQty")%>">
		</TD>
		<TD  class="light_cell" align=right>
		
			<%' Response.Write "Currency Align " & Session("CurrencyAlign")
			if Session("ordAmount")="" then
				Session("ordAmount") ="0"
			end if %><%
			if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency")%>
				<INPUT id=text17 name=ordAmount size="10" readonly  style='TEXT-ALIGN: right'
					value="<%= formatnumber(Session("ordAmount"))%>">
			<%else%>
				<INPUT id=text17 name=ordAmount size="10" readonly style='TEXT-ALIGN: right'
					value="<%=formatnumber(Session("ordAmount"))%>"><%=Session("Currency")%>
			<%end if %>
		</TD>
  </TR>
</table>
<%'WAL_add shipping address [start]%>

<%'WAL_add shipping address [end]%>
</center>
</div>

<BR>
<div align="center">
<center>
<table border="1" width="95%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<TR>
	<%'if len(trim(Session("getstyle"))) >0 then
		IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
		Else
			strTemp = "<TD class=light_cell colspan=13>"
			Response.Write(strTemp )
			Session("RSStyStruct").MoveFirst
			DO While Not Session("RSStyStruct").Eof
				IF Len(Session("getstyle"))=0 Then
					strValue = ""
				Else
					strValue = Session(Trim(Session("RSStyStruct").fields("cisegsdes")))
				End IF
				'wal_get the description for style filed
				if Session("RSStyStruct").FIELDS("LSEGENDMAJ") = True then 
					strStyleName =  Trim(Session("RSStyStruct").fields("cisegsdes"))
				end if
				strTemp ="<b> " & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</b>" & "<INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & strValue & ">"
				strTemp = strTemp & " " & Trim(Session("RSStyStruct").fields("cisegsepr")) & " "
				Response.Write(strTemp)

				Session("RSStyStruct").MoveNext
			Loop
		End IF
		'Session("getStyle")=""
	'end if
	%>
    <INPUT id=button5 name=button2  type="submit" value="Get <%=strStyleName%>">
	</TD>
	
	</TD>
</TR>
</form>
		<TR>
		<TD colSpan=1 width=5% class="dark_cell">
			
			<strong>Description</strong>
		</TD>
		<TD colSpan=9 width=80% class="light_cell">
			  <%=Session("LongDesc")%> - <%=session("colorDesc")%> </TD>
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
			<%end if %>	</TD>
  </tr>
</FORM>

<%
'Response.Write "<font size=2>"&len(trim(Session("getstyle")))
'Response.End 
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
End IF
%>

<%intScaleCnt = cdbl("0")%>
<FORM action="updateline.asp?From=<%=Request.QueryString ("From")%>" id=FORM2 method=post name=FORM2 onsubmit="return FrontPage_Form2_Validator(this)">
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
<%strTemp = "Disabled"%>
    <!--WMA Delete Commetion Field Start-->    
    <!-- TD width="5%" class="dark_cell" align=right>Comm%</TD -->
    <!--WMA Delete Commetion Field End-->
    <%if strFile = "cust" then%>
   		<TD width="5%" class="dark_cell" align=right></TD>
   	<%else%>
   		<TD width="5%" class="dark_cell" align=right>Disc%</TD>
   	<%end if%>
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
			<INPUT  name=txtord1 style='TEXT-ALIGN: right'size="5" maxlength=5 value="<%=Session("text1")%>" 
			<%IF intScaleCnt = 0 Then Response.Write(strTemp) End IF%> >
	  <%else%>
			<input type=hidden name=txtord1>
	  <%end if
	  end if%></P></TD>
    <TD width="10%" class="light_cell" >
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz2")) <> "" and not isnull(RSScale("sz2")) then%>
      <INPUT  name=txtord2 size="5" style='TEXT-ALIGN: right'maxlength=5  value="<%=Session("text2")%>" <%IF intScaleCnt<2   Then Response.Write(strTemp) End IF%>>
      <%else%>
			<input type=hidden name=txtord2>
	  <%end if
	  end if%></P></TD>
    <TD width="10%" class="light_cell" >
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz3")) <> "" and not isnull(RSScale("sz3")) then%>
      <INPUT  name=txtord3 size="5" style='TEXT-ALIGN: right'maxlength=5 value="<%=Session("text3")%>" <%IF intScaleCnt<3 Then Response.Write(strTemp) End IF%>>
      <%else%>
			<input type=hidden name=txtord3>
	  <%end if
	  end if%></P></TD>
    <TD width=10% class="light_cell">
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz4")) <> "" and not isnull(RSScale("sz4")) then%>
      <INPUT  name=txtord4  style='TEXT-ALIGN: right'value="<%=Session("text4")%>" <%IF intScaleCnt<4 Then Response.Write(strTemp) End IF%>
      size="5" maxlength=5 >
      <%else%>
			<input type=hidden name=txtord4>
	  <%end if
	  end if%></P></TD>
    <TD width=10% class="light_cell">
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz5")) <> "" and not isnull(RSScale("sz5")) then%>
      <INPUT name=txtord5 style='TEXT-ALIGN: right' value="<%=Session("text5")%>" <%IF intScaleCnt<5 Then Response.Write(strTemp) End IF%>
      size="5" maxlength=5>
     <%else%>
			<input type=hidden name=txtord5>
	  <%end if
	  end if%></P></TD>
    <TD width="10%" class="light_cell">
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz6")) <> "" and not isnull(RSScale("sz6")) then%>
      <INPUT name=txtord6 style='TEXT-ALIGN: right' value="<%=Session("text6")%>" <%IF intScaleCnt<6 Then Response.Write(strTemp) End IF%>
      size="5" maxlength=5>
      <%else%>
			<input type=hidden name=txtord6>
	  <%end if
	  end if%></P></TD>
    <TD width=10% class="light_cell">
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz7")) <> "" and not isnull(RSScale("sz7")) then%>
		<INPUT  name=txtord7 style='TEXT-ALIGN: right' value="<%=Session("text7")%>" <%IF intScaleCnt<7 Then Response.Write(strTemp) End IF%>
		size="5" maxlength=5></P>
	  <%else%>
			<input type=hidden name=txtord7>
	  <%end if
	  end if%></TD>
    <TD width="10%" class="light_cell">
      <P align=right>&nbsp
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
		if trim(RSScale("sz8")) <> "" and not isnull(RSScale("sz8")) then%>
		<INPUT  name=txtord8 style='TEXT-ALIGN: right' value="<%=Session("text8")%>" <%IF intScaleCnt<8 Then Response.Write(strTemp) End IF%>
		size="5" maxlength=5>
	  <%else%>
			<input type=hidden name=txtord8>
	  <%end if
	  end if%></P></TD>
  	<!--WMA Delete Commetion Field Start-->
    <!-- TD  class="light_cell" align=right>
    <%'check if style commissionable
      if strComm = true then
	  'check which sales rep logged
		if Session("RSCust").fields("SalesRep") = session("Rep") then
			session("DefComm") = Session("RSCust").fields("comm")%>
			<input type=text name="txtComm" readonly <%if trim(session("Comm")) = "" then%>value="<%=formatnumber(Session("RSCust").fields("comm"))%>"<%else%>value="<%=formatnumber(session("Comm"))%>"<%end if%>size=3>
	   <%else
			session("DefComm") = Session("RSCust").fields("comm2")
		%>  <input type=text name="txtComm" readonly <%if trim(session("Comm")) = "" then%>value="<%=formatnumber(Session("RSCust").fields("comm2"))%>"<%else%>value="<%=formatnumber(session("Comm"))%>"<%end if%>size=3>
	   <%end if%>
	<%else%>
		N/A
	<%end if%>
    </TD-->
    <%if strFile = "cust" then%>
		<TD class="light_cell" align=right>
			&nbsp
		</TD>
	<%else%>
		<TD class="light_cell" align=right>
			<input type=text name="txtDisc" <%if trim(session("Disc")) = "" then%>value="<%=formatnumber(Session("RSCust").fields("Disc"))%>"<%else%>value="<%=formatnumber(session("Disc"))%>"<%end if%>size=3>
		</TD>
	<%end if%>
    <!--WMA Delete Commetion Field End-->
    <TD class="light_cell" align=right><input type=text name="txtGrp" maxlength=1 value="<%=session("Grp")%>"size=3></TD>
    <TD  class="dark_cell" ></TD>
  </tr>

  <%
    'WAL_calc of ots display[start]
Dim rsOtsChk
set rsOtsChk = server.CreateObject ("ADODB.Recordset")
'wal_ add warehous in the condition if exists
if trim(session("WareCode")) = "" then    
	strSQl = "Select (style.stk1 - sum(ordline.qty1)) as sum1, (style.stk2 - sum(ordline.qty2)) as sum2, "& _
			 "(style.stk3 - sum(ordline.qty3)) as sum3, (style.stk4 - sum(ordline.qty4)) as sum4, "& _ 
			 "(style.stk5 - sum(ordline.qty5)) as sum5, (style.stk6 - sum(ordline.qty6)) as sum6, "& _
			 "(style.stk7 - sum(ordline.qty7)) as sum7, (style.stk8 - sum(ordline.qty8)) as sum8, "& _
			 "(style.stk1 + style.wip1 - sum(ordline.qty1)) as sum11, (style.stk2 + style.wip2 - sum(ordline.qty2)) as sum22, "& _
			 "(style.stk3 + style.wip3 - sum(ordline.qty3)) as sum33, (style.stk4 + style.wip4 - sum(ordline.qty4)) as sum44, "& _ 
			 "(style.stk5 + style.wip5 - sum(ordline.qty5)) as sum55, (style.stk6 + style.wip6 - sum(ordline.qty6)) as sum66, "& _
			 "(style.stk7 + style.wip7 - sum(ordline.qty7)) as sum77, (style.stk8 + style.wip8 - sum(ordline.qty8)) as sum88, "& _
			 "style.make from ordline, ordHdr, style where ordline.style+DTOS(ordline.complete)+ordline.cordtype+ordline.order+ordline.store+STR(ordline.lineno,6) like '" &Session("getstyle")& "%' "
			 
			 strSQl = strSQl & "and ordHdr.status <> 'C' and ordHdr.Status <> 'X' and ordline.order = ordHdr.order and ordline.style = style.style"
	rsOtsChk.Open strSQL, connt

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
		
		rsOtsChk.Open strSQL, connt
	end if
	if not rsOtsChk.eof and trim(Session("getstyle")) <> "" then
		Session("Make") = rsOtsChk("make")
	end if
else
	strSQl = "Select (styDye.stk1 - sum(ordline.qty1)) as sum1, (stydye.stk2 - sum(ordline.qty2)) as sum2, "& _
			 "(stydye.stk3 - sum(ordline.qty3)) as sum3, (stydye.stk4 - sum(ordline.qty4)) as sum4, "& _ 
			 "(stydye.stk5 - sum(ordline.qty5)) as sum5, (stydye.stk6 - sum(ordline.qty6)) as sum6, "& _
			 "(stydye.stk7 - sum(ordline.qty7)) as sum7, (stydye.stk8 - sum(ordline.qty8)) as sum8, "& _
			 "(stydye.stk1 + stydye.wip1 - sum(ordline.qty1)) as sum11, (stydye.stk2 + stydye.wip2 - sum(ordline.qty2)) as sum22, "& _
			 "(stydye.stk3 + stydye.wip3 - sum(ordline.qty3)) as sum33, (stydye.stk4 + stydye.wip4 - sum(ordline.qty4)) as sum44, "& _ 
			 "(stydye.stk5 + stydye.wip5 - sum(ordline.qty5)) as sum55, (stydye.stk6 + stydye.wip6 - sum(ordline.qty6)) as sum66, "& _
			 "(stydye.stk7 + stydye.wip7 - sum(ordline.qty7)) as sum77, (stydye.stk8 + stydye.wip8 - sum(ordline.qty8)) as sum88 "& _
			 "from ordline, ordHdr, stydye where ordline.style+DTOS(ordline.complete)+ordline.cordtype+ordline.order+ordline.store+STR(ordline.lineno,6) like '" &Session("getstyle")& "%' "
			
			 strSQl = strSQl & "and styDye.CWARECODE = '"& trim(session("WareCode")) &"' "
			 strSQl = strSQl & "and ordHdr.CWARECODE = '"& trim(session("WareCode")) &"' "
			 strSQl = strSQl & "and ordHdr.status <> 'C' and ordHdr.Status <> 'X' and ordline.order = ordHdr.order and ordline.style = styDye.style"
	rsOtsChk.Open strSQL, connt

	if rsOtsChk.EOF then'no orders for this style then get value in stock
		rsOtsChk.Close ()
		strSQl = "Select stydye.stk1 as sum1, stydye.stk2 as sum2, "& _
				 "stydye.stk3 as sum3, stydye.stk4 as sum4, "& _ 
				 "stydye.stk5 as sum5, stydye.stk6 as sum6, "& _
				 "stydye.stk7 as sum7, stydye.stk8 as sum8, "& _
				 "sum(stydye.stk1 + stydye.wip1) as sum11, sum(stydye.stk2 + stydye.wip2) as sum22, "& _
				 "sum(stydye.stk3 + stydye.wip3) as sum33, sum(stydye.stk4 + stydye.wip4) as sum44, "& _ 
				 "sum(stydye.stk5 + stydye.wip5) as sum55, sum(stydye.stk6 + stydye.wip6) as sum66, "& _
				 "sum(stydye.stk7 + stydye.wip7) as sum77, sum(stydye.stk8 + stydye.wip8) as sum88 "& _
				 "from stydye where style = '" &Session("getstyle")& "' and styDye.CWARECODE = '"& trim(session("WareCode")) &"'"
		
		rsOtsChk.Open strSQL, connt
	end if
end if
	if not rsOtsChk.eof and trim(Session("getstyle")) <> "" then
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
		<%End If 'wma%>				     
	  
	<%end if%>
	
	
	
<%If Session("ShowOTS")= "T" then'wma%>	
  <tr>
    <TD class="dark_cell">
      <%=Session("OTSField")%>
    </TD>
    
    <TD class="light_cell">
	<%if trim(RSScale("sz1")) <> "" and not isnull(RSScale("sz1"))then%>            
      <P align=right><INPUT id=text3 name=txtots1 style='TEXT-ALIGN: right'
      <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else	
				'wal_130522 check if to show Immdeiate OTS or OTS+WIP
				if session("ImmOTS") = "T" then	
					intOTS = cdbl(RSOTS("stk1")) - cdbl(RSOTS("ord1"))
				else
					intOTS = (cdbl(RSOTS("stk1")) + cdbl(RSOTS("wip1"))) - cdbl(RSOTS("ord1"))
				end if
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
      <P align=right><INPUT id=text3 name=txtots2 style='TEXT-ALIGN: right'
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
	  Else	
			'wal_130522 check if to show Immdeiate OTS or OTS+WIP
				if session("ImmOTS") = "T" then	
					intOTS = cdbl(RSOTS("stk2")) - cdbl(RSOTS("ord2"))
				else
					intOTS = (cdbl(RSOTS("stk2")) + cdbl(RSOTS("wip2"))) - cdbl(RSOTS("ord2"))
				end if
			
			strTemp = "value =" & intOTS
			Response.Write(strTemp)
      END IF
      %>
      size="5" disabled></P>
	<%end if%>              
    </TD>
      
    <TD class="light_cell">
	<%if trim(RSScale("sz3")) <> "" and not isnull(RSScale("sz3"))then%>            
      <P align=right><INPUT id=text3 name=txtots3 style='TEXT-ALIGN: right'
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else	
				'wal_130522 check if to show Immdeiate OTS or OTS+WIP
				if session("ImmOTS") = "T" then	
					intOTS = cdbl(RSOTS("stk3")) - cdbl(RSOTS("ord3"))
				else
					intOTS = (cdbl(RSOTS("stk3")) + cdbl(RSOTS("wip3"))) - cdbl(RSOTS("ord3"))
				end if	
				'intOTS = (cdbl(RSOTS("stk3")) + cdbl(RSOTS("wip3"))) - cdbl(RSOTS("ord3"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>
      size="5" disabled></P>
	<%end if%>              
    </TD>
      
    <TD class="light_cell">
	<%if trim(RSScale("sz4")) <> "" and not isnull(RSScale("sz4"))then%>            
      <P align=right><INPUT id=text3 name=txtots4 style='TEXT-ALIGN: right'
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else		
				'wal_130522 check if to show Immdeiate OTS or OTS+WIP
				if session("ImmOTS") = "T" then	
					intOTS = cdbl(RSOTS("stk4")) - cdbl(RSOTS("ord4"))
				else
					intOTS = (cdbl(RSOTS("stk4")) + cdbl(RSOTS("wip4"))) - cdbl(RSOTS("ord4"))
				end if
				'intOTS = (cdbl(RSOTS("stk4")) + cdbl(RSOTS("wip4"))) - cdbl(RSOTS("ord4"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>
       size="5" disabled></P>
	<%end if%>               
    </TD>
    
    <TD class="light_cell">
	<%if trim(RSScale("sz5")) <> "" and not isnull(RSScale("sz5"))then%>            
      <P align=right><INPUT id=text3 name=txtots5 style='TEXT-ALIGN: right'
            <%		
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else	
				'wal_130522 check if to show Immdeiate OTS or OTS+WIP
				if session("ImmOTS") = "T" then	
					intOTS = cdbl(RSOTS("stk5")) - cdbl(RSOTS("ord5"))
				else
					intOTS = (cdbl(RSOTS("stk5")) + cdbl(RSOTS("wip5"))) - cdbl(RSOTS("ord5"))
				end if
				'intOTS = (cdbl(RSOTS("stk5")) + cdbl(RSOTS("wip5"))) - cdbl(RSOTS("ord5"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>
     size="5" disabled></P>
	<%end if%>             
     </TD>
     
    <TD class="light_cell">
	<%if trim(RSScale("sz6")) <> "" and not isnull(RSScale("sz6"))then%>        
      <P align=right><INPUT id=text3 name=txtots6 style='TEXT-ALIGN: right'
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else	
				'wal_130522 check if to show Immdeiate OTS or OTS+WIP
				if session("ImmOTS") = "T" then	
					intOTS = cdbl(RSOTS("stk6")) - cdbl(RSOTS("ord6"))
				else
					intOTS = (cdbl(RSOTS("stk6")) + cdbl(RSOTS("wip6"))) - cdbl(RSOTS("ord6"))
				end if
				'intOTS = (cdbl(RSOTS("stk6")) + cdbl(RSOTS("wip6"))) - cdbl(RSOTS("ord6"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>
      size="5" disabled></P>
	<%end if%>          
     </TD>
     
    <TD class="light_cell">
	<%if trim(RSScale("sz7")) <> "" and not isnull(RSScale("sz7"))then%>        
      <P align=right><INPUT id=text3 name=txtots7 style='TEXT-ALIGN: right'
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else		
				'wal_130522 check if to show Immdeiate OTS or OTS+WIP
				if session("ImmOTS") = "T" then	
					intOTS = cdbl(RSOTS("stk7")) - cdbl(RSOTS("ord7"))
				else
					intOTS = (cdbl(RSOTS("stk7")) + cdbl(RSOTS("wip7"))) - cdbl(RSOTS("ord7"))
				end if
				'intOTS = (cdbl(RSOTS("stk7")) + cdbl(RSOTS("wip7"))) - cdbl(RSOTS("ord7"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>

      size="5" disabled></P>
	<%end if%>          
     </TD>
      
	        
    <TD class="light_cell">
	<%if trim(RSScale("sz8")) <> "" and not isnull(RSScale("sz8"))then%>    
      <P align=right><INPUT id=text3 name=txtots8 style='TEXT-ALIGN: right'
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else				
				'wal_130522 check if to show Immdeiate OTS or OTS+WIP
				if session("ImmOTS") = "T" then	
					intOTS = cdbl(RSOTS("stk8")) - cdbl(RSOTS("ord8"))
				else
					intOTS = (cdbl(RSOTS("stk8")) + cdbl(RSOTS("wip8"))) - cdbl(RSOTS("ord8"))
				end if
				'intOTS = (cdbl(RSOTS("stk8")) + cdbl(RSOTS("wip8"))) - cdbl(RSOTS("ord8"))
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
      <p align=right><INPUT id=text12 name=txtots style='TEXT-ALIGN: right'
            <%
		
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else
				'wal_130522 check if to show Immdeiate OTS or OTS+WIP
				if session("ImmOTS") = "T" then	
					intOTS = cdbl(RSOTS("Totstk")) - cdbl(RSOTS("Totord"))
				else
					intOTS = (cdbl(RSOTS("Totstk")) + cdbl(RSOTS("Totwip"))) - cdbl(RSOTS("Totord"))
				end if
				
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
				<%End If%>					
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
				type="submit" value="Add to Order" onclick="Up_lines(this.form)">
				<!--INPUT id=button2 name=button2 style="HEIGHT: 24px; WIDTH: 95px" 
				type="submit" value="Remove Line" onclick="Re_lines(this.form)"-->
				
				</P></TD>
  
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
    
    <!--<input type="hidden" name="save" value="">-->
    <input type="hidden" name="remline" value="">
    <input type="hidden" name="SlctColor" value="<%=ucase(Session("getstyle"))%>">
    </form>
    
<form id=form3 name=form3 action="ordCharge.asp?save=T" method="POST" onsubmit="return checkformlines()">
<TABLE border=0 cellPadding=1 cellSpacing=1  width="95%" align=center>
 <%if Session("RSLine").BOF and Session("RSLine").EOF then
	  emptylinesflag="Y"
	else
	  emptylinesflag="N"
	end if%>
<INPUT type=hidden value="<%=emptylinesflag%>" name=islinesempty>
  <TR>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD></TD>
    <TD style="WIDTH: 40%" width="40%" align=right>
    <%'check to display charge button[start]%>
    <%if emptylinesflag = "N" and Request.QueryString ("From") <> "Cat" then%>
		<!--INPUT name=btnCharge type="button" value="View Order Charges" onclick="javascript:openwindow('ordCharge.asp');"-->
    <%end if%>
    <%'check to display charge button[end]%>
	
	
   </TR>
   
</TABLE>
<div align="center">
<center>
<table border="1" width="95%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
        
<TR>
	<TD class="dark_cell" align=center width=1%>></TD>
	<TD width="15%" class="dark_cell"><%=RSStruct("ciseghead")%>&nbsp;</TD>
	<TD width="30%" class="dark_cell">Description</TD>
    
	<TD width="10%" class="dark_cell">Color</TD>
	
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
'	Response.Write "<Font size=3>" & "<br>"& Session("foundDisc")
'	Response.End 
	%>	
	<%if Session("foundDisc") = true then%>
		<TD width="5%" class="dark_cell" align=right>Group</TD>
		<TD width="8%" class="dark_cell" align=right>Gross Price</TD>
		<%if strFile = "cust" then%>
			<TD width="5%" class="dark_cell" align=right>&nbsp</TD>
		<%else%>
			<TD width="5%" class="dark_cell" align=right>Disc%</TD>
		<%end if%>
	<%else%>
		<TD width="5%" class="dark_cell" align=right colspan=3>Group</TD>
		<!--TD width="10%" class="dark_cell" align=right colspan=2>Gross Price</TD-->
	<%end if%>


			
	<TD width="8%" class="dark_cell" align=right>Price</TD>
	<%if trim(Session("SizePerLine")) = "T" then%>
		<TD width="5%" class="dark_cell" align=right>Qty.</TD>
		<TD width="8%" class="dark_cell" align=right>Size</TD>
	<%else%>
		<TD width="5%" colspan=2 class="dark_cell" align=right>Qty.</TD>
	<%end if%>
	<%'wal_check to show ship date
	  if session("ShowExpected") = "T" or strFile="rep" then
	%>
		<TD width=10% class="dark_cell" align=center>Expected Ship</TD>
    <%else%>
		<TD width=10% class="dark_cell" align=center>&nbsp</TD>
    <%end if%>
	<TD width="15%" class="dark_cell" align=right>Amount</TD>       
</TR>
<%
IF Not Session("RSLine").EOF OR Not Session("RSLine").BOF   Then
   'open record with color code
   Set rsColorDsc = server.CreateObject("ADODB.Recordset")
	strsql = "SELECT DISTINCT * FROM CODES WHERE CDEFCODE+CRLTFIELD+CFLD_NAME='NNCOLOR'"
	rsColorDsc.Open strsql,Conn 
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
			strSql="select * from scale where type='S' And scale='" & rsSty("scale") & "'"
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
		'strGrp = "txtGrp"&trim(Session("RSLine").fields("style"))
		strTemp = "<TR>"
		Response.Write(strTemp)

		strTemp = "<TD Class=light_cell width=0><input type=checkbox name=chkLines  value='" & Session("RSLine").fields("lineNo") & "'></TD><TD class=""light_cell""><A HREF=""Dispordline.asp?From="&Request.QueryString ("From")&"&LineNo=" &Session("RSLine").fields("lineno")& "&Size=" &strSizeNo& "&Style=" & Session("RSLine").fields("style") &  """>"& Session("RSLine").fields("style") & "</A></TD>"'" & "<A HREF=""Dispordline.asp?Style=" & Session("RSLine").fields("style") &  """>" & Session("RSLine").fields("style") & "</A></TD>"
		'strTemp = "<TD class=""light_cell""><font size=""2"" color=""#000080"" face=""Arial"">" & Session("RSLine").fields("style") & "</font></TD>"
		Response.Write(strTemp)

		strTemp = "<TD class=""light_cell"">" & Session("RSLine")("desc1") & "</TD>"
		Response.Write(strTemp)
		'get color description
		rsColorDsc.Filter = "ccode_no='" & Trim(mid(trim(Session("RSLine").Fields("style").Value),session("styleSize")+2,session("colorSize"))) & "'"
		if not rsColorDsc.EOF then
			strTemp = "<TD  class=""light_cell"">" & RSColorDsc.Fields("cdiscrep").Value &"</TD>"
			Response.Write(strTemp)
		end if
		rsColorDsc.Filter = ""
		'WMA
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
		
		if Session("foundDisc")=true and strFile <> "cust" then
			strTemp = "<TD class=""light_cell"" align=right>&nbsp</TD>"
			Response.Write(strTemp)
		end if	

		'Session("RSLine").fields("price") = Session("price") 
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
		'WMA
		if (session("ShowExpected") = "T" or strFile="rep") and cstr(Session("RSLine").fields("complete"))<> "" and cstr(Session("RSLine").fields("complete"))<> "12:00:00 AM" then 
			strTemp = "<TD class=""light_cell"" align=center>" & cstr(Session("RSLine").fields("complete")) & "</TD>"
		else
			strTemp = "<TD class=""light_cell"" align=center></TD>"
		end if
		Response.Write(strTemp)


		intAmount = cdbl(Session("RSLine").fields("totqty")) * cdbl(Session("RSLine").fields("price"))'
		if Trim(Session("CurrencyAlign"))="LEFT" then
			strTemp = "<TD  class=""light_cell"" align=right>" & Session("Currency") & formatnumber(intAmount,2)   & "</font></TD>"
		else
			strTemp = "<TD  class=""light_cell"" align=right>" & formatnumber(intAmount,2) & Session("Currency") & "</font></TD>"
		end if 
		Response.Write(strTemp)

		'strTemp = "<TD  class=""light_cell""><font size=""2"" color=""#000080"" face=""Arial"">" & Session("Store") & "</font></TD>"
		'Response.Write(strTemp)
							
		Session("RSLine").movenext

	Loop
	Session("RSLine").MoveFirst()%>
	</table>
	</center>
						
	<table cellspacing="0" width=95% style="border-collapse: collapse" bordercolor="#111111" cellpadding="0">
	<tr>
	<br><td align=left><input type=button value="Remove checked Line(s)" onclick="return RmvLines();"></td>
	<td align=right><INPUT id=button4 name=button3 style="HEIGHT: 24px; WIDTH: 96px" type="submit" value="Finish Order" ></TD>
	</tr>
	</table>
	</div>
<%
else
	strTemp = "<input type=hidden name=save value="""">"
	Response.Write(strTemp)
End IF
%>
</Form>
</TABLE>
</center>
</div>
<P>&nbsp;</P>
<%End IF%>




	
	
</BODY>
</HTML>