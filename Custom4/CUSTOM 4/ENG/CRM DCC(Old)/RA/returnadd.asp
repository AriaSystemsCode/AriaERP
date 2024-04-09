<%@ Language=VBScript %>
<%
Response.Buffer=true
Response.Expires=-1


IF Session("ID")="" And Session("rep")="" Then
	'Response.Redirect("../default.asp")%>
	<script language="javascript">
	parent.location.href="../login.asp"
	</script>
<%End IF

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

Set Connt =  Server.CreateObject("ADODB.Connection")
Connt.Open Application("DataConnectionString") 

Set Session("RSCust")= Server.CreateObject("ADODB.RecordSet")
strsql = "select * from customer where type+account+store='M" & custid & "'"
Session("RSCust").open strsql,connt


%>
<%if Trim(Request.Form("txtEntereded")) = "" then
		strSMon = month(date())
		strSDay = day(date())
		if Len(strSDay) = 1 then
			strSDay = "0" & strSDay
		End if
		if Len(strSMon) = 1 then
			strSMon = "0" & strSMon
		End if
		dFrom = strSMon& "/" & strSDay & "/" & year(date())
  else
		dFrom = Trim(Request.Form("txtEntereded"))
  end if
  dTo = date()+10
  strEMon = month(dTo)
  if Len(strEMon) = 1 then
	strEMon = "0" & strEMon
  End if
  strEDay = day(dTo)
  if Len(strEDay) = 1 then
	strEDay = "0" & strEDay
  End if
  'dTo = formatdatetime(Cdate(dTo & "/1/" & year(date())) - 1, 0)
  if Trim(Request.Form("txtVoided")) = "" then					  
		dTo = strEMon & "/" & strEDay & "/" & year(date())
  else
		dTo = Trim(Request.Form("txtVoided"))
  end if
%>
<html>
<head>
<link REL="stylesheet" HREF="../images/<%=session("Theme")%>/order.css" TYPE="text/css">
<title>CRM - Return Authorization Request</title>
<script LANGUAGE="javascript" src="../checkForm.js">
</script>
<script language="JavaScript">
function CheckRespForm(objForm)
{
			if (objForm.txtcarton.value == "")
			{
				alert(objForm.txtcarton.getAttribute("ERRORTXT",false))
				return false
			}
	
	return true
}

function Search_Stores(objInput)
{
	objInput.SearchStoresFlag.value = "YES";
}
function submitNext()
{
if (document.form1.txtCartonNo.value == "" )
		{
	alert(document.form1.txtCartonNo.getAttribute("ERRORTXT",false))
	document.form1.txtCartonNo.focus() 
	return false
	}
//	return true
	if (isNaN(document.form1.txtCartonNo.value))
	{
		alert ("Please enter only digit characters in the Carton No. field!");
    return false
	}
	document.form1.action = 'returnhsave.asp';
	document.form1.submit();
}
function getInvoice(strCustID,strInv)
{
	window.open("BrowInv.asp?CustID=" + strCustID + "&Inv="+strInv,"BrowInvoice","scrollbars=Yes,resizable=Yes,location=NO,toolbar=NO,menubar=NO,WIDTH=200,HEIGHT=200")
	//alert(strInv)
}
function getOrder(strCustID,strOrder)
{
	window.open("BrowOrder.asp?CustID=" + strCustID + "&Order="+strOrder,"BrowOrder","scrollbars=Yes,resizable=Yes,location=NO,toolbar=NO,menubar=NO,WIDTH=200,HEIGHT=200")
	//alert(strInv)
}
</script>

</head>

<body>
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
<p><br><br><br></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected <%=Session("custfield")%> is <b><%=Session("customerid")%> - <%=RSTemp.fields("btname").value%></b></P>
	</TD>
	<TD align=right><a href="repcust.asp"><b>Get <%=Session("custfield")%></b></a></TD>
	<!-- ARD -->
</TR>
</table>

 <%Set RSTemp=nothing
  ConnTemp.close
  Set ConnTemp=nothing

End IF%>
<Table width=95% border=1 height=50 align=center>
<TR>
<TD class=title>Request RA</TD>
</TR>
</Table>
<%
'wal_130674 correct check on privilage case multi user
	if Application("CustomerLoginUsing")= "User"  then
		strAppUserVar = Session("userID")
	ElseIf Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"ADDRA") <= 0 Then
%>
<Table border=0 width=95% align=center>
	<tr>
	<td class="Title"><font color="Yellow">
		You're not authorized to view this page</font>
	</td>
	</tr>
</Table>

<%
	Response.End 
End If
%>

	<%IF compWork = "Y" Then%>


<%
'DB Connection
set conn=server.CreateObject("ADODB.connection")
conn.Open  Application("DataConnectionString")
'RecordSets
set rsLocation = server.CreateObject("ADODB.RecordSet")
set rsReason = server.CreateObject("ADODB.RecordSet")
set rsDivision = server.CreateObject("ADODB.RecordSet")
'Set rsStoreResult = Server.CreateObject("ADODB.RECORDSET")
'Queries
sqlLocation ="SELECT Cwarecode, Cdesc FROM Warehous" 
sqlReason = "SELECT Cdefcode, Cfld_name, Ccode_no, Cdiscrep FROM  Codes WHERE cdefcode+crltfield+cfld_name='NNREASON' order by ccode_no asc, cdefcode asc"
sqlDivision = "SELECT Cdefcode, Cfld_name, Ccode_no, Cdiscrep FROM  Codes WHERE cdefcode+crltfield+cfld_name='NNCDIVISION' ORDER BY Ccode_no ASC,Cdefcode ASC"
'open RecordSets
rsLocation.Open sqlLocation, conn
rsReason.Open sqlReason, conn
rsDivision.Open sqlDivision, conn
' for the Store find..
%>

<!--<form action="returnhsave.asp?logintype=R" method="post" id="form1">-->

<script Language="JavaScript"><!--
function FrontPage_Form2_Validator(theForm)
{

  if (theForm.txtCartonNo.value == "")
  {
    alert("Please enter a value for the \"txtCartonNo\" field.");
    theForm.txtCartonNo.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtCartonNo.value;
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
    alert("Please enter only digit characters in the \"txtCartonNo\" field.");
    theForm.txtCartonNo.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "1"))
  {
    alert("Please enter a value greater than or equal to \"1\" in the \"txtCartonNo\" field.");
    theForm.txtCartonNo.focus();
    return (false);
  }
  return (true);
}
//--></script>
<script Language="JavaScript">
<!--
function FrontPage_Form3_Validator(theForm)
{

  if (theForm.txtCartonNo.value == "")
  {
    alert("Please enter a value for the \"txtCartonNo\" field.");
    theForm.txtCartonNo.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtCartonNo.value;
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
    alert("Please enter only digit characters in the \"txtCartonNo\" field.");
    theForm.txtCartonNo.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "1"))
  {
    alert("Please enter a value greater than or equal to \"1\" in the \"txtCartonNo\" field.");
    theForm.txtCartonNo.focus();
    return (false);
  }
  return (true);
}
//-->
</script>

<div align="center">
  <center>

<table border="1" width="95%" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<tr>
	<td>
		<strong>Enter Return Authorization Request:</strong>
	</td>
</tr>
</table>

  </center>
</div>

<div align="center">
  <center>

<table border="1" width="95%" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">


<!--webbot BOT="GeneratedScript" PREVIEW=" " startspan --><script Language="JavaScript" Type="text/javascript"><!--
function FrontPage_Form1_Validator(theForm)
{

  if (theForm.txtCartonNo.value == "")
  {
    alert("Please enter a value for the \"txtCartonNo\" field.");
    theForm.txtCartonNo.focus();
    return (false);
  }

  if (theForm.txtCartonNo.value.length > 4)
  {
    alert("Please enter at most 4 characters in the \"txtCartonNo\" field.");
    theForm.txtCartonNo.focus();
    return (false);
  }

  var checkOK = "0123456789-";
  var checkStr = theForm.txtCartonNo.value;
  var allValid = true;
  var validGroups = true;
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
    allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit characters in the \"txtCartonNo\" field.");
    theForm.txtCartonNo.focus();
    return (false);
  }
  return (true);
}
//--></script><!--webbot BOT="GeneratedScript" endspan --><form action="../common/ordfindstore.asp?logintype=R" method="post" id="form1" name="FrontPage_Form1" onsubmit="return FrontPage_Form1_Validator(this)" language="JavaScript">
  <tr>
    <td width="20%" class="dark_cell"><strong><%=session("StoreField")%></strong> </td>
    <td class="light_cell"><input type="text" size="20" name="selectStore" maxlength=8  value="<%=Session("StoreValue")%>" readonly>
        <input type="submit" name="button2" value="Get <%=session("StoreField")%>" style="height: 24; width: 100"> 
    </td>
   </tr>


<script Language="JavaScript"><!--
function FrontPage_Form3_Validator(theForm)
{

  if (theForm.txtCartonNo.value == "")
  {
    alert("Please enter a value for the \"Carton No.\" field.");
    theForm.txtCartonNo.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtCartonNo.value;
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
    alert("Please enter only digit characters in the \"Carton No.\" field.");
    theForm.txtCartonNo.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "1"))
  {
    alert("Please enter a value greater than or equal to \"1\" in the \"Carton No.\" field.");
    theForm.txtCartonNo.focus();
    return (false);
  }
  return (true);
}
//-->
  </script>

	<tr>
	    <td width="20%" class="dark_cell"><strong>Entered</strong></td>
	    <td width="26%" class="light_cell">
	    <input type="text" size="11" name="txtEntereded" value="<%=dFrom%>" disabled onFocus="javascript:vDateType='1'" onBlur="DateFormat(this,this.value,event,true,'1')">
	    <input type="hidden" name="txtEntered" value="<%=dFrom%>">
	    </td>
	</tr>
	<tr>
	    <td width="14%" class="dark_cell"><strong>Void After</strong></td>
	    <td width="20%" class="light_cell">
	    <input type="text" size="11" name="txtVoided" value="<%=dTo%>" disabled onFocus="javascript:vDateType='1'" onBlur="DateFormat(this,this.value,event,true,'1')">
	    <input type="hidden" name="txtVoid" value="<%=dTo%>">
	    </td>
	</tr>
	<tr>
	    <td width="14%" class="dark_cell"><strong>Carton No.</strong></td>
	    <td width="20%" class="light_cell">
        <!--webbot bot="Validation" s-data-type="Integer" s-number-separators="x" b-value-required="TRUE" i-maximum-length="4" --><input type="text" size="11" maxlength=4 name="txtCartonNo" value="1" REQ="YES" ERRORTXT="Please enter a value in the Carton No. field!"></td>
	</tr>
  <tr>
      <td width="20%" class="dark_cell"><strong>Location</strong></td>
      <td width="40%" class="light_cell"><select name="selectLocation" size="1" style="HEIGHT: 22px; WIDTH: 220px">
				<option selected value="<%=rsLocation("Cwarecode")%>"><%=rsLocation("Cdesc")%></option>
            <%rsLocation.MoveNext()
            Do While Not  rsLocation.EOF %>
            <option value="<%=rsLocation("Cwarecode")%>"><%=rsLocation("Cdesc")%></option>
            <%rsLocation.MoveNext()
            Loop%>
		</select>
        </td>
    </tr>
    <tr>
        <td valign=top width="16%" class="dark_cell"><strong>Reason</strong></td>
        <td valign=top class="light_cell"><%'=Session("selectReason")%>
		<select name="selectReason" size="1" id="select1">
            <%rsReason.MoveFirst()%>
            <% Do While Not rsReason.EOF 
			if rsReason("CdefCode") = "D" then
			selectedFlag="selected" 
			else
			selectedFlag="" 
			end if %>
			<option <%=selectedFlag%> value="<%=rsReason("Ccode_no")%>"><%=rsReason("Cdiscrep")%></option>
			<% if selectedFlag="selected" then
			rsReason.MoveNext() 
			end if 
			rsReason.MoveNext()
			Loop%>
		</select> 
        </td>
    </tr>
    <tr>
        <td width="20%" class="dark_cell"><strong>Division</strong></td>
        <td width="40%" class="light_cell">
        <select name="selectDivision" size="1">
            <%'rsDivision.MoveFirst()%>
            <% Do while Not rsDivision.EOF 
				if  rsDivision("CdefCode") = "D" then 
					selectedFlag="selected" 
				else
					selectedFlag="" 
				end if %>
				<option <%=selectedFlag%> value="<%=rsDivision("Ccode_no")%>"><%=rsDivision("Cdiscrep")%></option>
				<%if selectedFlag="selected" then
					rsDivision.MoveNext() 
				end if
				rsDivision.MoveNext() 
		    Loop %>
        </select> 
        </td>
    </tr>
    <tr>
		<td width="20%" class="dark_cell"><strong>P/O #</strong></td>
        <td class="light_cell">
			<Input Type="Text" Name="txtPO" value="<%=Request.Form ("txtPO")%>"size='15' maxlength=15>
		</td>
	</tr>
	<tr>
		<td width="20%" class="dark_cell"><strong>Order</strong></td>
        <td class="light_cell">
			<Input Type="Text" Name="txtOrder" size="6" value="<%=Request.Form ("txtOder")%>">
			<input type="button" value='...' Name="GetOrder" onClick="return getOrder('<%=custid%>',this.form.txtOrder.value)">
		</td>
		
	</tr>
    <tr>
        <td width="20%" class="dark_cell"><strong>Invoice</strong></td>
        <td width="40%" class="light_cell">
			<Input Type="Text" Name="Invoice" value="<%=Request.Form ("Invoice")%>"size='6'>
			<input type="button" value='...' Name="GetInvoice" onClick="return getInvoice('<%=custid%>',this.form.Invoice.value)">
			<Input type="CHECKBOX" Name="RetInvoice" value="YES">Return Entire Invoice
		</td>
	</tr>
	
    </table>
  </center>
</div>
<table width=95% align=center>
    <tr>
        <td width="50%">
        </td>
			<td width="50%">
			<p align="right">
			<input type="submit" name="B1" value="Next"   onclick ="return submitNext()" >
			</p>
			</td>
        </tr>
    </table>
    
</form>

<%
'Terminate
conn.Close()

set conn= nothing
set rsReason = nothing
set rsDivision = nothing
set rsLocation = nothing
End IF
'Free Session Variables
Session("Division")= ""
%>
</body>
</html>