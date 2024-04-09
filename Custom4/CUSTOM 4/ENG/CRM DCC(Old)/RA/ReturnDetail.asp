<%@ Language=VBScript %>
<%
'Response.CacheControl  = "no-cache"
'Response.AddHeader  "Pragma", "no-cache"
'Response.Expires = -1
Response.Buffer = true

%>
<%
IF Session("ID")="" And Session("rep")="" Then
'	Response.Redirect("../default.asp")%>
<script language="javascript">
	parent.location.href ="../login.asp"
</script>
<%End IF
'Response.Write "<font size=3>DIVISION == "&Request("selectDivision")&"</font>"
'if Request("selectDivision")<>"" then
	'Session("Division")=Trim(Request("selectDivision"))
'Else
'	Session("Division") = Session("CatDivision")
'end if
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
constr =Application("DataConnectionString")
set Connt = server.CreateObject("ADODB.Connection")
connt.Open constr
'WAL_ check if there are records to be deleted[start]
if Request.QueryString("mode") = "D" and trim(Request.Form ("chkLines")) <> "" then
	Dim arrID
	arrID = Split(trim(Request.Form ("chkLines")),",")
	Session("rsReturnLine").MoveFirst()
	strSql = ""
	For i = 0 to UBound(arrID)
		
		'strSql = "cret_linno='" &Trim(arrID(i))& "'"
		strSql = "cra_linno='" &Trim(arrID(i))& "'"
		Session("rsReturnLine").Filter = strSql
		if not Session("rsReturnLine").eof then
			Session("rsReturnLine").Delete()
		end if
		Session("rsReturnLine").Filter = ""
		'strSql = strSql 
		'if i <> UBound(arrID) then
		'	strSql = strSql & " or "
		'End if
	Next	
	'Response.Write "<br><br><font size=2>"&strSql
	'Response.End 
	
	'Session("rsReturnLine").Flush
	
end if
'WAL_ check if there are records to be deleted[end]
%>
<html>
<head>
<SCRIPT LANGUAGE=javascript>
<!--

function hideloadingmsg() 

{
	document.all.loadingmsg.style.display = 'none';
    document.all.loadingmsg.style.visibility = 'hidden';

}

//-->

</SCRIPT>

<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<title>CRM - Return Authorization Request</title>
</head>
<body>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
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
	<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>

<%End IF%>
<%IF compWork = "Y" Then%>
<%Dim strSql ' as string

If Len(Trim(Session("OrderFlag"))) = 0 Then
	Set Session("rsReturnLine") = server.CreateObject("ADODB.recordset")
	Set rsReturnLine1 = server.CreateObject("ADODB.recordset")
    strSql = "select * from raline where rano+style+cra_linno='%"& session("LineNo") &"' "
    'strSql = "select * from raline where rano+style+cret_linno='%"& session("LineNo") &"' "
    Session("rsReturnLine").open  strSql, connt, 2, 4
    'Response.Write "<font size=3>REC"&Session("rsReturnLine").recordcount&"</font>"
End IF
If IsObject(Session("rsReturnLine")) Then
	If Not Session("rsReturnLine").EOF Then
		Session("rsReturnLine").MoveFirst
	End If
End If
'Response.Write "<font size=3>LINE"&session("LineNo")&"</font>"
%>

<script language="JavaScript">
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
		objInput.Typeline.value = "U";
	}

	function Re_lines() 
	{
	
		if (document.form3.chkLines.checked == false)
		{
			alert("Please select a line!")
			return false;
		}
		else
		{
			document.form3.action = 'ReturnDetail.asp?mode=D';
			document.form3.submit();
		}
	}
    </script>

<%' Script to check fields of quantity in the form

'RecordSets
set rsReason = server.CreateObject("ADODB.RecordSet")
set rsStyle = Server.CreateObject("ADODB.RecordSet")
'Queries
sqlReason = "SELECT Cdefcode, Cfld_name, Ccode_no, Cdiscrep FROM  Codes WHERE Cdefcode+crltfield+Cfld_name = 'NNREASON' ORDER BY Cdefcode"
rsReason.Open sqlReason, connt

Dim DefReasonDes
rsReason.MoveFirst()
rsReason.Filter = "Ccode_no ='"& Trim(Session("selectReason2"))&"'"
IF Not (rsReason.EOF And rsReason.Bof) Then
	DefReasonDes = rsReason("Cdiscrep")
	rsReason.Filter  = ""
End IF
Dim rs, RSStymaj, rsRetStyStruct ' as ADODB.RECORDSET

set rsRetStyStruct=server.CreateObject("ADODB.recordset")
'strSql="select ciseghead from icistru where citemrecty+cisegno ='U1' And lsegendmaj=.T."
'NEK [Start] Cuz there could be 3 divisions not 2 only so the .T. will be second line
' the Description is written in the 1st Segment Only . Regardless of their amount
strSql="select ciseghead from icistru where citemrecty+cisegno ='U1'"
rsRetStyStruct.open strSql,connt 
'NEK [End]
%>
<Table Width=95%  border=1 align=center>
<TR><TD class=Title>Return Authorization Details</TD></TR>
</Table>
<form action="returnDetailRedirect.asp?firsttime=1" method="post" name="FORM1" id="FORM1">
<div align="center">
<center>
<table border="1" width="95%" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
        <tr>
            <td width="16%" class="dark_cell"><strong><%=session("CustField")%><strong></td>
            <td colspan="8" width="16%" class="light_cell">
            <%if len(trim(session("id")))>0 then%>
				<%=Session("RSCust").fields("account")%> - <%=Session("RSCust").fields("btname")%></td>
            <%else %>
				<%=Session("customerid")%> - <%=Session("RSCust").fields("btname")%></td>
            <%end if%>
        </tr>
        <tr>
            <td width="15%" class="dark_cell">Ship From</td>
            <td width="20%" class="dark_cell">Location</td>
            <td width="15%" class="dark_cell">Reason</td>
            <td width="15%" class="dark_cell">Division</td>
            <td class="dark_cell" align=center>Entered</td>
            <td class="dark_cell" align=center>Void After</td>
            <td width="5%" class="dark_cell" align=right>Total Qty.</td>
            <td width="5%" class="dark_cell" align=right>Total Amount</td>
        </tr>
        <tr>
            <td class="light_cell"><%=Session("selectStore")%>&nbsp;</td>
            <td class="light_cell"><%=Session("LocDesc")%>&nbsp;</td>
            <td class="light_cell"><%=DefReasonDes%>&nbsp;</td>
            <td class="light_cell">
            <%'Get Description of the Divisin
				Set rsDesc=Server.CreateObject ("ADODB.Recordset")
				MySQL= "SELECT Cdiscrep FROM Codes WHERE cdefcode+crltfield+cfld_name='NNCDIVISION' AND Ccode_no='" & Trim(Session("Division"))& "'"	
				rsDesc.Open MySQL, Connt			
				if rsDesc.EOF AND rsDesc.BOF Then
				'error
				else
				Response.Write rsDesc("Cdiscrep")
				end if
				rsDesc.Close
			%> &nbsp;</td>
            <td class="light_cell" align=center><%=Session("txtEntered")%></td>
            <td class="light_cell" align=center><%=Session("txtVoid")%></td>
            <td class="light_cell" align=right><input type="text" size="20" name="txtQty" value="<%=Session("TotalQty")%>" id="text1" style="WIDTH: 45px" readonly ></td>
            <td  class="light_cell" align=right><input type="text" size="20" name="txtAmount" id="text2" style="WIDTH: 70px" readonly 
            <%if Session("CurrencyAlign")="LEFT" then%>
				value="<%=Session("Currency")%><%=FormatNumber(Session("TotalAmount"))%>"
            <%else%>
				value="<%=FormatNumber(Session("TotalAmount"))%><%=Session("Currency")%>"
            <%end if %> > </td>
        </tr>
        <tr>
            <td width="15%" class="dark_cell">Order</td>
            <td width="15%" class="dark_cell">Invoice</td>
            <td width="15%" class="dark_cell">CustPO</td>
            <td class="dark_cell">&nbsp;</td>
            <td class="dark_cell">&nbsp;</td>
            <td class="dark_cell">&nbsp;</td>
            <td width="10%" class="dark_cell">&nbsp;</td>
            <td width="10%" class="dark_cell">&nbsp;</td>
        </tr>
        <tr>
            <td width="15%" class="light_cell"><%=Session("Order")%>&nbsp;</td>
            <td width="15%" class="light_cell"><%=Session("Invoice")%>&nbsp;</td>
            <td width="15%" class="light_cell"><%=Session("P/O")%> &nbsp;</td>
            <td class="light_cell">&nbsp;</td>
            <td class="light_cell">&nbsp;</td>
            <td class="light_cell">&nbsp;</td>
            <td width="10%" class="light_cell">&nbsp;</td>
            <td width="10%" class="light_cell">&nbsp;</td>
        </tr>
    </table>
      </center>
    </div>
    <BR>
    <div align="center">
    <center>
    <table border="1" width=95% style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
        <tr>
        
	       <%
	       Session("rsRetStyStruct").MoveFirst()
				IF Session("rsRetStyStruct").EOF And Session("rsRetStyStruct").BOF Then
				Else
					strTemp = "<TD class=dark_cell colspan=10><strong>"
					Response.Write(strTemp)
					
					Session("rsRetStyStruct").MoveFirst
					DO While Not Session("rsRetStyStruct").Eof
						IF Len(Session("getstyle"))=0 Then
							strValue = ""
						Else
							strValue = Session(Trim(Session("rsRetStyStruct").fields("cisegsdes")))
						End IF
						strTemp =  Trim(Session("rsRetStyStruct").fields("cisegsdes")) & "<INPUT name=" & Trim(Session("rsRetStyStruct").fields("cisegsdes")) & " size=""" & Session("rsRetStyStruct").fields("nisegsize") &  """ maxlength="""& Session("rsRetStyStruct").fields("nisegsize") & """ value=" & strValue & ">"
						strTemp = strTemp & " " & Trim(Session("rsRetStyStruct").fields("cisegsepr")) & " "
						Response.Write(strTemp)
						Session("rsRetStyStruct").MoveNext
					Loop

				End IF
			'Session("getStyle")=""
			%>
			 <strong>
				<input type="submit" name="button2" value="Get Style" id="button5" style="height: 24; width: 80">
				
			 </td>
        </tr>
        </Form>
        <tr>
            <td class="dark_cell">
				<p align="left"><strong>Description</strong></p>
            </td>
            <td colSpan="8" class="light_cell">
				<p align="left"><strong><%=Session("LongDesc")%></strong></p>
            </td>
            
        </tr>
</form>
<%
Set RSOTS = server.CreateObject("ADODB.recordset")
strSql="select * from style where style='" & Session("getstyle") & "' and status+cstygroup like 'A%'"
RSOTS.open strSql,connt 
Dim intOTS ' as integer

IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
	Set RSScale = server.CreateObject("ADODB.recordset")
	strSql="select * from scale where type+scale+prepak ='S"&RSOTS("scale")&"'"
	RSScale.open strSql,connt 
End IF

' Second Form start here ..
%>
<script Language="JavaScript"><!--
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
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord1.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
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
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord2.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
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
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord3.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
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
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord4.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
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
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord5.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
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
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord6.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
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
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord7.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
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
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord8.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
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
  if (theForm.Typeline.value=="R")
  {
	if (form2.StyleChoosed.value=="F")
	{
		alert("Please select the item you want to delete from the lower table and click “Remove Line”!");
		return false;
	}
  }
  return (true);
}


function CheckLineRecordset()
{
	if (form3.RecordsetEmptyFlag.value == "T")
	{ 
		alert ("Return Authorization request form is empty; can’t save!");
		return (false);
	}
	return (true)
}

//-->
        </script>
  
 <form action="Retupdateline.asp" method="post" name="form2" id="FORM2" onsubmit="return FrontPage_Form2_Validator(this)" name="FrontPage_Form2">
  
		<input type="hidden" name="Typeline" value>
		<input type="hidden" name="SlctColor" value="<%=ucase(Session("getstyle"))%>">
		<%
		  if Trim(Session("getstyle")) = "" then
			styleischoosed = "F"
		  else
			styleischoosed = "T"
		  end if
		%>
		<input type="hidden" id=StyleChoosed name=StyleChoosed value="<%=styleischoosed%>">
        <tr>
            <td class="dark_cell"><p align="left"><strong>Reason</strong></p>
            </td>
        
            <td colspan="8" class="light_cell">
			<p align="LEFT">
			
            <select name="selectReason" size="1" id="select1" >
			    <%rsReason.MoveFirst()%>
			    <%Do While Not rsReason.EOF 
					if Trim(rsReason("Ccode_no"))=Trim(Session("selectReason2")) then
						selectedFlag="selected" 
					else
						selectedFlag="" 
					end if 
				%>	<option <%=selectedFlag%> value="<%=rsReason("Ccode_no")%>"><%=rsReason("Cdiscrep")%></option>
				<%rsReason.MoveNext()
				  Loop
				%>
			</select> 
            </p>
           </td>
        </tr>
        <tr>
<%intScaleCnt = cdbl("0")%>
            <td width="10%" class="dark_cell">&nbsp;</td>
            <td width="10%" class="dark_cell"><p align="right"><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz1"))
					intScaleCnt = cdbl(RSScale("cnt"))
				Else
					Response.Write("Size1")
				End IF
			%></p>
            </td>
            <td width="10%" class="dark_cell"><p align="right"><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz2"))
				Else
					Response.Write("Size2")
				End IF
			%></p>
            </td>
            <td width="10%" class="dark_cell"><p align="right"><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz3"))
				Else
					Response.Write("Size3")
				End IF
			%></p>
            </td>
            <td width="10%" class="dark_cell"><p align="right"><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz4"))
				Else
					Response.Write("Size4")
				End IF
			%></p>
            </td>
            <td width="10%" class="dark_cell"><p align="right"><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz5"))
				Else
					Response.Write("Size5")
				End IF
			%></p>
            </td>
            <td width="10%" class="dark_cell"><p align="right"><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz6"))
				Else
					Response.Write("Size6")
				End IF
			%></p>
            </td>
            <td width="10%" class="dark_cell"><p align="right"><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz7"))
				Else
					Response.Write("Size7")
				End IF
			%></p>
            </td>
            <td width="10%" class="dark_cell"><p align="right"><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz8"))
				Else
					Response.Write("Size8")
				End IF
			%></p>
            </td>
        </tr>
        <tr>
            <td width="10%" class="dark_cell"><p align="left">Qty.</p>
            </td>
            <td width="10%" class="light_cell" align=right><p align="right">
            
						<%strTemp = "Disabled"%>
						<%if Session("text1")="0" then
								Session("text1")=""
						 end if
						 if Session("text2")="0" then
							Session("text2")=""
						 end if
						 if Session("text3")="0" then
							Session("text3")=""
						 end if
						 if Session("text4")="0" then
							Session("text4")=""
						 end if
						 if Session("text5")="0" then
							Session("text5")=""
						 end if
						 if Session("text6")="0" then
							Session("text6")=""
						 end if
						 if Session("text7")="0" then
							Session("text7")=""
						 end if
						 if Session("text8")="0" then
							Session("text8")=""
						 end if
						%>
            <input type="text" size="5" name="txtord1" value="<%=Session("text1")%>" <%IF intScaleCnt=0   Then Response.Write(strTemp) End IF%> id="text3" maxlength="5" ></p>
            </td>
            <td width="10%" class="light_cell"><p align="right">
           
            <input type="text" size="5" name="txtord2" value="<%=Session("text2")%>" <%IF intScaleCnt<2   Then Response.Write(strTemp) End IF%> id="text3" maxlength="5" ></p>
            </td>
            <td width="10%" class="light_cell"><p align="right">
           
            <input type="text" size="5" name="txtord3" value="<%=Session("text3")%>" <%IF intScaleCnt<3   Then Response.Write(strTemp) End IF%> id="text3" maxlength="5" ></p>
            </td>
            <td width="10%" class="light_cell"><p align="right">
           
            <input type="text" size="5" name="txtord4" value="<%=Session("text4")%>" <%IF intScaleCnt<4   Then Response.Write(strTemp) End IF%> id="text17" maxlength="5" ></p>
            </td>
            <td width="10%" class="light_cell" align=right><input type="text" size="5" name="txtord5" value="<%=Session("text5")%>" <%IF intScaleCnt<5   Then Response.Write(strTemp) End IF%> id="text16" maxlength="5" ></td>
            <td width="10%" class="light_cell"><p align="right">
            <input type="text" size="5" name="txtord6" value="<%=Session("text6")%>" <%IF intScaleCnt<6   Then Response.Write(strTemp) End IF%> id="text15" maxlength="5" ></p>
            </td>
            <td width="10%" class="light_cell"><p align="right">
          
            <input type="text" size="5" name="txtord7" value="<%=Session("text7")%>" <%IF intScaleCnt<7   Then Response.Write(strTemp) End IF%> id="text14" maxlength="5" ></p>
            </td>
            <td width="10%" class="light_cell"><p align="right">
         
            <input type="text" size="5" name="txtord8" value="<%=Session("text8")%>"<%IF intScaleCnt<8   Then Response.Write(strTemp) End IF%> id="text3" maxlength="5" ></p>
            </td>
        </tr>
    </table>

      </center>
    </div>

 <TABLE border=0 cellPadding=1 cellSpacing=1  width=95% align=center>
  
  <TR>
    <TD width="20%" style="WIDTH: 20%">
      <p align="left">&nbsp;</p></TD>
    <TD width="25%" colSpan=2 style="WIDTH: 25%"></TD>
    <TD width="15%"></TD>
    <TD width="20%" colSpan=2>
      <P align=right>
      
				
				<INPUT id=button3 name=button3 style="HEIGHT: 24px; WIDTH: 96px" 
				type="submit" value="Update Line" onclick="Up_lines(this.form)">&nbsp;

				<!--INPUT id=button2 name=button2 style="HEIGHT: 24px; WIDTH: 95px" 
				type="submit" value="Remove Line" onclick="Re_lines(this.form)"-->
				
				</P></TD>
  
  </TR>
    </TABLE>
<TABLE  border=0 cellPadding=1 cellSpacing=1 width=95% align=center>
  
  <TR>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
   </TR>
    </TABLE>

  
 
</form>
<form action="actionretauthsave.asp" method="POST" name="form3" id="form3" onsubmit="return CheckLineRecordset()">

<TABLE border=0 cellPadding=1 cellSpacing=1  width=95% align=center>
  
  <TR>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%">
      <p align="right">
      
				<INPUT id=button4 name=button3 style="HEIGHT: 24px; WIDTH: 96px" type="submit" value="Save RA" >
				</TD>
		<%' to check if the recordset of the lines is empty or not ..
		if isobject(Session("rsReturnline")) then%>
			<%if Session("rsReturnLine").BOF AND Session("rsReturnLine").EOF then
				EmptyFlag = "T"
			  else 
				EmptyFlag="F"
			  end if
		end if%>
		<input type="Hidden" name="RecordsetEmptyFlag" value="<%=EmptyFlag%>" >
   </TR>
   
</TABLE>

<div align="center">
<center>
<table border="1" cellspacing="0" width=95% style="border-collapse: collapse" bordercolor="#111111" cellpadding="0">

    <tr>
        <td class="dark_cell">></td>
        <td class="dark_cell"><%=rsRetStyStruct("ciseghead")%>&nbsp;</td>
        <td class="dark_cell">Description</td>
        <td class="dark_cell">Reason</td>
        <td class="dark_cell" ALIGN=RIGHT>Qty.</td>
        <td class="dark_cell" ALIGN=RIGHT>Price </td>
        <td class="dark_cell" ALIGN=RIGHT>Amount</td>
    </tr>
<%

         if isobject(Session("rsReturnline")) then 

          IF Not Session("rsReturnLine").EOF OR Not Session("rsReturnLine").BOF   Then
				Session("rsReturnLine").movefirst
				sqlStyle = "SELECT Style, Desc1 FROM Style where status+cstygroup like 'A%'"  
				rsStyle.Open sqlStyle, connt
						Do While Not Session("rsReturnLine").EOF
							strTemp = "<TR>"
							Response.Write(strTemp)
							'wma
							strTemp = "<TD Class=light_cell><input type=checkbox name=chkLines  value='" & Session("rsReturnLine")("cra_linno") & "'></TD><TD  Class=light_cell><A HREF=""ReturnDisplaySelectedRecord.asp?LineNo=" & Session("rsReturnLine").fields("cra_linno") & "&Style=" & Session("rsReturnLine").fields("style") & "&Reason=" & Session("rsReturnLine").Fields("Reason") & """>" & Session("rsReturnLine").fields("style") & "</A></TD>"
							'strTemp = "<TD Class=light_cell><input type=checkbox name=chkLines  value='" & Session("rsReturnLine")("cret_linno") & "'></TD><TD  Class=light_cell><A HREF=""ReturnDisplaySelectedRecord.asp?LineNo=" & Session("rsReturnLine").fields("cret_linno") & "&Style=" & Session("rsReturnLine").fields("style") & "&Reason=" & Session("rsReturnLine").Fields("Reason") & """>" & Session("rsReturnLine").fields("style") & "</A></TD>"
							Response.Write(strTemp)
							'Description
							Dim StyleDes 
							rsStyle.MoveFirst()
							
							Do While Not rsStyle("Style") = Session("rsReturnLine").Fields("style")
								rsStyle.MoveNext()
							Loop
							StyleDes = rsStyle("Desc1")
							rsReason.MoveFirst()
							
							
							strTemp = "<TD  Class=light_cell>" & StyleDes & "</TD>"
							Response.Write(strTemp)
							'Reason
							
							Dim ReasonDes 
							rsReason.MoveFirst()
							
							Do While Not rsReason("Ccode_no") = Session("rsReturnLine").Fields("Reason")
								rsReason.MoveNext()
							Loop
							ReasonDes = rsReason("Cdiscrep")
							rsReason.MoveFirst()
							
							strTemp = "<TD  Class=light_cell> &nbsp;" & ReasonDes &"</TD>"
							Response.Write(strTemp)
							
							strTemp = "<TD  Class=light_cell align=right>" & Session("rsReturnLine").fields("totqty") & "</TD>"
							Response.Write(strTemp)
							
							IF Session("CurrencyAlign")="LEFT" then
								strTemp = "<TD  Class=light_cell align=right>" & Session("Currency") & FormatNumber(Session("rsReturnLine").fields("price")) & "</TD>"							
							else
								strTemp = "<TD  Class=light_cell align=right>" & FormatNumber(Session("rsReturnLine").fields("price")) & Session("Currency") & "</TD>"
							end if 
							Response.Write(strTemp)

							
							intAmount = cdbl(Session("rsReturnLine").fields("totqty")) * cdbl(Session("rsReturnLine").fields("price"))
							IF Session("CurrencyAlign")="LEFT" then
								strTemp = "<TD  Class=light_cell align=right>" & Session("Currency") & FormatNumber(intAmount) & "</TD>"
							else
								strTemp = "<TD  Class=light_cell align=right>" & FormatNumber(intAmount) & Session("Currency") & "</TD>"
							end if 
							Response.Write(strTemp)

												
							Session("rsReturnLine").movenext

						Loop%>
						</table>
						</center>
						
						<table cellspacing="0" width=95% style="border-collapse: collapse" bordercolor="#111111" cellpadding="0">
						<tr>
							<td align=left><input type=button value="Remove checked Line(s)" onclick="Re_lines()"></td>
						</tr>
						</table>
						</div>
	<%	  Else%>
		</table>
		</center>
		</div>
<%
          End IF
        End if
          
End IF%>

</Form>
<p>&nbsp;</p>
</body>
</html>