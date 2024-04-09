<%@ Language=VBScript %>

<% Response.CacheControl = "no-cache" 
 Response.AddHeader "Pragma", "no-cache" 
 Response.Expires = -1 
 Response.Buffer=true

 
IF Trim(Session("ID")) = "" AND Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
End IF
%>

<HTML>
<HEAD>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/Catalog.css" TYPE="text/css">
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<title>CRM - Catalog - Check Cart</title>
</head>
<body >

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
	if (FORM2.StyleChoosed.value=="F")
	{
		alert("Please select the item you want to delete from the lower table and click “Remove Line”!");
		return false;
	}
  }
  return (true);
}




//function Up_lines(objInput)
//{
//	objInput.Typeline.value = "U";
//	if (objInput.txtord1.value <= 0 && objInput.txtord2.value <= 0 && objInput.txtord3.value <= 0 && objInput.txtord4.value <= 0 && objInput.txtord5.value <= 0 && objInput.txtord6.value <= 0 && objInput.txtord7.value <= 0 && objInput.txtord8.value <= 0 )
//		{
	//		alert("The number of items must be greater than zero!");
		//	return false;
		//}
		//return true;
	//}


//function Re_lines(objInput) 
//{
	//objInput.Typeline.value = "R";
  
	//if (document.FORM2.StyleChoosed.value=="F")
	//{
	//	alert("Please select the item you want to delete from the lower table and click “Remove Line”!");
	//	return false;
	//}
	//return true;
//}

function Up_lines(objInput)
	{
		objInput.Typeline.value = "U";
	}

	function Re_lines(objInput) 
	{
		objInput.Typeline.value = "R";
	}

function checkformlines()
 {
	if (document.form3.islinesempty.value=="Y" )
	{
		alert ("Catalog form is empty;can't save!");
		return false;
	}
	return true;
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
	<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=sESSION("RSCUST").FIELDS("btname").value%></P>
	</TD>
	<TD align=right><a href="repcust.asp">Get <%=session("CustField")%></a></TD>
	<!-- ARD -->
</TR>
</table>
 <%ENd if%>

<Table width=95% height=50 align=center border=1>
<TR>
<TD class=title>Catalog</TD>
</TR>
</Table>

<%
Dim rs, RSColor, RSStymaj, RSStruct ' as ADODB.RECORDSET
'Dim strSql ' as string

set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

	Set Session("RSStyStruct") = server.CreateObject("Adodb.RecordSet")

	strsql = "select * from icistru where citemrecty='U'"
	Session("RSStyStruct").open strsql,conn



set RSStruct=server.CreateObject("ADODB.recordset")
'strSql="select ciseghead from icistru where citemrecty='U' And cisegno='1' And lsegendmaj=.T."
strSql="select ciseghead from icistru where citemrecty='U' And cisegno='1' "
RSStruct.open strSql,conn 

%>
<FORM action="<%IF Session("M_STYVIEW") = "P" Then	Response.Write("catpage.asp")Else	Response.Write("catSearch.asp?search=Y&group=ALL")End IF%>" id=FORM1 method=post name=FORM1>
	<Table width=95% align=center border=0>
	<TR>
		<TD> <STRONG>Catalog Details.</STRONG><TD>
	<TR>
	</Table>

<table border="1" width="95%" align=center>
  <TR>
		<TD width=16% class="dark_cell"><%=session("CustField")%></TD>
		<TD colspan=7 width=16% class="dark_cell"><%=Session("RSCust").fields("account")%> - <%=Session("RSCust").fields("btname")%></TD>
  </TR>
  <TR>
		<TD width=12% class="dark_cell"><%=session("StoreField")%></TD>
		<TD width=12% class="dark_cell">P.O.#</TD>
		<TD width=12% class="dark_cell">Division</TD>
		<TD width=12% class="dark_cell">Season</TD>
		<TD width=12% class="dark_cell" align=center>Start Date</TD>
		<TD width=12% class="dark_cell" align=center>Complete Date</TD>
		<TD width=12% class="dark_cell" align=right>Total Qty.</TD>
		<TD width=12% class="dark_cell" align=right>Total Amount </TD>
  </TR>
  <TR>
		<TD width=16% class="dark_cell"><%=Session("StoreID")%></TD>
		<TD width=16% class="dark_cell"><INPUT id=txtpono name=txtpono size="15" maxlength="15" value="<%If trim(request("txtpono"))=""  Then response.write Session("PO") Else response.write trim(request("txtpono")) End IF%>" ></TD>
		<TD width=16% class="dark_cell"><%'Get Description of the Divisin
			Set rsDesc=Server.CreateObject ("ADODB.Recordset")
			MySQL= "SELECT Cdiscrep FROM Codes WHERE Cfld_name='CDIVISION' AND Cdefcode='N' AND Crltfield='N' AND Ccode_no='" & Trim(Session("CatDivision"))& "'"	
			rsDesc.Open MySQL, Conn
			if rsDesc.EOF AND rsDesc.BOF Then
				'error
			else
				Response.Write rsDesc("Cdiscrep")
			end if
			rsDesc.Close%>
		<%'=Session("Division")%></TD>
		<TD width=16% class="dark_cell"><%'Get Description of the Season
				MySQL= "SELECT Cdiscrep FROM Codes WHERE Cfld_name='SEASON' AND Cdefcode='N' AND Crltfield='N' AND Ccode_no='" & Trim(Session("StyleColor"))& "'"	
				rsDesc.Open MySQL, Conn
				if rsDesc.EOF AND rsDesc.BOF Then
					Response.Write ("All")'all seasons 
					session("Season")="*"
				else
					Response.Write rsDesc("Cdiscrep")
				end if
				rsDesc.Close%>
		<%'=Session("Season")%></font></P>
		</TD>
		<TD width=16% class="dark_cell" align=center><%=Session("Start")%></TD>
		<TD width=16% class="dark_cell" align=center><%=Session("Completed")%></TD>
		<TD width=16% class="dark_cell"><INPUT id=text17 name=ordQty size="8" readonly value="<%=Session("ordQty")%>"></TD>
		<TD width=16% class="dark_cell"><INPUT id=text17 name=ordAmount size="8" readonly 
		value="<%if Session("ordAmount")<> "" then
					if Session("CurrencyALign")="LEFT" then
						Response.write Session("Currency") & FormatNumber(Session("ordAmount"))
					else
						Response.write FormatNumber(Session("ordAmount")) & Session("Currency")
					end if %>
		<%else
			if Session("CurrencyALign")="LEFT" then
				Response.Write Session("Currency") & FormatNumber(0)
			else
				Response.Write FormatNumber(0) & Session("Currency") 
			end if 	 
		end if %>"></TD>
  </TR>
</table>
<BR>
<BR>
<table border="1" width="95%" align=center>
  <tr>
			<%
				IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
				Else
					strTemp = "<TD class=dark_cell colspan=10>"
					Response.Write(strTemp)
					'Response.Write("getStyle == " &Session("getstyle"))
					Session("RSStyStruct").MoveFirst
					DO While Not Session("RSStyStruct").Eof
						IF Len(Session("getstyle"))=0 Then
							strValue = ""
						Else
							strValue =  Session(Trim(Session("RSStyStruct").fields("cisegsdes")))
						End IF
					'	Response.Write  "cisegsdes  ==  " & strValue
						strTemp = Trim(Session("RSStyStruct").fields("cisegsdes")) & "<INPUT name='" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "' size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & strValue & ">"
					 	strTemp = strTemp & " " & Trim(Session("RSStyStruct").fields("cisegsepr")) & " "
						Response.Write(strTemp)
						Session("RSStyStruct").MoveNext
					Loop
				End IF
			%>

		  <INPUT id=button5 name=button2  type="submit" value="Get Style">
		</TD>
		</TR>
</form>
		<TR>
		<TD  colSpan=1 class="dark_cell">Description</TD>
		<TD  colSpan=9 class="dark_cell"><%=Session("LongDesc")%>&nbsp;</TD>
  </tr>
</FORM>

<%
Set RSOTS = server.CreateObject("ADODB.recordset")
strSql="select * from style where style='" & Session("getstyle") & "'"
RSOTS.open strSql,conn 
Dim intOTS ' as integer

IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
	Set RSScale = server.CreateObject("ADODB.recordset")
	strSql="select * from scale where type='S' And scale='" & RSOTS("scale") & "'"
	RSScale.open strSql,conn 
End IF
%>

<%intScaleCnt = cdbl("0")%>
<FORM action="catupdateline.asp" id=FORM2 method=post name=FORM2 onsubmit="return FrontPage_Form2_Validator(this)" >
  <input  type='hidden' name='txttemp2' id='txttemp2'>
  
  <tr>
    <TD width="10%" class="dark_cell" ></TD>
    <TD width="10%" class="dark_cell" align=right>
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz1"))
					intScaleCnt = cdbl(RSScale("cnt"))
				Else
					Response.Write("Size1")
				End IF
			%></TD>
    <TD width="10%" class="dark_cell" align=right>
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz2"))
				Else
					Response.Write("Size2")
				End IF
			%></TD>
    <TD width="10%" class="dark_cell" align=right>
       <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz3"))
				Else
					Response.Write("Size3")
				End IF
			%>
      </TD>
    <TD width="10%" class="dark_cell" align=right>
        <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz4"))
				Else
					Response.Write("Size4")
				End IF
			%></TD>
    <TD  class="dark_cell" align=right>
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz5"))
				Else
					Response.Write("Size5")
				End IF
			%></TD>
    <TD class="dark_cell" align=right>
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz6"))
				Else
					Response.Write("Size6")
				End IF
			%></TD>
    <TD width="10%" class="dark_cell" align=right>
       <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz7"))
				Else
					Response.Write("Size7")
				End IF
			%></TD>
    <TD width="10%" class="dark_cell" align=right>
      <%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz8"))
				Else
					Response.Write("Size8")
				End IF
			%>
      </TD>
<%strTemp = "Disabled"%>
    <TD width="10%" class="dark_cell" align=right>Total</TD>
  </tr>
  <tr>
    <TD width="10%" class="dark_cell">Order</TD>
    <TD width="10%" class="dark_cell" align=right>
      <INPUT id=text3 name=txtord1 size="5" maxlength=5 value="<%=Session("text1")%>" <%IF intScaleCnt = 0 Then Response.Write(strTemp) End IF%>></TD>
    <TD  class="dark_cell" align=right>
      <INPUT id=text3 name=txtord2 size="5" maxlength=5  value="<%=Session("text2")%>" <%IF intScaleCnt<2   Then Response.Write(strTemp) End IF%>></TD>
    <TD width="10%" class="dark_cell" align=right>
      <INPUT id=text3 name=txtord3 size="5" maxlength=5 value="<%=Session("text3")%>" <%IF intScaleCnt<3 Then Response.Write(strTemp) End IF%>></TD>
    <TD width=10% class="dark_cell" align=right>
      <INPUT id=text17 name=txtord4  value="<%=Session("text4")%>" <%IF intScaleCnt<4 Then Response.Write(strTemp) End IF%> size="5" maxlength=5></TD>
    <TD width=10% class="dark_cell" align=right>
      <INPUT id=text16 name=txtord5  value="<%=Session("text5")%>" <%IF intScaleCnt<5 Then Response.Write(strTemp) End IF%> size="5" maxlength=5></TD>
    <TD width="10%" class="dark_cell" align=right>
      <INPUT id=text15 name=txtord6  value="<%=Session("text6")%>" <%IF intScaleCnt<6 Then Response.Write(strTemp) End IF%> size="5" maxlength=5></TD>
    <TD width=10% class="dark_cell" align=right>
      <INPUT id=text14 name=txtord7  value="<%=Session("text7")%>" <%IF intScaleCnt<7 Then Response.Write(strTemp) End IF%> size="5" maxlength=5></TD>
    <TD width="10%" class="dark_cell" align=right>
      <INPUT id=text3 name=txtord8  value="<%=Session("text8")%>" <%IF intScaleCnt<8 Then Response.Write(strTemp) End IF%> size="5" maxlength=5></TD>
    <TD width="10%" class="dark_cell"  align=right>
      <!--<INPUT id=text3 name=txtord 
      style="HEIGHT: 22px; LEFT: 18px; TOP: 2px; WIDTH: 55px" size="20" disabled>--></font></P></TD>
  </tr>
  <tr>
    <TD class="dark_cell">O.T.S.</TD>
    <TD class="dark_cell" align=right>
      <INPUT id=text3 name=txtots1 
      <%
		
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else
				
				intOTS = (cdbl(RSOTS("stk1")) + cdbl(RSOTS("wip1"))) - cdbl(RSOTS("ord1"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %> size="6" disabled></TD>
    <TD class="dark_cell" align=right>
      <P align=right><INPUT id=text3 name=txtots2 
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else
				intOTS = (cdbl(RSOTS("stk2")) + cdbl(RSOTS("wip2"))) - cdbl(RSOTS("ord2"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>

      size="6" disabled></TD>
    <TD class="dark_cell" align=right>
      <INPUT id=text3 name=txtots3 
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else
				
				intOTS = (cdbl(RSOTS("stk3")) + cdbl(RSOTS("wip3"))) - cdbl(RSOTS("ord3"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>

      size="6" disabled></TD>
    <TD class="dark_cell" align=right>
      <INPUT id=text3 name=txtots4 
            <%
		
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else
				
				intOTS = (cdbl(RSOTS("stk4")) + cdbl(RSOTS("wip4"))) - cdbl(RSOTS("ord4"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>

       size="6" disabled></TD>
    <TD class="dark_cell" align=right>
      <INPUT id=text3 name=txtots5 
            <%
		
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else
				
				intOTS = (cdbl(RSOTS("stk5")) + cdbl(RSOTS("wip5"))) - cdbl(RSOTS("ord5"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>

     size="6" disabled></TD>
    <TD class="dark_cell" align=right>
      <INPUT id=text3 name=txtots6 
            <%
		
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else
				
				intOTS = (cdbl(RSOTS("stk6")) + cdbl(RSOTS("wip6"))) - cdbl(RSOTS("ord6"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>

      size="6" disabled></TD>
    <TD class="dark_cell" align=right><INPUT id=text3 name=txtots7 
            <%
		
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else
				
				intOTS = (cdbl(RSOTS("stk7")) + cdbl(RSOTS("wip7"))) - cdbl(RSOTS("ord7"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>

      size="6" disabled></TD>
    <TD class="dark_cell" align=right><INPUT id=text3 name=txtots8 
            <%
	  IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else
				
				intOTS = (cdbl(RSOTS("stk8")) + cdbl(RSOTS("wip8"))) - cdbl(RSOTS("ord8"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>

      size="6" disabled></TD>
    <TD class="dark_cell" align=right> 
      <INPUT id=text12 name=txtots 
            <%
      IF  RSOTS.EOF  And RSOTS.BOF   Then
			Else
				
				intOTS = (cdbl(RSOTS("Totstk")) + cdbl(RSOTS("Totwip"))) - cdbl(RSOTS("Totord"))
				strTemp = "value =" & intOTS
				Response.Write(strTemp)
      END IF
      %>

      size="6" disabled></TD>
  </tr>
</table>
<TABLE border=0 cellPadding=1 cellSpacing=1  width="95%" align=center>
  
  <TR>
    <TD width="20%" style="WIDTH: 20%">
      <p align="left">&nbsp;</p></TD>
    <TD width="25%" colSpan=2 style="WIDTH: 25%"></TD>
    <TD width="15%"></TD>
    <TD width="20%" colSpan=2 align='right'>
				<%if Trim(Session("getstyle")) = "" then
					styleischoosed = "F"
				  else
					styleischoosed = "T"
				end if%>
				<input type="hidden" id=StyleChoosed name=StyleChoosed value="<%=styleischoosed%>">
				
				<INPUT id=button3 name=button3 style="HEIGHT: 24px; WIDTH: 96px" 
				type="submit" value="Update Line" onclick="Up_lines(this.form)"></FONT>&nbsp;

				<INPUT id=button2 name=button2 style="HEIGHT: 24px; WIDTH: 95px" 
				type="submit" value="Remove Line" onclick="Re_lines(this.form)"></TD>
  </TR>
    </TABLE>

<TABLE  border=0 cellPadding=1 cellSpacing=1 width="95%" align=center>
  
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
    

<TABLE border=0 cellPadding=1 cellSpacing=1  width="95%" align=center>
  
  <form id=form3 name=form3 action="saveord.asp" onsubmit="return checkformlines()">
  <input type='hidden' name='txttemp' id='txttemp' >
  
  <TR>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%">
      <p align="right"><FONT size=2>
      
				<INPUT id=button4 name=button3 style="HEIGHT: 24px; WIDTH: 96px" type="submit" value="Save Order" ></FONT></TD>
				<%if Session("RSLine").BOF and Session("RSLine").EOF then
					emptylinesflag="Y"
				  else
					emptylinesflag="N"
				  end if%>
				<INPUT type=hidden value="<%=emptylinesflag%>" name=islinesempty>
   </TR>
   </Form>
</TABLE>
 

      <table border="1" width="95%" align=center>
        
        <TR>
          <TD width="20%" class="dark_cell"><%=RSStruct("ciseghead")%>&nbsp;</TD>
          <TD width="35%" class="dark_cell">Description</TD>
          <TD width="15%" class="dark_cell">Qty.</TD>
   		  <TD width="15%" class="dark_cell" align=right>Price </TD>
          <TD width="15%" class="dark_cell" align=right>Amount </TD>
          
          </TR>
        
          <%

  
          IF Not Session("RSLine").EOF OR Not Session("RSLine").BOF   Then
          Session("RSLine").movefirst
          
					strTemp = "<input type=hidden name=save value=""T"">"
					Response.Write(strTemp)
						Do While Not Session("RSLine").EOF
							strTemp = "<TR>"
							Response.Write(strTemp)

							strTemp = "<TD  bgColor=#ffffff class=light_cell>" & "<A HREF=""catDispordline.asp?Style=" & Session("RSLine").fields("style") &  """>" & Session("RSLine").fields("style") & "</A></TD>"
							'strTemp = "<TD  bgColor=#ffffff><font size=""2"" color=""#000080"" face=""Arial"">" & Session("RSLine").fields("style") & "</font></TD>"
							Response.Write(strTemp)

							strTemp = "<TD  bgColor=#ffffff class=light_cell>" & Session("RSLine").fields("desc1") & "</TD>"
							Response.Write(strTemp)

							strTemp = "<TD  bgColor=#ffffff class=light_cell>" & Session("RSLine").fields("totqty") & "</TD>"
							Response.Write(strTemp)
							
							if Session("CurrencyAlign")="LEFT" then
								strTemp = "<TD  bgColor=#ffffff class=light_cell align=right>" & Session("Currency") & FormatNumber(Session("RSLine").fields("price")) & "</TD>"
							else
								strTemp = "<TD  bgColor=#ffffff class=light_cell align=right>" & FormatNumber(Session("RSLine").fields("price")) & Session("Currency") & "</TD>"							
							end if 	
							Response.Write(strTemp)

							intAmount = cdbl(Session("RSLine").fields("totqty")) * cdbl(Session("RSLine").fields("price"))
							if Session("CurrencyAlign")="LEFT" then
								strTemp = "<TD  bgColor=#ffffff class=light_cell align=right>" & Session("Currency") & FormatNumber(intAmount) & "</TD>"
							else
								strTemp = "<TD  bgColor=#ffffff class=light_cell align=right>" & FormatNumber(intAmount) & Session("Currency") & "</TD>"							
							end if 		
							Response.Write(strTemp)

							Session("RSLine").movenext

						Loop
					else
						strTemp = "<input type=hidden name=save value="""">"
						Response.Write(strTemp)
          End IF
          
          %>
</TABLE>
<P>&nbsp;</P>
</BODY>
</HTML>