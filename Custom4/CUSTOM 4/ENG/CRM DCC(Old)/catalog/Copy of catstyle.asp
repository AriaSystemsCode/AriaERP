<%@ Language=VBScript %>
<%
Response.Buffer=true
Response.CacheControl = "no-cache" 
Response.AddHeader "Pragma", "no-cache" 
Response.Expires = -1 
 

IF Trim(Session("ID")) = "" AND Trim(Session("rep"))= "" Then
'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
	</script>	
<%End if

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

set connTemp=server.createobject("ADODB.Connection")
set RSTemp=server.createobject("ADODB.Recordset")
connTemp.open Application("DataConnectionString")
sqlTemp="select * from customer where account='"&session("customerid")&"'"
RSTemp.open sqlTemp,connTemp,1,3
%>

<html>
<head>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/Catalog.css" TYPE="text/css">
<Title>CRM - Catalog</Title>
<script language="JavaScript">
function check()
{  
document.form1.temp1.value=document.form1.txtord1.value;
document.form1.temp2.value=document.form1.txtord2.value;
document.form1.temp3.value=document.form1.txtord3.value;
document.form1.temp4.value=document.form1.txtord4.value;
if (eval(document.form1.txtord5)) 
{
document.form1.temp5.value=document.form1.txtord5.value;
}
if (eval(document.form1.txtord6)) 
{
document.form1.temp6.value=document.form1.txtord6.value;
}
if (eval(document.form1.txtord7)) 
{
document.form1.temp7.value=document.form1.txtord7.value;
}
if (eval(document.form1.txtord8)) 
{
document.form1.temp8.value=document.form1.txtord8.value;
}
	if (isNaN(document.form1.temp1.value))
		{
		alert ("Please enter character digits only in the quantity field!")
		document.form1.txtord1.focus();
		return false
		}
	if (isNaN(document.form1.temp2.value))
		{
		alert ("Please enter character digits only in the quantity field!")
		document.form1.txtord2.focus();
		return false
		}
		if (isNaN(document.form1.temp3.value))
		{
		alert ("Please enter character digits only in the quantity field!")
		document.form1.txtord3.focus();
		return false
		}
	
	if (isNaN(document.form1.temp4.value))
		{
		alert ("Please enter character digits only in the quantity field!")
	  document.form1.txtord4.focus();
		return false
		}

	if (isNaN(document.form1.temp5.value))
	{
		alert ("Please enter character digits only in the quantity field!")
		if (eval(document.form1.txtord5)) 
		{	
		document.form1.txtord5.focus();
		return false
		}
	}

	if (isNaN(document.form1.temp6.value))
	{	alert ("Please enter character digits only in the quantity field!")
		if (eval(document.form1.txtord6)) 
		{	
		document.form1.txtord6.focus();
		return false
		}
		
	}


	if (isNaN(document.form1.temp7.value))
	{	alert ("Please enter character digits only in the quantity field!")
		if (eval(document.form1.txtord7)) 
		{	
		document.form1.txtord7.focus();
		return false
		}
		
	}

	
	if (isNaN(document.form1.temp8.value))
		{alert ("Please enter character digits only in the quantity field!")
		if (eval(document.form1.txtord8)) 
		{	
		document.form1.txtord8.focus();
		return false
		}
		
	}


}
</script>
</head>
<body >

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
	<TD align=right><a href="repcust.asp"><b>Get Customer</b></a></TD>
	<!-- ARD -->
</TR>
</table>
 <%Set RSTemp=nothing
  ConnTemp.close
  Set ConnTemp=nothing
  End IF%>
<Table width=95% height=50 align=center border=1>
<TR>
<TD class=title>Catalog</TD>
</TR>

<form name="form1" id="form1" method="post" action="catredirect.asp">
<%
Dim Conn
Dim RSStyle
Dim strsql 'as string

Set Conn = server.CreateObject ("ADODB.CONNECTION")
conn.Open Application("DataConnectionString")


set RSMajor = Server.CreateObject("ADODB.RecordSet")
strsql = "select distinct style from spck_lin where style like '" & request("style") & "%'"
RSMajor.Open strsql,conn 


Set RSStyle = Server.CreateObject("ADODB.RecordSet")
strsql = "select * from style where cstymajor='" & request("style") & "'"
RSStyle.Open strsql,conn

Set RSStyStr = server.CreateObject("ADODB.RECORDSET")
	strsql = "select * from icistru where citemrecty='U' and lsegendmaj=.T."
	RSStyStr.Open strsql,Conn,2,4

	Dim intCount
	intCount = 0
	
	Do while Not RSStyStr.Eof
		intCount = intCount + cdbl(RSStyStr("nisegsize")) + 1
		RSStyStr.MoveNext 
	Loop
	intCount = intCount - 1

%>
<Table width=95% align=center><TR><TD align=center>
<font Face="Arial" color=#000080><b>
<br>
<img border="0" src="../styimg/<%=(Trim(request("style")))%>.jpg">
<p><%=Response.Write(RSStyle("desc"))%></p>

<p>Price: <%
'Ard,301633 [Start]
Set SYConn = server.CreateObject("ADODB.Connection")
SYConn.Open Application("SystemConnectionString")

Set RScomp = server.CreateObject("ADODB.Recordset")
strsql = "select ccont_code from syccomp where ccomp_id='" & Session("CompanyID") & "'"
RScomp.Open strsql,SYConn
IF Not(RScomp.EOF and RScomp.BOF) Then
	Set RSCurSign = server.CreateObject("ADODB.Recordset")
	strsql = "select * from sycint where ccont_code='" & RScomp.Fields("ccont_code").Value & "'"
	RSCurSign.Open strsql,SYConn
	if Not(RSCurSign.EOF and RSCurSign.BOF) Then
		Response.Write(RSCurSign.Fields("ccurrencyi").Value & " ")
	End IF
End IF
Response.Write(getstyprice(Trim(RSStyle("style")),1))
'=Response.Write(RSStyle("pricea"))
'Ard,301633 [End]
%>
<select size="1" name="lststyle">
<%
'ARD - Catalog Non-major Inf [Start]
	Set RSColorDsc = server.CreateObject("ADODB.Recordset")
	strsql = "SELECT * FROM CODES WHERE CDEFCODE='N' AND CFLD_NAME='COLOR' AND CRLTFIELD='N'"
	RSColorDsc.Open strsql,Conn 

	Select Case session("M_COLINFO")
		Case "B"
			do while not RSMajor.EOF
				RSColorDsc.Filter = "ccode_no='" & Trim(mid(trim(RSMajor("style")),intCount+2)) & "'"
				IF request("color") = Trim(mid(trim(RSMajor("style")),intCount+2)) Then
					strthisStyle = trim(RSMajor("style"))
					Response.Write("<option value=""" & trim(RSMajor("style")) & """ selected>" & Trim(mid(trim(RSMajor("style")),intCount+2)) & " - " & RSColorDsc.Fields("cdiscrep").Value )
				Else
					Response.Write("<option value=""" & trim(RSMajor("style")) & """>" & Trim(mid(trim(RSMajor("style")),intCount+2)) & " - " & RSColorDsc.Fields("cdiscrep").Value )
				End IF
				RSMajor.MoveNext 
			loop

		Case "C"
			do while not RSMajor.EOF
				IF request("color") = Trim(mid(trim(RSMajor("style")),intCount+2)) Then
					strthisStyle = trim(RSMajor("style"))
					Response.Write("<option value=""" & trim(RSMajor("style")) & """ selected>" & Trim(mid(trim(RSMajor("style")),intCount+2)))
				Else
					Response.Write("<option value=""" & trim(RSMajor("style")) & """>" & Trim(mid(trim(RSMajor("style")),intCount+2)))
				End IF
				RSMajor.MoveNext 
			loop

		Case "N"
			do while not RSMajor.EOF
				RSColorDsc.Filter = "ccode_no='" & Trim(mid(trim(RSMajor("style")),intCount+2)) & "'"
				IF request("color") = Trim(mid(trim(RSMajor("style")),intCount+2)) Then
					strthisStyle = trim(RSMajor("style"))
					Response.Write("<option value=""" & trim(RSMajor("style")) & """ select>" & RSColorDsc.Fields("cdiscrep").Value)
				Else
					Response.Write("<option value=""" & trim(RSMajor("style")) & """>" & RSColorDsc.Fields("cdiscrep").Value)
				End IF
				RSMajor.MoveNext 
			loop
	End Select

'do while not RSMajor.EOF
'	Response.Write("<option value=""" & trim(RSMajor("style")) & """>" & Trim(mid(trim(RSMajor("style")),intCount+2)))
'	RSMajor.MoveNext 
'loop
'ARD - Catalog Non-major Inf [End]
%>
</select>
</p>

<%
dim StyScale

StyScale = RSStyle("scale")

Set RSScale = Server.CreateObject("ADODB.RecordSet")
strsql = "select * from scale where type='S' And scale='" & StyScale & "'"
RSScale.Open strsql,conn

SzCount = RSScale("cnt")

%>

</TD></TR></Table>

<table width="95%" border="1" align=center>
<tr>
	<%
	IF cdbl(SzCount) > 0 Then%>
	<td width="87" class="dark_cell"><font color="#000080" face="arial">
		
		<%
			Response.Write(RSScale("sz1") & "</font></td>")
		End IF

		IF cdbl(SzCount) > 1 Then %>
	<td width="86" class="dark_cell"><font color="#000080" face="arial">
		<%
			Response.Write(RSScale("sz2") & "</font></td>")
		End IF

		IF cdbl(SzCount) > 2 Then
		%>
	<td width="87" class="dark_cell"><font color="#000080" face="arial">
		<%
			Response.Write(RSScale("sz3") & "</font></td>")
		End IF

		IF cdbl(SzCount) > 3 Then
		%>
	<td width="85" class="dark_cell"><font color="#000080" face="arial">
		<%
			Response.Write(RSScale("sz4") & "</font></td>")
		End IF

		IF cdbl(SzCount) > 4 then
		%>
	<td width="86" class="dark_cell"><font color="#000080" face="arial">
		<%
			Response.Write(RSScale("sz5") & "</font></td>")
		End IF

		IF cdbl(SzCount) > 5 Then
		%>
	<td width="86" class="dark_cell"><font color="#000080" face="arial">
		<%
			Response.Write(RSScale("sz6") & "</font></td>")
		End IF

		IF cdbl(SzCount) > 6 Then
		%>
	<td width="86" class="dark_cell"><font color="#000080" face="arial">
		<%
			Response.Write(RSScale("sz7") & "</font></td>")
		End IF

		IF cdbl(SzCount) > 7 Then
		%>
	<td width="95" class="dark_cell"><font color="#000080" face="arial">
		<%
			Response.Write(RSScale("sz8") & "</font></td>")
		End IF
		%>
</tr>
<tr>

<%
For inti = 1 to Cdbl(SzCount)
	strTemp = "<TD class=dark_cell><INPUT name=txtord" & inti & " size=6 maxlength=6></TD>"
	Response.Write(strTemp)
%>
<%
Next 
%>

</tr>
</table>
<br>
<Table Width=95% align=center><TR><td align=center>
<input name="submit" value="Submit" type="submit" onclick="return check()">
<input name="reset" value="Reset" type="reset">
<input name="style" value="<%=request("style")%>" type="hidden">
<input type=hidden name=temp1 >
<input type=hidden name=temp2 >
<input type=hidden name=temp3 >
<input type=hidden name=temp4 >
<input type=hidden name=temp5 >
<input type=hidden name=temp6 >
<input type=hidden name=temp7 >
<input type=hidden name=temp8 >

</TD></TR></Table>
</form>
<br>

</body>

<%
'*!*************************************************************
'*! Name      : GetStyPrice
'*! Developer : Ahmed M. Reda
'*! Date      : 06/21/2001
'*! Purpose   : Get style Price if there a contract or not
'*!*************************************************************
'*! Calls       : None
'*!*************************************************************
'*! Passed Parameters : strStyle    : Style code
'*!                     Qty         : Quantity
'*!*************************************************************
'*! Return      : Style price
'*!*************************************************************


FUNCTION GetStyPrice(strStyle,Qty)
Dim StyConn
Dim RSStyOrdHdr
Set StyConn = server.CreateObject("ADODB.Connection")
StyConn.Open Application("DataConnectionString")
Set RSStyOrdHdr = server.CreateObject("ADODB.RECOrdSET")
strtempsql = ""
strtempsql = strtempsql & "SELECT ORDER FROM ORDHDR WHERE ACCOUNT='" & CUSTID & "' AND CORDTYPE='C'"
strtempsql = strtempsql & " and status <> 'X' and status <> 'B' and start >= {" & Date() & "} and complete <= {" & Date() & "}"

RSStyOrdHdr.open strtempsql,StyConn


GetStyPrice = 0
IF Not(RSStyOrdHdr.EOF And RSStyOrdHdr.BOF) Then ' Contract Exist
	Dim RSStyLine
	Set RSStyLine = server.CreateObject("adodb.recordset")
	strtempsql = ""
	strtempsql = strtempsql & "SELECT * FROM ORDLINE WHERE ORDER = '" & RSSTYORDHDR.FIELDS("ORDER").VALUE & "' and style='" & style & "'"
	RSStyLine.Open strtempsql,StyConn
	If Not(RSStyLine.EOF And RSStyLine.Bof)then 
		GetStyPrice = RSStyLine.Fields("Price").Value 
	End IF
Else ' Contract Not Exist
	strtempsql = ""
	strtempsql = strtempsql & "Select * from customer where type='M' and account='" & custid & "'"
	Set RSSTYCustomer = server.CreateObject("ADODB.Recordset")
	RSSTYCustomer.Open strtempsql,StyConn
	IF Not(RSSTYCustomer.EOF And RSSTYCustomer.BOF) Then
	
		strtempsql = "select * from style where style='" & strStyle & "'"
		'Response.Write(strtempsql)
		Set RSSTYStyle = server.CreateObject("adodb.recordset")
		RSSTYStyle.Open strtempsql,StyConn
	
		IF Not(RSSTYStyle.EOF And RSSTYStyle.BOF) Then
	
			Select Case RSSTYCustomer.Fields("pricelvl").Value 
			Case "A"
				GetStyPrice = RSSTYStyle.Fields("Pricea").Value 
			Case "B"
				GetStyPrice = RSSTYStyle.Fields("Priceb").Value 
			Case "C"
				GetStyPrice = RSSTYStyle.Fields("Pricec").Value 
			Case "Q"
				IF qty < RSSTYStyle.Fields("natqtyb").Value then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Pricea").Value)
				End if
				
				IF qty > RSSTYStyle.Fields("natqtyb").Value and qty < RSSTYStyle.Fields("natqtyc").Value then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Priceb").Value)
				End if
				
				IF qty > RSSTYStyle.Fields("natqtyc").Value then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Pricec").Value) 
				End if
			End select
		End IF
	End IF
End IF
'GetStyPrice = cint(GetStyPrice)
End Function


%>