<%@ Language=VBScript %>
<%
'Response.Buffer=true
Response.CacheControl = "no-cache" 
Response.AddHeader "Pragma", "no-cache" 
Response.Expires = -1 
 'Response.Write trim(application("WareCode"))
 'Response.End 
IF Trim(Session("ID")) = "" AND Trim(Session("rep"))= "" Then
'Response.redirect "../login.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
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
'First Get the price level for this customer
Session("PriceLvl")=GetCustPriceLvl(custid)
	
Dim Conn
set conn=server.createobject("ADODB.Connection")
conn.open Application("DataConnectionString")

Dim SqlConn
Set SqlConn = server.CreateObject("ADODB.Connection")
SqlConn.Open Application("SqlServer")

Set rsStyStr = server.CreateObject("ADODB.RECORDSET")
strsql = "select * from icistru where citemrecty='U' and lsegendmaj=.T."
rsStyStr.Open strsql,Conn,2,4
'NEK[Start] Get Record Count
Set rsStyCount = server.CreateObject("ADODB.RECORDSET")
'strsqlCount = "select count(*) as COUNT from icistru where citemrecty='U'"
strsqlCount = "select * from icistru where citemrecty='U'"
rsStyCount.Open strsqlCount,Conn,2,4
I = 0 
DO WHILE (NOT rsStyCount.EOF) 
	LSEGENDMAJ	=	rsStyCount.FIELDS("LSEGENDMAJ").Value
	'Response.Write "<FONT SIZE=3>LSEGENDMAJ" & LSEGENDMAJ & "</FONT><BR><BR>"
	I = I + 1
	IF LSEGENDMAJ = TRUE THEN EXIT DO  
rsStyCount.MoveNext 
LOOP
MAJOR = I  

'Response.Write "<FONT SIZE=3><br><br><br>MAJOR============== " & session("M_COLINFO")& session("colorSize")&session("styleSize") & "</FONT>"
'NEK[End]
'session("M_COLINFO") = "N"
Dim intCount
intCount = 0
	
Do while Not rsStyStr.Eof
	intCount = intCount + cdbl(RSStyStr("nisegsize")) + 1
	rsStyStr.MoveNext 
Loop
intCount = intCount - 1

%>

<html>
<head>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/Catalog.css" TYPE="text/css">
<Title>CRM - <%=trim(Session("CatalogField"))%></Title>
<script language="JavaScript">
function check()
{  
	if (eval(document.form1.txtord1).value != '') 
	{
		document.form1.temp1.value=document.form1.txtord1.value;
	}

	if (eval(document.form1.txtord2).value != '') 
	{
		document.form1.temp2.value=document.form1.txtord2.value;
	}

	if (eval(document.form1.txtord3).value != '') 
	{
		document.form1.temp3.value=document.form1.txtord3.value;
	}
	if (eval(document.form1.txtord4).value != '') 
	{
		document.form1.temp4.value=document.form1.txtord4.value;
	}

	if (eval(document.form1.txtord5).value != '') 
	{
		document.form1.temp5.value=document.form1.txtord5.value;
	}
	if (eval(document.form1.txtord6).value != '') 
	{
		document.form1.temp6.value=document.form1.txtord6.value;
	}
	if (eval(document.form1.txtord7).value != '') 
	{
		document.form1.temp7.value=document.form1.txtord7.value;
	}
	if (eval(document.form1.txtord8).value != '') 
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
function chkQty(ctrl,val)
{
	val = "document.form1.txtord" + val;
	//alert(val);
	if (isNaN(ctrl.value))
	{
		alert ("Please enter numbers only in the quantity field!");
		//eval(val).focus;
		return false;
	}
}
</script>
<SCRIPT LANGUAGE=javascript>
<!--

function hideloadingmsg() 

{

	document.all.loadingmsg.style.display = 'none';
	document.all.loadingmsg.style.visibility = 'hidden';
	
}
function getColor(val)
{
	var val;
	//window.alert(val);
	document.form1.action = 'catstyle.asp?color=' + val + '&PageID=<%=Request.QueryString ("PageID")%>&style=<%=trim(Request.QueryString("style"))%>&GRP=<%=trim(Request.QueryString("GRP"))%>'
	document.form1.submit();
}
//-->

</SCRIPT>
</head>
<body onLoad="hideloadingmsg();">
<div name='loadingmsg' id='loadingmsg' align=center style='display:inline;visibility:visible'>
	<p align=center>
		<A align=center style="font-family=verdana;font-size=12px;font-weight=bold;"><font color=red>Please standby, ..Loading data from server.</font></A>
	</p>
</DIV>


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
	<P>Your currently selected <%=session("CustField")%> is <b><%=Session("customerid")%> - <%=Session("rscust")("btname")%></b></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>
<%End IF%>
<Table width=95% height=50 align=center border=1>
<TR>
<TD class=title><%=trim(Session("CatalogField"))%></TD>
</TR>
<Font size=3>
<form name="form1" id="form1" method="post" action="catredirect.asp?PageID=<%=Request.QueryString ("PageID")%>">
<%
dim cnnSQL, rsStyGroup
set cnnSQL  = server.CreateObject ("Adodb.connection")
cnnSQL.Open Application("SqlServer") 

'ARD - To view if it's selected for Groups[Start]
DIM strStyMajor
strStyMajor = Request.QueryString("style")

'Response.Write "M_STYVIEW == " & strStyMajor
IF Session("M_STYVIEW") = "G" Then
	Dim rsStyles ' As ADODB.recordset
	Set rsStyles = server.CreateObject("ADODB.recordset")
	'strsql = "SELECT DISTINCT * FROM STYLE WHERE STYLE LIKE '" & trim(strStyMajor) & "%' AND CSTYGROUP='" & Request.QueryString("GRP") & "'"
	'wal_check if the user has a style profile then get the styles he has only
	if  trim(session("styProfile"))<> "" then
		strsql = "select distinct FOXStyle.* from styleprofiledetail , OPENROWSET('MSDASQL', '"
		strSQL = strSQL + Application("DataConnectionString")		
		'wael
		'strSQL = strSQL + "', 'Select * From  style WHERE cstymajor LIKE """ & Trim(strStyMajor) & "%"" AND CSTYGROUP=""" & trim(Request.QueryString("GRP")) & """ order by style') FOXStyle where cStyleProfileCode='"&trim(session("styProfile"))&"' AND cStyleGROUP='" & trim(Request.QueryString("GRP")) & "' And cstyle LIKE '" & Trim(strStyMajor) & "%' and FOXStyle.style = styleprofiledetail.cStyle COLLATE Latin1_General_CI_AS"
        strSQL = strSQL + "', 'Select * From  style WHERE cstymajor = """ & strStyMajor & """ AND CSTYGROUP=""" & trim(Request.QueryString("GRP")) & """ order by style') FOXStyle where cStyleProfileCode='"&trim(session("styProfile"))&"' AND cStyleGROUP='" & trim(Request.QueryString("GRP")) & "' And cstyle LIKE '" & Trim(strStyMajor) & "%' and FOXStyle.style = styleprofiledetail.cStyle COLLATE Latin1_General_CI_AS"
        'wael
		'strsql = "SELECT DISTINCT cstyle as cstyMajor,cstylegroup as cstyGroup FROM StyleProfileDetail WHERE cstyle LIKE '" & Trim(strStyMajor) & "%' AND CSTYleGROUP='" & trim(Request.QueryString("GRP")) & "' And cStyleProfileCode = '"& trim(session("styProfile"))&"'order by cstyle"
		rsStyles.Open strsql,cnnSQL
	else
	    'wael
		'strsql = "SELECT DISTINCT * FROM STYLE WHERE cstymajor LIKE '" & Trim(strStyMajor) & "%' AND CSTYGROUP='" & trim(Request.QueryString("GRP")) & "' order by style"
		strsql = "SELECT DISTINCT * FROM STYLE WHERE cstymajor = '" & strStyMajor & "' AND CSTYGROUP='" & trim(Request.QueryString("GRP")) & "' order by style"
		'wael
		rsStyles.Open strsql,conn
	end if
	if not rsStyles.eof then
		rsStyles.MoveFirst
	end if
	'Response.Write "strsql ===" & Application("DataConnectionString")'&strsql & rsStyles.recordcount
	'Response.End 
%>
	<Table width=95% align=center cellspacing=0 cellpadding=0><TR><TD align=center>
	<b><br><img border="0" src="../styimg/<%=(Trim(request("style")))%>.jpg">
	<p><%=Response.Write(Trim(rsStyles.Fields("cstymajor").Value))%> - <%=Response.Write(Trim(rsStyles.Fields("desc1").Value))%></p>
	<p>Price:
	<%
	if session("PriceCode")="" then'no price
		if Session("CurrencyAlign")="LEFT" then
			Response.Write(Session("Currency") & FormatNumber(getstyprice(Trim(rsStyles.Fields("Style").Value),1)))
		else
			Response.Write(FormatNumber(getstyprice(Trim(rsStyles.Fields("Style").Value),1)) & Session("Currency"))
		end if 
	else
		if Session("CurrencyAlign")="LEFT" then
			Response.Write(Session("Currency") & FormatNumber(GetPriceCode(trim(rsStyles("style")),session("PriceCode"))))
		else
			Response.Write(FormatNumber(GetPriceCode(trim(rsStyles("style")),session("PriceCode"))) & Session("Currency"))
		end if 
	end if	%>
	<select size="1" name="lststyle" onchange="getColor(this.value);">
	<%
	'ARD - Catalog Non-major Inf [Start]
		Set rsColorDsc = server.CreateObject("ADODB.Recordset")
		strsql = "SELECT DISTINCT * FROM CODES WHERE CDEFCODE+CRLTFIELD+CFLD_NAME='NNCOLOR'"
		rsColorDsc.Open strsql,Conn ,1,3
		dim strColor
		strColor = ""
		Select Case session("M_COLINFO")
			Case "B":
				do while not rsStyles.EOF 
					if cint(MAJOR) >= 2 then ' THe Major is Divided into more than one sector
						'Response.Write "<option>"&Trim(mid(trim(rsStyles.Fields("style").Value),len(trim(rsStyles.Fields("style").Value))- intCount))&"</option>"					
						rsColorDsc.Filter = "ccode_no='" & Trim(MID(trim(rsStyles.Fields("style").Value),INSTRREV(trim(rsStyles.Fields("style").Value) , "-")+1,LEN(trim(rsStyles.Fields("style").Value))))  & "'"
						if trim(rsStyles.Fields("style").Value) = trim(Request.Form ("lststyle")) then
							strSelected = "selected"
						else
							strSelected = ""
						end if
						'Response.write "<option> ccode_no='" & Trim(mid(trim(rsStyles.Fields("style").Value),INSTRREV(trim(rsStyles.Fields("style").Value)),"-")) & "'" & "</option>"
						'Response.write "<option>" & rsColorDsc.recordcount & "</option>"
						if rsColorDsc.RecordCount > 0 then
							'check if its a diff color then display
							if strColor = RSColorDsc.Fields("cdiscrep") then
							else
								strColor = RSColorDsc.Fields("cdiscrep")
								response.Write("<option value=""" & trim(rsStyles.Fields("style").Value) & """>" & Trim(MID(trim(rsStyles.Fields("style").Value),INSTRREV(trim(rsStyles.Fields("style").Value) , "-")+1,LEN(trim(rsStyles.Fields("style").Value)))) & " - " & RSColorDsc.Fields("cdiscrep").Value )
							end if
						end if 	
					else
						'Response.Write "<option >IN ELSE</option>"					
						'Response.Write "<option>"&Trim(mid(trim(rsStyles.Fields("style").Value),intCount+2))&"</option>"					
						rsColorDsc.Filter = "ccode_no='" & Trim(mid(trim(rsStyles.Fields("style").Value),session("StyleSize")+2,session("colorSize"))) & "'"
						if trim(rsStyles.Fields("style").Value) = trim(Request.Form ("lststyle")) then
							strSelected = "selected"
						else
							strSelected = ""
						end if
						if rsColorDsc.RecordCount > 0 then
							'check if its a diff color then display
							if strColor = RSColorDsc.Fields("cdiscrep") then
							else
								strColor = RSColorDsc.Fields("cdiscrep")
								'response.Write("<option value=""" & trim(rsStyles.Fields("style").Value) & """>" & Trim(MID(trim(rsStyles.Fields("style").Value),INSTRREV(trim(rsStyles.Fields("style").Value) , "-")+1,LEN(trim(rsStyles.Fields("style").Value)))) & " - " & RSColorDsc.Fields("cdiscrep").Value )
								Response.Write("<option value=""" & trim(rsStyles.Fields("style").Value) & """ "& strSelected &">" & Trim(mid(trim(rsStyles.Fields("style").Value),session("styleSize")+2,session("colorSize"))) & " - " & RSColorDsc.Fields("cdiscrep").Value )
							end if
						end if 	
						'Response.Write "<option>"&rsColorDsc.RecordCount &"</option>"					
						'if OldCode <> Trim(mid(trim(rsStyles.Fields("style").Value),session("styleSize")+2,session("colorSize"))) then
							'OldCode = Trim(mid(trim(rsStyles.Fields("style").Value),session("styleSize")+2,session("colorSize")))
							'Response.Write("<option value=""" & trim(rsStyles.Fields("style").Value) & """ "& strSelected &">" & Trim(mid(trim(rsStyles.Fields("style").Value),session("styleSize")+2,session("colorSize"))) & " - " & RSColorDsc.Fields("cdiscrep").Value )
						'end if	
					end if
					rsStyles.MoveNext 
				loop

			Case "C":
				do while not rsStyles.EOF
					if trim(rsStyles.Fields("style").Value) = trim(Request.Form ("lststyle")) then
						strSelected = "selected"
					else
						strSelected = ""
					end if
					if OldCode <> Trim(mid(trim(rsStyles.Fields("style").Value),session("styleSize")+2,session("colorSize"))) then
						OldCode = Trim(mid(trim(rsStyles.Fields("style").Value),session("styleSize")+2,session("colorSize")))
						Response.Write("<option value=""" & trim(rsStyles.Fields("style").Value) & """ "& strSelected &">" & Trim(mid(trim(rsStyles.Fields("style").Value),session("styleSize")+2,session("colorSize"))))
					end if	
					
					rsStyles.MoveNext 
				loop

			Case "N":
				do while not rsStyles.EOF
					rsColorDsc.Filter = "ccode_no='" & Trim(mid(trim(rsStyles.Fields("style").Value),session("styleSize")+2,session("colorSize"))) & "'"
					if trim(rsStyles.Fields("style").Value) = trim(Request.Form ("lststyle")) then
						strSelected = "selected"
					else
						strSelected = ""
					end if
					if rsColorDsc.RecordCount > 0 then
					'check if its a diff color then display
							if strColor = RSColorDsc.Fields("cdiscrep") then
							else
								strColor = RSColorDsc.Fields("cdiscrep")
								Response.Write("<option value=""" & trim(rsStyles.Fields("style").Value) & """ "& strSelected &">" & RSColorDsc.Fields("cdiscrep").Value)
							end if
					end if
					rsStyles.MoveNext 
				loop
		End Select
	rsStyles.MoveFirst 
	'dim StyScale

	'if session("M_COLINFO") = "C" and Request.QueryString("color") <>"" then
	'	StyScale = Right(Request.QueryString ("color"),3)
	'else
	'	StyScale = rsStyles.Fields("scale").Value 
'	end if
	

	
	'Set RSScale = Server.CreateObject("ADODB.RecordSet")
	'strsql = "SELECT * FROM SCALE WHERE type+scale+prepak like 'S"&trim(StyScale)&"%'"
	'RSScale.Open strsql,conn,1,3
	'SzCount = RSScale("cnt")
	'Response.write "<option>" & SzCount & "</option>"
	'Response.End 
	'ARD - Catalog Non-major Inf [End]
 %>
	</select></p>
	</TD></TR>
	</Table>
<%End IF
'ARD - To view if it's selected for Groups[End]
%>

<%
'ARD - To view if it's selected for Packs[Start]
'Response.Write "<font size=3>M_STYVIEW== " & Session("M_STYVIEW") & "</font>"
IF Session("M_STYVIEW") = "P" Then
	Set rsStyles = server.CreateObject("ADODB.recordset")
	if trim(session("styProfile"))<> "" then
		strsql = "select distinct FOXStyle.* from styleprofiledetail , OPENROWSET('MSDASQL', '"
		strSQL = strSQL + Application("DataConnectionString")		
		'wael
		'strSQL = strSQL + "', 'Select * From  style WHERE cstymajor LIKE """ & Trim(strStyMajor) & "%"" AND CSTYGROUP=""" & trim(Request.QueryString("GRP")) & """ order by style') FOXStyle where cStyleProfileCode='"&trim(session("styProfile"))&"' AND cStyleGROUP='" & trim(Request.QueryString("GRP")) & "' And cstyle LIKE '" & Trim(strStyMajor) & "%' and FOXStyle.style = styleprofiledetail.cStyle COLLATE Latin1_General_CI_AS"
        strSQL = strSQL + "', 'Select * From  style WHERE cstymajor = """ & strStyMajor & """ AND CSTYGROUP=""" & trim(Request.QueryString("GRP")) & """ order by style') FOXStyle where cStyleProfileCode='"&trim(session("styProfile"))&"' AND cStyleGROUP='" & trim(Request.QueryString("GRP")) & "' And cstyle LIKE '" & Trim(strStyMajor) & "%' and FOXStyle.style = styleprofiledetail.cStyle COLLATE Latin1_General_CI_AS"
        'wael
		'strsql = "SELECT DISTINCT cstyle as cstyMajor,cstylegroup as cstyGroup FROM StyleProfileDetail WHERE cstyle LIKE '" & Trim(strStyMajor) & "%' AND CSTYleGROUP='" & trim(Request.QueryString("GRP")) & "' And cStyleProfileCode = '"& trim(session("styProfile"))&"'order by cstyle"
		rsStyles.Open strsql,cnnSQL
	
	else
		strsql = "SELECT DISTINCT * FROM STYLE WHERE STYLE LIKE '" & strStyMajor & "%'"
		rsStyles.Open strsql,conn
	end if
'	Response.Write "<font size=3> " & strsql & "</font>"
%>
	<Table width=95% align=center><TR><TD align=center>
	<b><br><img border="0" src="../styimg/<%=(Trim(request("style")))%>.jpg">
	<p><%=Response.Write(Trim(rsStyles.Fields("desc").Value))%></p>

	Price:<%
	if session("PriceCode")="" then'no price
		if Session("CurrencyAlign")="LEFT" then
			Response.Write(Session("Currency") & FormatNumber(getstyprice(Trim(rsStyles.Fields("Style").Value),1)))
		else
			Response.Write(FormatNumber(getstyprice(trim(rsStyles("style")),1)) & Session("Currency"))
		end if 
	else
		if Session("CurrencyAlign")="LEFT" then
			Response.Write(Session("Currency") & FormatNumber(GetPriceCode(trim(rsStyles("style")),session("PriceCode"))))
		else
			Response.Write(FormatNumber(GetPriceCode(trim(rsStyles("style")),session("PriceCode"))) & Session("Currency"))
		end if 
	end if	%>
	<select size="1" name="lststyle" onchange="getColor(this.value);">
	<%
	'ARD - Catalog Non-major Inf [Start]
		Set rsColorDsc = server.CreateObject("ADODB.Recordset")
		strsql = "SELECT * FROM CODES WHERE CDEFCODE+CRLTFIELD+CFLD_NAME='NNCOLOR'"
		rsColorDsc.Open strsql,Conn 

		Select Case session("M_COLINFO")
			Case "B"
				do while not rsStyles.EOF 
					rsColorDsc.Filter = "ccode_no='" & Trim(mid(trim(rsStyles.Fields("style").Value),session("styleSize")+2,session("colorSize"))) & "'"
					if trim(rsStyles.Fields("style").Value) = trim(Request.Form ("lststyle")) then
						strSelected = "selected"
					else
						strSelected = ""
					end if
					Response.Write("<option value=""" & trim(rsStyles.Fields("style").Value) & """ "& strSelected &">" & Trim(mid(trim(rsStyles.Fields("style").Value),session("styleSize")+2,session("colorSize"))) & " - " & RSColorDsc.Fields("cdiscrep").Value )
					rsStyles.MoveNext 
				loop

			Case "C"
				do while not rsStyles.EOF
					if trim(rsStyles.Fields("style").Value) = trim(Request.Form ("lststyle")) then
						strSelected = "selected"
					else
						strSelected = ""
					end if
					Response.Write("<option value=""" & trim(rsStyles.Fields("style").Value) & """ "& strSelected &">" & Trim(mid(trim(rsStyles.Fields("style").Value),session("styleSize")+2,session("colorSize"))))
					rsStyles.MoveNext 
				loop

			Case "N"
				do while not rsStyles.EOF
					rsColorDsc.Filter = "ccode_no='" & Trim(mid(trim(rsStyles.Fields("style").Value),session("styleSize")+2,session("colorSize"))) & "'"
					if trim(rsStyles.Fields("style").Value) = trim(Request.Form ("lststyle")) then
						strSelected = "selected"
					else
						strSelected = ""
					end if
					Response.Write("<option value=""" & trim(rsStyles.Fields("style").Value) & """ "& strSelected &">" & RSColorDsc.Fields("cdiscrep").Value)
					rsStyles.MoveNext 
				loop
		End Select

	'do while not RSMajor.EOF
	'	Response.Write("<option value=""" & trim(RSMajor("style")) & """>" & Trim(mid(trim(RSMajor("style")),intCount+2)))
	'	RSMajor.MoveNext 
	'loop
	rsStyles.MoveFirst 
	'dim StyScale

	'StyScale = rsStyles.Fields("scale").Value 
	'Set RSScale = Server.CreateObject("ADODB.RecordSet")
	'strsql = "SELECT * FROM SCALE WHERE TYPE+scale+prepak like 'S" & StyScale & "%'"
	'RSScale.Open strsql,conn
	'SzCount = RSScale("cnt")
	'ARD - Catalog Non-major Inf [End]
	%>
	</select>
	</TD></TR>
	</Table>
<%	
End IF
'ARD - To view if it's selected for Packs[End]
%>
<br><br>
<table width="70%" border="0" align=center>
<%'wal_add code to get the size scale to allow extend size scale[star]
'Dim rsSty ' As ADODB.recordset
'wal_039365 add a session recordset to hold the scale for the style majo selected
Set session("rsSty") = server.CreateObject("ADODB.recordset")
StyScale = ""
'strsql = "SELECT DISTINCT * FROM STYLE WHERE STYLE LIKE '" & trim(strStyMajor) & "%' AND CSTYGROUP='" & Request.QueryString("GRP") & "'"
if trim(session("styProfile"))<> "" then
		strsql = "select distinct FOXStyle.* from styleprofiledetail , OPENROWSET('MSDASQL', '"
		strSQL = strSQL + Application("DataConnectionString")		
		'wael
		'strSQL = strSQL + "', 'Select * From  style WHERE cstymajor LIKE """ & Trim(strStyMajor) & "%"" AND CSTYGROUP=""" & trim(Request.QueryString("GRP")) & """ order by style') FOXStyle where cStyleProfileCode='"&trim(session("styProfile"))&"' and FOXStyle.style = styleprofiledetail.cStyle COLLATE Latin1_General_CI_AS"
		strSQL = strSQL + "', 'Select * From  style WHERE cstymajor = """ & strStyMajor & """ AND CSTYGROUP=""" & trim(Request.QueryString("GRP")) & """ order by style') FOXStyle where cStyleProfileCode='"&trim(session("styProfile"))&"' and FOXStyle.style = styleprofiledetail.cStyle COLLATE Latin1_General_CI_AS"
		'wael

		'strsql = "SELECT DISTINCT cstyle as cstyMajor,cstylegroup as cstyGroup FROM StyleProfileDetail WHERE cstyle LIKE '" & Trim(strStyMajor) & "%' AND CSTYleGROUP='" & trim(Request.QueryString("GRP")) & "' And cStyleProfileCode = '"& trim(session("styProfile"))&"'order by cstyle"
		session("rsSty").Open strsql,cnnSQL,1,3
	
	else
	    'wael
		'strsql = "SELECT * FROM STYLE WHERE cstymajor LIKE '" & Trim(strStyMajor) & "%' AND CSTYGROUP='" & trim(Request.QueryString("GRP")) & "' order by style"
		strsql = "SELECT * FROM STYLE WHERE cstymajor = '" & strStyMajor & "' AND CSTYGROUP='" & trim(Request.QueryString("GRP")) & "' order by style"
		'wael
		session("rsSty").Open strsql,conn,1,3
	end if

'Response.Write strsql&rssty.RecordCount 
'Response.End 
Set RSScale = Server.CreateObject("ADODB.RecordSet")
do while not session("rsSty").EOF 
	'check if there is a different scale
	if StyScale = session("rsSty").Fields("scale").Value then
	else
	StyScale = session("rsSty").Fields("scale").Value 
	strsql = "SELECT * FROM SCALE WHERE TYPE+scale+prepak like 'S" & StyScale & "%'"
	RSScale.Open strsql,conn
	SzCount = RSScale("cnt")
%>
<tr>
	<TD class="dark_cell" width="50" >&nbsp;</TD>
	<%
	'IF cdbl(SzCount) > 0 Then%>
	<td align=right class="dark_cell">
		<%Response.Write(RSScale("sz1") & "</td>")
		'End IF
		'IF cdbl(SzCount) > 1 Then %>
	<td align=right class="dark_cell">
		<%
			Response.Write(RSScale("sz2") & "</td>")
		'End IF

		'IF cdbl(SzCount) > 2 Then
		%>
	<td  align=right class="dark_cell">
		<%
			Response.Write(RSScale("sz3") & "</td>")
		'End IF
'
		'IF cdbl(SzCount) > 3 Then
		%>
	<td align=right class="dark_cell">
		<%
			Response.Write(RSScale("sz4") & "</td>")
		'End IF

		'IF cdbl(SzCount) > 4 then
		%>
	<td align=right class="dark_cell"><%Response.Write(RSScale("sz5") & "</td>")
		'End IF

		'IF cdbl(SzCount) > 5 Then
		%>
	<td align=right class="dark_cell"><%Response.Write(RSScale("sz6") & "</td>")
		'End IF

		'IF cdbl(SzCount) > 6 Then
		%>
	<td align=right class="dark_cell"><%Response.Write(RSScale("sz7") & "</td>")
		'End IF

		'IF cdbl(SzCount) > 7 Then%>
	<td  align=right class="dark_cell"><%Response.Write(RSScale("sz8") & "</td>")
		'End IF
		%>
</tr>

<%
'WAL_calc of ots display[start]
Dim rsOtsChk
set rsOtsChk = server.CreateObject ("ADODB.Recordset")
if trim(Request.QueryString ("color")) = "" then
	strVal = session("rsSty").Fields("style")
else
	strVal = Request.QueryString ("color")
end if
'Response.Write strVal
 'wal_ add warehous in the condition if exists and select from stydye
if trim(session("WareCode")) = "" then
	strSQl = "Select (style.stk1 - sum(ordline.qty1)) as sum1, (style.stk2 - sum(ordline.qty2)) as sum2, "& _
		 "(style.stk3 - sum(ordline.qty3)) as sum3, (style.stk4 - sum(ordline.qty4)) as sum4, "& _ 
		 "(style.stk5 - sum(ordline.qty5)) as sum5, (style.stk6 - sum(ordline.qty6)) as sum6, "& _
		 "(style.stk7 - sum(ordline.qty7)) as sum7, (style.stk8 - sum(ordline.qty8)) as sum8, "& _
		 "(style.stk1 + style.wip1 - sum(ordline.qty1)) as sum11, (style.stk2 + style.wip2 - sum(ordline.qty2)) as sum22, "& _
		 "(style.stk3 + style.wip3 - sum(ordline.qty3)) as sum33, (style.stk4 + style.wip4 - sum(ordline.qty4)) as sum44, "& _ 
		 "(style.stk5 + style.wip5 - sum(ordline.qty5)) as sum55, (style.stk6 + style.wip6 - sum(ordline.qty6)) as sum66, "& _
		 "(style.stk7 + style.wip7 - sum(ordline.qty7)) as sum77, (style.stk8 + style.wip8 - sum(ordline.qty8)) as sum88, "& _
		 "style.make from ordline, ordHdr, style where ordline.style+DTOS(ordline.complete)+ordline.cordtype+ordline.order+ordline.store+STR(ordline.lineno,6) like '" &trim(strVal)& "%' "
		
		 strSQl  = strSQl & "and ordHdr.status <> 'C' and ordHdr.Status <> 'X' and ordline.order = ordHdr.order and ordline.style = style.style order by style.style"

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
				 "style.make from style where style like '" &trim(strVal)& "%' order by style"
				
		rsOtsChk.Open strSQL, conn,1 ,3
	end if
	if not rsOtsChk.eof and trim(strStyMajor) <> "" then
		Session("Make") = rsOtsChk("make")
		strFlag = "T"
	end if
else
	'wal_check if the record exists in the stydye
	dim rsChk
	set rsChk=server.CreateObject ("ADODB.recordset")
	rsChk.Open "select * from stydye where style like '" & strVal & "%' and stydye.CWARECODE = '"& trim(session("WareCode")) &"'",conn
	if rsChk.EOF then'style doesn't exists then display 0s
		strSQl = "Select (0 - sum(ordline.qty1)) as sum1, (0 - sum(ordline.qty2)) as sum2, "& _
		 "(0 - sum(ordline.qty3)) as sum3, (0 - sum(ordline.qty4)) as sum4, "& _ 
		 "(0 - sum(ordline.qty5)) as sum5, (0 - sum(ordline.qty6)) as sum6, "& _
		 "(0 - sum(ordline.qty7)) as sum7, (0 - sum(ordline.qty8)) as sum8, "& _
		 "(0 - sum(ordline.qty1)) as sum11, (0 - sum(ordline.qty2)) as sum22, "& _
		 "(0 - sum(ordline.qty3)) as sum33, (0 - sum(ordline.qty4)) as sum44, "& _ 
		 "(0 - sum(ordline.qty5)) as sum55, (0 - sum(ordline.qty6)) as sum66, "& _
		 "(0 - sum(ordline.qty7)) as sum77, (0 - sum(ordline.qty8)) as sum88 "& _
		 " from ordline, ordHdr where ordline.style+DTOS(ordline.complete)+ordline.cordtype+ordline.order+ordline.store+STR(ordline.lineno,6) like '" & strVal & "%' "
		 
		 strSQl = strSQl & "and ordHdr.CWARECODE = '"& trim(session("WareCode")) &"' "
		 
		 strSQl  = strSQl & "and ordHdr.status <> 'C' and ordHdr.Status <> 'X' and ordline.order = ordHdr.order"

		rsOtsChk.Open strSQL, conn
	'Response.Write "<font size=2>"& strSQL
	'Response.End 
		if rsOtsChk.EOF then'no orders for this stydye then get value in stock
			'rsOtsChk.Close ()
		end if
	else
		strSQl = "Select (stydye.stk1 - sum(ordline.qty1)) as sum1, (stydye.stk2 - sum(ordline.qty2)) as sum2, "& _
		 "(stydye.stk3 - sum(ordline.qty3)) as sum3, (stydye.stk4 - sum(ordline.qty4)) as sum4, "& _ 
		 "(stydye.stk5 - sum(ordline.qty5)) as sum5, (stydye.stk6 - sum(ordline.qty6)) as sum6, "& _
		 "(stydye.stk7 - sum(ordline.qty7)) as sum7, (stydye.stk8 - sum(ordline.qty8)) as sum8, "& _
		 "(stydye.stk1 + stydye.wip1 - sum(ordline.qty1)) as sum11, (stydye.stk2 + stydye.wip2 - sum(ordline.qty2)) as sum22, "& _
		 "(stydye.stk3 + stydye.wip3 - sum(ordline.qty3)) as sum33, (stydye.stk4 + stydye.wip4 - sum(ordline.qty4)) as sum44, "& _ 
		 "(stydye.stk5 + stydye.wip5 - sum(ordline.qty5)) as sum55, (stydye.stk6 + stydye.wip6 - sum(ordline.qty6)) as sum66, "& _
		 "(stydye.stk7 + stydye.wip7 - sum(ordline.qty7)) as sum77, (stydye.stk8 + stydye.wip8 - sum(ordline.qty8)) as sum88 "& _
		 " from ordline, ordHdr, stydye where ordline.style+DTOS(ordline.complete)+ordline.cordtype+ordline.order+ordline.store+STR(ordline.lineno,6) like '" &trim(strVal)& "%' "
		 
		 strSQl = strSQl & "and stydye.CWARECODE = '"& trim(session("WareCode")) &"' "
		 strSQl = strSQl & "and ordHdr.CWARECODE = '"& trim(session("WareCode")) &"' "
		 strSQl  = strSQl & "and ordHdr.status <> 'C' and ordHdr.Status <> 'X' and ordline.order = ordHdr.order and ordline.style = stydye.style order by stydye.style"

		rsOtsChk.Open strSQL, conn
	'Response.Write "<font size=2>"& strSQL
	'Response.End 
		if rsOtsChk.EOF then'no orders for this stydye then get value in stock
			rsOtsChk.Close ()
			strSQl = "Select stydye.stk1 as sum1, stydye.stk2 as sum2, "& _
					 "stydye.stk3 as sum3, stydye.stk4 as sum4, "& _ 
					 "stydye.stk5 as sum5, stydye.stk6 as sum6, "& _
					 "stydye.stk7 as sum7, stydye.stk8 as sum8, "& _
					 "sum(stydye.stk1 + stydye.wip1) as sum11, sum(stydye.stk2 + stydye.wip2) as sum22, "& _
					 "sum(stydye.stk3 + stydye.wip3) as sum33, sum(stydye.stk4 + stydye.wip4) as sum44, "& _ 
					 "sum(stydye.stk5 + stydye.wip5) as sum55, sum(stydye.stk6 + stydye.wip6) as sum66, "& _
					 "sum(stydye.stk7 + stydye.wip7) as sum77, sum(stydye.stk8 + stydye.wip8) as sum88 "& _
					 " from stydye where style like '" &trim(strVal)& "%' and stydye.CWARECODE = '"& trim(session("WareCode")) &"' order by style"
					
			rsOtsChk.Open strSQL, conn,1 ,3
		end if
	end if
	
	if not rsOtsChk.eof and trim(strStyMajor) <> "" then
		'Session("Make") = rsOtsChk("make")
		strFlag = "T"
	end if
end if
'Response.Write "<font size=2>"& strSQL&rsOtsChk.RecordCount 'trim(application("WareHous"))''rsOtsChk.RecordCount 

%>
<TR>
<TD class="dark_cell" width="50" ><%=trim(RSScale("cDim1"))%></TD>
<%
For inti = 1 to 8'Cdbl(SzCount)
	'check if there ia a size else disable the text
	if trim(RSScale("sz"&inti)) <> "" then
		strTemp = "<TD class=light_cell align=right><INPUT type=text name=txtord" & trim(RSScale("cDim1")) & trim(RSScale("sz"&inti)) & " style='TEXT-ALIGN: right' size=10 maxlength=5 onChange=""chkQty(this,'" & trim(RSScale("sz"&inti)) & "');""></TD>"
	else
		strTemp = "<TD class=light_cell align=right><INPUT type=text name=txtord" & trim(RSScale("cDim1")) & trim(RSScale("sz"&inti)) & " disabled style='TEXT-ALIGN: right' size=10 maxlength=5></TD>"
	end if
	Response.Write(strTemp)
%>
<%
Next 
%>
</TR>

<%If Session("ShowOTS")= "F" then'wma%>
	<TD class="dark_cell" width="50" >Availability</TD>
	<%'check to display avalability or not
	If strFlag = "T" then
		For i = 1 to Cdbl(SzCount)%>
			<%if cdbl(rsOtsChk("sum"&i)) > 0 then%>
				<TD class="light_cell" style="background-color: green">
				<input type=hidden name="txtType<%=trim(RSScale("sz"&i))%>" value="green">
				&nbsp;
			<%elseif cdbl(rsOtsChk("sum"&i&i)) > 0 then%>
				<TD class="light_cell" style="background-color: orange">
				<input type=hidden name="txtType<%=trim(RSScale("sz"&i))%>" value="orange">
				&nbsp;
			<%else%>
			 	<TD class="light_cell" align=center style="background-color: red">
			 	&nbsp;
			<%end if%>
			
	<%
		Next
	End if 
	%>
<%end if'wma%>

<%If Session("ShowOTS")= "T" then'wma%>
	<tr>
	<TD class="dark_cell" width="50" ><%=Session("OTSField")%></TD>
	<%'OTS View Stock + WIP - SO
	If strFlag = "T" then
		For i = 1 to 8'Cdbl(SzCount)
			'wal_130522 check if to show Immdeiate OTS or OTS+WIP
			if session("ImmOTS") = "T" then%>	
				<TD class="light_cell" align=right><%=cdbl(rsOtsChk("sum"&i))%></td>
			<%else%>
			
				<TD class="light_cell" align=right><%=cdbl(rsOtsChk("sum"&i&i))%></td>
			<%end if%>
				<!--INPUT id=txtots1 name=txtots1 value=<%=cdbl(rsOtsChk("sum"&i&i))%> style="TEXT-ALIGN: right" size="5" disabled-->							
	<%
		Next
	Else
		For i = 1 to 8'Cdbl(SzCount)%>
				<TD class="light_cell" align=right>0</td>
	<%
		Next
	End if 
	%>
	</tr>
<%end if'wma%>

<%
RSScale.close
end if
session("rsSty").MoveNext 
loop
%>
</table>
<br>
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
			<%end if%>				
		</TD>
	</TR>
</Table>
<br>
<Table Width=95% align=center><TR><td align=center>
<input name="submit1" value="Add to Order" type="submit" ><!--onclick="return check()">-->
<input name="reset" value="Reset" type="reset">
<input name="style" value="<%=request("style")%>" type="hidden">
<input type=hidden name=temp1 value=0>
<input type=hidden name=temp2 value=0>
<input type=hidden name=temp3 value=0>
<input type=hidden name=temp4 value=0>
<input type=hidden name=temp5 value=0>
<input type=hidden name=temp6 value=0>
<input type=hidden name=temp7 value=0>
<input type=hidden name=temp8 value=0>

</TD></TR></Table>
</form>
</font>
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


'FUNCTION GetStyPrice(strStyle,Qty)
'Dim StyConn
'Dim RSStyOrdHdr
'Set StyConn = server.CreateObject("ADODB.Connection")
'StyConn.Open Application("DataConnectionString")
'Set RSStyOrdHdr = server.CreateObject("ADODB.RECOrdSET")
'strtempsql = ""
'strtempsql = strtempsql & "SELECT ORDER FROM ORDHDR WHERE account+cordtype+order  like '" & CUSTID & "C%'"
'strtempsql = strtempsql & " and status <> 'X' and status <> 'B' and start >= {" & Date() & "} and complete <= {" & Date() & "}"
'RSStyOrdHdr.open strtempsql,StyConn


'GetStyPrice = 0
'IF Not(RSStyOrdHdr.EOF And RSStyOrdHdr.BOF) Then ' Contract Exist
'	Dim RSStyLine
'	Set RSStyLine = server.CreateObject("adodb.recordset")
'	strtempsql = ""
'	strtempsql = strtempsql & "SELECT * FROM ORDLINE cordtype+order+STR(lineno,6) like 'C" & RSSTYORDHDR.FIELDS("ORDER").VALUE & "' and style='" & style & "'"
'	RSStyLine.Open strtempsql,StyConn
'	If Not(RSStyLine.EOF And RSStyLine.Bof)then 
'		GetStyPrice = RSStyLine.Fields("Price").Value 
'	End IF
'Else ' Contract Not Exist
'	strtempsql = ""
'	strtempsql = strtempsql & "Select * from customer where type+account+store like 'M" & custid & "%'"
'	Set RSSTYCustomer = server.CreateObject("ADODB.Recordset")
'	RSSTYCustomer.Open strtempsql,StyConn
'	IF Not(RSSTYCustomer.EOF And RSSTYCustomer.BOF) Then
'	
''		strtempsql = "select * from style where style='" & strStyle & "'"
'		'Response.Write(strtempsql)
'		Set RSSTYStyle = server.CreateObject("adodb.recordset")
'		RSSTYStyle.Open strtempsql,StyConn
'	
'		IF Not(RSSTYStyle.EOF And RSSTYStyle.BOF) Then
'	
'			Select Case RSSTYCustomer.Fields("pricelvl").Value 
'			Case "A"
'				GetStyPrice = RSSTYStyle.Fields("Pricea").Value 
'			Case "B"
'				GetStyPrice = RSSTYStyle.Fields("Priceb").Value 
'			Case "C"
'				GetStyPrice = RSSTYStyle.Fields("Pricec").Value 
'			Case "Q"
'				IF cint(Qty) < cint(RSSTYStyle.Fields("natqtyb").Value) then
'					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Pricea").Value)
'				End if
'				
'				IF cint(qty) > cint(RSSTYStyle.Fields("natqtyb").Value) and cint(qty) < cint(RSSTYStyle.Fields("natqtyc").Value) then
'					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Priceb").Value)
'				End if
'				
'				IF cint(qty) > cint(RSSTYStyle.Fields("natqtyc").Value) then
'					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Pricec").Value) 
'				End if
'			End select
'		End IF
'	End IF
'End IF
'GetStyPrice = cint(GetStyPrice)
'End Function
FUNCTION GetStyPrice(strStyle,Qty)
Dim StyConn
Dim RSStyOrdHdr
Set StyConn = server.CreateObject("ADODB.Connection")
StyConn.Open Application("DataConnectionString")
Set RSStyOrdHdr = server.CreateObject("ADODB.RECOrdSET")
strtempsql = ""
strtempsql = strtempsql & "SELECT ORDER FROM ORDHDR WHERE ACCOUNT+CORDTYPE+order like '" & CUSTID & "C%'"
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

	'HDM don't go to read the proce level from customer file it's already read in Session("PriceLvl")
	strtempsql = ""
	'strtempsql = strtempsql & "Select * from customer where type+account+store ='M"&custid&"'"
	'Set RSSTYCustomer = server.CreateObject("ADODB.Recordset")
	'RSSTYCustomer.Open strtempsql,StyConn
	'IF Not(RSSTYCustomer.EOF And RSSTYCustomer.BOF) Then
	IF Trim(Session("PriceLvl")) <> "" Then
		strtempsql = "select * from style where style='" & strStyle & "' order by Style"
		'Response.Write(strtempsql)
		Set RSSTYStyle = server.CreateObject("adodb.recordset")
		RSSTYStyle.Open strtempsql,StyConn
		IF Not(RSSTYStyle.EOF And RSSTYStyle.BOF) Then
			'Response.Write("ok")
			'Select Case RSSTYCustomer.Fields("pricelvl").Value 
			Select Case Trim(Session("PriceLvl"))
			'Case "A"
			'	GetStyPrice = RSSTYStyle.Fields("Pricea").Value 
			'Case "B"
			'	GetStyPrice = RSSTYStyle.Fields("Priceb").Value 
			'Case "C"
			'	GetStyPrice = RSSTYStyle.Fields("Pricec").Value 
			Case "Q"
				IF cint(Qty) < cint(RSSTYStyle.Fields("natqtyb").Value) then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Pricea").Value)
				End if
				
				IF cint(qty) > cint(RSSTYStyle.Fields("natqtyb").Value) and cint(qty) < cint(RSSTYStyle.Fields("natqtyc").Value) then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Priceb").Value)
				End if
				
				IF cint(qty) > cint(RSSTYStyle.Fields("natqtyc").Value) then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Pricec").Value) 
				End if
			Case Else
				GetStyPrice = CDbl(RSSTYStyle.Fields(Trim(Session("PriceLvl"))).Value) 
			End select
		End IF
	End IF
End IF
GetStyPrice = FormatNumber(CDbl(GetStyPrice),2)
End Function
'HDM [Start] new function to determine the price level for the customer
Function GetCustPriceLvl(strCustID)

	Dim StyConn
	Dim RSStyOrdHdr
	Set StyConn = server.CreateObject("ADODB.Connection")
	StyConn.Open Application("DataConnectionString")

	strtempsql = ""
	strtempsql = "Select * from customer where type+account+store ='M"&custid&"'"
	Set RSSTYCustomer = server.CreateObject("ADODB.Recordset")
	RSSTYCustomer.Open strtempsql,StyConn
	IF Not(RSSTYCustomer.EOF And RSSTYCustomer.BOF) Then
		If trim(RSSTYCustomer("pricelvl")) = "" Then
			GetCustPriceLvl = "Pricea"
		ElseIf UCase(RSSTYCustomer("pricelvl")) = "Q" Then
			GetCustPriceLvl = RSSTYCustomer("pricelvl")
		Else
			GetCustPriceLvl = Trim("Price"&RSSTYCustomer("pricelvl"))
		End If
	End If

End Function
'wal_131300 [Start] new function get the price in price code file
Function GetPriceCode(strStyle,strCode)
	Set StyConn = server.CreateObject("ADODB.Connection")
	StyConn.Open Application("DataConnectionString")
	
	set rsStyPrice = server.CreateObject("ADODB.Recordset")
	set rsPriceCode = server.CreateObject("ADODB.Recordset")
	if session("CcurrCode")  = "" then
		rsstyprice.Open "select * from CSTPRICE where priccode = '"&trim(strCode)&"' and stydv = '"&strStyle&"' ",StyConn,1,3
	else
		rsstyprice.Open "select * from CSTPRICE where priccode = '"&trim(strCode)&"' and stydv = '"&strStyle&"' and ccurrcod = '"& trim(session("CcurrCode")) &"' ",StyConn,1,3
	end if
	'Response.Write "<br> select * from CSTPRICE where priccode = '"&trim(strCode)&"' and stydv = '"&strStyle&"' "	
	if not rsstyprice.EOF then
		'validate the profile date
		rspricecode.Open "select * from cstprich where priccode = '"&trim(strCode)&"'",StyConn,1,3
		'Response.Write rspricecode("dvldprfr")
		'Response.End 
		if not isnull(rspricecode("dvldprfr")) and not isnull(rspricecode("dvldprto")) and rspricecode("dvldprto") <> "" and rspricecode("dvldprfr") <> "" then
			'check that there are value
			if (date() > rspricecode("dvldprfr") and date() < rspricecode("dvldprto")) then
				if isnull(rsstyprice("pricedv")) or cdbl(rsstyprice("pricedv")) = 0 or rsstyprice("pricedv") = ""then
					GetPriceCode = GetStyPrice(trim(strStyle),1)
				else
					GetPriceCode = rsstyprice("pricedv")
				end if
			else
				GetPriceCode = GetStyPrice(trim(strStyle),1)
			end if
		else'not valid date then get the default price
			if isnull(rsstyprice("pricedv")) or cdbl(rsstyprice("pricedv")) = 0 or rsstyprice("pricedv") = ""then
				GetPriceCode = GetStyPrice(trim(strStyle),1)
			else
				GetPriceCode = rsstyprice("pricedv")
			end if
			'GetPriceCode = rsstyprice("pricedv")'GetStyPrice(trim(strStyle),1)
		end if
	else
		GetPriceCode = GetStyPrice(trim(strStyle),1)
	end if
end function
%>
