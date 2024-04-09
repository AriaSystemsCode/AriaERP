	<%@ Language=VBScript %>
<%
Response.Buffer=True
Response.Expires=-1
If trim(Session("PO"))="" Then
	Session("PO")=trim(request("txtpono"))
Else
End If
Session("Season") = Session("StyleColor")
If Trim(Session("ID")) = "" And Session("rep")= "" Then
'Response.redirect "../default.asp"%>
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

' To handle the case of the rep.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF


'set records per page
Const NumPerPage = 6

'Retrive what page we are currently on
Dim CurrPage

IF Request.QueryString("CurrPage")="" Then
	CurrPage = 1		'First Page
Else
	CurrPage = Request.QueryString("CurrPage")	
END IF
'update ship address if changed[start]
'if Request.form ("selShipAdd") = "A" then
'	session("RSCust")("stname")    = Request.Form ("txtadd1")
'		
'	session("RSCust")("caddress1") = Request.Form ("txtadd2")
'	
''	session("RSCust")("caddress2") = Request.Form ("txtadd3")
'		
'	Session("Add3") = Request.Form ("txtAdd4")
'			
'	session("RSCust")("caddress6") = Request.Form ("txtadd5")
'		
'	Session("chgAdd") = "A"
'	Session("ShipAdd") = true
'end if
'update ship address if changed[end]

Dim Conn
Set Conn = Server.CreateObject("ADODB.Connection")
conn.Open Application("DataConnectionString")
'//////////////////Added by rmz at 06/18/2001/////////////////////////////////////
'///////////////////Modified by rmz at 06/25/2001///////////////////////
    Dim strPack,strSQL,strCon
    Dim connSQL
    Dim rsPacks
   

'/////////////////////////////////////////////////////////////////////////////////
		set connSQL= server.CreateObject ("ADODB.Connection")
		'if Application("DBType") = "SQL" then
		'	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source="&Trim(session("SQLServer"))&";uid=" & Session("SqlUserName") & ";pwd=" & Session("SqlPassWord") & ""
		'else
		'	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=OraOLEDB.Oracle;Data Source=DB99;user id=ARIA;password=ARIAADMN"
		'end if

		connSQL.Open Application("SqlServer")

		'Get packs ID from SQl Server tables
		set rsPacks = Server.CreateObject ("ADODB.Recordset")
		strSQL= "select CustGroup.PackID from CustClassification ,CustGroup  "
		strSQL = strSQL & " where CustClassification.CustGroup=CustGroup.GroupID "
		strSQL = strSQL & " AND CustClassification.CustID='" & Trim(CustID) & "'"
		
		rsPacks.Open strSQL,connSQL
		'Response.Write(strSQL)
		'GET the Pack_id Information.
		IF Not (rsPacks.EOF And rsPacks.BOF) Then
			strsql = "Select PACK_ID, ACCOUNT, SEASON, CDIVISION from spck_hdr where "
			strsql = strsql & " type+account+pack_id = 'P*****" & Trim(rsPacks.Fields("PackID").Value) & "'"
		Else
			strsql = "Select PACK_ID, ACCOUNT, SEASON, CDIVISION from spck_hdr where "
			strsql = strsql & " type+account+pack_id like 'P" & Trim(CustID) & "WEB%" & "' or type+account+pack_id like 'P*****WEB%'"
		End IF
		'Response.Write "strsql==="&strsql
		'Response.End 
	
		Dim RShdr
		Set RShdr = Server.CreateObject("ADODB.RECORDSET")
		RShdr.Open strSql,Conn,2,4
		
		'Response.Write(strsql)
		'Response.End 

		Set RSStyStr = server.CreateObject("ADODB.RECORDSET")
		strsql = "select * from icistru where citemrecty+cisegno like 'U%' and lsegendmaj=.T."
		RSStyStr.Open strsql,Conn,2,4
	
	Set RSline = Server.CreateObject("ADODB.RECORDSET")
	If Not(RShdr.EOF And RShdr.BOF) Then
		'Prepare the Packs Where condition to include all packs found for this customer + the general pack
		Dim strPackWhere
		strPackWhere = ""
		Do While Not RShdr.EOF
			If Trim(strPackWhere) = "" Then
				strPackWhere = "'" & RShdr("PACK_ID") & "'"
			Else
				strPackWhere = strPackWhere & ",'" & RShdr("PACK_ID") & "'"
			End If
			RShdr.MoveNext 
		Loop
	RShdr.MoveFirst 
	'Response.Write("Season=" & Session("Season"))
	'Response.End 
		'If Session("Season") = "All" or Trim(Session("Season"))="" or Trim(Session("Season"))="*"  or UCase(Session("Season"))="NONE" Then
			'strsql = "select spck_lin.style as spckSty, style.* "
			'strsql = strsql & " from SPCK_LIN, STYLE "
			'strsql = strsql & " where SPCK_LIN.TYPE+SPCK_LIN.ACCOUNT+SPCK_LIN.PACK_ID+SPCK_LIN.STYLE like"
			'strsql = strsql & " 'P" & Trim(rshdr.Fields("ACCOUNT").Value) & Trim(rshdr.Fields("PACK_ID").Value)  & "%'"
			'strsql = strsql & " and SPCK_LIN.STYLE = STYLE.STYLE  order by spckSty group by style.style"


			strsql = "select spck_lin.style as spckSty ,style.*"
			strsql = strsql & " from SPCK_LIN, STYLE "
			strsql = strsql & " where (SPCK_LIN.TYPE+SPCK_LIN.ACCOUNT+SPCK_LIN.PACK_ID+SPCK_LIN.STYLE like"
			strsql = strsql & " 'P" & Trim(CustID) & "%'"
			strsql = strsql & " Or SPCK_LIN.TYPE+SPCK_LIN.ACCOUNT+SPCK_LIN.PACK_ID+SPCK_LIN.STYLE like"
			strsql = strsql & " 'P*****%')"
			strsql = strsql & " And InList(SPCK_LIN.PACK_ID," & strPackWhere & ")"
			
			If Not(Session("Season") = "All" or Trim(Session("Season"))="" or Trim(Session("Season"))="*"  or UCase(Session("Season"))="NONE") Then
				strsql = strsql & " And	style.season='" & Session("Season") & "'"
			End If
			
			If Not(Session("CatDivision") = "All" or Trim(Session("CatDivision"))="" or Trim(Session("CatDivision"))="*"  or UCase(Session("CatDivision"))="NONE") Then
				strsql = strsql & " And Style.cDivision='" & Session("CatDivision") & "'"
			End If
			
			If Trim(Request("Style")) = "" Then
			Else
				strsql = strsql & " And Style.Style Like '" & Request("Style") & "%'"
			End If
			strsql = strsql & " and SPCK_LIN.STYLE = STYLE.STYLE  order by spckSty group by style.style"
		'Else
		'	strsql = "select spck_lin.style as spckSty, style.* "
		'	strsql = strsql & " from SPCK_LIN, STYLE "
		'	strsql = strsql & " where SPCK_LIN.TYPE+SPCK_LIN.ACCOUNT+SPCK_LIN.PACK_ID+SPCK_LIN.STYLE like"
		'	strsql = strsql & " 'P" & Trim(rshdr.Fields("ACCOUNT").Value) & Trim(rshdr.Fields("PACK_ID").Value)  & "%'"
		'	strsql = strsql & " and SPCK_LIN.STYLE = STYLE.STYLE and style.season='"&Session("Season")&"' order by spckSty group by style.style"
		'End If
	Else
		'Response.Write("<Table width=""95%"" align=center><TR><TD>No record found.</TD></TR></Table>")
	End If
	
		
	'Set Cursor Location
	RSline.CursorLocation = 2'adUseClient

	'Set Cachesize to no. of records/page
	RSline.CacheSize = NumPerPage
	'Response.Write("<br><font size=3>RSLine="&strSql&"</font>")
	'Response.End 

	RSline.Open strSql,Conn
	RSline.PageSize = NumPerPage
	TotalPages = RSline.PageCount 
If RSline.PageCount > 0 Then
 	RSline.AbsolutePage = CurrPage
	Dim intCount
	intCount = 0
	
	Do while Not RSStyStr.Eof
		intCount = intCount + cdbl(RSStyStr("nisegsize")) + 1
		RSStyStr.MoveNext 
	Loop
	intCount = intCount - 1
 End if

set connTemp=server.createobject("ADODB.Connection")
set RSTemp=server.createobject("ADODB.Recordset")
connTemp.open Application("DataConnectionString")
sqlTemp="select * from customer where account='"&session("customerid")&"'"
RSTemp.open sqlTemp,connTemp,1,3

%>

<html>
<head>
<title>CRM - Catalog</title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/Catalog.css" TYPE="text/css">

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
<p><br><br><BR></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected customer is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></P>
	</TD>
	<TD align=right><a href="repcust.asp">Get Customer</a></TD>
	<!-- ARD -->
</TR>
</table>
 <%End IF%>

<Table width=95% height=50 align=center border=1>
<TR>
<TD class=title>Catalog</TD>
</TR>
</Table>



<%'Response.Write "<font size=10>compWork==="&compWork&"</font>"
IF compWork = "Y" Then
%>
<Table Width=95% border=0 align=center>
<TR>
	<TD>
		<p align="right"><a href="catcustord.asp">check shopping cart</a></p></TD>
</TR>
<%
IF RShdr.EOF And RShdr.BOF Then
	Response.Write("<TR><TD ><b> No records match your criteria. </b></TD></TR>")
Else
%>
</Table>

<Form method=post Action="" Name=form1>


<Font face="Arial">

<table border="0" width="95%" align="center">
<tr>
<%

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 'Set Count to Zero
  Dim Count
  Count = 0
  'Response.Write(RSline.Source)
IF RSline.EOF And RSline.BOF Then
	Response.Write("<TR><TD align=center><br><b> No records match your criteria. </b></TD></TR>")
End IF
do while not RSline.EOF And Count < RSline.PageSize

    
  IF Count = 3 or Count = 6 or Count = 9 or Count = 12 then
		strTemp = "</tr><tr>"
		Response.Write(strTemp)
  End IF

	If strmajor = Trim(RSline("cstymajor")) Then
	Else
		strTemp = "<td width=""33%"" align=center><table border=""0"" width=""44%""><TR>"
		Response.Write(strTemp)
		strTemp = "<td width=""100%""><img border=""0"" src=""../styimg/"
		Response.Write(strTemp)
		strTemp = Trim(mid(trim(RSline("style")),1,intCount)) & ".jpg"" width=""80"" height=""100""></td>"
		Response.Write(strTemp)
		strTemp = "</TR><TR>"
		Response.Write(strTemp)
		strTemp = "<td width=""100%""><A href=""catstyle.asp?style=" & Trim(mid(trim(RSline("style")),1,intCount)) & """>"
		Response.Write(strTemp)
		'Read Setup to know the data to be displayed Style#-Description-Both
		Select Case Session("M_STYINFO")
		Case "D"
			strTemp = Response.Write(RSline("desc")) & "</A>"
		Case "C"
			strTemp = Response.Write(RSline("cStyMajor")) & "</A>"
		Case "B"
			strTemp = Response.Write(RSline("cStyMajor") & "-" & RSline("desc")) & "</A>"
		End Select
		
		Response.Write(strTemp)
		strTemp = "</td></tr></table></td>"      
		Response.Write(strTemp)
		Count = Count + 1
	End if
	strmajor = Trim(RSline("cstymajor"))
	RSline.MoveNext 
loop
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
%> 
</tr> 
</table>
<BR>

<Table width=95% border='0' align='center'><TR><TD align=center>
<%
'Display next/prev 
IF CurrPage > 1 THEN
	'show prev 
	Response.Write("<A href='catpage.asp?Currpage=" & Currpage-1 & "& Style=" & Request("Style")&"'><img src=""../images/"&Trim(Session("Theme"))&"/back.gif""></A>")
END IF

Response.Write("  ")

'Show next Button
IF RSline.EOF And RSline.Bof THen
Else
	IF Cint(CurrPage)<> Cint(TotalPages) Then
		Response.Write("<A href='catpage.asp?Currpage=" & Currpage+1 & "&Style=" & Request("Style")&"'><img src=""../images/"&Trim(Session("Theme"))&"/next.gif""></A>")
    end if
End IF


%>

</TD></TR></Table>
<P>&nbsp;</P>
</font>
<%End If
End IF
%>
</BODY>
</HTML>

