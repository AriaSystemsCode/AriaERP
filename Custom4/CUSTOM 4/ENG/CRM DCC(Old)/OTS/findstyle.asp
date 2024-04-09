<%@ LANGUAGE="VBSCRIPT" %>
<%Response.Buffer = true%>
<%Session("Group")=Request("Group")
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
%>
	<script language="javascript">
	location.href="../login.asp"
	</script>
<%End if

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
'IF Len(Trim(Session("rep")))>0 Then
'	IF Trim(Session("customerid")) = "" Then
		'Response.Redirect("../repcust.asp")%>
		
<%'END IF
strFile  = "reb"
compWork = "Y"
strCheck = ""

if Request.QueryString ("Select") = "A" then'select all							
  strCheck = "checked"
  if isobject(Session("rsSelect")) then
	Session("rsSelect") = ""
  end if
elseif Request.QueryString ("Select") = "N" and Request.QueryString ("firsttime") <> ""  then'clear select
  strCheck = ""
  if isobject(Session("rsSelect")) then
	Session("rsSelect") = ""
  end if
end if
if isobject(Session("rsSelect")) then
	if Request.QueryString ("firsttime") <> "" then
		set Session("rsSelect") = server.CreateObject("ADODB.RecordSet")
		if not Session("rsSelect") is nothing then
			if Session("rsSelect").state <> 0 then
				Session("rsSelect").close
			end if
		end if
		call Session("rsSelect").fields.append("style",129,20)
		call Session("rsSelect").fields.append("No",14)
		Session("rsSelect").open
	end if
else'recrdset not there then create it
	set Session("rsSelect") = server.CreateObject("ADODB.RecordSet")
	call Session("rsSelect").fields.append("style",129,20)
	call Session("rsSelect").fields.append("No",14)
	Session("rsSelect").open
end if

'check if there condition for selection
mypage=request("curpage")


'fill session recordset wz selected values
'get loop count
intLoop   = cdbl(Request.QueryString ("PageCount"))
strPaging = Request.QueryString ("Paging")'check the direction of paging
'code for paging

If  mypage="" then
   mypage=1
end if
'Response.Write "<font size=2>"&Session("rsSelect").recordcount&"<br>"
if Request.QueryString ("Select") = "N" and Request.QueryString ("firsttime") <> ""  then
else
	for ctr=0 to intLoop - 1
		if trim(Request.Form ("ID"&ctr)) <> "" then
			Session("rsSelect").filter = "style = '"& Request.Form ("ID"&ctr) &"'"
			if Session("rsSelect").eof then
				Session("rsSelect").addnew
					Session("rsSelect")("style") = Request.Form ("ID"&ctr)
					if strPaging = "N" then
						Session("rsSelect")("NO") = myPage-1
					else
						Session("rsSelect")("NO") = myPage+1
					end if
				Session("rsSelect").update
			end if
			Session("rsSelect").filter = ""
		end if
	next
end if
'Response.Write "<font size=2>"&Session("rsSelect").recordcount&"<br>"
'Response.End 
'code for handling multi selection from multi pages[end]
Set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

Set RSCodes = Server.CreateObject("ADODB.RECORDSET")
strSql = "Select ccode_no, cdiscrep, cdefcode from codes where cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam like 'NCSTYGROUP%' and cdiscrep <> '' Order By cCode_No"
RSCodes.Open strSql,Conn

IF Not IsObject(Session("RSStyStruct")) Then
	Set Session("RSStyStruct") = Server.CreateObject("ADODB.RecordSet")
	strsql = "select * from icistru where citemrecty+cisegno like'U%'"
	Session("RSStyStruct").open strsql,conn
End IF
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

<title>CRM - Style Selection Criteria</title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</head>
<body onLoad="hideloadingmsg()">
<div name='loadingmsg' id='loadingmsg' align=center style='display:inline;visibility:visible'>
	<p align=center>
		<A align=center style="font-family=verdana;font-size=12px;font-weight=bold;"><font color=red>Please standby, ..Loading data from server.</font></A>
	</p>
</DIV>

<SCRIPT LANGUAGE=javascript>
<!--
function openwindow(filename) 
{  
	window.open(filename,'','toolbar=no,status=no,scrollbars=no,location=no,menubar=no,directories=no,top=' + ((window.screen.height-400)/2) + ',left=' + ((window.screen.width-800)/2) + ',width=800,height=400')
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

</table>
<%End IF%>
<Table border=1 align=center width=95%>
<TR>
<TD class=Title>Select Style</TD>
</TR>
</TAble>

<%IF compWork = "Y" Then%>

<%
IF Not(Session("RSStyStruct").EOF And Session("RSStyStruct").BOF) Then
	Session("RSStyStruct").MoveFirst
	DO While Not Session("RSStyStruct").Eof
		if Trim(Request("firsttime"))<>"" then
			strValue = Request.Form(Trim(Session("RSStyStruct").fields("cisegsdes")))
			Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) = strValue
		else
			strValue = Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) 
			'strValue = Session(Trim(Session("RSStyStruct").fields("cisegsdes")))
		end if
		strRequiredStyle = Trim(strRequiredStyle) & Trim(strValue)
		strRequiredStyle = Ucase(strRequiredStyle) & "%"
		strRequiredStyle = strRequiredStyle & Trim(Session("RSStyStruct").fields("cisegsepr"))
		
	Session("RSStyStruct").MoveNext
	Loop
	'Response.Write("<font size=3>Style"&strRequiredStyle)
End IF
strStyle = strRequiredStyle
' To handle the case of the rep.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF
'HDM[Start]
'WAL_Apply condition on season and division[start]
Dim strSeason, strDivision

if Trim(Session("Season"))="All" then
	strSeason = "NONE"
else
	strSeason = Trim(Session("Season"))
end if 

if strSeason = "ALL" or strSeason = "" or strSeason = "NONE" or strSeason = "*" then
	strSeason	="NONE"
end if
if Trim(Session("Division"))="" Or Trim(Session("Division"))="*" or UCase(Trim(Session("Division")))="ALL" then
	strDivision = "NONE"
else
	strDivision = Trim(Session("Division"))
end if 

'If Trim(UCase(strSeason)) = "NONE" or len(Trim(UCase(strSeason)))>4Then
If Trim(UCase(strSeason)) = "NONE" or instr(Trim(UCase(strSeason)),",") <>0 Then
	'Multiple Seasons WMA 5/24/2004 START
	'strSeasonCondition = " Season in('" & Replace(Trim(Session("StyleColor")),",","','") & "') "	
	strSeasConArr  = split(Session("StyleColor"),",")
	intSeasConCount = int(UBound(strSeasConArr)/24)
	strSeasonCondition = strSeasonCondition & " AND ("
	for i = 0 to  intSeasConCount 
		for j = i*24 to (i*24+24) -1
			if j = UBound(strSeasConArr) then
				strSQLall = strSQLall + "'" &  strSeasConArr(j) & "'"
			elseif j < UBound(strSeasConArr) then
				if ((j+1) mod 24 = 0 and j<>0) then
					strSQLall = strSQLall + "'" &  strSeasConArr(j) & "'"			
				else
					strSQLall = strSQLall + "'" &  strSeasConArr(j) & "',"			
				end if	
			end if		
		next		
		if i =0 then 'first item
			strSeasonCondition = strSeasonCondition & " Season in("& strSQLall &") " 
		else
			strSeasonCondition = strSeasonCondition & " or Season in(" & strSQLall & ") " 
		end if
		strSQLall = ""
	next
	strSeasonCondition = strSeasonCondition & " )"
    'Multiple Seasons WMA 5/24/2004 END		
    
Else
	strSeasonCondition = " Season='" & strSeason & "'"
End If


If Trim(UCase(strDivision)) = "NONE" Then
	strDivisionCondition = ""
Else
	If Trim(strSeasonCondition) ="" Then
		strDivisionCondition = " cdivision='" & strDivision & "'"
	Else
		strDivisionCondition = " And cdivision='" & strDivision & "'"
	End If
End If
Dim strSeasDivWhere

strSeasDivWhere = strSeasonCondition & strDivisionCondition
If trim(strSeasDivWhere) <> "" And Left(Trim(strSeasDivWhere),3) <> "AND" Then
	strSeasDivWhere = " AND " & strSeasDivWhere
End If
'WAL_Apply condition on season and division[end]
if len(Request.Form("Style")) <> 0 then
	Session("Style") = Request.Form("Style")
end if
if len(Request.Form("Color")) <> 0 then
	Session("Color")    = Request.Form("Color")
end if
if len(Request.Form("Group")) <> 0 then
	Session("Group")    = Request.Form("Group")
end if
lcStr1 = ""
If Not Len(Session("Style")) = 0 Then 
  lcStr1 = " And Style.cStyMajor='" & UCASE(Session("Style")) & "'" 
End If 

If Not Len(Session("Color")) = 0 Then 
  If Not Len(lcStr1) = 0 Then 
    lcStr1 = lcStr1 + "AND Style LIKE '%" & UCASE(Session("Color")) & "%'" 
  Else 
    lcStr1 = " And Style LIKE '%" & UCASE(Session("Color")) & "%'" 
  End If 
End If 
'HDM [END]

if trim(Session("Group"))="" or trim(Ucase(Session("Group")))="ALL" then
	strSQL = "SELECT * FROM Style where " & MakeQuery("status+cstygroup", Session("ShowCatalogVal"), 10, false) & " and style like '" & strRequiredStyle & "' "
	strSQL = strSQL& strSeasDivWhere  & " order by style"
else
	strSQL = "SELECT * FROM Style where style like '" & strRequiredStyle & "' and status+cstygroup like 'A" & Session("Group")&"' "
	strSQL = strSQL& strSeasDivWhere  & " order by style"
end if

'Response.Write "<font size=5>" &  Session("season") & "<hr>"
'Response.Write "<font size=5>" &  strSQL & "<hr>"
Const NumPerPage  = 25

Dim CurPage
If Request.QueryString("CurPage") = "" then
    CurPage = 1 'We're on the first page
Else
    CurPage = Request.QueryString("CurPage")
End If

Set Session("RSSTyResult") = Server.CreateObject("ADODB.RECORDSET")
Session("RSSTyResult").CursorLocation = 2
Session("RSSTyResult").CacheSize = NumPerPage
Session("RSSTyResult").CursorType = 3

'Response.Write "<font size=2>strsql =="&strsql&"<Br><br>"
'Response.End 
'Response.write "Make Query==" & MakeQuery("status+cstygroup", Session("ShowCatalogVal"), 10, false)
Session("RSSTyResult").Open  strsql, conn,1,3

IF Not(Session("RSSTyResult").EOF AND Session("RSSTyResult").BOF) Then
	Session("RSSTyResult").PageSize = NumPerPage
	TotalPages = Session("RSSTyResult").PageCount 
	Session("RSSTyResult").AbsolutePage = CurPage
End IF
Dim Count
Count = 0
IF ( Session("RSSTyResult").EOF and Session("RSSTyResult").bof)  Then
%>
<CENTER>
	<table width=95% align=center>
		<tr>
			<td>
				<strong>No Styles Found.</strong>
			</td>
		</tr>
	</table>
</CENTER>
<%
Else
%>
<form name=frm method=post>

<div align="center">
<center>

<table border="1" width="95%" cellpadding="0" cellspacing="0" style="border-collapse: collapse" bordercolor="#111111">
<TR>
	<TD class="dark_cell" width=1%>>></TD>
	<TD width=22% class="dark_cell">Style</TD>
	<TD Width=45% class="dark_cell">Description</TD>
	<%IF Session("M_STYVIEW") = "G" and request("Group")= "ALL" Then%>
		<TD class=dark_cell>Group</TD>
	<%End IF%>
	<TD width=5% class="dark_cell">Price</TD>
</TR>
<%
Dim rsTemSty
Set rsTemSty = server.CreateObject("ADODB.RECORDSET")
'First Get the price level for this customer
Session("PriceLvl")=GetCustPriceLvl(custid)
	
Do While Not Session("RSSTyResult").EOF And Count < NumPerPage
	strFound = ""
	Session("rsSelect").filter = "style = '"& Session("RSSTyResult")("style") &"'"
	if not Session("rsSelect").eof then
		strFound = "F"
	end if
	Session("rsSelect").filter = ""
%>
	<TR>
	
		<%if Request("Select") = "" then%>			
			<TD class=light_cell><input type=checkbox name="ID<%=Count%>" value="<%=Session("RSSTyResult")("style")%>"  <%if strFound = "F" then %> checked <%end if%>>
		<%else%>
			<TD class=light_cell><input type=checkbox name="ID<%=Count%>" value="<%=Session("RSSTyResult")("style")%>"  <%if strFound = "F" then %> checked <%else%> <%=strCheck%><%end if%>>
		<%end if%>
		
	</TD>
<%	strTemp  = "<TD width=22% class=light_cell>"
	Response.Write(strTemp)
	%>
		<%=trim(Session("RSSTyResult")("style"))%>				
	<%
	strTemp  = "<TD width=45% class=light_cell>"
	Response.Write(strTemp)
	
	Response.Write("&nbsp;"&trim(Session("RSSTyResult")("desc1")))
	
	Response.Write("</td>")
					
	IF Session("M_STYVIEW") = "G" and request("Group")= "ALL" Then
		Response.Write("<TD class=light_cell>")
						
		RSCodes.Filter = "ccode_no='"& Trim(Session("RSSTyResult").Fields("cstygroup").Value)& space(6-(len(Trim(Session("RSSTyResult").Fields("cstygroup").Value)))) &"'"
		if not rscodes.EOF then
			Response.Write("&nbsp;"&rscodes.Fields("cdiscrep").Value)
		end if
		Response.Write("</td>")
	End IF
					
	strTemp  = "<TD width=5% class=light_cell align=center>"
	Response.Write(strTemp)
	'Ard - 301633 - Add the price [Start]
	'NEK [Start] -Adding Currency and its Alignment
	if Session("CurrencyAlign")="LEFT" then
		Response.Write(Session("Currency") & FormatNumber(GetStyPrice(trim(Session("RSSTyResult")("style")),1)))
	else
		Response.Write(FormatNumber(GetStyPrice(trim(Session("RSSTyResult")("style")),1)) & Session("Currency"))
	end if 	
	'NEK [End] -Adding Currency and its Alignment 
	'Ard - 301633 - Add the price [End]
	Response.Write("</td>")

Session("RSSTyResult").MoveNext
	Count = Count + 1
	if Session("M_STYVIEW") = "P" and Request("logintype")="O" then
		rsTemSty.Close 
	End IF
Loop

%>
<%	
' delete records that exist before
Session("rsSelect").filter = "NO = '"& myPage &"'"
if Session("rsSelect").eof then
else
	Session("rsSelect").movefirst
	do while not Session("rsSelect").eof
		Session("rsSelect").Delete
	Session("rsSelect").movenext
	loop
end if
Session("rsSelect").filter = ""
%>
</table>
</center>
</div>
</center>
</div>
<Table border=0 width=95% align=center>
<TR>
<%'check condition for selection
  if Request.QueryString ("Select") = "A" then'clear all
%>	<Td width=50% align=left><input type=button onclick="javascript:document.frm.action='findstyle.asp?firsttime=F&Logintype=<%=Request("LoginType")%>&Group=<%=Request("Group")%>&Select=N&CurPage=<%=mypage%>';document.frm.submit()" value="Clear All"></td>
<%else%>
	<Td width=50% align=left><input type=button onclick="javascript:document.frm.action='findstyle.asp?firsttime=&Logintype=<%=Request("LoginType")%>&Group=<%=Request("Group")%>&Select=A&CurPage=<%=mypage%>';document.frm.submit()" value="Select All"></td>
<%end if%>
<Td width=50% align=right>
	<input type=button value="Show Avalaiblity" onclick="javascript:document.frm.action='showAva.asp?firsttime=F&Select=<%=Request("Select")%>&PageCount=<%=Count%>&CurPage=<%=myPage%>';document.frm.submit()">
	<input type=button value="Print Report" onclick="javascript:document.frm.action='showReport.asp?firsttime=F&Select=<%=Request("Select")%>&PageCount=<%=Count%>&CurPage=<%=myPage%>';document.frm.submit()">
</td>
</TR>
</Table>
<Table border=0 width=95% align=center>
<TR>
<TD>
<%

	IF count > 0 Then
		Response.Write("<Center><font face=Arial size=2>Page " & CurPage & " of " & TotalPages & "</font></center>")
		Response.Write "<table align=center border=0><tr><td>"
		if CurPage > 1 then
			'We are not at the beginning, show the back button%>
				<a href="javascript:document.frm.action='findstyle.asp?firsttime=&Select=<%=Request("Select")%>&Logintype=<%=Request("LoginType")%>&curpage=<%=curpage - 1%>&Group=<%=Request("Group")%>&Paging=P&PageCount=<%=Count%>';document.frm.submit()"><img border=0 src="../images/<%=Session("theme")%>/back.gif"></a>
		<%End If

		if CInt(CurPage) <> CInt(TotalPages) then
		    'We are not at the end, show a next button%>
				<a href="javascript:document.frm.action='findstyle.asp?firsttime=&Select=<%=Request("Select")%>&Logintype=<%=Request("LoginType")%>&curpage=<%=curpage+ 1%>&Group=<%=Request("Group")%>&Paging=N&PageCount=<%=Count%>';document.frm.submit()"><img border=0 src="../images/<%=Session("theme")%>/next.gif"></a>
		<%End If
	End IF
		%>
</TD>
</TR>
</Table>
</form>
<%End IF
End if%>

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
%>

</body>
</html>
<%
function makeQuery(fieldname, ValuesList, NoOfItems, notflag)
	dim ValuesArray
	ValuesArray = split(ValuesList, ",", -1, 1)
	strQuery="( "
	for i=0 to ubound(ValuesArray) 
		if (i mod NoOfItems) = 0 then
			if i <> 0 then
				if notflag = false then
					strQuery = strQuery & ") or "
				else
					strQuery = strQuery & ") and "
				end if
			end if
			strQuery = strQuery & fieldname
			if notflag = false then
				strQuery = strQuery & " IN ( "
			else
				strQuery = strQuery & " not in ( "
			end if
		end if
		strQuery = strQuery & "'A" & ValuesArray(i) & "'"
		if (((i+1) mod NoOfItems) <> 0) and i <> ubound(ValuesArray)  then
			strQuery = strQuery & ","
		end if
		
		if i = ubound(ValuesArray) then
				strQuery = strQuery & ") "
		end if
	next 
	strQuery = strQuery & ")"
	makeQuery = strQuery
end function
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
GetCustPriceLvl = "Pricea"
End Function

%>