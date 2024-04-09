<%@ Language=VBScript %>
<%
Response.Buffer=True
'RESPONSE.WRITE DATE()
'RESPONSE.END
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"	%>
	<script language="javascript">
	parent.location.href ="../login.asp"
	</script>
<%End if

'NEK 1/6/2003[Start] Returns the Recordset of a specific Code in Codes.dbf
function getCodes(CodeName)
'Response.Write "IN Function"
	set CodesConn = server.CreateObject("adodb.connection")
	set rsSQLgetCodes = server.CreateObject("adodb.recordset")
	
	CodesConn.open Application("DataConnectionString")
	strSQLgetCodes = "select * from codes where cdefcode+crltfield+cfld_name like 'NN" & Ucase(CodeName) & "%' order by Cdiscrep"
	
	rsSQLgetCodes.Open strSQLgetCodes , CodesConn ,2,4
	set getCodes = rsSQLgetCodes
end function
'NEK 1/6/2003[End]


' To handle the case of the rep.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF


IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" and request("chkCust") = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
	if request("chkCust") <> "" Then
		custid = ""
		Session("customerid") = ""
	end if
End IF



'set records per page
Const NumPerPage = 25

'Retrive what page we are currently on
Dim CurrPage

IF Request.QueryString("CurrPage")="" Then
	CurrPage = 1		'First Page
Else
	CurrPage = Request.QueryString("CurrPage")	
END IF


set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
set rsOrdersMatch = server.CreateObject("ADODB.RecordSet")
dim bDay,bMonth,bYear,eDay,eMonth,eYear
if Trim(Ucase(Request.Form("txtBeginDate")))<>"" then
 Session("BeginDate") = (Trim(Ucase(Request.Form("txtBeginDate"))))
 bYear = year(Trim(Ucase(Request.Form("txtBeginDate"))))
 bMonth = month(Trim(Ucase(Request.Form("txtBeginDate"))))
 if bMonth<10 then
	bMonth ="0"&bMonth
 end if 
 bDay = day(Trim(Ucase(Request.Form("txtBeginDate"))))
 if bDay<10 then
	bDay ="0"&bDay
 end if 
 Session("BeginDate")=byear & bmonth & bday
End IF

IF Trim(Ucase(Request.Form("txtEndDate")))<>"" Then
	Session("EndDate")  = (Trim(Ucase(Request.Form("txtEndDate"))))
	 eYear = year(Trim(Ucase(Request.Form("txtEndDate"))))
	 eMonth = month(Trim(Ucase(Request.Form("txtEndDate"))))
	  if eMonth<10 then
			eMonth ="0"&eMonth
	end if 
 	 eDay = day(Trim(Ucase(Request.Form("txtEndDate"))))
     if eDay<10 then
		eDay ="0"&eDay
	end if 

	Session("EndDate")=eyear&emonth&eday
End IF

IF Request.Form("selectStatus")<>"" Then
	Session("Status") = Request.Form("selectStatus")
End IF
'NEK [Start]
IF Request.Form("txtOrderNoStart")<>"" Then
	Session("txtOrderNoStart") = Request.Form("txtOrderNoStart")
end if 
IF Request.Form("txtOrderNoEnd")<>"" Then
	Session("txtOrderNoEnd") = Request.Form("txtOrderNoEnd")
end if 

IF Request.Form("selectSeason") <> "" then
	Session("selectSeason") = Trim(Request.Form("selectSeason"))
end if	

IF Request.Form("selectDivision")<>"" Then
	Session("selectDivision") = Request.Form("selectDivision")
end if 
if Request.Form("txtStartShipDate") <> "" then
	Session("txtStartShipDate") = Request.Form("txtStartShipDate")
end if 
if Request.Form("txtEndShipDate") <> "" then
	Session("txtEndShipDate") = Request.Form("txtEndShipDate")
end if 
if Request.Form("txtCompleteDate") <> "" then
	Session("txtCompleteDate") = Request.Form("txtCompleteDate")
end if 
'NEK [End]
''''''''''''''''''''''''''''''''''''''''''''
'Set Cursor Location
rsOrdersMatch.CursorLocation = 2'adUseClient

'Set Cachesize to no. of records/page
rsOrdersMatch.CacheSize = NumPerPage

''''''''''''''''''''''''''''''''''''''''''''


'Build the query according to entered values ..
'NEK [Start] 1/5/2002 Add the CDivision, Complete and Season to the Select Statement
strSQL = "SELECT ordhdr.order, Ordhdr.Book,Ordhdr.Ship, Ordhdr.Bookamt,customer.btname, ordhdr.status, ordhdr.entered, ordhdr.store,ordhdr.cordtype,customer.stname,ordhdr.complete,ordhdr.season,ordhdr.cdivision ,ordhdr.start FROM Ordhdr, customer WHERE ordhdr.account = customer.account"
'strSQL = strSQL & " ordhdr.account+ordhdr.cordtype+ordhdr.order like '" & CustID &"%' "
IF Trim(custID) <> ""  Then
	strSQL = strSQL & " and ordhdr.account+ordhdr.cordtype+ordhdr.order like '" & CustID &"%'"
	strSQL = strSQL & " and (customer.Type+customer.account+customer.store like 'M" & CustID & "%' or customer.Type+customer.account+customer.store like 'S" & CustID & "%')"
	
End if
if Session("Authority") = "" and Trim(Session("rep")) <> "" then
	strSQL = strSQL &" and ordhdr.rep1 = '"&Trim(Session("rep"))&"'"
end if
strSQL = strSQL & " and customer.store=ordhdr.store "
if Session("txtOrderNoStart")<> "" and  Session("txtOrderNoEnd")<> "" then
	'NEK[Start]1/6/2003
	'testexistSQL = "SELECT Order,Status,Entered,Start, Complete, Store, Season,Custpo, Cdivision, Book, Ship, Cancel, Open, Bookamt, Shipamt,Cancelamt, Openamt,cordtype FROM Ordhdr WHERE Account+cordtype+order like '" & CustID & "%' AND Order='" & Trim(Request.Form("txtOrderNo")) & "' "
	testexistSQL = "SELECT Order,Status,Entered,Start, Complete, Store, Season,Custpo, Cdivision, Book, Ship, Cancel, Open, Bookamt, Shipamt,Cancelamt, Openamt,cordtype FROM Ordhdr WHERE Account+cordtype+order like '" & CustID & "%' AND Order between '" & Trim(Session("txtOrderNoStart")) & "' and '" & Trim(Session("txtOrderNoEnd")) & "'"
	'NEK[End]1/6/2003
	set rsorders = server.CreateObject("ADODB.RecordSet")
	rsorders.Open testexistSQL, conn
	'Response.Write "<font size=2>"&testexistSQL&"</font><br><Br>"

	if rsorders.BOF AND rsorders.EOF then
		'NEK [Start] 1/6/2003
		'strSQL=strSQL & " AND Order LIKE '" & Request.Form("txtOrderNo") & "%'"
		'strSQL=strSQL & " AND Order between  '" & Session("txtOrderNoStart")& "%' and '" & Session("txtOrderNoEnd")& "%'"
		if Session("txtOrderNoStart")<>"" and  Session("txtOrderNoEnd")<> "" then 
			strSQL = strSQL & "  and (ordhdr.order between '" & Session("txtOrderNoStart")& "' and '" & Session("txtOrderNoEnd") & "')"
		else	
			'Response.Write "IN ELSE<br><br>"
			if Session("txtOrderNoStart")<>"" and Session("txtOrderNoEnd")= "" then
				strSQL = strSQL & "  and ordhdr.order >= '" & Session("txtOrderNoStart")& "'"
			end if 
			if Session("txtOrderNoStart")= "" and Session("txtOrderNoEnd")<> "" then
				strSQL = strSQL & "  and ordhdr.order <= '" & Session("txtOrderNoEnd")& "'"
			end if 
		end if 
	else 
		if Session("txtOrderNoStart")<>"" and  Session("txtOrderNoEnd")<> "" then 
			strSQL = strSQL & "  and (ordhdr.order between '" & Session("txtOrderNoStart")& "' and '" & Session("txtOrderNoEnd") & "')"
		else	
			'Response.Write "IN ELSE<br><br>"
			if Session("txtOrderNoStart")<>"" and Session("txtOrderNoEnd")= "" then
				strSQL = strSQL & "  and ordhdr.order >= '" & Session("txtOrderNoStart")& "'"
			end if 
			if Session("txtOrderNoStart")= "" and Session("txtOrderNoEnd")<> "" then
				strSQL = strSQL & "  and ordhdr.order <= '" & Session("txtOrderNoEnd")& "'"
			end if 
		end if 
			
	end if	
else if Session("txtOrderNoStart")<>"" and Session("txtOrderNoEnd")= "" then
				strSQL = strSQL & "  and ordhdr.order >= '" & Session("txtOrderNoStart")& "'"
else if Session("txtOrderNoStart")= "" and Session("txtOrderNoEnd")<> "" then
	strSQL = strSQL & "  and ordhdr.order <= '" & Session("txtOrderNoEnd")& "'"
		'NEK [End ]1/6/2003 	
end if 
end if 
	
end if

if Session("BeginDate") <> "" then
	strSQL = strSQL & " AND dtos(Entered) >='" & Session("BeginDate") & "'"
end if 
if Session("EndDate") <>"" then
	strSQL = strSQL & " AND dtos(Entered) <='" & Session("EndDate") & "'"
end if
if Session("Status") <> "ALL" then
	strSQL = strSQL & " AND Ordhdr.Status='" & Session("Status") & "'"
end if
'NEK[Start] 1/6/2003
if Trim(Session("txtStartShipDate")) <> "" or Trim(Session("txtEndShipDate")) <>"" then
	strSQL = strSQL & " AND Start between ctod('" & Trim(Session("txtStartShipDate")) & "') and ctod('" & Trim(Session("txtEndShipDate"))& "')"
end if 
if Trim(Session("txtCompleteDate")) <> ""  then
	strSQL = strSQL & " AND Complete = ctod('" & Trim(Session("txtCompleteDate")) & "')" 
end if 
if Trim(Session("StyleColor"))  = "" then
else
	if Trim(Session("selectSeason")) = "All" then
	'Multiple Seasons WMA 5/24/2004 START
	'strSQL = strSQL & " AND ordhdr.Season in('" & Replace(Trim(Session("StyleColor")&",*"),",","','") & "') "
	strSeasConArr  = split(Session("StyleColor")&",*",",")
	intSeasConCount = int(UBound(strSeasConArr)/24)
	strSQL = strSQL & " AND ("
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
			strSQL = strSQL & " ordhdr.Season in("& strSQLall &") " 
		else
			strSQL = strSQL & " or ordhdr.Season in(" & strSQLall & ") " 
		end if
		strSQLall = ""
	next
	strSQL = strSQL & " )"
    'Multiple Seasons WMA 5/24/2004 END
	else	
		strSQL = strSQL & " AND ordhdr.Season = '" & Trim(Session("selectSeason")) & "'"	
	end if 
end if
if Trim(Session("selectDivision")) <> "All"  then
	strSQL = strSQL & " AND ordhdr.cDivision = '" & Trim(Session("selectDivision")) & "'"
end if 
'NEK [End]1/6/2003

strSQL = strSQL &" order by ordhdr.order"
'Response.Write("<br><font size=3>"&strSQL&"</font><br>")
'Response.Write("<br><font size=3>Connection:"&Application("DataConnectionString")&"</font><br>")
rsOrdersMatch.Open strSQL, conn ,2 ,4 

'Response.Write("<br><font size=3>RecordCount = "&rsOrdersMatch.EOF  &"</font>")
'Response.End 
'NEK [Start] 1/6/2002 If the result is one Order only it redirects to its details directly
if rsOrdersMatch.RecordCount =1 then
	Response.Redirect ("OrdStatusDetail.asp?OrderNo=" & rsOrdersMatch.Fields("Order").Value &"&Type=" & rsOrdersMatch.Fields("cordtype").Value )
end if 	
'NEK [End] 1/6/2002


IF Not (rsOrdersMatch.EOF And rsOrdersMatch.BOF) Then
rsOrdersMatch.PageSize = NumPerPage
TotalPages = rsOrdersMatch.PageCount 
rsOrdersMatch.AbsolutePage = CurrPage
End IF

%>


<HTML>
<HEAD>
<meta http-equiv="Content-Type" content="text/html; charset=windows-1256">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<Title>CRM - Check Order Status</Title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</HEAD>
<body bgcolor="#aecae6" topmargin="0" leftmargin="0" >

<%IF strFile = "cust" Then%>

<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><BR></p>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><BR></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>

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
 <%
End IF%>

<Table width=95% align=center border=1 height="50">
<TR>
<TD class="Title">Check Order Status</TD>
</TR>
</Table>

<%IF compWork = "Y" Then%>

<br>
  <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%">
      <% 
	if rsOrdersMatch.EOF AND rsOrdersMatch.BOF then%>	
<table border=0 >
<tr><td>
		<b>No orders match your criteria. </b>
</td></tr>
<tr><td><A HREF="ordStatus.asp">back</A>
</td></tr>
</table>
	<%
	else%>
      <font class=dark_cell><strong>The following Orders match the entered selection criteria:</strong></font>
      

</td>
    </tr>
  </table>

<div align="center">
  <center>

<table border="1" width="95%" bordercolor="#111111" cellspacing="0" height="63" style="border-collapse: collapse" cellpadding="0">
	<%IF Trim(Session("customerid")) <> ""  Then%>
		<tr>
		    <td class="dark_cell" height="19">
				<%=session("CustField")%>
			</td>
		    <td colspan=7 class="light_cell" height="19">
				
				<%=Session("RSCust").fields("account")%> - <%=Session("RSCust").fields("btname")%>
				
			</td>
		</tr>
	<%end if%>
	<tr>
        <td class="dark_cell" height="11">Order #</td>
        <td class="dark_cell" height="11">Status</td>
        <td class="dark_cell" height="11" align=left>Customer</td>
        <td class="dark_cell" height="11" align=center>Entered</td>
		
		
        <td class="dark_cell" height="11" align=right>Booked Qty.</td>
        <td class="dark_cell" height="11" align=right >Shipped Qty.</td>
		<td class="dark_cell" height="11" align=right >Amount&nbsp</td>
		<td class="dark_cell" height="11" align=left>&nbsp<%=session("StoreField")%></td>
		<!--td class="dark_cell" height="11"><%=session("StoreField")%></td-->
        
    </tr>	
	
	<%
		
 'Set Count to Zero
  Dim Count
  Count = 0
  Do while not rsOrdersMatch.EOF And Count < rsOrdersMatch.PageSize%>
		<tr>
			<td class="light_cell" height="21">
				<A HREF="OrdStatusDetail.asp?OrderNo=<%= rsOrdersMatch("Order")%>&Type=<%=rsOrdersMatch.Fields("cordtype").Value %>"><%= rsOrdersMatch("Order")%></A>
			</td>
			<td class="light_cell" height="21">
				<%Select case rsOrdersMatch("Status")
					case "O":
						Response.Write "Open"
					case "C":
						Response.Write "Complete"
					case "H":
						Response.Write "Hold"
					case "B":
						Response.Write "Reported"
					case "X":
						Response.Write "Canceled"
				end select%>
			</td>
			<td class="light_cell" height="21" align=left><%response.write rsOrdersMatch("btname")%></td>
			<td class="light_cell" height="21" align=center><%If rsOrdersMatch("Entered")<>"12:00:00 AM" then response.write rsOrdersMatch("Entered") Else response.write "N/A"%></td>
		
			<td class="light_cell" height="21" align=right><%response.write rsOrdersMatch("Book")%>&nbsp</td>
			<td class="light_cell" height="21" align=right><%response.write rsOrdersMatch("Ship")%>&nbsp</td>
			<td class="light_cell" height="21" align=right>
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & rsOrdersMatch("BookAmt")%>
			<%else%>
				<%=rsOrdersMatch("BookAmt") & Session("Currency")%>
			<%end if %>
			<%'response.write "$"&rsOrdersMatch("BookAmt")%>
			
			&nbsp</td>
			<td class="light_cell" align=left height="21">
				<% if Trim(rsOrdersMatch("Store"))= "" then
						Response.Write "Main" & " - " & rsOrdersMatch("Stname")
					else
						Response.Write rsOrdersMatch("Store") & " - " & rsOrdersMatch("Stname")
						
					end if%>
			</td>
			<!--td class="light_cell" height="21">
				<% if Trim(rsOrdersMatch("Store"))= "" then
						Response.Write "Main" & " - " & rsOrdersMatch("Stname")
					else
						Response.Write rsOrdersMatch("Store") & " - " & rsOrdersMatch("Stname")
						
					end if%>
			</td>
			<%'NEK [Start]get Division Name by Code
			strgetDivisionByCode = "select Distinct(Cdiscrep) as Description from codes where cdefcode+ccode_no+crltfield+cfld_name ='N" & rsOrdersMatch("cdivision") & "NCDIVISION'"	
			'Response.Write "<font size=2>"&strgetDivisionByCode&"</font><br>"
			set rsgetDivisionByCode = server.CreateObject("adodb.recordset")
			rsgetDivisionByCode.Open strgetDivisionByCode,conn,2,4
			strgetSeasonByCode = "select Distinct(Cdiscrep) as Description from codes where cdefcode+ccode_no+crltfield+cfld_name ='N" & rsOrdersMatch("season") & "NSEASON'"	
			set rsgetSeasonByCode = server.CreateObject("adodb.recordset")
			rsgetSeasonByCode.Open strgetSeasonByCode,conn,2,4
			'NEK [End]get Division Name by Code
			%>
			<td class="light_cell" height="21" align=center><%If rsOrdersMatch("Start")<>"12:00:00 AM" then response.write rsOrdersMatch("Start") Else response.write "N/A"%></td>
			<td class="light_cell" height="21" align=center><%If rsOrdersMatch("Entered")<>"12:00:00 AM" then response.write rsOrdersMatch("Entered") Else response.write "N/A"%></td>
			<td class="light_cell" height="21" align=center><%If rsOrdersMatch("Complete")<>"12:00:00 AM" then response.write rsOrdersMatch("Complete") Else response.write "N/A"%></td>
			<td class="light_cell" height="21"><%If rsgetSeasonByCode.EOF then   Response.Write "N/A" else  response.write rsgetSeasonByCode.Fields("Description").Value%></td>
			<td class="light_cell" height="21"><%If rsgetDivisionByCode.EOF then response.write "N/A" Else response.write rsgetDivisionByCode.Fields("Description").Value %></td-->				
			
		</tr>	
		<%
		count = count + 1
		rsOrdersMatch.MoveNext()
	Loop
end if 
%>
</table>
  </center>
</div>
<BR>
<%	End IF %>

<Table width=95% align=center> <TR><TD align=center>

<%
IF Not (rsOrdersMatch.EOF And rsOrdersMatch.BOF) Then
Response.Write ("Page " & Currpage & "of " & rsOrdersMatch.PageCount & "<br>")
End IF

'Display next/prev 
IF CurrPage > 1 THEN
	'show prev %>
	<A href="ordstatusresponse.asp?chkCust=<%=request("chkCust")%>&Currpage=<%=Currpage-1%>"><img border=0 src="../images/<%=Session("Theme")%>/back.gif"></A>&nbsp;
<%END IF
'Show next Button
IF rsOrdersMatch.EOF And rsOrdersMatch.Bof THen
Else
	IF Cint(CurrPage)<> Cint(rsOrdersMatch.PageCount) Then%>
		<A href="ordstatusresponse.asp?chkCust=<%=request("chkCust")%>&Currpage=<%=Currpage+1%>"><img border=0 src="../images/<%=Session("THEME")%>/next.gif"></A>
	<%End IF
End IF

%>
</TD></TR></Table>
<br>
<br>
</BODY>
</HTML>