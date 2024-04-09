<%@ Language=VBScript %>
<%
Response.Buffer=True
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

Set Session("RSStyStruct")=server.CreateObject("ADODB.recordset")
if not Session("RSStyStruct") is nothing then
	if Session("RSStyStruct").state <> 0 then
		Session("RSStyStruct").close
	end if
end if
Session("getstyle")=""
Session("LongDesc") = ""
Session("ShortDesc") = ""
Session("Price") = ""
Session("Disc") = ""
Session("Comm") = ""
Session("Grp") = ""
session("Discount") = ""
Session("DefTax")  = ""
session("Tax") = ""
Session("Taxable")   = ""
Session("ordAmount") = ""
Session("ordQty")= ""
Session("text1") = ""
Session("text2") = ""
Session("text3") = ""
Session("text4") = ""
Session("text5") = ""
Session("text6") = ""
Session("text7") = ""
Session("text8") = ""
strSql="select * from icistru where citemrecty='U'"
Session("RSStyStruct").open strSql,conn 
'get def completion date
Set RSSetups = Server.CreateObject("ADODB.RecordSet")
RSSetups.Open "SELECT Mdata_def FROM setups where Capp_id+Cfld_name = 'SOM_COMPDATE'",Conn

intCompDate = RSSetups("Mdata_def")
dTo = date() + intCompDate
Session("Completed") = dTo
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
IF Request.Form("selectSeason")<>"" Then
	Session("selectSeason") = Request.Form("selectSeason")
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

'Set Cursor Location
rsOrdersMatch.CursorLocation = 2'adUseClient
'Set Cachesize to no. of records/page
rsOrdersMatch.CacheSize = NumPerPage
'Build the query according to entered values ..

strSQL = "SELECT ordhdr.order, Ordhdr.Book,Ordhdr.Ship, Ordhdr.Bookamt,customer.btname, ordhdr.status, ordhdr.entered, ordhdr.store,ordhdr.cordtype,customer.stname,ordhdr.complete,ordhdr.season,ordhdr.cdivision ,ordhdr.start FROM Ordhdr, customer WHERE ordhdr.account = customer.account "
'strSQL = strSQL & " ordhdr.account+ordhdr.cordtype+ordhdr.order like '" & CustID &"%'"
IF Trim(custID) <> ""  Then
	strSQL = strSQL & " and ordhdr.account+ordhdr.cordtype+ordhdr.order like '" & CustID &"%'"
	strSQL = strSQL & " and (customer.Type+customer.account+customer.store like 'M" & CustID & "%' or customer.Type+customer.account+customer.store like 'S" & CustID & "%')"
	
End if
if Session("Authority") = "" then
	strSQL = strSQL &" and ordhdr.rep1 = '"&Trim(Session("rep"))&"'"
end if
'strSQL = strSQL & " and (customer.Type+customer.account+customer.store like 'M" & CustID & "%' or customer.Type+customer.account+customer.store like 'S" & CustID & "%')"
strSQL = strSQL & " and customer.store=ordhdr.store "

if Session("txtOrderNoStart")<> "" and  Session("txtOrderNoEnd")<> "" then
	strSQL = strSQL & "  and (ordhdr.order between '" & trim(Session("txtOrderNoStart")) & "' and '" & trim(Session("txtOrderNoEnd")) & "')"
elseif trim(Session("txtOrderNoStart"))<>"" and trim(Session("txtOrderNoEnd"))= "" then
	strSQL = strSQL & "  and ordhdr.order >= '" & trim(Session("txtOrderNoStart"))& "'"
elseif trim(Session("txtOrderNoStart"))= "" and trim(Session("txtOrderNoEnd"))<> "" then
	strSQL = strSQL & "  and ordhdr.order <= '" & trim(Session("txtOrderNoEnd"))& "'"
end if 
if Session("BeginDate") <> "" then
	strSQL = strSQL & " AND dtos(Entered) >='" & Session("BeginDate") & "'"
end if 
if Session("EndDate") <>"" then
	strSQL = strSQL & " AND dtos(Entered) <='" & Session("EndDate") & "'"
end if

if Trim(Session("txtStartShipDate")) <> "" or Trim(Session("txtEndShipDate")) <>"" then
	strSQL = strSQL & " AND Start between ctod('" & Trim(Session("txtStartShipDate")) & "') and ctod('" & Trim(Session("txtEndShipDate"))& "')"
end if 
if Trim(Session("txtCompleteDate")) <> ""  then
	strSQL = strSQL & " AND Complete = ctod('" & Trim(Session("txtCompleteDate")) & "')" 
end if 
if Trim(Session("selectSeason")) = "All" then
	'Multiple Seasons WMA 5/24/2004 START
	'strSQL = strSQL & " AND ordhdr.Season = '" & Trim(Session("selectSeason")) & "'"
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
if Trim(Session("selectDivision")) <> "All"  then
	strSQL = strSQL & " AND ordhdr.cDivision = '" & Trim(Session("selectDivision")) & "'"
end if 

strSQL = strSQL & " AND Ordhdr.Status='B'"
strSQL = strSQL &" order by ordhdr.order"
'Response.Write("<br><font size=3>"&strSQL&"</font><br>")
'Response.Write("<br><font size=3>Connection:"&Application("DataConnectionString")&"</font><br>")
rsOrdersMatch.Open strSQL, conn ,2 ,4 

'Response.Write("<br><font size=3>RecordCount = "&rsOrdersMatch.EOF  &"</font>")
'Response.End 
'NEK [Start] 1/6/2002 If the result is one Order only it redirects to its details directly
if rsOrdersMatch.RecordCount =1 then
	Response.Redirect ("modifyorder.asp?OrderNo=" & rsOrdersMatch.Fields("Order").Value &"&Type=" & rsOrdersMatch.Fields("cordtype").Value )
end if 	
'NEK [End] 1/6/2002


IF Not (rsOrdersMatch.EOF And rsOrdersMatch.BOF) Then
rsOrdersMatch.PageSize = NumPerPage
TotalPages = rsOrdersMatch.PageCount 
rsOrdersMatch.AbsolutePage = CurrPage
End IF
'Response.Write "<font size=2>"&rsOrdersMatch.RecordCount 
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

	<!-- ARD -->
	<TD colspan=13>
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
<TD class="Title">Select Order</TD>
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
<tr><td><A HREF="getorder.asp">back</A>
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
	
	<tr>
        <td class="dark_cell" height="11">Order #</td>
        <td class="dark_cell" height="11">Customer</td>
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
				<A HREF="modifyorder.asp?come=F&OrderNo=<%=rsOrdersMatch("Order")%>&Type=<%=rsOrdersMatch.Fields("cordtype").Value %>"><%= rsOrdersMatch("Order")%></A>
			</td>
			<td class="light_cell" height="21"><%response.write rsOrdersMatch("btname")%>&nbsp</td>
			<td class="light_cell" height="21" align=center><%If rsOrdersMatch("Entered")<>"12:00:00 AM" then response.write rsOrdersMatch("Entered") Else response.write "N/A"%></td>
		
			<td class="light_cell" height="21" align=right><%response.write rsOrdersMatch("Book")%>&nbsp</td>
			<td class="light_cell" height="21" align=right><%response.write rsOrdersMatch("Ship")%>&nbsp</td>
			<td class="light_cell" height="21" align=right><%response.write "$"&rsOrdersMatch("BookAmt")%>&nbsp</td>
			<td class="light_cell" align=left height="21">
				<% if Trim(rsOrdersMatch("Store"))= "" then
						Response.Write "Main" & " - " & rsOrdersMatch("Stname")
					else
						Response.Write rsOrdersMatch("Store") & " - " & rsOrdersMatch("Stname")
						
					end if%>
			</td>			
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
	<A href="orderlist.asp?chkCust=<%=request("chkCust")%>&Currpage=<%=Currpage-1%>"><img border=0 src="../images/<%=Session("Theme")%>/back.gif"></A>&nbsp;
<%END IF
'Show next Button
IF rsOrdersMatch.EOF And rsOrdersMatch.Bof THen
Else
	IF Cint(CurrPage)<> Cint(rsOrdersMatch.PageCount) Then%>
		<A href="orderlist.asp?chkCust=<%=request("chkCust")%>&Currpage=<%=Currpage+1%>"><img border=0 src="../images/<%=Session("THEME")%>/next.gif"></A>
	<%End IF
End IF

%>
</TD></TR></Table>
<br>
<br>
</BODY>
</HTML>