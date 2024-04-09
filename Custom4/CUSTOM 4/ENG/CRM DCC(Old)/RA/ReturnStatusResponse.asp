<%@ Language=VBScript %>
<%
Response.Buffer=true
IF Session("ID")="" And Session("rep")="" Then
'	Response.Redirect("../default.asp")%>
<script language="javascript">
	parent.location.href ="../login.asp"
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
%>

<html>
<head>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
<title>CRM - Check Return Authorization Status</title>
</head>
<body topmargin="0" leftmargin="0" marginheight="0" marginwidth="0" bgcolor="#aecae6" >

<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br></p>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
	<TD colspan=13>
	<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>
 <%End IF%>
<Table border=1 width=95% height=50 align=center>
<TR>
<TD class=title>Check RA Status</TD>
</TR>
</Table>

<%IF compWork = "Y" Then%>

<%
set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
set rsRanosMatch = server.CreateObject("ADODB.RecordSet")
dim bDay,bMonth,bYear,eDay,eMonth,eYear
if Trim(Ucase(Request.Form("txtBeginDate")))<>"" then
bDay =day(Trim(Ucase(Request.Form("txtBeginDate")))) 
if bDay<10 then
	bDay = "0"&bDay
end if
bMonth = month(Trim(Ucase(Request.Form("txtBeginDate"))))
if bMonth<10 then
	bMonth = "0"&bMonth
end if
bYear = year(Trim(Ucase(Request.Form("txtBeginDate"))))

BeginDate = bYear&bMonth&bDay
end if 
if Trim(Ucase(Request.Form("txtEndDate")))<>"" then
eDay = day(Trim(Ucase(Request.Form("txtEndDate"))))
if eDay<10 then
	eDay = "0"&eDay
end if

eMonth = month(Trim(Ucase(Request.Form("txtEndDate"))))
if eMonth<10 then
	eMonth = "0"&eMonth
end if
eYear = year(Trim(Ucase(Request.Form("txtEndDate"))))
EndDate = eYear&eMonth&eDay
end if
'BeginDate = (Trim(Ucase(Request.Form("txtBeginDate"))))
'EndDate  = (Trim(Ucase(Request.Form("txtEndDate"))))
Status = Request.Form("selectStatus")
'Build the query according to entered values ..
if len(trim(session("rep")))>0 then
	
	strSQL = "SELECT DISTINCT Retauth.Rano,Retauth.Order, Retauth.Status,CodesReasn.Cdiscrep AS ReasonDesc, "
	strSQL = strSQL & " Retauth.Radate,Retauth.Void, Retauth.Store, Retauth.order, Retauth.invoice,"
	strSQL = strSQL & " Customer.Stname FROM Retauth, Customer,Codes AS CodesReasn"
	strSQL = strSQL & " WHERE "
	strSQL = strSQL & " (Customer.type+Customer.account+Customer.store like 'M"& Session("customerid") &"%' or "
	strSQL = strSQL & " Customer.type + Customer.account+Customer.store like 'S"& Session("customerid") &"%' ) and "
	strSQL = strSQL & " Customer.Store=Retauth.Store AND CodesReasn.Ccode_no = Retauth.Reason AND "
	strSQL = strSQL & " Retauth.Account + Retauth.rano like'" & Session("customerid") & "%'"
	'Response.Write(strSQL)
else
	'WMA Item cannot be found in the collection error debug
	'strSQL = "SELECT DISTINCT Retauth.Rano,Retauth.Order, Retauth.Status, CodesReason.Cdiscrep AS easonDesc,"
	strSQL = "SELECT DISTINCT Retauth.Rano,Retauth.Order, Retauth.Status, CodesReasn.Cdiscrep AS reasonDesc,"
	strSQL = strSQL & " Retauth.Radate,Retauth.Void, Retauth.Store, Retauth.order, Retauth.invoice,"
	strSQL = strSQL & " Customer.Stname FROM Retauth, Customer ,Codes AS CodesReasn"
	strSQL = strSQL & " WHERE "
	strSQL = strSQL & " (Customer.type+Customer.account+Customer.store like 'M"& Session("ID") &"%' or "
	strSQL = strSQL & " Customer.type+Customer.account+Customer.store like 'S"& Session("ID") &"%' ) and "
	strSQL = strSQL & " Customer.Store=Retauth.Store AND CodesReasn.Ccode_no = Retauth.Reason AND "
	strSQL = strSQL & " Retauth.Account+Retauth.rano like'" & Session("ID") & "%'"
	
end if
if Trim(Request.Form("txtRanoNo"))<>"" then
	if len(trim(session("rep")))>0 then
		testexistSQL = "SELECT Cartons, Auth, Authamt, Rano,Status,Radate,Void, Store, Reason, Cdivision, Nreta_bud, Nreta_rec, Nreta_can, Nreta_opn, Nrtopnamt FROM Retauth WHERE Account='" & Session("customerID") & "' AND Rano='" & Request.Form("txtRanoNo") & "'"
	else
     	testexistSQL = "SELECT Cartons, Auth, Authamt, Rano,Status,Radate,Void, Store, Reason, Cdivision, Nreta_bud, Nreta_rec, Nreta_can, Nreta_opn, Nrtopnamt FROM Retauth WHERE Account='" & Session("ID") & "' AND Rano='" & Request.Form("txtRanoNo") & "'"	
	end if
	
	rsRanosMatch.Open testexistSQL, conn
	
	if rsRanosMatch.BOF AND rsRanosMatch.EOF then
		strSQL=strSQL & " AND Rano LIKE '" & Request.Form("txtRanoNo") & "%'"
	
	else
	
		'if len(trim(session("rep")))>0 then
		'	Response.Redirect ("repReturnStatusDetail.asp?RanoNo=" & Request.Form("txtRanoNo"))
		'else
		  Response.Redirect ("ReturnStatusDetail.asp?RanoNo=" & Request.Form("txtRanoNo"))
		'end if
	 end if
	 rsRanosMatch.close()
end if

if BeginDate <> "" then
strSQL = strSQL & " AND dtos(Radate) >='" & BeginDate & "'"
end if 
if EndDate <>"" then
strSQL = strSQL & " AND dtos(Radate) <='" & EndDate & "'"
end if
if Status <> "ALL" then
strSQL = strSQL & " AND Retauth.Status='" & Status & "'"
end if



'the strSQL was build above .. 
'Now start paging.
Const NumPerPage = 25
Dim CurPage
If Request.QueryString("CurPage") = "" then
    CurPage = 1 'We're on the first page
Else
    CurPage = Request.QueryString("CurPage")
End if

set rsRanosMatchResult = server.CreateObject("ADODB.RecordSet")
rsRanosMatchResult.CursorLocation = 3 ' adUseClient
rsRanosMatchResult.CacheSize = NumPerPage

If Request.QueryString("CurPage") = "" then
	strSQL = strSQL
else
	strSQL = Request.form("strSQL")
end if


rsRanosMatchResult.Open strSQL, conn


%>

<BR>
  <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%">
      <% 
	if rsRanosMatchResult.EOF AND rsRanosMatchResult.BOF then%>	
<table border=0 >
<tr><td>
		<b>There are no records matching the entered selection criteria. </b>
</td></tr>
<tr><td>
<%If trim(Session("ID"))<>"" Then%>
<A HREF="returnStatus.asp">back</A>
<%ELSE%>
<A HREF="repchkra.asp">back</A>
<%End IF%>
</td></tr>
</table>
	<%
	else
		
		rsRanosMatchResult.MoveFirst()
		rsRanosMatchResult.PageSize = NumPerPage

		Dim TotalPages
		TotalPages = rsRanosMatchResult.PageCount
		rsRanosMatchResult.AbsolutePage = CurPage

		'Counting variable for our recordset
		Dim count%>
	
<font class=dark_cell><strong>The following Return Authorizations match the entered selection criteria:</strong></font>

      </td>
    </tr>
  </table>

<br>
<div align="center">
  <center>
<table border="1" width="95%" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
	<tr>
        <td class="dark_cell">
			<strong><%=session("CustField")%>
		</td>
        <td colspan=6 class="light_cell">
			
						
			<%=Session("RSCust").fields("account")%> - <%=Session("RSCust").fields("btname")%>
			
			
		</td>
    </tr>
	<tr>
        <td class="dark_cell"><strong>R/A #</strong></td>
        <td class="dark_cell"><strong>Invoice #</strong></td>
        <td class="dark_cell"><strong>Order #</strong></td>
        <td class="dark_cell"><strong>Status</strong>
        </td>
        <td class="dark_cell" align=center><strong>Entered</strong>
        </td>
        <td class="dark_cell" align=center><strong>Void After</strong>
        </td>
        
        <td class="dark_cell"><strong>Ship From</strong>
        </td>
    </tr>	

	
	<%
		Count = 0
		Do while not rsRanosMatchResult.EOF And Count < rsRanosMatchResult.PageSize%>
			<tr>
				<td class="light_cell">
					<A HREF="ReturnStatusDetail.asp?RanoNo=<%= rsRanosMatchResult("Rano")%>"><%= rsRanosMatchResult("Rano")%></A> &nbsp;</td>
				<td class="light_cell">
				<%if trim(rsRanosMatchResult("invoice"))= "" then%>
					N/A
				<%else%>
					<%= rsRanosMatchResult("invoice")%> &nbsp;
				<%end if%></td>
				<td class="light_cell">
				<%if trim(rsRanosMatchResult("order"))= "" then%>
					N/A
				<%else%>
					<%= rsRanosMatchResult("order")%> &nbsp;
				<%end if%>
				</td>
				<td class="light_cell">
					<%Select case rsRanosMatchResult("Status")
						case "O":
							Response.Write "Open"
						case "C":
							Response.Write "Complete"
						case "E":
							Response.Write "Electronic"
					end select%> &nbsp;</td>
				<td class="light_cell" align=center><%= rsRanosMatchResult("Radate")%> &nbsp;</td>
				<td class="light_cell" align=center><%= rsRanosMatchResult("Void")%> &nbsp;</td>
				<!--td class="light_cell" align=left><%= rsRanosMatchResult("reasonDesc")%> &nbsp;</td-->
				<td class="light_cell">
					<% if Trim(rsRanosMatchResult("Store"))= "" then
							Response.Write "Main" & " - " & rsRanosMatchResult("Stname")
						else
							Response.Write rsRanosMatchResult("Store") & " - " & rsRanosMatchResult("Stname")
						
						end if%> &nbsp;</td>
			</tr>	
			<%
			Count = Count + 1
			rsRanosMatchResult.MoveNext()
		Loop
	
%>

<SCRIPT LANGUAGE=javascript>
<!--
function SubmitNavigation(formaction)
{
	document.form1.action = formaction;
	document.form1.submit();
	
}
//-->
    </SCRIPT>
</table>
  </center>
</div>
<table width =95% align=center>
<tr>
	<form action="" name="form1" id="form1" method="post">
	<%if trim(Request.QueryString ("curpage")) = "" then
		searchQuery = strSQL
	else
		searchQuery = Request.form("strSQL")
	end if
	%>
	<Input type = "HIDDEN" name ="strSQL" Value ="<%=searchQuery%>">
	<td colspan = 5 align="center">
	<%Response.Write( "Page " & CurPage & " of " & TotalPages & "<br>")
	if CurPage > 1 then
        'We are not at the beginning, show the prev button%>
        <a href="javascript:SubmitNavigation('ReturnStatusResponse.asp?CurPage=<%=curpage-1%>')"><IMG border=0 src="../images/<%=Session("Theme")%>/back.gif"></a>
	<%End If

	if CInt(CurPage) <> CInt(TotalPages) then
        'We are not at the end, show a next button%>
       <a href="javascript:SubmitNavigation('ReturnStatusResponse.asp?CurPage=<%=curpage+1%>')" ><IMG border=0 src="../images/<%=Session("Theme")%>/next.gif"></a>
    <%End If
%>
	</td>
	</form>
</tr>
</table>
<BR>
<%
	conn.Close
	set rsRanosMatchResult = nothing
	End IF
end if ' for rsRanoMatchResult.BOF and EOF
%>
</BODY>
</HTML>