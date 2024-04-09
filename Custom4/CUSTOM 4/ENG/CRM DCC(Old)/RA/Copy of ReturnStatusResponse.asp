<%@ Language=VBScript %>
<%
Response.Buffer=true
IF Session("ID")="" And Session("rep")="" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
	</script>	
<%END IF 

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
<LINK REL=stylesheet HREF="../images/<%=Session("Theme")%>/RA.css" TYPE="text/css">
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
	<P>Your currently selected customer is <%=Session("customerid")%> - <%=RSTemp.fields("btname").value%></P>
	</TD>
	<TD align=right><a href="repcust.asp">Get Customer</a></TD>
	<!-- ARD -->
</TR>
</table>
 <%Set RSTemp=nothing
  ConnTemp.close
  Set ConnTemp=nothing

End IF%>
<Table border=1 width=95% height=50 align=center>
<TR>
<TD class=title>Check R/A Status</TD>
</TR>
</Table>

<%IF compWork = "Y" Then%>

<%
set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
set rsRanosMatch = server.CreateObject("ADODB.RecordSet")

BeginDate = (Trim(Ucase(Request.Form("txtBeginDate"))))
EndDate  = (Trim(Ucase(Request.Form("txtEndDate"))))
Status = Request.Form("selectStatus")
'Build the query according to entered values ..
if len(trim(session("rep")))>0 then
	strSQL = "SELECT DISTINCT Retauth.Rano,Retauth.Order, Retauth.Status, Retauth.Radate,Retauth.Void, Retauth.Store, Customer.Stname FROM Retauth, Customer WHERE Customer.Store=Retauth.Store AND Retauth.Account='" & Session("customerID") & "' AND Customer.Account ='" & Session("customerID") & "'"
else
	strSQL = "SELECT DISTINCT Retauth.Rano,Retauth.Order, Retauth.Status, Retauth.Radate,Retauth.Void, Retauth.Store, Customer.Stname FROM Retauth, Customer WHERE Customer.Store=Retauth.Store AND Retauth.Account='" & Session("ID") & "' AND Customer.Account ='" & Session("ID") & "'"
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

if BeginDate = "" then
else
strSQL = strSQL & " AND Radate >={" & BeginDate & "}"
end if 
if EndDate ="" then
else
strSQL = strSQL & " AND Radate <={" & EndDate & "}"
end if
if Status = "ALL" then
else
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
	
<strong>The following Return Authorizations matched the entered selection criteria:</strong>

      </td>
    </tr>
  </table>

<br>
<div align="center">
  <center>
<table border="1" width="95%" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
	<tr>
        <td class="dark_cell">
			<strong>Account
		</td>
        <td colspan=4 class="light_cell">
			
						
			<%=Session("RSCust").fields("account")%> - <%=Session("RSCust").fields("btname")%>
			
			
		</td>
    </tr>
	<tr>
        <td class="dark_cell"><strong>R/A #</strong>
        </td>
        <td class="dark_cell"><strong>Status</strong>
        </td>
        <td class="dark_cell"><strong>Entered</strong>
        </td>
        <td class="dark_cell"><strong>Void After</strong>
        </td>
              <td class="dark_cell"><strong>Store</strong>
        </td>
    </tr>	

	
	<%
		Count = 0
		Do while not rsRanosMatchResult.EOF And Count < rsRanosMatchResult.PageSize%>
			<tr>
				<td class="light_cell">
					
					
					<A HREF="ReturnStatusDetail.asp?RanoNo=<%= rsRanosMatchResult("Rano")%>"><%= rsRanosMatchResult("Rano")%></A> &nbsp;</td>
				<td class="light_cell">
					<%Select case rsRanosMatchResult("Status")
						case "O":
							Response.Write "Open"
						case "C":
							Response.Write "Complete"
						case "E":
							Response.Write "Electronic"
					end select%> &nbsp;</td>
				<td class="light_cell"><%= rsRanosMatchResult("Radate")%> &nbsp;</td>
				<td class="light_cell"><%= rsRanosMatchResult("Void")%> &nbsp;</td>
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
        <a href="javascript:SubmitNavigation('ReturnStatusResponse.asp?CurPage=<%=curpage-1%>')"><IMG src="../images/<%=Session("Theme")%>/back.gif"></a>
	<%End If

	if CInt(CurPage) <> CInt(TotalPages) then
        'We are not at the end, show a next button%>
       <a href="javascript:SubmitNavigation('ReturnStatusResponse.asp?CurPage=<%=curpage+1%>')" ><IMG src="../images/<%=Session("Theme")%>/next.gif"></a>
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