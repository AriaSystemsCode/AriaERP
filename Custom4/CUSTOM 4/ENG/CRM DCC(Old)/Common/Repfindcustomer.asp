<%@ Language=VBScript %>
<%
Response.Buffer=True
Response.Expires=-1%>
	
<%

If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
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
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF
%>

<HTML>
<HEAD>
<Title>CRM - Select Customer</Title>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/order.css">
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<SCRIPT LANGUAGE=javascript>
<!--
function SubmitNext()
{
	document.FORM2.action = 'orderhsave.asp';
	document.FORM2.submit()
}
//-->
</SCRIPT>


</head>
<body>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br><br><br><br><br><br><br><br><br><br><br></p>

<%If request("Cust")="C" Then%>
<FORM action="Repfindcustomer.asp?Cust=C" method=POST id=form1 name=form1>
<%End If%>
<%If request("Cust")="I" Then%>
<FORM action="Repfindcustomer.asp?Cust=I" method=POST id=form1 name=form1>
<%End If%>
<div align="center">
  <center>
<table bordercolor="#111111" border="1" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	 <tr>
		<td Align="center" width="80%" class="dark_cell" >
			<font color=white><%=session("CustField")%> :</font><INPUT name="txtCustomer" size="5"  maxlength="5" value="<%=Request("txtCustomer")%>">
			<font color=white>Name:</font><INPUT name="txtCustomerName" size="20"  maxlength="30" value="<%=Request("txtCustomerName")%>">  
			<font color=white>Phone:</font><INPUT name="txtCustomerPhone" size="15"  maxlength="15" value="<%=Request("txtCustomerPhone")%>">    
			<%if Session("Authority")="Full" then%>
			<font color=white>Status:</font>
			<select name="selectStatus">
				<option value="ALL" <%if Request("selectStatus")="ALL" then %>selected <%end if%>>ALL</option>
				<option value="A" <%if Request("selectStatus")="A" then %>selected <%end if%>>Active</option>
				<option value="X" <%if Request("selectStatus")="X" then %>selected <%end if%>>Cancelled</option>
				<option value="H" <%if Request("selectStatus")="H" then %>selected <%end if%>>Hold</option>
				<option value="P" <%if Request("selectStatus")="P" then %>selected <%end if%>>Potential</option>
			</select>
			<%end if%>
		</td>
		<td Align="left" class="dark_cell" >
			&nbsp;<input type="submit" name="B2" value="Get <%=session("CustField")%>">&nbsp;<input type="reset" name="B3" value="Reset"></td>
     </tr>
     </table>
  </center>
</div>
</FORM>
<BR>
<BR>

<%' Display the second table %>
<%
set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
set rsCehckCustomerExist =Server.CreateObject ("ADODB.Recordset")
'Response.Write "<font size=3>Authority Session "&Session("Authority")&"</font><br>"

'HDM [Start] re-design the logic of filtering data
strSQL="SELECT Account,Stname,Status,Phone1 FROM Customer "

Dim strWhereClause   'as string to hold the where calause
'Response.Write(Trim(Request("txtCustomer")))
	'Do we have an account to search
	If Trim(Request("txtCustomer")) <> "" Then
		'strWhereClause = " And account Like '" & Trim(Ucase(Request("txtCustomer"))) &"%'"
		strWhereClause = " And Upper(account) Like '" & Trim(Ucase(Request("txtCustomer"))) &"%'"
	End If
	
	'Do we have Phone?
	If Trim(Ucase(Request("txtCustomerPhone"))) <> "" Then
		strWhereClause = strWhereClause & " And Phone1 Like'" & Trim(Ucase(Request("txtCustomerPhone"))) &"%'"
	End If
	
	
	'Do we have a customer name?
	If Trim(Ucase(Request("txtCustomerName"))) <> "" Then
	'	strWhereClause = strWhereClause & " And stName Like '" & Trim(Ucase(Request("txtCustomerName"))) &"%'"
		strWhereClause = strWhereClause & " And Upper(stName) Like '" & Trim(Ucase(Request("txtCustomerName"))) &"%'"
	End If	

	'Here dedtermine if the user can have access to all customers or not
	If Session("Authority")="Full" Then
		'IF the user has full access, did he choose a certain status
		If Trim(UCase(Request("selectStatus")))="ALL" Then
			'No Status condition needed
		Else
			'Yes user choose a ceertain status then includee it in the where clause
			If Trim(Ucase(Request("selectStatus"))) = "ALL" Then
			Else
				strWhereClause = strWhereClause & " And Status='" & Trim(Ucase(Request("selectStatus"))) &"'"
			End If
			
		End If
	Else
		'HDM [start] Rep. can find any customer regardless his status
		'strWhereClause = strWhereClause & " And Status='A'"
		strWhereClause = strWhereClause & " And (SalesRep='"& Session("Rep") &"' or Rep2='" & Session("Rep") &"')"
	End If
	strWhereClause = "Where " & "Type+account+store like 'M%' " & strWhereClause 
strSQL = strSQL & strWhereClause

'if Trim(Ucase(Request("txtCustomer"))) <> "" or Trim(Ucase(Request("txtCustomerName"))) <> "" or Trim(Ucase(Request("txtCustomerPhone"))) <> "" then '1
'	if Session("Authority")="Full" then ''NEK' User in SR.txt with full authority or not
'		
'		if Trim(Ucase(Request("selectStatus"))) <> "ALL" then
'			'HDM strSQL= strSQL & " and status='"&Trim(Ucase(Request("selectStatus")))&"'"
'			strWhereClause= " where status='"&Trim(Ucase(Request("selectStatus")))&"'"
'		end if	
'	else''' User not in Sr.txt only have authority on his customers only
'		'HDM strSQL="SELECT Account,Stname,Status,Phone1 FROM Customer where 1=1 and Status='A' AND (Salesrep='" & Session("Rep") & "' OR Rep2='" & Session("Rep") & "')"
'		strSQL="SELECT Account,Stname,Status,Phone1 FROM Customer Where Status='A' AND (Salesrep='" & Session("Rep") & "' OR Rep2='" & Session("Rep") & "')"
'	end if	'NEK'end if of the if statement'
'	
'	if Trim(Ucase(Request("txtCustomer"))) <> "" then
'		'HDM strSQL = strSQL & " and Type+account+store like 'M" & Trim(Ucase(Request("txtCustomer")))&"%' "
'		If Trim(strWhereClause) = "" Then
'			strWhereClause = "Where "
'	end if	
'	if Trim(Ucase(Request("txtCustomerName"))) <> "" then
'		strSQL= strSQL & " and type+stname like 'M" & Trim(Ucase(Request("txtCustomerName")))&"%'"
'	end if
'	if Trim(Ucase(Request("txtCustomerPhone"))) <> "" then
'		strSQL= strSQL & " and type+phone1 like 'M" & Trim(Ucase(Request("txtCustomerPhone")))&"%'"
'	end if
'	
'else '1
'	if Session("Authority")="Full" then ''NEK' User in SR.txt with full authority or not
'		strSQL = "SELECT Account,Stname,Status,Phone1 FROM Customer WHERE Type+account+store like 'M%'"
'		if Trim(Ucase(Request("selectStatus"))) <> "ALL" and Trim(Ucase(Request("selectStatus"))) <> "" then
'			strSQL= strSQL & " and status='" & Trim(Ucase(Request("selectStatus"))) & "'"
'			Response.Write("HERE2<BR> "& Trim(Ucase(Request("selectStatus")))&"<BR>")
'		end if
'	else''' User not in Sr.txt only have authority on his customers only
'		strSQL = "SELECT Account,Stname,Status,Phone1 FROM Customer WHERE Type+account+store like 'M%' and Status='A' AND (Salesrep='" & Session("Rep") & "' OR Rep2='" & Session("Rep") & "') "
'	end if	'NEK'end if of the if statement'
'	'strSQL = "SELECT Account,Stname FROM Customer WHERE Type+account+store like 'M%' AND (Salesrep='" & Session("Rep") & "' OR Rep2='" & Session("Rep") & "') "
'end if'1
if Request.QueryString("CurPage")  = 0 Then
	CurPage = 1
else
	CurPage = Request.QueryString("CurPage")
end if
'Response.Write "<font size=3>"&strSQL&"</font>"
NumPerPage= 20
'Response.Write "<font size=3>"&strSQL&"</font>"
rsCehckCustomerExist.CursorLocation = 3
rsCehckCustomerExist.CacheSize = NumPerPage
'Response.Write("<font size=5>" & strSQL)
'Response.End 
rsCehckCustomerExist.Open strSQL,conn
rsCehckCustomerExist.Sort = "Account"
rsCehckCustomerExist.PageSize = NumPerPage
TotalPages = rsCehckCustomerExist.PageCount 
'Response.Write("<Font size='3'>" &strSQL &"</font><br>")

if rsCehckCustomerExist.BOF AND rsCehckCustomerExist.EOF then
	%>
	<Table width=95% align=center>
	<TR>
	<TD>
	<b>No <%=session("CustField")%>(s) found</b>
	</TD>
	</TR>
	</Table>
	<%
else
	rsCehckCustomerExist.AbsolutePage = CurPage
	%>
	<div align="center">
	  <center>
	<table border="1" width=100% bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
		<TR>
			<TD width=25% class="dark_cell"><Font face="Arial"><Strong><%=session("CustField")%> ID</Strong></Font>
			</TD>
			<TD Width=40% class="dark_cell"><Font face="Arial"><Strong><%=session("CustField")%> Name</Strong></Font>
			</TD>
			<TD Width=25% class="dark_cell"><Font face="Arial"><Strong>Phone</Strong></Font>
			</TD>
			<TD Width=10% class="dark_cell"><Font face="Arial"><Strong>Status</Strong></Font>
			</TD>
		</TR>
	<%do while not rsCehckCustomerExist.eof and Count < NumPerPage %>
	<TR>
	<TD width="25%" class="light_cell">
		<%if request("Cust")="C" then%>
		<Font face="Arial" color="#ffffff">
		&nbsp;<a href="../Represolvcust.asp?CustID=<%=server.URLEncode(rsCehckCustomerExist("Account"))%>&Status=<%=rsCehckCustomerExist("Status")%>"><%=rsCehckCustomerExist("Account")%></a>
		</font>
		<%end if%>
		<%if request("Cust")="I" then%>
			&nbsp;<a href="../invoice/Repinvoice.asp?CustID=<%=rsCehckCustomerExist("Account")%>"><%=rsCehckCustomerExist("Account")%></a>
		<%end if%> &nbsp;</td>
		
	<TD width="25%" class="light_cell">
	&nbsp;<%=rsCehckCustomerExist("Stname")%> &nbsp;</td>
<TD width="25%" class="light_cell">&nbsp;
<%=rsCehckCustomerExist("Phone1")%>
 &nbsp;</td>

		<TD width="25%" class="light_cell">&nbsp;
	<%select case rsCehckCustomerExist("Status")
	case "A":
		Response.Write "Active"
	case "H":
		Response.Write "Hold"
	case "P":
		Response.Write "Potential"
	case "X":
		Response.Write "Cancelled"
	end select
	%>
	 &nbsp;</td>
	<% rsCehckCustomerExist.MoveNext()
	
			Count = Count + 1
		loop%>

</table>
<br><br>
	<%if CurPage > 1 then%>
	
		<a href="Repfindcustomer.asp?curpage=<%=curpage - 1%>&Cust=C&txtCustomer=<%=Request("txtCustomer")%>&txtCustomerName=<%=Request("txtCustomerName")%>&txtCustomerPhone=<%=Request("txtCustomerPhone")%>&selectStatus=<%=Request("selectStatus")%>"><img border=0 src="../images/<%=Session("theme")%>/back.gif"></a>&nbsp;
	<%end if%>
	<%
		Response.Write("<font size=2>Page " & CurPage & " of " & TotalPages & "</font>")
	%>&nbsp;&nbsp;&nbsp;
	<%if cint(CurPage) <> cint(TotalPages) then%>
	
		<a href="Repfindcustomer.asp?curpage=<%=curpage+ 1%>&Cust=C&txtCustomer=<%=Request("txtCustomer")%>&txtCustomerName=<%=Request("txtCustomerName")%>&txtCustomerPhone=<%=Request("txtCustomerPhone")%>&selectStatus=<%=Request("selectStatus")%>"><img border=0 src="../images/<%=Session("theme")%>/next.gif"></a>
	<%end if%>				  </center>
</div>
	<%end if ' for the customers if exist or not%>

</BODY>
</HTML>