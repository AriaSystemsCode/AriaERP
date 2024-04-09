<%@ Language=VBScript %>
<%
Response.Buffer=true
Response.Expires=-1
%>

<html>
<head>
<link REL="stylesheet" HREF="../images/<%=session("Theme")%>/Ra.css" TYPE="text/css">
<title>CRM - Browse Invoices</title>
<SCRIPT LANGUAGE=javascript>
<!--
function setInvoice(invNo)
{
	window.opener.form1.Invoice.value=invNo
	window.close()
}
//-->
</SCRIPT>


</Head>
<body>
<form name=Form1 method=post>
<Table width=95% border=1 height=50 align=center>
<TR>
<TD class=title>Select Invoice</TD>
</TR>
</Table>
<%
Dim cnFoxConnection
Set cnFoxConnection = Server.CreateObject("ADODB.Connection")
cnFoxConnection.ConnectionString = Trim(Application("DataConnectionString"))
cnFoxConnection.Open

Dim rsInvoice
Set rsInvoice = Server.CreateObject("ADODB.RecordSet")
'Adjust paging 20 per page
NumPerPage = 20
Dim CurPage
If Request.QueryString("CurPage") = "" then
	CurPage = 1 'We're on the first page
Else
	CurPage = Request.QueryString("CurPage")
End If
rsInvoice.CacheSize = NumPerPage
Dim strInvoicesSQL
Dim strCustID
strCustID = Trim(Request.QueryString("CustID"))
strCustID = strCustID & Space(5-len(strCustID))
strOrder = Trim(Request.QueryString("Order"))
if trim(strOrder) = "" then
	strInvoicesSQL = "Select invoice,InvDate from INVHDR where Account+Invoice Like '"& strCustID & Trim(Request.QueryString("Inv"))&"%' and Status <> 'V'"
else
	strInvoicesSQL = "Select invoice,InvDate from INVHDR where Account+Invoice Like '"& strCustID & Trim(Request.QueryString("Inv"))&"%' and Status <> 'V' and Order like '" &trim(strOrder)& "%' "
end if
rsInvoice.Open strInvoicesSQL, cnFoxConnection, 3, 1
rowcount=rsInvoice.recordcount

If rsInvoice.EOF And rsInvoice.BOF Then
	Response.Write("<p Align='Center'><Font Size=3>No Invoices Found</font></p>")
	Response.Write("<SCRIPT LANGUAGE=javascript>")
	Response.Write("window.opener.form1.Invoice.value=''")
	Response.Write("</SCRIPT>")
	Response.End
End If

%>
<DIV Align='Center'>
<Table width='100%'>
<tr><td class="dark_cell" colspan='2'></td></tr>
<tr>
	<td class="dark_cell">Invoice</td>
	<td class="dark_cell">Date</td>
</tr>
<%rsInvoice.PageSize = NumPerPage
Dim TotalPages
TotalPages = rsInvoice.PageCount
if cint(CurPage) > cint(TotalPages) then CurPage=TotalPages
rsInvoice.AbsolutePage = CurPage
Do While Not rsInvoice.EOF and count < rsInvoice.PageSize%>
<tr>
	<td class="dark_cell"><A href="JAVASCRIPT:setInvoice('<%=rsInvoice("Invoice")%>')"><%=rsInvoice("Invoice")%></a></td>
	<td class="light_cell"><%=rsInvoice("InvDate")%></td>
</tr>
<%Count = Count + 1
rsInvoice.MoveNext%>
<%Loop%>
<%if rsInvoice.RecordCount > 20 then%>
	<tr align=center>
		<td colspan=2 valign="top"  align="center">
		<%if CurPage > 1 then %>  <!--We are not at the beginning, show the prev button -->
				<A href="javascript:document.Form1.action ='BrowInv.asp?CustID=<%=Trim(Request.QueryString("CustID"))%>&Inv=<%=Trim(Request.QueryString("Inv"))%>&curpage=<%=curpage - 1%>';document.Form1.submit();"><strong>Prev </strong></font></a><%Response.Write("  | ")%>
		<%End If
  		     if CInt(CurPage) <> CInt(TotalPages) then  'We are not at the end, show a next button
			%>	
			<A href="javascript:document.Form1.action ='BrowInv.asp?CustID=<%=Trim(Request.QueryString("CustID"))%>&Inv=<%=Trim(Request.QueryString("Inv"))%>&curpage=<%=curpage + 1%>';document.Form1.submit();"><strong>Next </strong></font></a>
			<%end if%>
			<BR><font color="navy"><B>Page <%=CurPage%> OF <%=TotalPages%> </B>
			<BR><B>Results :<%=rowcount%> Records.</B></font>
		</td>
	</tr>
<%end if%>
</table>
</div>
</form>
</body>
</HTML>