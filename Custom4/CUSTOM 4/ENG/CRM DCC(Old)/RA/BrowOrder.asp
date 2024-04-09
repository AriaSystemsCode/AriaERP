<%@ Language=VBScript %>
<%
Response.Buffer=true
Response.Expires=-1
%>

<html>
<head>
<link REL="stylesheet" HREF="../images/<%=session("Theme")%>/Ra.css" TYPE="text/css">
<title>CRM - Browse Orders</title>
<SCRIPT LANGUAGE=javascript>
<!--
function setOrder(OrdNo)
{
	//alert(OrdNo)
	//alert(window.opener.form1.txtOrder.value)
	var strRetVal;
	strRetVal = ''+OrdNo;
	window.opener.form1.txtOrder.value = strRetVal;
	//alert(window.opener.form1.txtOrder.value)
	window.close();
}
//-->
</SCRIPT>


</Head>
<body>
<form name=Form1 method=post>
<Table width=95% border=1 height=50 align=center>
<TR>
<TD class=title>Select Order</TD>
</TR>
</Table>
<%
Dim cnFoxConnection
Set cnFoxConnection = Server.CreateObject("ADODB.Connection")
cnFoxConnection.ConnectionString = Trim(Application("DataConnectionString"))
cnFoxConnection.Open
'Adjust paging 20 per page
NumPerPage = 20
Dim CurPage
If Request.QueryString("CurPage") = "" then
	CurPage = 1 'We're on the first page
Else
	CurPage = Request.QueryString("CurPage")
End If

Dim rsOrder
Set rsOrder = Server.CreateObject("ADODB.RecordSet")
rsOrder.CacheSize = NumPerPage
Dim strOrderSQL
Dim strCustID
strCustID = Trim(Request.QueryString("CustID"))
strCustID = strCustID & Space(5-len(strCustID))
strInv = Trim(Request.QueryString("Inv"))
if strInv = "" then
	strOrderSQL = "Select Order,Complete from ORDHDR where account+cordtype+order Like '"& strCustID &"O"& Trim(Request.QueryString("Order"))&"%' "
else
	strOrderSQL = "Select ORDHDR.Order,ORDHDR.Complete from ORDHDR, InvHdr where ORDHDR.account+ORDHDR.cordtype+ORDHDR.order Like '"& strCustID &"O"& Trim(Request.QueryString("Order"))&"%' and InvHdr.Invoice like '"&Trim(strInv)&"%' and InvHdr.Order = ORDHDR.Order "
end if
'Response.Write "<font size=3>"&strOrderSQL&"</font>"
'Response.End 
rsOrder.Open strOrderSQL, cnFoxConnection, 3, 1
rowcount=rsOrder.recordcount
If rsOrder.EOF And rsOrder.BOF Then
	Response.Write("<p Align='Center'><Font Size=3>No Orders Found</font></p>")
	Response.Write("<SCRIPT LANGUAGE=javascript>")
	Response.Write("window.opener.form1.txtOrder.value=''")
	Response.Write("</SCRIPT>")
	Response.End
End If

%>

<DIV Align='Center'>
<Table width='100%'>
<tr><td class="dark_cell" colspan='2'></td></tr>
<tr>
	<td class="dark_cell">Order</td>
	<td class="dark_cell">Comp.Date</td>
</tr>
<%
rsOrder.PageSize = NumPerPage
Dim TotalPages
TotalPages = rsOrder.PageCount
if cint(CurPage) > cint(TotalPages) then CurPage=TotalPages
rsOrder.AbsolutePage = CurPage
Do While Not rsOrder.EOF and count < rsOrder.PageSize%>
<tr>
	<td class="dark_cell"><A href="JAVASCRIPT:setOrder('<%=rsOrder("Order")%>')"><%=rsOrder("Order")%></a></td>
	<td class="light_cell"><%=rsOrder("Complete")%></td>
</tr>
<%
Count = Count + 1
rsOrder.MoveNext%>
<%Loop%>
<%if rsOrder.RecordCount > 20 then%>
	<tr align=center>
		<td colspan=2 valign="top"  align="center">
		<%if CurPage > 1 then %>  <!--We are not at the beginning, show the prev button -->
				<A href="javascript:document.Form1.action ='BrowOrder.asp?CustID=<%=Trim(Request.QueryString("CustID"))%>&Order=<%=Trim(Request.QueryString("Order"))%>&curpage=<%=curpage - 1%>';document.Form1.submit();"><strong>Prev </strong></font></a><%Response.Write("  | ")%>
		<%End If
  		     if CInt(CurPage) <> CInt(TotalPages) then  'We are not at the end, show a next button
			%>	
			<A href="javascript:document.Form1.action ='BrowOrder.asp?CustID=<%=Trim(Request.QueryString("CustID"))%>&Order=<%=Trim(Request.QueryString("Order"))%>&curpage=<%=curpage + 1%>';document.Form1.submit();"><strong>Next </strong></font></a>
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