<%@ LANGUAGE="VBSCRIPT" %>
<html>

<head><LINK REL=stylesheet HREF="../images/<%=Session("THEME")%>/invoice.css" TYPE="text/css">
<meta http-equiv="Content-Type" content="text/html; charset=windows-1256">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
<title>CRM - Credit Card Collection</title>
</head>
<%
dim strSQL
if Request("lstType")="Deposit" then 'Deposit Payment Report
	strSQL = "select " 
	strSQL = strSQL + "Pnptrans.Order, Pnptrans.Account, (Pnptrans.Amount)*-1 as Amount , "
	'strSQL = strSQL + "Pnptrans.Trandate, Pnptrans.trantype, Pnptrans.ctransid, "
	strSQL = strSQL + "Pnptrans.Trandate, ctod(SUBSTR(ctransid,5,2)+'/'+SUBSTR(ctransid,7,2)+'/'+SUBSTR(ctransid,1,4)) as PNPdate , Pnptrans.ctrn_type, Pnptrans.ctransid, "
	strSQL = strSQL + "Customer.btname, "
	strSQL = strSQL + "salesrep.Repcode as Rep1,salesrep.Name, "
	strSQL = strSQL + "Ordhdr.Entered as OrderDate, "
	strSQL = strSQL + "Ordcharg.ntotchg as totOrder, "
	strSQL = strSQL + "'"& Request("txtBeginDate") &"' as BeginDate,'"& Request("txtEndDate") &"' as EndDate "
	strSQL = strSQL + "From pnptrans, Customer,Ordhdr,salesrep, Ordcharg "
	strSQL = strSQL + "Where "
	strSQL = strSQL + "pnptrans.Account = Customer.Account "
	strSQL = strSQL + "and Customer.Type = 'M' "
	strSQL = strSQL + "and Ordhdr.Rep1 = salesrep.Repcode "
	strSQL = strSQL + "and pnptrans.Order = Ordhdr.Order "
	strSQL = strSQL + "and pnptrans.Order = Ordcharg.Corder "
	strSQL = strSQL + "and pnptrans.ctrn_type <> 'Invoice' "
else 'Invoice Payment Report
	strSQL = "select " 
	strSQL = strSQL + "Pnptrans.Invoice, Pnptrans.Order, Pnptrans.Account, (Pnptrans.Amount)*-1 as Amount, "
	'strSQL = strSQL + "Pnptrans.Trandate, Pnptrans.trantype, Pnptrans.ctransid, "
	strSQL = strSQL + "Pnptrans.Trandate, ctod(SUBSTR(ctransid,5,2)+'/'+SUBSTR(ctransid,7,2)+'/'+SUBSTR(ctransid,1,4)) as PNPdate , Pnptrans.ctrn_type, Pnptrans.ctransid, "
	strSQL = strSQL + "Customer.btname, "
	strSQL = strSQL + "salesrep.Repcode as Rep1,salesrep.Name, "
	strSQL = strSQL + "Invhdr.InvDate, Invhdr.Totalchg as totInvoice, "
	strSQL = strSQL + "round(((Invhdr.Totalchg / Ordcharg.Ntotchg) * Ordcharg.ndeposit),2) as InvDeposit, "
	strSQL = strSQL + "'"& Request("txtBeginDate") &"' as BeginDate,'"& Request("txtEndDate") &"' as EndDate "
	strSQL = strSQL + "From pnptrans, Customer,Invhdr,salesrep, Ordcharg "
	strSQL = strSQL + "Where pnptrans.Account = Customer.Account "
	strSQL = strSQL + "and Customer.Type = 'M' "
	strSQL = strSQL + "and Invhdr.Rep1 = salesrep.Repcode "
	strSQL = strSQL + "and pnptrans.Invoice = Invhdr.Invoice "
	strSQL = strSQL + "and pnptrans.Order = Ordcharg.Corder "
	strSQL = strSQL + "and pnptrans.ctrn_type = 'Invoice' "
end if

'Search Criteria 
'Customer Name
if Request("txtCustomerName")<>"" then	strSQL = strSQL + "and Upper(Customer.btname) like '"& Ucase(Request("txtCustomerName"))& "%' "
'Rep. Name
if Request("txtRepName")<>"" then	strSQL = strSQL + "and Upper(salesrep.Name) like '"& Ucase(Request("txtRepName")) & "%' "
'Order No
if Request("txtOrderNoStart")<>"" then	strSQL = strSQL + "and Pnptrans.Order >= '"& Request("txtOrderNoStart") &"' "
if Request("txtOrderNoEnd")<>"" then strSQL = strSQL + "and pnptrans.Order <= '"& Request("txtOrderNoEnd") &"' "
'Account
if Request("txtAccountStart")<>"" then	strSQL = strSQL + "and Pnptrans.Account >= '"& Request("txtAccountStart") &"' "
if Request("txtAccountEnd")<>"" then strSQL = strSQL + "and pnptrans.Account <= '"& Request("txtAccountEnd") &"' "
'Rep
if Session("Authority")="Full" then
	if Request("txtRepStart")<>"" then	strSQL = strSQL + "and salesrep.Repcode >= '"& Request("txtRepStart") &"' "
	if Request("txtRepEnd")<>"" then strSQL = strSQL + "and salesrep.Repcode <= '"& Request("txtRepEnd") &"' "
else
	strSQL = strSQL + "and salesrep.Repcode = '"& Session("rep") &"' "
end if	
'PNP Tranasction
if Request("txtPNPTrans")<>"" then strSQL = strSQL + "and pnptrans.ctransid = '"& Request("txtPNPTrans") &"' "
'Transaction Date Constrain
'if Request("txtBeginDate")<>"" then strSQL = strSQL + "and pnptrans.Trandate >= {"& Request("txtBeginDate") &"} "
'if Request("txtBeginDate")<>"" then strSQL = strSQL + "and pnptrans.Trandate >= {"& Request("txtBeginDate") &"} "
'if Request("txtEndDate")<>"" then strSQL = strSQL + "and pnptrans.Trandate <= {"& Request("txtEndDate") &"} "
if Request("txtBeginDate")<>"" then strSQL = strSQL + "and ctod(SUBSTR(ctransid,5,2)+'/'+SUBSTR(ctransid,7,2)+'/'+SUBSTR(ctransid,1,4)) >= {"& Request("txtBeginDate") &"} "
if Request("txtEndDate")<>"" then strSQL = strSQL + "and ctod(SUBSTR(ctransid,5,2)+'/'+SUBSTR(ctransid,7,2)+'/'+SUBSTR(ctransid,1,4)) <= {"& Request("txtEndDate") &"} "

'Order by based on report type
if Request("lstType")="Deposit" then
	strSQL = strSQL + "Order by Pnptrans.Order,Pnptrans.Trandate,Pnptrans.cTranTime "
else
	strSQL = strSQL + "Order by Pnptrans.Invoice,Pnptrans.Trandate,Pnptrans.cTranTime "
end if	
'======================================================================================
'======================================================================================
'Create The ADO Connection and Recordset
set oConn=server.CreateObject("ADODB.connection")
oConn.Open Application("DataConnectionString")

'Now we must create a Record Set object:
set session("oRs") = Server.CreateObject("ADODB.Recordset")
session("oRs").ActiveConnection = oConn
'Response.Write "<font size=5>" & strSQL
session("oRs").Open strSQL
'Response.Write  session("oRs").recordcount & "<hr>"

if session("oRs").EOF AND session("oRs").BOF then
%>
	<Table width=95% height=50 align=center border=1>
	<TR>
	<TD class=title>Credit Card Collection</TD>
	</TR>
	</Table>
	<br><br><br>
	<table border=0 >
	<tr><td>
			<b>No records match your criteria. </b>
	</td></tr>
	<tr><td><A HREF="CreditCollection.asp">back</A>
	</td></tr>
	</table>
<%
Response.End 
end if

'reportname that we will use to pass
if Request("lstType")="Deposit" then 'Deposit Payment Report
	reportname = "CreditCollection.rpt"
else
	reportname = "CreditCollectionInv.rpt"
end if

' CREATE THE APPLICATION OBJECT one time                                                                     
If Not IsObject (session("oApp")) Then                              
	Set session("oApp") = Server.CreateObject("CrystalRuntime.Application")
End If                                                                
Path = Request.ServerVariables("PATH_TRANSLATED")                     
While (Right(Path, 1) <> "\" And Len(Path) <> 0)                      
	iLen = Len(Path) - 1                                                  
	Path = Left(Path, iLen)                                               
Wend                                                                  
                                                                      
'OPEN THE REPORT (but destroy any previous one first)                                                     
If IsObject(session("oRpt")) then
	Set session("oRpt") = nothing
End if
Set session("oRpt") = session("oApp").OpenReport(path & reportname, 1)

'These lines disable the Error reporting mechanism
'session("oRpt").MorePrintEngineErrorMessages = False
'session("oRpt").EnableParameterPrompting = False
'======================================================================================
'======================================================================================
 
'dynamically generated ADO recordset and ttx fileb:
session("oRpt").DiscardSavedData
set Database = session("oRpt").Database
set Tables = Database.Tables
set Table1 = Tables.Item(1)
Table1.SetPrivateData 3, session("oRs") 

On Error Resume Next                                                  
session("oRpt").ReadRecords                                           
If Err.Number <> 0 Then                                               
' Response.Write "An Error has occured on the server in attempting to access the data source" & "<HR>"
' Response.Write  Err.Number & "<HR>"
' Response.Write Err.Description  & "<HR>"
Else

  If IsObject(session("oPageEngine")) Then                              
  	set session("oPageEngine") = nothing
  End If
set session("oPageEngine") = session("oRpt").PageEngine
End If                                                                


' INSTANTIATE THE CRYSTAL REPORTS SMART VIEWER
viewer = Request.Form("Viewer")
viewer= "ActiveX"

If cstr(viewer) = "ActiveX" then
%>
<!-- #include file="../crystal/SmartViewerActiveX.asp" -->
<%
ElseIf cstr(viewer) = "Netscape Plug-in" then
%>
<!-- #include file="../crystal/ActiveXPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "Java using Browser JVM" then
%>
<!-- #include file="../crystal/SmartViewerJava.asp" -->
<%
ElseIf cstr(viewer) = "Java using Java Plug-in" then
%>
<!-- #include file="../crystal/JavaPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "HTML Frame" then
	Response.Redirect("htmstart.asp")
Else
	Response.Redirect("rptserver.asp")
End If

%>



<body>
<br>
<br>
<CENTER>
<a href = "CreditCollection.asp"><Img border=0 src="../Images/<%=session("Theme")%>/back.gif"></a>
</CENTER>
<br>
</BODY>
</HTML>