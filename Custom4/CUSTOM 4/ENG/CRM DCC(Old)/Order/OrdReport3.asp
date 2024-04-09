<%@ Language=VBScript %>
<%Response.Buffer=true
if Trim(Session("ID")) = "" AND Trim(Session("rep")) = "" then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
	</script>	
<%END IF 

if Trim(Session("ID")) = "" then
	custID = Session("customerid")
else
	custID = Session("ID")
end if

set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
'RecordSets
set session("rsOrdersMatch") = server.CreateObject("ADODB.RecordSet")'orders info
set session("rsInvoiceMatch") = server.CreateObject("ADODB.RecordSet")'invoice info

Dim RSScale
Set RSScale = server.CreateObject("ADODB.Recordset")

BeginDate = (Trim(Request.Form("txtBeginDate")))
EndDate  = (Trim(Request.Form("txtEndDate")))
'WAL_Build orders select[start]
strSQL = "SELECT Ordline.Account,Ordline.Order,Ordline.Start, Ordline.Complete, Ordline.Style, Ordline.Group, Ordline.Qty1, "
strSQL = strSQL & " Ordline.Qty2,  Ordline.Qty3, Ordline.Qty4,  Ordline.Qty5, Ordline.Qty6, Ordline.Qty7, Ordline.Qty8, "
strSQL = strSQL & " Scale.sz1,  Scale.sz2, Scale.sz3,  Scale.sz4, Scale.sz5, Scale.sz6, Scale.sz7, Scale.sz8, "
strSQL = strSQL & " Ordline.Totqty, Ordline.Gros_Price, Ordline.Disc_pcnt, Ordline.Comm1, Ordline.Price,Ordline.Piktkt, Ordhdr.status, "
strSQL = strSQL & " Ordhdr.priority,Ordhdr.custpo,Ordhdr.entered,customer.stname,Ordhdr.book,Ordhdr.ship, "
strSQL = strSQL & " Ordhdr.cancel,Ordhdr.open, Space(200) as Invoices"
strSQL = strSQL & " FROM Ordline, Ordhdr, Customer, Scale WHERE "
strSQL = strSQL & " Ordline.cordtype+Ordline.Order+STR(ordline.lineno,6)like '"&request("Type")&Request("OrderNo")&"%' "
strSQL = strSQL & " AND ordhdr.account+ordhdr.cordtype+ordhdr.order like '" & CustID & request("Type")&Request("OrderNo")&"'"
strSQL = strSQL & " AND scale.type='S' and scale.scale = ordLine.scale AND customer.type+customer.account+customer.store like 'M"&CustID&"%' order by lineno"
 
session("rsOrdersMatch").Open strSQL, conn,2,4
'format a string with all invoices
set rsOrderInvoices = Server.CreateObject ("ADODB.RecordSet")
strSQL = " Select DISTINCT * From InvLine where Order+Str(lineno,6)+invoice like '"& Request("OrderNo") &"%'"
rsOrderInvoices.Open strSQL , conn
IF rsOrderInvoices.Eof And rsOrderInvoices.BOF Then
	'Response.Write("No Invoices found for this order.")
Else
	Invoices =""
	Dim strTemp ' as string
	Do While Not rsOrderInvoices.Eof
		If strTemp = rsOrderInvoices("invoice") Then
		Else
			Invoices = Invoices & rsOrderInvoices("invoice")& ", "
		End IF
		strTemp = rsOrderInvoices("invoice")
		rsOrderInvoices.MoveNext 
	Loop
End IF
session("rsOrdersMatch").fields("Invoices") = Invoices
'WAL_Build orders select statment[end]

'WAL_Build Invoice select statment[start]
strInvSQL = "SELECT  invhdr.order, invhdr.invoice,InvHDR.invdate ,invhdr.piktkt as InvTKT, invline.style, invline.price,"
strInvSQL = strInvSQL & " invline.Qty1 As Ship1, invline.Qty2 As Ship2, invline.Qty3 As Ship3, "
strInvSQL = strInvSQL & " invline.Qty4 As Ship4, invline.Qty5 As Ship5, invline.Qty6 As Ship6, invline.Qty7 As Ship7, "
strInvSQL = strInvSQL & " invline.Qty8 As Ship8, invLine.totqty as invtotqty, invline.lineno as invline FROM invLine, InvHDR where"
strInvSQL = strInvSQL & " invline.order+STR(invline.lineno,6)+invline.invoice like '" & Request("OrderNo") & "%' AND "
strInvSQL = strInvSQL & " invline.invoice=invhdr.invoice"

session("rsInvoiceMatch").Open strInvSQL, conn,2,4
'WAL_Build Invoice select statment[end]

%>

<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<Title>CRM - Check Order Status - Order # <%=request("orderNo")%></Title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</HEAD>
<body bgcolor="#aecae6" topmargin="0" leftmargin="0">

<BR>
<br>
<%
if session("rsOrdersMatch").eof and session("rsOrdersMatch").bof then
else
	session("rsOrdersMatch").MoveFirst 
	reportname = "OrderReport.rpt"
	If Not IsObject (session("oApp")) Then                              
		Set session("oApp") = Server.CreateObject("Crystal.CRPE.Application")
		'NOTE:Prev. line For old Version
		'Set session("oApp") = Server.CreateObject("CrystalRuntime.Application")
	End If                                                                
	Path = Request.ServerVariables("PATH_TRANSLATED")                     
	While (Right(Path, 1) <> "\" And Len(Path) <> 0)                      
	iLen = Len(Path) - 1                                                  
	Path = Left(Path, iLen)                                               
	Wend                                                                  
	If IsObject(session("oRpt")) then
		Set session("oRpt") = nothing
	End if
	Set session("oRpt") = session("oApp").OpenReport(path & reportname, 1)
	'session("oRpt").MorePrintEngineErrorMessages = False
	'session("oRpt").EnableParameterPrompting = False

	session("oRpt").DiscardSavedData
	'WAL_E302083,1 set field values in the report [Start]
	Set fs = Session("oRpt").FormulaFields
	Set f1 = fs.Item(5)
	'Set f2 = fs.Item(6)
	'Set f3 = fs.Item(7)
	f1.Text = "'"& session("CustField") &"'"
	'NEK[Start]1/9/2003
	'f2.Text = "'"& session("Currency") &"'"
	'f3.Text = "'"& session("CurrencyAlign") &"'"
	'NEK[End]1/9/2003
	'WAL_E302083,1 set field values in the report [End]
	set Database = session("oRpt").Database
	set Tables = Database.Tables
	set Table1 = Tables.Item(1)
	Table1.SetPrivateData 3,session("rsOrdersMatch")

	' Sub Reoprt  ''''''''''''''''''''''''''''''''''''
	set session("subRep") = session("oRpt").OpenSubReport("InvReport.rpt")'
	'Set fs_sub = Session("subRep").FormulaFields
	'NEK [Start]1/9/2001 Set The Currenc Alignment 
	'	Set f1 = fs_sub.Item(2) ' Currency
	'	f1.Text = "'"& session("Currency") &"'"
	'	Set f2 = fs_sub.Item(3) ' Currency Align
	'	f2.Text = "'"& session("CurrencyAlign") &"'"
	set subDatabase = session("subRep").Database
	set subTables = subDatabase.Tables
	set subTable1 = subTables.Item(1)
	subTable1.SetPrivateData 3, session("rsInvoiceMatch")
	'End  Sub Reoprt  ''''''''''''''''''''''''''''''''''''

	On Error Resume Next                                                  
	session("oRpt").ReadRecords                                           
	If Err.Number <> 0 Then                                               
	  Response.Write "An Error has occured on the server in attempting to access the data source"
	Else

	  If IsObject(session("oPageEngine")) Then                              
	  	set session("oPageEngine") = nothing
	  End If
	set session("oPageEngine") = session("oRpt").PageEngine
	End If                                                                

	viewer = Request.Form("Viewer")
	viewer = "ActiveX"
	
	'viewer = Request.querystring("Viewer")
	
	'viewer = "Java using Java Plug-in"
	'This line collects the value passed for the viewer to be used, and stores
	'it in the "viewer" variable.

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
	'The above If/Then/Else structure is designed to test the value of the "viewer" varaible
	'and based on that value, send down the appropriate Crystal Smart Viewer.

	'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
end if

%>
<br>
<BR>
<Center>
<a href = "OrdStatusDetail.asp?OrderNo=<%=Request("OrderNo")%>&Type=<%=Request("Type")%>"><Img border=0 src="../Images/<%=session("Theme")%>/back.gif"></a>
</CENTER>
<br>
</BODY>
</HTML>
