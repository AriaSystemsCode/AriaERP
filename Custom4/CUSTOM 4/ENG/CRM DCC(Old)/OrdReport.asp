<%@ Language=VBScript %>
<%Response.Buffer = true%>
<%
if Trim(Session("ID")) = "" AND Trim(Session("rep")) = "" then
	Response.redirect "default.asp"
end if
if Trim(Session("ID")) = "" then
	custID = Session("customerid")
else
	custID = Session("ID")
end if

%>
<%
set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
'RecordSets
set session("rsOrdersMatch") = server.CreateObject("ADODB.RecordSet")

 
BeginDate = (Trim(Request.Form("txtBeginDate")))
EndDate  = (Trim(Request.Form("txtEndDate")))

strSQL = "SELECT Ordline.Account,Ordline.Order,Ordline.Start, Ordline.Complete, Ordline.Style, Ordline.Qty1,  Ordline.Qty2,  Ordline.Qty3, Ordline.Qty4,  Ordline.Qty5, Ordline.Qty6, Ordline.Qty7, Ordline.Qty8,  Ordline.Totqty,  Ordline.Price,  Ordline.Piktkt, Ordhdr.status,Ordhdr.priority,Ordhdr.custpo,Ordhdr.entered,customer.stname,Ordhdr.book,Ordhdr.ship,Ordhdr.cancel,Ordhdr.open,OrdLine.Qty1 As Ship1,OrdLine.Qty2 As Ship2,OrdLine.Qty3 As Ship3,OrdLine.Qty4 As Ship4,OrdLine.Qty5 As Ship5,OrdLine.Qty6 As Ship6,OrdLine.Qty7 As Ship7,OrdLine.Qty8 As Ship8,Space(200) As Invoices  FROM Ordline,Ordhdr,Customer WHERE Ordline.Account = Ordhdr.Account AND Ordline.Order = Ordhdr.Order  AND Ordline.Account = Customer.Account AND customer.type = 'M' AND Ordline.Account='" & custID & "' AND Ordline.Order='" & Request("OrderNo") & "'"
session("rsOrdersMatch").Open strSQL, conn,2,4

set rsOrderInvoices = Server.CreateObject ("ADODB.RecordSet")
strSQL = " Select * From InvLine where Order = '"& Request("OrderNo") &"'"
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
	'Invoices=left(Invoices,len(Invoices)-1)
session("rsOrdersMatch").fields("Invoices") = Invoices
do while not session("rsOrdersMatch").EOF

	rsOrderInvoices.Filter = "style='" & Trim(session("rsOrdersMatch").fields("Style")) & "'"  
		qty1=0
		qty2=0
		qty3=0
		qty4=0
		qty5=0
		qty6=0
		qty7=0
		qty8=0	
		'RSInvLine.MoveFirst 
		Do while Not rsOrderInvoices.EOF
			qty1 = qty1 + cdbl(rsOrderInvoices("qty1"))
			qty2 = qty2 + cdbl(rsOrderInvoices("qty2"))
			qty3 = qty3 + cdbl(rsOrderInvoices("qty3"))
			qty4 = qty4 + cdbl(rsOrderInvoices("qty4"))
			qty5 = qty5 + cdbl(rsOrderInvoices("qty5"))
			qty6 = qty6 + cdbl(rsOrderInvoices("qty6"))
			qty7 = qty7 + cdbl(rsOrderInvoices("qty7"))
			qty8 = qty8 + cdbl(rsOrderInvoices("qty8"))
			rsOrderInvoices.MoveNext 
	
		Loop
		session("rsOrdersMatch").fields("Ship1") = qty1
		session("rsOrdersMatch").fields("Ship2") = qty2
		session("rsOrdersMatch").fields("Ship3") = qty3
		session("rsOrdersMatch").fields("Ship4") = qty4
		session("rsOrdersMatch").fields("Ship5") = qty5
		session("rsOrdersMatch").fields("Ship6") = qty6
		session("rsOrdersMatch").fields("Ship7") = qty7
		session("rsOrdersMatch").fields("Ship8") = qty8
	session("rsOrdersMatch").MoveNext()
loop		


%>

<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<Title>CRM - Check Order Status - Order # <%=request("orderNo")%></Title>

</HEAD>
<body bgcolor="#aecae6" topmargin="0" leftmargin="0">
<BR>
<Center>
<a href = "OrdStatusDetail.asp?OrderNo=<%=Request("OrderNo")%>"><Img src="Images\back.gif"></a>
</CENTER>
<br>
<%
reportname = "OrderReport.rpt"
If Not IsObject (session("oApp")) Then                              
	Set session("oApp") = Server.CreateObject("Crystal.CRPE.Application")
	
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

'set ddd = session("oRpt").opensubreport("InvoiceDetail")



'oApp2.OpenReport (dd , 

session("oRpt").DiscardSavedData
set Database = session("oRpt").Database

set Tables = Database.Tables

set Table1 = Tables.Item(1)

Table1.SetPrivateData 3, session("rsOrdersMatch")




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

viewer = Request.QueryString("viewer")
' by yas@
[start]
'viewer = Request.Form("Viewer")
'viewer = "ActiveX"
' [end]
'viewer = "Java using Browser JVM"
'This line collects the value passed for the viewer to be used, and stores
'it in the "viewer" variable.

If cstr(viewer) = "ActiveX" then
%>
<!-- #include file="SmartViewerActiveX.asp" -->
<%
ElseIf cstr(viewer) = "Netscape Plug-in" then
%>
<!-- #include file="ActiveXPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "JVM" then
%>
<!-- #include file="SmartViewerJava.asp" -->
<%
ElseIf cstr(viewer) = "Java-Plug-in" then
%>
<!-- #include file="JavaPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "HTML Frame" then
	Response.Redirect("htmstart.asp")
Else
	Response.Redirect("rptserver.asp")
End If
'The above If/Then/Else structure is designed to test the value of the "viewer" varaible
'and based on that value, send down the appropriate Crystal Smart Viewer.
%>
<br>
<BR>
<Center>
<a href = "OrdStatusDetail.asp?OrderNo=<%=Request("OrderNo")%>"><Img src="Images\back.gif"></a>
</CENTER>
<br>
</BODY>
</HTML>
<%
session("rsOrdersMatch").close
conn.Close 
%>