<%@ Language=VBScript %>
<%Response.Buffer=true
'Response.Expires=-1
if Trim(Session("rep")) = "" and trim(Session("ID"))= "" then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 

if trim (Session("rep")) = "" then
	CurCust = Session("ID")
else 
	CurCust = Session("CustomerID")
end if
'Response.Write "Customer Id"&CurCust
'Response.Write "Company Id"&Session("CompanyID")
'Response.end
%>

<%
set conn=server.CreateObject("ADODB.connection")
set Sysconn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
Sysconn.Open Application("SystemConnectionString")
'RecordSets
Set session("rsInvoiceMatch1") = server.CreateObject("ADODB.RecordSet")

strSQL = "SELECT	distinct Invhdr.Account,Invhdr.Shipamt, CodesShipvia.cDiscrep as ShipVia , Invhdr.Invoice, Invhdr.Store,Invhdr.Custpo,Invhdr.Rep1,"
strSQL = strSQL & " Invhdr.Rep2,Invhdr.Cartons,Invhdr.Weight,Invhdr.Dept,Customer.Cvatno AS CustVATNo,"
strSQL = strSQL & " CodesTerms.Cdiscrep AS TermsDisc,Invhdr.Order,Invhdr.Invdate,Invhdr.duedate,"
strSQL = strSQL & " Invhdr.Piktkt,Customer.Btname,Customer.Caddress1,Customer.Caddress3,Customer.Caddress4,"
strSQL = strSQL & " Customer.Caddress5,Customer.Caddress6,ShiptoCustomer.Stname AS ShiptoStname,"
strSQL = strSQL & " ShiptoCustomer.Caddress1 AS ShiptoAddress1,ShiptoCustomer.Caddress3 AS ShiptoAddress3,"
strSQL = strSQL & " ShiptoCustomer.Caddress4 AS ShiptoAddress4,ShiptoCustomer.Caddress5 AS ShiptoAddress5,"
strSQL = strSQL & " ShiptoCustomer.Caddress6 AS ShiptoAddress6 ,"
strSQL = strSQL & " Space(30) as Ccom_name ,Space(20) as Ccom_phon,"
strSQL = strSQL & " Space(30) as Comp_address1 ,Space(30) as Comp_address2 ,"
strSQL = strSQL & " Space(30) as Comp_address3 , Space(30) as Comp_address5,"
strSQL = strSQL & " Space(20) as Comp_address6 , Invhdr.Note1 , Invhdr.tax_amt ,"
strSQL = strSQL & " Invhdr.tax_rate ,Invhdr.Totalchg , Space(30) as Ccurrdesc ,"
strSQL = strSQL & " Invhdr.Freight , InvHdr.Insur ,InvHdr.COD_amt ,"
strSQL = strSQL & " Space(30) as VAT , Invhdr.Discount "
strSQL = strSQL & " FROM	Invhdr,Invline,Customer, Customer AS ShiptoCustomer ,"'Objects ,"
strSQL = strSQL & " Codes AS CodesShipvia,Codes AS CodesTerms where"


strSQL = strSQL & " Invhdr.Account+Invhdr.Invoice='"&CurCust & Request("InvNo") & "'"

strSQL = strSQL & " AND CodesShipvia.Cdefcode+CodesShipvia.Crltfield+CodesShipvia.Cfld_name='NNSHIPVIA'"
strSQL = strSQL & " AND CodesShipvia.Ccode_no = Invhdr.Shipvia"

strSQL = strSQL & " AND CodesTerms.Cdefcode+CodesTerms.Crltfield+CodesTerms.Cfld_name='NNCTERMCODE'"
strSQL = strSQL & " AND CodesTerms.Ccode_no=Invhdr.Ctermcode "
strSQL = strSQL & " AND Invline.invoice+STR(Invline.lineno,6) like '"&Request("InvNo")&"%'"
strSQL = strSQL & " AND Customer.Type+Customer.Account+Customer.Store like 'M" & CurCust& "%'"
strSQL = strSQL & " AND  ShiptoCustomer.Account=Invhdr.Account"
strSQL = strSQL & " AND  (ShiptoCustomer.Type+ShiptoCustomer.Account+ShiptoCustomer.Store like 'M" & CurCust& "%' or ShiptoCustomer.Type+ShiptoCustomer.Account+ShiptoCustomer.Store like 'S" & CurCust& "%')"
strSQL = strSQL & " AND ShiptoCustomer.Store=Invhdr.Store"

'Response.Write(strsql)

session("rsInvoiceMatch1").Open strSQL, conn,2,4
if session("rsInvoiceMatch1").EOF then
	Response.Write "No Data!"
	Response.End 
end if

'''''''''''''''*********************NEK Getting Company Data

sqlgetCompanyData = "Select * from Syccomp where ccomp_id='"&Session("CompanyID")&"'"
set rsgetCompanyData = server.CreateObject ("adodb.recordset")
rsgetCompanyData.Open sqlgetCompanyData , Sysconn

if not(rsgetCompanyData.EOF and rsgetCompanyData.bof) then
'
	sqlgetCurrency = "select ccurrdesc from syccurr where ccurrcode = '"&rsgetCompanyData("ccurrcode")&"'"
	set rsgetCurrency = server.CreateObject("adodb.recordset")
	rsgetCurrency.Open sqlgetCurrency , Sysconn

	set Session("rsgetLOGO") = server.CreateObject("adodb.recordset")
	sqlgetLOGO = "select GObject from Objects where Cobject_id in(select Cobject_Id from Objlink where CObjLink='LOGO' and CObjlnktyp='*')"
	Session("rsgetLOGO").Open sqlgetLOGO , conn
	session("rsInvoiceMatch1").fields("Ccom_name")= rsgetCompanyData("Ccom_name")
	session("rsInvoiceMatch1").fields("Ccom_Phon")= rsgetCompanyData("Ccom_phon")
	session("rsInvoiceMatch1").fields("Comp_address1")= rsgetCompanyData("Caddress1")
	session("rsInvoiceMatch1").fields("Comp_address2")= rsgetCompanyData("Caddress2")
	session("rsInvoiceMatch1").fields("Comp_address3")= trim(rsgetCompanyData("Caddress3"))
	session("rsInvoiceMatch1").fields("Comp_address5")= rsgetCompanyData("Caddress5")
	session("rsInvoiceMatch1").fields("Comp_address6")= rsgetCompanyData("Caddress6")
	session("rsInvoiceMatch1").fields("Ccurrdesc")= rsgetCurrency("Ccurrdesc")

end if

set rsgetVAT = server.CreateObject("adodb.recordset")
sqlgetVAT = "select mdata_def from setups where cfld_name='M_TAX_REFE'"
rsgetVAT.Open sqlgetVAT , conn

if not (rsgetVAT.EOF and rsgetVAT.BOF ) then
'	session("rsInvoiceMatch1").fields("VAT")=rsgetVAT("mdata_def")
else
'	session("rsInvoiceMatch1").fields("VAT")=""
end if


'''''''''''''''*********************NEK Getting Company Data

set session("rsInvoiceDetail1") = Server.CreateObject ("ADODB.RecordSet")
strSQL = "SELECT Invline.Style, Invline.Desc1, Invline.Price, Invline.Qty1, Invline.Qty2, Invline.Qty3,"
strSQL = strSQL & " Invline.Qty4, Invline.Qty5, Invline.Qty6, Invline.Qty7, Invline.Qty8, Invline.Totqty,"
strSQL = strSQL & " (Invline.Totqty * Invline.Price) AS Amount, Scale.Sz1, Scale.Sz2,  Scale.Sz3,"
strSQL = strSQL & " Scale.Sz4, Scale.Sz5, Scale.Sz6, Scale.Sz7, Scale.Sz8 ,Invline.invoice ,Invline.Flag , Style.CcomCode FROM Invline,Style,Scale WHERE"
strSQL = strSQL & " InvLine.invoice+STR(InvLine.lineno,6) like '"&request("InvNo")&"%'"
strSQL = strSQL & " AND Style.Style=Invline.Style"
strSQL = strSQL & " AND Scale.Type+Scale.Scale+Scale.prepak = 'S'+Style.Scale"
'strSQL = strSQL & " AND Invline.Order='" & session("rsInvoiceMatch1")("Order") & "'"
strSQL = strSQL & " Order By Invline.Lineno"

session("rsInvoiceDetail1").open strsql,conn
'Response.Redirect "data.asp"
'''''''''''''''''''''''''''''''''''''''''''''''
%>
<HTML>
<head>
<title>CRM - Invoice Status - Report</title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</head>
<body bgcolor="#aecae6" topmargin="0" leftmargin="0"  background="Tile1.gif">
<br>

<br>
<%

reportname = "Invoice2.rpt"
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



'Response.Write(path & reportname)
Set session("oRpt") = session("oApp").OpenReport(path & reportname, 1)

session("oRpt").DiscardSavedData
'WAL_E302083,1 set field values in the report [Start]
Set fs = Session("oRpt").FormulaFields
Set f1 = fs.Item(2)
Set f2 = fs.Item(3)
Set f3 = fs.Item(4)
Set f4 = fs.Item(5)

f1.Text = "'"& trim(session("CustField")) &"'"
f2.Text = "'"& trim(session("StoreField")) &"'"
f3.Text = "'"& trim(session("Currency")) &"'"
f4.Text = "'"& trim(session("CurrencyAlign")) &"'"

'WAL_E302083,1 set field values in the report [End]
set Database = session("oRpt").Database
set Tables = Database.Tables
set Table1 = Tables.Item(1)
Table1.SetPrivateData 3,session("rsInvoiceMatch1")

set Table2 = Tables.Item(2)
Table2.SetPrivateData 3,session("rsgetLOGO")

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
set session("subRep") = session("oRpt").OpenSubReport("InvDetail.rpt")
Set fs_sub = Session("subRep").FormulaFields
'NEK [Start]1/9/2003
Set f1_sub = fs_sub.Item(1)
Set f2_sub = fs_sub.Item(2)
f1_sub.Text = "'"& trim(session("Currency")) &"'"
f2_sub.Text = "'"& trim(session("CurrencyAlign")) &"'"
'NEK [End]1/9/2003
set subDatabase = session("subRep").Database
set subTables = subDatabase.Tables
set subTable1 = subTables.Item(1)
subTable1.SetPrivateData 3, session("rsInvoiceDetail1")


On Error Resume Next                                                  
session("oRpt").ReadRecords                                           
If Err.Number <> 0 Then                                               
  Response.Write Err.Description
  Response.End 
Else

  If IsObject(session("oPageEngine")) Then                              
  	set session("oPageEngine") = nothing
  End If
set session("oPageEngine") = session("oRpt").PageEngine
End If                                                                

'viewer = Request.Form("Viewer")
'HDM[Start] Make it java preview

'viewer = "ActiveX"
'viewer = "HTML"
viewer = Request.QueryString ("Viewer")
'HDM[End]

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
%>
<br>
<br>
<CENTER>
<a href = "invoicedetail.asp?InvNo=<%=Request("InvNo")%>"><Img border=0 src="../Images/<%=session("Theme")%>/back.gif"></a>
</CENTER>
<br>
</BODY>
</HTML>
<%
''''''''''''''''''''''''''''''''''''''''''

'session("rsInvoiceMatch1").close
'session("rsInvoiceDetail1").close
'conn.Close 
%>
