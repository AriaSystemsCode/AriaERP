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

%>
<%
set conn=server.CreateObject("ADODB.connection")
set Sysconn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
Sysconn.Open Application("SystemConnectionString")

'RecordSets
Set session("rsInvoiceMatch1") = server.CreateObject("ADODB.RecordSet")

strSQL = "SELECT	distinct Invhdr.Account,Invhdr.Shipamt,Invhdr.Invoice, Invhdr.Store,Invhdr.Custpo,Invhdr.Rep1,"
strSQL = strSQL & " Invhdr.Rep2,Invhdr.Cartons,Invhdr.Weight,Invhdr.Dept,CodesShipvia.Cdiscrep AS ShipviaDisc,"
strSQL = strSQL & " CodesTerms.Cdiscrep AS TermsDisc,Invhdr.Order,Invhdr.Invdate,Invhdr.Shipdate,"
strSQL = strSQL & " Invline.Pack_id,Customer.Btname,Customer.Caddress1,Customer.Caddress3,Customer.Caddress4,"
strSQL = strSQL & " Customer.Caddress5,Customer.Caddress6,ShiptoCustomer.Stname AS ShiptoStname,"
strSQL = strSQL & " ShiptoCustomer.Caddress1 AS ShiptoAddress1,ShiptoCustomer.Caddress3 AS ShiptoAddress3,"
strSQL = strSQL & " ShiptoCustomer.Caddress4 AS ShiptoAddress4,ShiptoCustomer.Caddress5 AS ShiptoAddress5,"
strSQL = strSQL & " ShiptoCustomer.Caddress6 AS ShiptoAddress6 , Space(20) as Ccom_phone"
strSQL = strSQL & " FROM	Invhdr,Invline,Customer, Customer AS ShiptoCustomer,"
strSQL = strSQL & " Codes AS CodesShipvia,Codes AS CodesTerms where"

strSQL = strSQL & " Invhdr.Account+Invhdr.Invoice='"&CurCust & Request("InvNo") & "'"

strSQL = strSQL & " AND CodesShipvia.Cdefcode+CodesShipvia.Crltfield+CodesShipvia.Cfld_name='NNSHIPVIA'"
strSQL = strSQL & " AND CodesShipvia.Ccode_no = Invhdr.Shipvia"

strSQL = strSQL & " AND CodesTerms.Cdefcode+CodesTerms.Crltfield+CodesTerms.Cfld_name='NNCTERMCODE'"
strSQL = strSQL & " AND CodesTerms.Ccode_no=Invhdr.Ctermcode "
strSQL = strSQL & " AND Invline.invoice+STR(Invline.lineno,6) like '"&Request("InvNo")&"%'"
strSQL = strSQL & " AND Customer.Type+Customer.Account+Customer.Store like 'M" & CurCust& "%'"
'strSQL = strSQL & " AND  ShiptoCustomer.Account=Invhdr.Account"
strSQL = strSQL & " AND  (ShiptoCustomer.Type+ShiptoCustomer.Account+ShiptoCustomer.Store like 'M" & CurCust& "%' or ShiptoCustomer.Type+ShiptoCustomer.Account+ShiptoCustomer.Store like 'S" & CurCust& "%')"
strSQL = strSQL & " AND ShiptoCustomer.Store=Invhdr.Store"


session("rsInvoiceMatch1").open strSQL ,conn ,1,3
'**********************NEK =Getting COmpany Data'
sqlGetCompanyDetails = "Select * from Syccomp where ccomp_id="&Session("CompanyID")
set rsGetCompanyDetails = server.CreateObject("adodb.recordset")
rsGetCompanyDetails.Open sqlGetCompanyDetails, Sysconn
Response.Write "Comapny Phone : "&rsGetCompanyDetails("Ccom_phone")
Response.End

session("rsInvoiceMatch1").fields("Ccom_phone")=rsGetCompanyDetails("Ccom_phone")
'**********************NEK = Cmpany Data'
'Response.Redirect "data.asp"
'Response.Write "Record Count " &session("rsInvoiceMatch1").recordcount & "<br>"

reportname = "InvoiceReport.rpt"
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

session("oRpt").DiscardSavedData
set Database = session("oRpt").Database
set Tables = Database.Tables
set Table1 = Tables.Item(1)
Table1.SetPrivateData 3, session("rsInvoiceMatch1")
	
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

		'viewer = Request.QueryString ("viewer")
		viewer="ActiveX"
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
%>

