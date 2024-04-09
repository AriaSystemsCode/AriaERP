<%@ Language=VBScript %>
<%
Response.CacheControl  = "no-cache"
Response.AddHeader  "Pragma", "no-cache"
Response.Expires = -1
Response.Buffer = true
%>
<HTML>
<head>
<HEAD><LINK REL=stylesheet HREF="../images/<%=Session("Theme")%>/invoice.css" TYPE="text/css">
<meta http-equiv="Content-Type" content="text/html; chars%et=windows-1256">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<Title>CRM - Payments</Title></head><body>
<%
'***************************************************************************************
'first time only we read the pramters
if Request.Form("txtAction")="" then 
	Session("PNPCase")= Request.QueryString("PNPCase")
	Session("PNPPayMethod")= Request.Form("paymethod") 'credit/onlinecheck
	Session("PNPDeposit")= Request.Form("card-amount")
	Session("PNPInvoice")= Request.QueryString("InvNo")
	Session("PNPOrder")= session("OrdNo")
end if


'***************************************************************************************
'Get active account / Success and bad Links / Basic Deposit Transaction / Order
dim conn, rsInvHdr, rsCustomer,rsOrder , strSQL

if Session("PNPCase")= "Deposit" then 
	Session("PNPusername")= trim(session("rscust")("Account")) 
	if Request.Form("txtAction")="" then Session("PNPsuccssLnk")= "http://" & GetURL & "Order/saveord.asp?txtDisc="& Request("txtDisc") &"&txtDep="& Request("txtDep") &"&hidDep="& Request("hidDep") &""
	
elseif Session("PNPCase")= "DepositAdd" then 
	'Session("PNPusername")= trim(session("rscust")("Account")) 
	set conn=server.CreateObject("ADODB.connection")
	conn.Open Application("DataConnectionString")
	set rsOrder = server.CreateObject("ADODB.recordset")
	strSQL="select Account from OrdHdr where Order='"& Session("PNPOrder") &"'"
	rsOrder.Open strSQL,conn
	Session("PNPusername")= trim(rsOrder("Account"))
	rsOrder.Close()
	Set rsOrder = Nothing		

	if Request.Form("txtAction")="" then Session("PNPsuccssLnk")= "http://" & GetURL & "Order/saveModOrd.asp?txtDisc="& Request("txtDisc") &"&txtDep="& Request("txtDep") &"&hidDep="& Request("hidDep") &""	
	
elseif Session("PNPCase")= "DepositRefund" or Session("PNPCase")="DepositCancel" then 
	'Session("PNPusername")= trim(session("rscust")("Account")) 	
	set conn=server.CreateObject("ADODB.connection")
	conn.Open Application("DataConnectionString")
	set rsOrder = server.CreateObject("ADODB.recordset")
	strSQL="select Account from OrdHdr where Order='"& Session("PNPOrder") &"'"
	rsOrder.Open strSQL,conn
	Session("PNPusername")= trim(rsOrder("Account"))
	rsOrder.Close()
	Set rsOrder = Nothing				
	
	if Request.Form("txtAction")="" and Session("PNPCase")= "DepositRefund" then 
		Session("PNPsuccssLnk") = "http://" & GetURL & "Order/saveModOrd.asp?txtDisc="& Request("txtDisc") &"&txtDep="& Request("txtDep") &"&hidDep="& Request("hidDep") &""	
	elseif  Request.Form("txtAction")="" and Session("PNPCase")= "DepositCancel" then 		
			Session("PNPsuccssLnk") = "http://" & GetURL & "Order/cancelOrd.asp"	
	end if		

	'Get existing PNPTransaction Basic Record	
	set conn=server.CreateObject("ADODB.connection")
	conn.Open Application("DataConnectionString")
	set rsTrans = server.CreateObject("ADODB.recordset")
	strSQL="select * from PNPTrans where lBasic=.T. and Order='"& Session("PNPOrder") &"'"
	rsTrans.Open strSQL,conn
	
	if rsTrans.EOF and rsTrans.BOF and Session("PNPCase")= "DepositRefund" then 'redirect the user if ther arn't related transaction record
		Session("PNPbadLnk") = "http://" & GetURL & "Order/modCharge.asp?Save=T&FinalStatus=problem&MErrMsg=Not found existing Credit Card Deposit to void !!"
		Response.Redirect  Session("PNPbadLnk")
	elseif rsTrans.EOF and rsTrans.BOF and Session("PNPCase")= "DepositCancel" then 'redirect the user if ther arn't related transaction record		
		Session("PNPbadLnk") = "http://" & GetURL & "Order/modifyorder.asp?OrderNo="&session("OrdNo")&"&Type=T&FinalStatus=problem&MErrMsg=Not found existing Credit Card Deposit to void !!"
		Response.Redirect  Session("PNPbadLnk")
	else
		Session("PNPorderID")= trim(rsTrans("cTransid"))
	end if


else 'Ivoice
	'Invoice payment will not go throw with check of course
	Session("PNPPayMethod") = "credit"
	
	set conn=server.CreateObject("ADODB.connection")
	conn.Open Application("DataConnectionString")

	set rsInvHdr = server.CreateObject("ADODB.recordset")
	'strSQL="select Invoice, Npaied, Order, Totalchg, Account from InvHdr where Invoice='"& Session("PNPInvoice") &"'"
	strSQL="select Invoice, Order, Totalchg, Account from InvHdr where Invoice='"& Session("PNPInvoice") &"'"

	rsInvHdr.CursorLocation = 3
	rsInvHdr.CursorType = adOpenStatic
	rsInvHdr.LockType = 4

	rsInvHdr.Open strSQL,conn

	Session("PNPusername")= trim(rsInvHdr("Account"))
	Session("PNPOrder")= trim(rsInvHdr("Order"))
	if Request.Form("txtAction")="" then Session("PNPsuccssLnk")= "http://" & GetURL & "Invoice/InvoicePaymentFrame.asp?chkCust="& Session("chkCust") &"&InvSuccNo="& Session("PNPInvoice") &""
	
	rsInvHdr.Close()
	Set rsInvHdr = Nothing						
end if	



'***************************************************************************************
'Get Active Customer information values
set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
set rsCustomer = server.CreateObject("ADODB.recordset")
strSQL="select Account, btname, caddress12, caddress22, caddress32, caddress62, phone1, caddress42, caddress52 from customer where Account='"& Session("PNPusername") &"'"
'Response.Write "<font size=5>" &  strSQL & "<hr>"
'Response.End 
rsCustomer.Open strSQL,conn

Session("PNPaddr1")= trim(rsCustomer("caddress12"))
Session("PNPaddr2")= trim(rsCustomer("caddress22"))
Session("PNPcardnumber")= ""
Session("PNPname")= ""
Session("PNPcity")= trim(rsCustomer("caddress32"))
'Session("PNPcountry")= if (trim(session("rscust")("caddress62")))="USA" then Response.Write "US" else Response.Write trim(session("rscust")("caddress62")) end if
Session("PNPcountry")= trim(rsCustomer("caddress62"))
Session("PNPEMail")= ""       
Session("PNPlastbilled")= ""       
Session("PNPname")= trim(rsCustomer("btname"))
Session("PNPexp") =""
Session("PNPphone")= trim(rsCustomer("phone1"))       			       
Session("PNPstate")= trim(rsCustomer("caddress42"))       
Session("PNPstatus")= ""
Session("PNPsuccess")= ""
Session("PNPzip")= trim(rsCustomer("caddress52"))                                   
'Session("PNPstatus")= myVal                            
rsCustomer.close
set rsCustomer=nothing 



'***************************************************************************************
'Amount Initial Values
if Request("txtAmount")="" then
	if Session("PNPCase")= "Deposit" or Session("PNPCase")= "DepositAdd"  then
		Session("PNPAmount")= cdbl(Session("PNPDeposit"))	 	
	elseif Session("PNPCase")= "DepositRefund" or Session("PNPCase")= "DepositCancel" then
		Session("PNPAmount")= -cdbl(Session("PNPDeposit"))	 
	elseif Session("PNPCase")= "Invoice" then
		'Session("PNPAmount")= cdbl(rsInvHdr("Totalchg")) / 2
		Session("PNPAmount")= cdbl(Session("PNPDeposit"))		
	end if
else 'Get What User Input
	Session("PNPAmount")= cdbl(Request("txtAmount"))
end if	


'***************************************************************************************
'Get Active URL to redirect User Online
function GetURL()
	dim strURL 'as string
	dim myArray 'as array
	strURL = Request.ServerVariables("SCRIPT_NAME")
	myArray = Split(strURL,"/")
	strURL=""
'	strURL = Request.ServerVariables("LOCAL_ADDR") & "/"
	strURL = Request.ServerVariables("Server_Name") & "/"
	for i = 0 to UBound(myArray)-2
		if myArray(i)<>"" and right(myArray(i),3)<> "asp" then
			strURL = strURL + myArray(i) & "/"
		end if
	next
	GetURL = strURL
end function


'***************************************************************************************
'Redirect ot SSL Server Page
dim strParamters 'as string
'strParamters =  "https://www.pay-order.com/billblassny/PaymentformSSL.asp?" 
'strParamters =  "http://webserver/pay-order/billblassny/PaymentFormSSL.asp?"
'strParamters =  "http://webserver/crmdemo/pnpcom/PaymentFormSSL.asp?"
if Session("UseExtSSL") = "T" then
	strParamters = session("SSLURL") & "/PaymentFormSSL.asp?" 'default
else	
	strParamters =  "PaymentFormSSL.asp?" 'default
end if

strParamters =  strParamters & "PNPCase="&Session("PNPCase")&""
strParamters =  strParamters & "&PNPPayMethod="&Session("PNPPayMethod")&""
strParamters =  strParamters & "&PNPDeposit="&Session("PNPDeposit")&""
strParamters =  strParamters & "&PNPInvoice="&Session("PNPInvoice")&""
strParamters =  strParamters & "&PNPOrder="&Session("PNPOrder")&""
strParamters =  strParamters & "&PNPusername="&Session("PNPusername")&""
strParamters =  strParamters & "&PNPsuccssLnk="&SERVER.URLEncode(Session("PNPsuccssLnk"))&""
strParamters =  strParamters & "&PNPbadLnk="&SERVER.URLEncode(Session("PNPbadLnk"))&""
strParamters =  strParamters & "&PNPorderID="&Session("PNPorderID")&""
strParamters =  strParamters & "&PNPAmount="&Session("PNPAmount")&""
strParamters =  strParamters & "&CRMPath="&GetURL&""
strParamters =  strParamters & "&chkCust="&Session("chkCust")&""
strParamters =  strParamters & "&Theme="&Session("Theme")&""

'Active Customer Paramters
strParamters =  strParamters & "&PNPaddr1="&Session("PNPaddr1")&""
strParamters =  strParamters & "&PNPaddr2="&Session("PNPaddr2")&""
strParamters =  strParamters & "&PNPcity="&Session("PNPcity")&""
strParamters =  strParamters & "&PNPcountry="&Session("PNPcountry")&""
strParamters =  strParamters & "&PNPname="&Session("PNPname")&""
strParamters =  strParamters & "&PNPphone="&Session("PNPphone")&""
strParamters =  strParamters & "&PNPstate="&Session("PNPstate")&""
strParamters =  strParamters & "&PNPzip="&Session("PNPzip")&""              

'Response.Write "<font size=5>" & Session("PNPsuccssLnk") & "<hr>"
'Response.Write "<font size=5>" & strParamters & "<hr>"
Response.Redirect strParamters

'***************************************************************************************
'***************************************************************************************
%>

