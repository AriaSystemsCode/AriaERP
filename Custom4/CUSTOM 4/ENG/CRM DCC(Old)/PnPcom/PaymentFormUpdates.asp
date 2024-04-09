<%@ Language=VBScript %>
<%
Response.CacheControl  = "no-cache"
Response.AddHeader  "Pragma", "no-cache"
Response.Expires = -1
Response.Buffer = true
%>
<!--#include file="../common/Add2Log.asp"-->
<!--#include file="../order/addcredit.asp"-->
<!--#include file="../order/adddebit.asp"-->

<%
'***************************************************************************************
%>
<HTML>
<HEAD><LINK REL=stylesheet HREF="../images/<%=Session("Theme")%>/invoice.css" TYPE="text/css">
<meta http-equiv="Content-Type" content="text/html; chars%et=windows-1256">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<Title>CRM - Payments</Title></head><body>
<br><br>
<Table width="95%" align=center height=50 border=1>
<TR><TD class=title>CRM Payments</TD></TR></Table>
<%

'***************************************************************************************
'first time only we read the pramters
if Request.Form("txtAction")="" then 
	Session("txtAction")= Request.QueryString("txtAction")
	Session("PNPMessage")= Request.QueryString("PNPMessage")
	Session("PNPsuccssLnk")= Request.QueryString("PNPsuccssLnk")
	Session("PNPbadLnk")= Request.QueryString("PNPbadLnk")
	Session("PNPMessageResault")= Request.QueryString("PNPMessageResault")
	Session("PNPCase")= Request.QueryString("PNPCase")
	Session("PNPorderID")= Request.QueryString("PNPorderID")
	Session("PNPcard-amount")= Request.QueryString("PNPcard-amount")
	Session("PNPusername")= Request.QueryString("PNPusername")
	Session("PNPauth-code")= Request.QueryString("PNPauth-code")
	Session("PNPauth-msg")= Request.QueryString("PNPauth-msg")
	Session("PNPOrder")= Request.QueryString("PNPOrder")
	Session("PNPInvoice")= Request.QueryString("PNPInvoice")
	Session("PNPFinalStatus")= Request.QueryString("PNPFinalStatus")
	Session("PNPname")= Request.QueryString("PNPname")
end if


'***************************************************************************************
'*****************************************************************************************
'Select Transaction Type
'select case Session("txtAction")
'*****************************************************************************************
'Bill Member / Bill after Update / Online Check  
if Session("txtAction") = "Trans" or Session("txtAction") = "Trans_Add" or Session("txtAction") = "TransCheck" then
		 'Bill members (tras or update then trans)
		 if Session("PNPFinalStatus") = "success" then 
		   'Response.Redirect "True Link"
		   	if Session("PNPCase")= "Deposit" or Session("PNPCase")= "DepositAdd" then
				AddTrans()  'Adding PNP Transaction record				
				Response.Redirect Session("PNPsuccssLnk")
			else 'Invoice
				AddTrans()  'Adding PNP Transaction record								
				'UpdateInvTrans()'Modify invoice flag, Add 2 Logs and Add 2 Credit
				Response.Redirect Session("PNPsuccssLnk")
			end if
		 Else 
		end if

'***************************************************************************************
'#1 Refund Deposit Value
'#2 Online check
'#3 First login at this page then we "query_member"
else
	Response.Write "<font size=5>" & Session("PNPsuccssLnk") & "<hr>"
	
	if Session("PNPCase")= "DepositRefund" or Session("PNPCase")="DepositCancel" then '#1 Refund Deposit Value	
		if Session("PNPFinalStatus") = "success" then 
			AddTrans()  'Adding PNP Transaction record				
			Response.Redirect Session("PNPsuccssLnk")				
		elseif Session("PNPCase")= "DepositRefund" then 		
			Response.Redirect  Session("PNPbadLnk")			
		elseif Session("PNPCase")="DepositCancel" then
			Response.Redirect  Session("PNPbadLnk")			
		end if	 

'***************************************************************************************
	elseif  Session("PNPPayMethod")= "onlinecheck" then '#2 Online check
'***************************************************************************************
	else '#3 First login at this page then we "query_member"
'***************************************************************************************			
	end if
end if  
'***************************************************************************************
'***************************************************************************************


'***************************************************************************************
'Add Transaction To PNP Transaction file
function AddTrans()
	strSQL= "Insert into pnptrans (cTransid, Amount,ctrn_type , Account, Trandate, cTrantime, cAuthcode, cAuthmsg, Order, Invoice, cFinalstat)"
	'Negativ Values
	if Session("PNPCase")= "Deposit" or Session("PNPCase")= "DepositAdd" or Session("PNPCase")= "Invoice" then
		strSQL = strSQL + "values ('"& Session("PNPorderID") &"', "& -Session("PNPcard-amount") &", '"& Session("PNPCase") &"','"& Session("PNPusername") &"', {"& date() &"},'"& time() &"','"& Session("PNPauth-code") &"', '"& Session("PNPauth-msg") &"', '"& Session("PNPOrder") &"', '"& Session("PNPInvoice") &"', '"& Session("PNPFinalStatus") &"')"
	'Positive Values		
	elseif Session("PNPCase")= "DepositRefund" or Session("PNPCase")= "DepositCancel" then
		strSQL = strSQL + "values ('"& Session("PNPorderID") &"', "& Session("PNPcard-amount") &", '"& Session("PNPCase") &"','"& Session("PNPusername") &"', {"& date() &"},'"& time() &"','"& Session("PNPauth-code") &"', '"& Session("PNPauth-msg") &"', '"& Session("PNPOrder") &"', '"& Session("PNPInvoice") &"', '"& Session("PNPFinalStatus") &"')"		
	end if
	'Response.Write "<font size=5>" & strSQL
	conn.Execute(strSQL)
	

	'add to credit
	if Session("PNPCase")= "DepositAdd" then '!!Deposit in save order
		call addCredit("Order# "&Session("PNPOrder"),Session("PNPcard-amount"),Session("PNPusername"),Session("PNPname"))
		Add2Log "", Session("PNPusername"),"Order Deposit Online Payment",Session("PNPOrder"),"Invoice No. "& Session("PNPInvoice") &""		
	elseif Session("PNPCase")= "Invoice" then
		call addCredit("Invoice# "&Session("PNPInvoice"),Session("PNPcard-amount"),Session("PNPusername"),Session("PNPname"))		
		Add2Log "", Session("PNPusername"),"Invoice Online Payment",Session("PNPOrder"),"Invoice No. "& Session("PNPInvoice") &""		

	'add to debit	
	elseif Session("PNPCase")= "DepositRefund" or Session("PNPCase")= "DepositCancel" then
		call addDebit("Order# "&Session("PNPOrder"),Session("PNPcard-amount"),Session("PNPusername"),Session("PNPname"))
		Add2Log "", Session("PNPusername"),"Refund Deposit Online Payment",Session("PNPOrder"),"Invoice No. "& Session("PNPInvoice") &""		
	end if	

end function


'***************************************************************************************
''Update Invoice Pyament flag (Npaied)
'function UpdateInvTrans()
'	'Modify invoice flag
'	Application.Lock 						
'	rsInvHdr("Npaied")=true
'	rsInvHdr.UpdateBatch 
'	rsInvHdr.Close()
'	Set rsInvHdr = Nothing						
'	Application.UnLock
'end function

'***************************************************************************************
'***************************************************************************************
%>