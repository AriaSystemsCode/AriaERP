<%@ Language=VBScript %>
<%
Response.CacheControl  = "no-cache"
Response.AddHeader  "Pragma", "no-cache"
Response.Expires = -1
Response.Buffer = true
'***************************************************************************************
'first time only we read the pramters
if Request.Form("txtAction")="" then 
	Session("PNPCase")= Request.QueryString("PNPCase")
	Session("PNPPayMethod")= Request.QueryString("PNPPayMethod")
	Session("PNPDeposit")= Request.QueryString("PNPDeposit")
	Session("PNPInvoice")= Request.QueryString("PNPInvoice")
	Session("PNPOrder")= Request.QueryString("PNPOrder")
	Session("PNPusername")= Request.QueryString("PNPusername")
	Session("PNPsuccssLnk")= Request.QueryString("PNPsuccssLnk")		
	Session("PNPbadLnk")= Request.QueryString("PNPbadLnk")
	Session("PNPorderID")= Request.QueryString("PNPorderID")
	Session("PNPAmount")= Request.QueryString("PNPAmount")
	Session("CRMPath")= Request.QueryString("CRMPath")
	Session("chkCust")= Request.QueryString("chkCust")
	Session("Theme")= Request.QueryString("Theme")

	'Active Customer Paramters	
	Session("PNPaddr1")= Request.QueryString("PNPaddr1")
	Session("PNPaddr2")= Request.QueryString("PNPaddr2")
	Session("PNPcity")= Request.QueryString("PNPcity")
	Session("PNPcountry")= Request.QueryString("PNPcountry")
	Session("PNPname")= Request.QueryString("PNPname")
	Session("PNPphone")= Request.QueryString("PNPphone")
	Session("PNPstate")= Request.QueryString("PNPstate")
	Session("PNPzip")= Request.QueryString("PNPzip")
	
'	Response.Write Session("PNPCase") & "<hr>" 
'	Response.Write Session("PNPPayMethod")& "<hr>" 
'	Response.Write Session("PNPDeposit")& "<hr>" 
'	Response.Write Session("PNPInvoice")& "<hr>" 
'	Response.Write Session("PNPOrder")& "<hr>" 
'	Response.Write Session("PNPusername")& "<hr>" 
'	Response.Write Session("PNPsuccssLnk")& "<hr>" 
'	Response.Write Session("PNPbadLnk")& "<hr>" 
'	Response.Write Session("PNPorderID")& "<hr>" 
'	Response.Write Session("PNPAmount")& "<hr>" 
'	Response.Write Session("CRMPath")& "<hr>" 
'	Response.Write Session("chkCust")& "<hr>" 
'	Response.Write Session("Theme")& "<hr>"
'	Response.Write Session("PNPaddr1")& "<hr>" 
'	Response.Write Session("PNPaddr2")& "<hr>" 
'	Response.Write Session("PNPcity")& "<hr>" 
'	Response.Write Session("PNPcountry")& "<hr>" 
'	Response.Write Session("PNPname")& "<hr>" 
'	Response.Write Session("PNPphone")& "<hr>" 
'	Response.Write Session("PNPstate")& "<hr>" 
'	Response.Write Session("PNPzip")& "<hr>"  
'	Response.End 
end if
%>
<HTML>
<HEAD>
<meta http-equiv="Content-Type" content="text/html; chars%et=windows-1256">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<LINK REL=stylesheet HREF="http://<%=Session("CRMPath")%>/images/<%=Session("Theme")%>/invoice.css" TYPE="text/css">
<Title>CRM - Payments</Title></head><body>
<br><br>
<Table width="95%" align=center height=50 border=1>
<TR><TD class=title>CRM Payments</TD></TR></Table>
<%
'***************************************************************************************
'*****************************************************************************************
'Select Transaction Type
select case Request("txtAction")
'*****************************************************************************************
'Bill Member
case "Trans"
	if DataChanged then 'Update Customer Credit Info first then do transaction		
		Session("PNPMessage")= "Update then Bill existing Member..."
		
		Set PNPObj = CreateObject("pnpcom.main")
		
		'Update and Bill Member
		'Update First
		STR = "mode=update_member"
		STR = STR + "&publisher-name=pnpdemo"
		STR = STR + "&publisher-password=pnpdemo"
		STR = STR + "&username="& Session("PNPusername") &""
		STR = STR + "&card-amount="& Session("PNPAmount") &""
		STR = STR + "&enddate=20201231"
		STR = STR + "&email="& Session("PNPEMail") &""		
		STR = STR + "&publisher-email=wma@aria.com.eg"
		
		'STR = STR + "&card-cvv=032"
				

		'Updated fields only if user change it
		if mid(Request("CardNumber"),5,1) <> "*" then STR = STR + "&card-number="& Request("CardNumber") &""
		if Session("PNPname") <> trim(Request("name")) then	STR = STR + "&card-name="& Request("Name") &""
		if Session("PNPaddr1") <> trim(Request("Street")) then	STR = STR + "&card-address1="& Request("Street") &""
		if Session("PNPaddr2") <> trim(Request("Street2")) then STR = STR + "&card-address2="& Request("Street2") &""
		if Session("PNPcity") <> trim(Request("city")) then STR = STR + "&card-city="& Request("City") &""
		if Session("PNPcountry") <> trim(Request("country")) then STR = STR + "&card-country="& Request("Country") &""
		if Session("PNPEMail") <> trim(Request("EMail")) then STR = STR + "&email="& Request("EMail") &""
		if Session("PNPphone") <> trim(Request("phone")) then STR = STR + "&phone="& Request("phone") &""
		if Session("PNPstate") <> trim(Request("State")) then STR = STR + "&card-state="& Request("State") &""
		if Session("PNPZip") <> trim(Request("Zip")) then STR = STR + "&card-zip="& Request("Zip") &""
		if Session("PNPexp") <> trim(Request("CardEXPm") &"/"& Request("CardEXPy"))then	STR = STR + "&card-exp="& Request("CardEXPm") &"/"& Request("CardEXPy") &""		
		
		'Response.Write STR & "<br>"
 		Results = PNPObj.doTransaction("",STR,"","")	
 		
 		
		'Bill after update
		STR = "mode=bill_member"		
		STR = STR + "&publisher-name=pnpdemo"
		STR = STR + "&publisher-password=pnpdemo"
		STR = STR + "&username="& Session("PNPusername") &""
		STR = STR + "&card-amount="& Session("PNPAmount") &""
		STR = STR + "&enddate=20201231"
		STR = STR + "&email="& Session("PNPEMail") &""		
		STR = STR + "&publisher-email=wma@aria.com.eg"
			
'		STR = STR + "&card-cvv=032"

		'Response.Write STR & "<br>"
		Results = PNPObj.doTransaction("",STR,"","")
		getResault()	
		
		 if Session("PNPFinalStatus") = "success" then 
			Session("PNPMessageResault")= "Update and Bill existing Member Success..."
			'Redirect to third Page with all parameters
			Response.Redirect GetParamters
		 Else 
		   Session("PNPMessageResault")= "Update and Bill existing Member Failed..."
		end if
		

	  			
	  			
'***************************************************************************************
	else'Use Existing Customer Credit Info to do transaction	
		Session("PNPMessage")= "Bill existing Member..."
		
		STR = "mode=bill_member"
		STR = STR + "&publisher-name=pnpdemo"
		STR = STR + "&publisher-password=pnpdemo"
		STR = STR + "&publisher-email=wma@aria.com.eg"
		STR = STR + "&username="& Session("PNPusername") &""
		STR = STR + "&card-amount="& Session("PNPAmount") &""

						
		Set PNPObj = CreateObject("pnpcom.main")
		Results = PNPObj.doTransaction("",STR,"","")
			
		getResault()	
			
		if Session("PNPFinalStatus") = "success" then 
			Session("PNPMessageResault")= "Bill Existing Member Success..."
			'Redirect to third Page with all parameters
			Response.Redirect GetParamters
		Else 
			Session("PNPMessageResault")= "Bill Existing Member Failed..."
		end if
			 
	end if

'***************************************************************************************		 
'Update Successfoll'Do Transaction
case "Trans_Add"
	Session("PNPMessage")= "Bill and Add New Member..."	

	 Set PNPObj = CreateObject("pnpcom.main")

	'Add and Bill Member
	STR = "mode=auth|add_member"
	
	STR = STR + "&publisher-name=pnpdemo"
	STR = STR + "&publisher-password=pnpdemo"
	STR = STR + "&username="& Session("PNPusername") &""
	STR = STR + "&card-amount="& Session("PNPAmount") &""
	STR = STR + "&enddate=20201231"
	STR = STR + "&status=active"
	STR = STR + "&billcycle=0"							
	STR = STR + "&publisher-email=wma@aria.com.eg"
	
	STR = STR + "&card-number="& Request("CardNumber") &""
	STR = STR + "&card-name="& Request("Name") &""
	STR = STR + "&card-address1="& Request("Street") &""
	STR = STR + "&card-address2="& Request("Street2") &""
	STR = STR + "&card-city="& Request("City") &""
	STR = STR + "&card-country="& Request("Country") &""
	STR = STR + "&card-zip="& Request("Zip") &""
	STR = STR + "&card-state="& Request("State") &""
	STR = STR + "&card-exp="& Request("CardEXPm") &"/"& Request("CardEXPy") &"  "
	STR = STR + "&email="& Request("EMail") &""
	
	'STR = STR + "&card-cvv=032"
	
	Results = PNPObj.doTransaction("",STR,"","")
	
	getResault()	
	
	 if Session("PNPFinalStatus") = "success" then 
	   Session("PNPMessageResault")= "Add and Bill Member Success..."
			'redirect to third Page with all parameters
			Response.Redirect GetParamters
	 Else 
	   Session("PNPMessageResault")= "Add and Bill Member Failed..."
	end if
 
 
 
 '*****************************************************************************************
'Bill Online Check
case "TransCheck"

	Session("PNPMessage")= "Bill Online Check..."	

	 Set PNPObj = CreateObject("pnpcom.main")

	'Auth Online Check
	STR = "mode=auth"
	
	STR = STR + "&publisher-name=pnpdemo"
	STR = STR + "&publisher-password=pnpdemo"
		
	'STR = STR + "&username="& Session("PNPusername") &""
	'STR = STR + "&enddate=20201231"
	'STR = STR + "&status=active"
	'STR = STR + "&billcycle=0"							
	STR = STR + "&card-amount="& Session("PNPAmount") &""
	STR = STR + "&publisher-email=wma@aria.com.eg"
	
	'STR = STR + "&card-number="& Request("CardNumber") &""
	'STR = STR + "&card-exp="& Request("CardEXPm") &"/"& Request("CardEXPy") &"  "	
	
	STR = STR + "&paymethod=onlinecheck" 'Account Type ("checking", "savings", "credit")
	STR = STR + "&checknum="& Request("checknum") &"" 'Check #.
	STR = STR + "&routingnum="& Request("routingnum") &"" 'Bank Routing #
	STR = STR + "&accountnum="& Request("accountnum") &"" 'Checking Account #
	
	STR = STR + "&card-name="& Request("Name") &""
	STR = STR + "&card-address1="& Request("Street") &""
	STR = STR + "&card-address2="& Request("Street2") &""
	STR = STR + "&card-city="& Request("City") &""
	STR = STR + "&card-country="& Request("Country") &""
	STR = STR + "&card-zip="& Request("Zip") &""
	STR = STR + "&card-state="& Request("State") &""

	STR = STR + "&email="& Request("EMail") &""
	
	'STR = STR + "&card-cvv=032"
	
	Results = PNPObj.doTransaction("",STR,"","")
	
	getResault()	
	
	 if Session("PNPFinalStatus") = "success" then 
	   Session("PNPMessageResault")= "Add and Bill Member Success..."
			'redirect to third Page with all parameters
			Response.Redirect GetParamters
	 Else 
	   Session("PNPMessageResault")= "Add and Bill Member Failed..."
	end if
			 
	

'***************************************************************************************
'#1 Refund Deposit Value
'#2 Online check
'#3 First login at this page then we "query_member"
case else
	if Session("PNPCase")= "DepositRefund" or Session("PNPCase")="DepositCancel" then '#1 Refund Deposit Value	
		Session("PNPMessage")= "Refund existing Member..."
		STR = "mode=void"
		STR = STR + "&publisher-name=pnpdemo"
		STR = STR + "&publisher-password=pnpdemo"
		STR = STR + "&publisher-email=wma@aria.com.eg"
		STR = STR + "&txn-type=auth"
		STR = STR + "&username="& Session("PNPusername") &""
		STR = STR + "&card-amount="& Session("PNPAmount") &""
		STR = STR + "&orderID="& Session("PNPorderID") &""

		Set PNPObj = CreateObject("pnpcom.main")
		Results = PNPObj.doTransaction("",STR,"","")	
		getResault()	

		if Session("PNPFinalStatus") = "success" then 
			Session("PNPMessageResault")= "Refund Existing Member Success..."
			Response.Redirect GetParamters
		elseif Session("PNPCase")= "DepositRefund" then 		
			Session("PNPMessageResault")= "Refund Existing Member Failed..."
			Session("PNPbadLnk") = "http://" & Session("CRMPath") & "Order/modCharge.asp?Save=T&FinalStatus="& Session("PNPFinalStatus") &"&MErrMsg="& Session("MErrMsg") &""
			Response.Redirect GetParamters
			
		elseif Session("PNPCase")="DepositCancel" then
			Session("PNPMessageResault")= "Refund Existing Member Failed..."
			Session("PNPbadLnk") = "http://" & Session("CRMPath") & "Order/modifyorder.asp?OrderNo="&session("OrdNo")&"&Type=T&FinalStatus="& Session("PNPFinalStatus") &"&MErrMsg="& Session("MErrMsg") &""
			Response.Redirect GetParamters
		end if	 


'***************************************************************************************
	elseif  Session("PNPPayMethod")= "onlinecheck" then '#2 Online check

		Session("PNPMessage")= "Query Member..."
		Session("method_pay")="O"	'ordercharge method pay field
		 Set PNPObj = CreateObject("pnpcom.main")
		'query_member
		STR = "mode=query_member"
		STR = STR + "&publisher-name=pnpdemo"
		STR = STR + "&publisher-password=pnpdemo"
		STR = STR + "&username='"& Session("PNPusername") &"'"
		Results = PNPObj.doTransaction("",STR,"","")
		getResault()	
		 if Session("PNPFinalStatus") = "success" then 
		   Session("PNPMessageResault")= "Query Member Success..."
		   Session("PNPnextCase")= "TransCheck"	
		 Else 
			Session("PNPMessageResault")= "Member not exist then we will use CRM information"
			if Session("PNPCase")= "Deposit" then 'Deposit
				Session("PNPnextCase")= "Trans_Add"	
				
			else 'Invoice then 'Get Customer Data
				Session("PNPnextCase")= "TransCheck"	
				
			END IF			
		end if				 
'***************************************************************************************
	else '#3 First login at this page then we "query_member"

		Session("PNPMessage")= "Query Member..."
		 Set PNPObj = CreateObject("pnpcom.main")
		'query_member
		STR = "mode=query_member"
		STR = STR + "&publisher-name=pnpdemo"
		STR = STR + "&publisher-password=pnpdemo"
		STR = STR + "&username='"& Session("PNPusername") &"'"
		Results = PNPObj.doTransaction("",STR,"","")
		getResault()	
		 if Session("PNPFinalStatus") = "success" then 
		   Session("PNPMessageResault")= "Query Member Success..."
		   Session("PNPnextCase")= "Trans"	
		 Else 
			Session("PNPMessageResault")= "Member not exist then we will use CRM information"
			if Session("PNPCase")= "Deposit" then 'Deposit
				Session("PNPnextCase")= "Trans_Add"	
				
			else 'Invoice then 'Get Customer Data
				Session("PNPnextCase")= "Trans_Add"	
				
			END IF			
		end if
'***************************************************************************************			
	end if
end select  
'***************************************************************************************
'***************************************************************************************


'***************************************************************************************
'Collect Resault Session
sub getResault()
	 Dim objResults
	 Set objResults = Server.CreateObject("Scripting.Dictionary")

	 myArray = split (Results,"&")
	 for i = 0 to UBound(myArray)
	   myArray(i) = replace(myArray(i),"+"," ")
	   myArray(i) = replace(myArray(i),"%20"," ")
	   myArray(i) = replace(myArray(i),"%21","!")
	   myArray(i) = replace(myArray(i),"%23","#")
	   myArray(i) = replace(myArray(i),"%24","$")
	   myArray(i) = replace(myArray(i),"%25","%")
	   myArray(i) = replace(myArray(i),"%26","&")
	   myArray(i) = replace(myArray(i),"%27","'")
	   myArray(i) = replace(myArray(i),"%28","(")
	   myArray(i) = replace(myArray(i),"%29",")")
	   myArray(i) = replace(myArray(i),"%2c",",")
	   myArray(i) = replace(myArray(i),"%2d","-")
	   myArray(i) = replace(myArray(i),"%2e",".")
	   myArray(i) = replace(myArray(i),"%40","@")
	   myArray(i) = replace(myArray(i),"%2a","*******")
	   myArray(i) = replace(myArray(i),"%2f","/")        
	   pos = inStr(1,myArray(i),"=")
	   if (pos > 1) then
			myKey = Left(myArray(i),pos-1)
			myVal = Mid(myArray(i),pos+1)    
		
			'response.write "<font size=5>" & myKey + " = " + myVal + "<br>"
			objResults.Item(myKey) = myVal
				     
			if myKey = "addr1" then Session("PNPaddr1")= myVal
			if myKey = "addr2" then Session("PNPaddr2")= myVal
			if myKey = "cardnumber" then Session("PNPcardnumber")= myVal
			if myKey = "name" then Session("PNPname")= myVal
			if myKey = "exp" then Session("PNPexp")= myVal			
			if myKey = "city" then Session("PNPcity")= myVal
			if myKey = "country" then Session("PNPcountry")= myVal       
			if myKey = "email" then Session("PNPEMail")= myVal       
			if myKey = "lastbilled" then Session("PNPlastbilled")= myVal       
			if myKey = "phone" then Session("PNPphone")= myVal              
			if myKey = "state" then Session("PNPstate")= myVal       
			if myKey = "status" then Session("PNPstatus")= myVal
			if myKey = "success" then Session("PNPsuccess")= myVal
			if myKey = "zip" then Session("PNPzip")= myVal                                   
			if myKey = "status" then Session("PNPstatus")= myVal                                   
			if myKey = "FinalStatus" then Session("PNPFinalStatus")= myVal
			if myKey = "MErrMsg" then Session("MErrMsg")= myVal
			
			if myKey = "auth-code" then Session("PNPauth-code")= myVal
			if myKey = "auth-msg" then Session("PNPauth-msg")= myVal					
			if myKey = "card-amount" then Session("PNPcard-amount")= myVal					
			if myKey = "orderID" then Session("PNPorderID")= myVal					 
	    
	   End If
	 Next
	 
end sub

'***************************************************************************************
'Check if redit information changed
function DataChanged()
	dim blDataChanged
	if mid(Request("CardNumber"),5,1) <> "*" then
		blDataChanged = true		
	elseif Session("PNPaddr1") <> trim(Request("Street")) then
		blDataChanged = true		
	elseif Session("PNPaddr2") <> trim(Request("Street2")) then
		blDataChanged = true		
	elseif Session("PNPcity") <> trim(Request("city")) then
		blDataChanged = true	
	elseif Session("PNPcountry") <> trim(Request("country")) then
		blDataChanged = true
	elseif Session("PNPEMail") <> trim(Request("EMail")) then
		blDataChanged = true		
	elseif Session("PNPname") <> trim(Request("name")) then
		blDataChanged = true		
	elseif Session("PNPphone") <> trim(Request("phone")) then
		blDataChanged = true
	elseif Session("PNPstate") <> trim(Request("State")) then
		blDataChanged = true		
	elseif Session("PNPZip") <> trim(Request("Zip")) then
		blDataChanged = true																										
	elseif Session("PNPexp") <> trim(Request("CardEXPm") &"/"& Request("CardEXPy")) then
		blDataChanged = true																												
			
	else
		blDataChanged =false
	end if
	
	DataChanged	= blDataChanged

		
end function



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
'Get Card Type using intial Nombers
function GetCardType(CardNo)
	if left(CStr(CardNo),2)="51" or left(CStr(CardNo),2)="52" or left(CStr(CardNo),2)="53" or left(CStr(CardNo),2)="54" or left(CStr(CardNo),2)="55"  then 
		GetCardType = "MasterCard"
		Session("method_pay")="M" 'ordercharge method pay field
	elseif left(CStr(CardNo),1)="4" then 
		GetCardType = "VisaCard"
		Session("method_pay")="V" 		
		'Response.Write Session("method_pay")&"<br>"
	elseif left(CStr(CardNo),2)="34" or left(CStr(CardNo),2)="37" then 
		GetCardType = "AmExCard"
		Session("method_pay")="A" 				
	elseif left(CStr(CardNo),2)="30" or left(CStr(CardNo),2)="36" or left(CStr(CardNo),2)="38" then 
		GetCardType = "DinersClubCard"
		Session("method_pay")="C" 						
	elseif left(CStr(CardNo),4)="6011" then 
		GetCardType = "DiscoverCard"
		Session("method_pay")="D" 																																		
	elseif left(CStr(CardNo),4)="3088" or left(CStr(CardNo),4)="3096" or left(CStr(CardNo),4)="3112" or left(CStr(CardNo),4)="3158" or left(CStr(CardNo),4)="3337" or left(CStr(CardNo),4)="3528"  then 
		GetCardType = "JCBCard"
		Session("method_pay")="J" 								
	else
		GetCardType = ""
		Session("method_pay")="Z" 								
	end if				
end function

'call it to fill the Session("method_pay") every time
if Session("PNPPayMethod")= "credit" then  GetCardType(Session("PNPcardnumber"))


'***************************************************************************************
'Fill all parameters to use it after redirect
function GetParamters()
	strParamters =  "http://" & Session("CRMPath") & "pnpcom/PaymentFormUpdates.asp?"		
	strParamters =  strParamters & "txtAction="&Request("txtAction")&""	
	strParamters =  strParamters & "&PNPMessage="&Session("PNPMessage")&""	
	strParamters =  strParamters & "&PNPsuccssLnk="&Server.URLEncode(Session("PNPsuccssLnk"))&""
	strParamters =  strParamters & "&PNPbadLnk="&Server.URLEncode(Session("PNPbadLnk"))&""	
	strParamters =  strParamters & "&PNPMessageResault="&Session("PNPMessageResault")&""
	strParamters =  strParamters & "&PNPCase="&Session("PNPCase")&""
	strParamters =  strParamters & "&PNPorderID="&Session("PNPorderID")&""									
	strParamters =  strParamters & "&PNPcard-amount="&Session("PNPcard-amount")&""
	strParamters =  strParamters & "&PNPusername="&Session("PNPusername")&""
	strParamters =  strParamters & "&PNPauth-code="&Session("PNPauth-code")&""								
	strParamters =  strParamters & "&PNPauth-msg="&Session("PNPauth-msg")&""
	strParamters =  strParamters & "&PNPOrder="&Session("PNPOrder")&""
	strParamters =  strParamters & "&PNPInvoice="&Session("PNPInvoice")&""
	strParamters =  strParamters & "&PNPFinalStatus="&Session("PNPFinalStatus")&""
	strParamters =  strParamters & "&PNPname="&Session("PNPname")&""
'	Response.Write "<font size=5>" & strParamters
'	Response.End 
	GetParamters =  strParamters
end function
'***************************************************************************************
'***************************************************************************************

%>

<table border="0" width=95% cellspacing="0" cellpadding="0" align=center>
  <tr>
    <td width="100%">
		<b>
		<%'=Session("PNPMessage")&"<br>"%>
		<%'=Session("PNPusername")&"<br>"%>
		<%'=Session("PNPMessageResault")&"<br>"%>
		<%'=Session("method_pay")&"<br>"%>
		</b>	
	</td>
  </tr>
</table>

<br>
<table border="0" width=95% cellspacing="0" cellpadding="0" align=center>
  <tr>
    <td width="100%">
		<b>
			You safely transmit your order information via this secure page.
			<br>
			Please Complete This Form.
		</b>	
	</td>
  </tr>
</table>
<FORM name = Contract Method = post>
<input type=hidden name=txtAction value="<%=Session("PNPnextCase")%>">
<input type=hidden name=txtCase value="<%=Session("PNPCase")%>">
<table align=center border="1" bordercolor="#111111" WIDTH=80% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<%if Session("PNPFinalStatus")<>"success" and Request.Form("txtAction")<>"" then%>
	<tr>
		<td class="dark_cell" colspan =2 align=center>&nbsp;<font color=red><b>	
		<%
		Response.Write "The Payment Procedures were not completed successfully, because of" & "<br>"
		Response.Write "<b>"& UCase(Session("PNPFinalStatus"))&"</b>" &": "& Session("MErrMsg")
		%>
		</b></font></td>
	</tr>
<%end if%>
<!--tr>
	<td class=dark_cell nowrap>&nbsp;Billing Information<IMG height = 1 src = "images/pixel.gif" width =200></td>
	<td class=light_cell >&nbsp;</td> 
</tr-->
	
<tr>
	<td class=dark_cell>&nbsp;Payment Amount:</td>
	<td class=light_cell>&nbsp;<input name="txtAmount" value="<%=Session("PNPAmount")%>" <%if Session("PNPCase")= "Deposit" then Response.Write "disabled"%> type=text maxlength = 10 size = 30 tabindex = 1> *
	</td>
</tr>

<!--tr>
	<td class=dark_cell>&nbsp;Payment Method:</td>
	<td class=light_cell>&nbsp;<select name="paymethod" tabindex = 1>
			<option value="credit">Credit Card</option>
			<option value="onlinecheck">Online Check</option>
		</select>
	</td>
</tr-->
	
<tr>
	<td class=dark_cell >&nbsp;Name:</td>
	<td class=light_cell >&nbsp;<input name = Name value ="<%=Session("PNPname")%>"  maxlength = 100 size = 30 tabindex = 1> *</td>
</tr>       
	
<tr>
	<td class=dark_cell >&nbsp;Billing Address Line 1:</td>
	<td class=light_cell >&nbsp;<input name  = Street value ="<%=Session("PNPaddr1")%>" maxlength = 100 size = 30 tabindex = 3 > *</td>
</tr>
	
	<tr>
	<td class=dark_cell >&nbsp;Billing Address Line 2:</td>
	<td class=light_cell >&nbsp;<input name  = Street2 value ="<%=Session("PNPaddr2")%>" maxlength = 100 size = 30 tabindex = 3 ></td>
</tr>              
	
<tr>
	<td class=dark_cell >&nbsp;City:</td>
	<td class=light_cell >&nbsp;<input name = City  value = "<%=Session("PNPcity")%>" maxlength = 100 size = 15 tabindex = 4> *</td>
</tr>       
	
<tr>
	<td class=dark_cell >&nbsp;State:</td>
	<td class=light_cell >&nbsp;<input name = State value = "<%=Session("PNPstate")%>" maxlength = 100 size = 15 tabindex = 5> *</td>
	<!--td class=dark_cell >&nbsp;Reseller ID</td>
	<td class=light_cell >&nbsp;<input name = ResellerID maxlength = 8 size = 8 tabindex = 16 value='N-4vZ]7p'   > *Blank if not available.</td-->
</tr>
	
<tr>
	<td class=dark_cell >&nbsp;Zip/Postal Code:</td>
	<td class=light_cell >&nbsp;<input name = Zip value = "<%=Session("PNPzip")%>" maxlength = 100 size = 15 tabindex = 6> *</td>
	<!--td class=dark_cell >&nbsp;*Username </td>
	<td class=light_cell >&nbsp;<input name = UserName maxlength = 8 size = 8 tabindex = 17></td-->
</tr>
	
<tr>
	<td class=dark_cell >&nbsp;Country:</td>
	<td class=light_cell >&nbsp;<input name = Country value = "<%=Session("PNPcountry")%>" maxlength = 100 size = 15 tabindex = 7> *</td>
	<!--td class=dark_cell >&nbsp;*Password </td>
	<td class=light_cell >&nbsp;<input name = password maxlength = 100 size = 8 tabindex = 18></td-->
</tr>
	
	
<% if Session("PNPPayMethod")="credit" then%>
	<tr>
		<td class=dark_cell>&nbsp;Card Type:</td>
		<td class=light_cell >&nbsp;<input name = EZPASS type = hidden value = 0><select name=CardType tabindex=11>
		<option value = MasterCard <% if GetCardType(Session("PNPcardnumber"))="MasterCard" then Response.Write "selected" %>>MasterCard</option>
		<option value = VisaCard <% if GetCardType(Session("PNPcardnumber"))="VisaCard" then Response.Write "selected" %>>VisaCard</option>
		<option value = AmExCard <% if GetCardType(Session("PNPcardnumber"))="AmExCard" then Response.Write "selected" %>>AmExCard</option>
		<option value = DinersClubCard <% if GetCardType(Session("PNPcardnumber"))="DinersClubCard" then Response.Write "selected" %>>DinersClubCard</option>
		<option value = DiscoverCard <% if GetCardType(Session("PNPcardnumber"))="DiscoverCard" then Response.Write "selected" %>>DiscoverCard</option>
		<option value = JCBCard <% if GetCardType(Session("PNPcardnumber"))="JCBCard" then Response.Write "selected" %>>JCBCard</option>				
		</select> *</td>	
	</tr>
		
	<tr>
	<!--td class=dark_cell >*Company</td>
	<td class=light_cell >&nbsp;<input name = Company maxlength = 100 size = 30 tabindex = 2 
	   ></td-->
		<td class=dark_cell >&nbsp;Credit Card #:</td>
		<td class=light_cell >&nbsp;<input name = CardNumber value = "<%=Session("PNPcardnumber")%>" maxlength = 100 size = 25 tabindex = 12> *</td>
	</tr>
		
	<tr>
		<td class=dark_cell >&nbsp;Expiration Date:</td>
		<td class=light_cell >&nbsp;<select name=CardEXPm tabindex=13>
		<option value = 01 <%if left(Session("PNPexp"),2)="01" then Response.Write "selected" %>>01</option>
		<option value = 02 <%if left(Session("PNPexp"),2)="02" then Response.Write "selected" %>>02</option>
		<option value = 03 <%if left(Session("PNPexp"),2)="03" then Response.Write "selected" %>>03</option>
		<option value = 04 <%if left(Session("PNPexp"),2)="04" then Response.Write "selected" %>>04</option>
		<option value = 05 <%if left(Session("PNPexp"),2)="05" then Response.Write "selected" %>>05</option>
		<option value = 06 <%if left(Session("PNPexp"),2)="06" then Response.Write "selected" %>>06</option>
		<option value = 07 <%if left(Session("PNPexp"),2)="07" then Response.Write "selected" %>>07</option>
		<option value = 08 <%if left(Session("PNPexp"),2)="08" then Response.Write "selected" %>>08</option>
		<option value = 09 <%if left(Session("PNPexp"),2)="09" then Response.Write "selected" %>>09</option>
		<option value = 10 <%if left(Session("PNPexp"),2)="10" then Response.Write "selected" %>>10</option>
		<option value = 11 <%if left(Session("PNPexp"),2)="11" then Response.Write "selected" %>>11</option>
		<option value = 12 <%if left(Session("PNPexp"),2)="12" then Response.Write "selected" %>>12</option>
		</select>
		<select name=CardEXPy tabindex=14>
		<option value = 04 <%if right(Session("PNPexp"),2)="04" then Response.Write "selected" %>>04</option>
		<option value = 05 <%if right(Session("PNPexp"),2)="05" then Response.Write "selected" %>>05</option>
		<option value = 06 <%if right(Session("PNPexp"),2)="06" then Response.Write "selected" %>>06</option>
		<option value = 07 <%if right(Session("PNPexp"),2)="07" then Response.Write "selected" %>>07</option>
		<option value = 08 <%if right(Session("PNPexp"),2)="08" then Response.Write "selected" %>>08</option>
		<option value = 09 <%if right(Session("PNPexp"),2)="09" then Response.Write "selected" %>>09</option>
		<option value = 10 <%if right(Session("PNPexp"),2)="10" then Response.Write "selected" %>>10</option>
		<option value = 11 <%if right(Session("PNPexp"),2)="11" then Response.Write "selected" %>>11</option>
		<option value = 12 <%if right(Session("PNPexp"),2)="12" then Response.Write "selected" %>>12</option>
		<option value = 13 <%if right(Session("PNPexp"),2)="13" then Response.Write "selected" %>>13</option>
		<option value = 14 <%if right(Session("PNPexp"),2)="14" then Response.Write "selected" %>>14</option>
		</select> *</td>
	</tr>
		
<%end if%>

<!--tr>
	<td class=dark_cell >&nbsp;CVV2 Code</td>
	<td class=light_cell >&nbsp;<input name = CVV2 maxlength = 3 size = 3 tabindex = 15>*3 digits card verification</td>
</tr-->


<%if Session("PNPPayMethod")="onlinecheck" then %>
	<tr>
		<td class=dark_cell>&nbsp;Account Type:</td>
		<td class=light_cell>&nbsp;<select name="accttype" tabindex = 19>
				<option value= checking>Checking</option>
				<option value= savings>Savings</option>
				<option value= credit>Credit</option>
			</select> *
		</td>
	</tr>		
	<tr>
		<td class=dark_cell >&nbsp;Check #:	</td>
		<td class=light_cell >&nbsp;<input name = checknum maxlength = 100 size = 25 tabindex = 19 > *</td>
	</tr>
	<tr>
		<td class=dark_cell >&nbsp;Bank Routing #:</td>
		<td class=light_cell >&nbsp;<input name = routingnum maxlength = 100 size = 25 tabindex = 19 > *</td>
	</tr>
	<tr>
		<td class=dark_cell >&nbsp;Checking Account #:</td>
		<td class=light_cell >&nbsp;<input name = accountnum maxlength = 100 size = 25 tabindex = 19 > *</td>
	</tr>
<%end if%>


<!--tr>
	<td class=dark_cell >*&nbsp;Domain name </td>
	<td class=light_cell >&nbsp;<input name = DomainPrefix maxlength = 63 size = 17 tabindex = 8>
	<select name=DomainSuffix tabindex=9>
	<option value = com selected>com</option>
	<option value = net>net</option>
	<option value = org>org</option>
	<option value = edu>edu</option>
	<option value = cc>cc</option>
	<option value = ws>ws</option>
	<option value = tv>tv</option>
	<option value = Info>Info</option>
	<option value = Biz>Biz</option>
	<option value = CA>CA</option>
	<option value = US>US</option>
	<option value = Com.EG>Com.EG</option>
	<option value = NET.EG>NET.EG</option>
	<option value = ORG.EG>ORG.EG</option>
	<option value = COM.JO>COM.JO</option>
	<option value = NET.JO>NET.JO</option>
	<option value = ORG.JO>ORG.JO</option>
	<option value = edu.eg>edu.eg</option>
	<option value = BZ>BZ</option>
	<option value = AC>AC</option>
	<option value = LA>LA</option>
	<option value = web.tr>web.tr</option>
	<option value = ae>ae</option>
	</select></td>
</tr-->
<tr>
	<td class=dark_cell >&nbsp;E-Mail:</td>
	<td class=light_cell >&nbsp;<input name = EMail value = "<%=Session("PNPEMail")%>" maxlength = 100 size = 25 tabindex = 19 > *</td>
</tr>
	
<tr>
	<td class=dark_cell >&nbsp;Phone:</td>
	<td class=light_cell >&nbsp;<input name = phone value = "<%=Session("PNPphone")%>" maxlength = 100 size = 25 tabindex = 19 ></td>
</tr>


	
<!--tr>
<td class=dark_cell >&nbsp;Select Package</td>
<td class=light_cell colspan=3>&nbsp;<select name=Package tabindex=20><option value = "2|Plan I|100|100|0|100|9000|10|8" selected 
     >Plan I, Web 100MB, Mail 100MB, MY-SQL 25MB, MS-SQL 0MB  ( $10 Setup &amp; $8 Monthly)</option><option value = "3|Plan II|200|200|50|100|9000|10|32">Plan II, Web 200MB, Mail 200MB, MY-SQL 50MB, MS-SQL 50MB  ( $10 Setup &amp; $32 Monthly)</option><option value = "4|Plan III|500|500|100|100|9000|10|75">Plan III, Web 500MB, Mail 500MB, MY-SQL 150MB, MS-SQL 100MB  ( $10 Setup &amp; $75 Monthly)</option></select></td>
</tr-->
<tr>
	
	<td class="dark_cell" colspan =2  align = right>	
	<!--input type = hidden value = v name = Action><input type = "submit" value = "Submit Payment" Style ="BACKGROUND: #0066cc; FONT: bold 10pt Tahoma; COLOR: #f0bf00" name = verify tabindex=21></TD-->
	<!--input type = hidden value = v name = Action><input type = "button" value = "Submit Payment" Style ="BACKGROUND: #0066cc; FONT: bold 10pt Tahoma; COLOR: #f0bf00" onClick='CheckCardNumber(this.form)' name = verify tabindex=21></TD-->
	<input type = hidden value = v name = Action><input type = "button" value = "Cancel" onClick='Cancel(this.form)' name = cancel tabindex=21><input type = "button" value = "Submit Payment" onClick='CheckCardNumber(this.form)' name = verify tabindex=21></TD>
</TR>
</TABLE>
</FORM>






<SCRIPT LANGUAGE="JavaScript">

<!-- Begin
var Cards = new makeArray(8);
Cards[0] = new CardType("MasterCard", "51,52,53,54,55", "16");
var MasterCard = Cards[0];
Cards[1] = new CardType("VisaCard", "4", "13,16");
var VisaCard = Cards[1];
Cards[2] = new CardType("AmExCard", "34,37", "15");
var AmExCard = Cards[2];
Cards[3] = new CardType("DinersClubCard", "30,36,38", "14");
var DinersClubCard = Cards[3];
Cards[4] = new CardType("DiscoverCard", "6011", "16");
var DiscoverCard = Cards[4];
Cards[5] = new CardType("enRouteCard", "2014,2149", "15");
var enRouteCard = Cards[5];
Cards[6] = new CardType("JCBCard", "3088,3096,3112,3158,3337,3528", "16");
var JCBCard = Cards[6];
var LuhnCheckSum = Cards[7] = new CardType();

//Cancel
function Cancel(form) {
//check Payment Amount	
if (form.txtCase.value == "Deposit" ) {
    //window.location.href="http://<%=GetURL%>Order/ordChargeFrame.asp?Save=T";
    window.location.href="http://<%=Session("CRMPath")%>Order/ordChargeFrame.asp?Save=T";
	}
if (form.txtCase.value == "DepositAdd" ) {
    //window.location.href="http://<%=GetURL%>Order/modChargeFrame.asp?Save=T";
    window.location.href="http://<%=Session("CRMPath")%>Order/modChargeFrame.asp?Save=T";
	}	
if (form.txtCase.value == "Invoice" ) {
	//window.location.href="http://<%=GetURL%>Invoice/InvoicePaymentFrame.asp?chkCust=<%=Session("chkCust")%>";
	window.location.href="http://<%=Session("CRMPath")%>Invoice/InvoicePaymentFrame.asp?chkCust=<%=Session("chkCust")%>";
	}
}



/*************************************************************************\
CheckCardNumber(form)
function called when users click the "check" button.
\*************************************************************************/
function CheckCardNumber(form) {
var tmpyear;

//check Payment Amount	
if (form.txtAmount.value == "" ) {
    alert("You must indicate your Payment Amount");
    form.txtAmount.focus();
    form.txtAmount.select();
    return false;
	}

	Amt = form.txtAmount.value;
	if (isNaN(Amt))
	{
		alert("Please enter numbers only!");
		form.txtAmount.focus();
	    form.txtAmount.select();
		return false;
	}


if (form.Name.value == "" ) {
    alert("You must indicate your Name");
    form.Name.focus();
    form.Name.select();
    return false;
	}

/*
	if (form.Company.value == "" ) {
    alert("You must indicate your Company Name");
    form.Company.focus();
    form.Company.select();
    return false;
	}
*/

if (form.Street.value == "" ) {
    alert("You must indicate your Street Address");
    form.Street.focus();
    form.Street.select();
    return false;
	}

if (form.City.value == "" ) {
    alert("You must indicate your City");
    form.City.focus();
    form.City.select();
    return false;
	}

if (form.State.value == "" ) {
    alert("You must indicate your State");
    form.State.focus();
    form.State.select();
    return false;
	}
	
if (form.Zip.value == "" ) {
    alert("You must indicate your ZipCode");
    form.Zip.focus();
    form.Zip.select();
    return false;
	}
	
if (form.Country.value == "" ) {
    alert("You must indicate your Country");
    form.Country.focus();
    form.Country.select();
    return false;
	}


// Email

if (form.EMail.value == "" ) {
    alert("You must indicate your E-Mail Address");
    form.EMail.focus();
    form.EMail.select();
    return false;
	}

if (!isEmail(form.EMail.value) )  {
   alert("You must indicate Correct E-Mail Address");
   form.EMail.focus();
   form.EMail.select();
   return false;
   }
/*
if (form.UserName.value == "" ) {
  alert("You must indicate your User Name");
  form.UserName.focus();
  form.UserName.select();
  return false;
  }

if (form.UserName.value != "" ) {
	if (!doesStringHaveOnlyCharset(form.UserName.value.toLowerCase(),"qwertyuiopasdfghjklzxcvbnm1234567890"))
		{
		alert("your username must be alpha-numeric only");
		form.UserName.focus();
	    form.UserName.select();
		return false;
		}
  }

if (form.password.value == "" ) {
  alert("You must indicate your password");
  form.password.focus();
  form.password.select();
  return false;
  }

if (form.password.value.length < 8 ) {
  alert("Your password must be 8 Letters at least");
  form.password.focus();
  form.password.select();
  return false;
  }
*/

/*
// DomainPrefix
if (form.DomainPrefix.value == "" ) {
    alert("You must indicate your primary domain");
    form.DomainPrefix.focus();
    form.DomainPrefix.select();
    return false;
	}

if (!isStringDomainName(form.DomainPrefix.value) )  {
   alert("your primary domain must be alpha-numeric or have '-' only");
   form.DomainPrefix.focus();
   form.DomainPrefix.select();
   return false;
   }
*/

//Credit Card Payment
<%if Session("PNPPayMethod")="credit" then %>
	 // ezpass
	if (form.EZPASS.value == 0) {

		if (form.CardNumber.value.length == 0) {
			alert("Please enter a Card Number.");
			form.CardNumber.focus();
			form.CardNumber.select();
			return false;
			}
		/*
		if (form.CVV2.value.length != 3) {
		alert("CVV2 must be three digits");
		form.CVV2.focus();
		form.CVV2.select();
		return false;
		}
		*/

		tmpyear = "20" + form.CardEXPy.options[form.CardEXPy.selectedIndex].value;
		tmpmonth = form.CardEXPm.options[form.CardEXPm.selectedIndex].value;

		if (!(new CardType()).isExpiryDate(tmpyear, tmpmonth)) {
		alert("This card has already expired.");
		return false;
		}

		card = form.CardType.options[form.CardType.selectedIndex].value;
		var retval = eval(card + ".checkCardNumber(\"" + form.CardNumber.value +
		"\", " + tmpyear + ", " + tmpmonth + ");");
		cardname = "";


	}
	else
		{
		retval = true;
	} // end ezpass
<%end if%>

//Online Check Payment
<%if Session("PNPPayMethod")="onlinecheck" then %>
	//checknum / Check #.
	if (form.checknum.value == "" ) {
	    alert("You must indicate your Check #");
	    form.checknum.focus();
	    form.checknum.select();
	    return false;
		}

	//routingnum / Bank Routing #
	if (form.routingnum.value == "" ) {
	    alert("You must indicate your Bank Routing #");
	    form.routingnum.focus();
	    form.routingnum.select();
	    return false;
		}

	//accountnum /Checking Account #
	if (form.accountnum.value == "" ) {
	    alert("You must indicate your Checking Account #");
	    form.accountnum.focus();
	    form.accountnum.select();
	    return false;
		}				
	
	retval = true;
<%end if%>


if (retval)
	{
	// comment this out if used on an order form
	//cardname = "";
	form.verify.value = "Please Wait ...";
	form.verify.disabled = true;
	form.submit();
	}


//return false;

else {
	// The cardnumber has the valid luhn checksum, but we want to know which
	// cardtype it belongs to.
	alert("Please Check the card number or Card Type again.");
	form.CardNumber.select();
	return false;
   }


// CheckCardNumber(form) End
}




function CardType() {
var n;
var argv = CardType.arguments;
var argc = CardType.arguments.length;

this.objname = "object CardType";

var tmpcardtype = (argc > 0) ? argv[0] : "CardObject";
var tmprules = (argc > 1) ? argv[1] : "0,1,2,3,4,5,6,7,8,9";
var tmplen = (argc > 2) ? argv[2] : "13,14,15,16,19";

this.setCardNumber = setCardNumber;  // set CardNumber method.
this.setCardType = setCardType;  // setCardType method.
this.setLen = setLen;  // setLen method.
this.setRules = setRules;  // setRules method.
this.setExpiryDate = setExpiryDate;  // setExpiryDate method.

this.setCardType(tmpcardtype);
this.setLen(tmplen);
this.setRules(tmprules);
if (argc > 4)
this.setExpiryDate(argv[3], argv[4]);

this.checkCardNumber = checkCardNumber;  // checkCardNumber method.
this.getExpiryDate = getExpiryDate;  // getExpiryDate method.
this.getCardType = getCardType;  // getCardType method.
this.isCardNumber = isCardNumber;  // isCardNumber method.
this.isExpiryDate = isExpiryDate;  // isExpiryDate method.
this.luhnCheck = luhnCheck;// luhnCheck method.
return this;
}

/*************************************************************************\
boolean checkCardNumber([String cardnumber, int year, int month])
return true if cardnumber pass the luhncheck and the expiry date is
valid, else return false.
\*************************************************************************/
function checkCardNumber() {
var argv = checkCardNumber.arguments;
var argc = checkCardNumber.arguments.length;
var cardnumber = (argc > 0) ? argv[0] : this.cardnumber;
var year = (argc > 1) ? argv[1] : this.year;
var month = (argc > 2) ? argv[2] : this.month;

this.setCardNumber(cardnumber);
this.setExpiryDate(year, month);

if (!this.isCardNumber())
return false;
if (!this.isExpiryDate())
return false;

return true;
}
/*************************************************************************\
String getCardType()
return the cardtype.
\*************************************************************************/
function getCardType() {
return this.cardtype;
}
/*************************************************************************\
String getExpiryDate()
return the expiry date.
\*************************************************************************/
function getExpiryDate() {
return this.month + "/" + this.year;
}
/*************************************************************************\
boolean isCardNumber([String cardnumber])
return true if cardnumber pass the luhncheck and the rules, else return
false.
\*************************************************************************/
function isCardNumber() {
	var argv = isCardNumber.arguments;
	var argc = isCardNumber.arguments.length;
	var cardnumber = (argc > 0) ? argv[0] : this.cardnumber;
	
	//WMA avoid * start
	if (Contract.CardNumber.value.substr(4, 14) == "**************" ) {
		//alert("You using encrypted credit num");
		return true;     
	}
	
	if (!this.luhnCheck())
	return false;

	for (var n = 0; n < this.len.size; n++)
	if (cardnumber.toString().length == this.len[n]) {
		for (var m = 0; m < this.rules.size; m++) {
			var headdigit = cardnumber.substring(0, this.rules[m].toString().length);
			if (headdigit == this.rules[m])
				return true;
		}
		return false;
	}
	return false;
}

/*************************************************************************\
boolean isExpiryDate([int year, int month])
return true if the date is a valid expiry date,
else return false.
\*************************************************************************/
function isExpiryDate() {
var argv = isExpiryDate.arguments;
var argc = isExpiryDate.arguments.length;

year = argc > 0 ? argv[0] : this.year;
month = argc > 1 ? argv[1] : this.month;

if (!isNum(year+""))
return false;
if (!isNum(month+""))
return false;
today = new Date();
expiry = new Date(year, month);
if (today.getTime() > expiry.getTime())
return false;
else
return true;
}

/*************************************************************************\
boolean isNum(String argvalue)
return true if argvalue contains only numeric characters,
else return false.
\*************************************************************************/
function isNum(argvalue) {
argvalue = argvalue.toString();

if (argvalue.length == 0)
return false;

for (var n = 0; n < argvalue.length; n++)
if (argvalue.substring(n, n+1) < "0" || argvalue.substring(n, n+1) > "9")
return false;

return true;
}

function isEmail(str) {
  // are regular expressions supported?
  var supported = 0;
  if (window.RegExp) {
    var tempStr = "a";
    var tempReg = new RegExp(tempStr);
    if (tempReg.test(tempStr)) supported = 1;
  }
  if (!supported)
    return (str.indexOf(".") > 2) && (str.indexOf("@") > 0);
  var r1 = new RegExp("(@.*@)|(\\.\\.)|(@\\.)|(^\\.)");
  var r2 = new RegExp("^.+\\@(\\[?)[a-zA-Z0-9\\-\\.]+\\.([a-zA-Z]{2,3}|[0-9]{1,3})(\\]?)$");
  return (!r1.test(str) && r2.test(str));
}
function doesStringHaveOnlyCharset( s, charset ) {
    var i;
    for( i=0; i<s.length; i++ ) {
        if( charset.indexOf(s.charAt(i))==-1 ) {
            return false;
        }
    }
    return true;
}


function isStringDomainName( s ) {
    var lowerS = s.toLowerCase();
    return doesStringHaveOnlyCharset(lowerS,"qwertyuiopasdfghjklzxcvbnm1234567890-");
}



/*************************************************************************\
boolean luhnCheck([String CardNumber])
return true if CardNumber pass the luhn check else return false.
Reference: http://www.ling.nwu.edu/~sburke/pub/luhn_lib.pl
\*************************************************************************/
function luhnCheck() {
var argv = luhnCheck.arguments;
var argc = luhnCheck.arguments.length;

var CardNumber = argc > 0 ? argv[0] : this.cardnumber;

if (! isNum(CardNumber)) {
return false;
  }

var no_digit = CardNumber.length;
var oddoeven = no_digit & 1;
var sum = 0;

for (var count = 0; count < no_digit; count++) {
var digit = parseInt(CardNumber.charAt(count));
if (!((count & 1) ^ oddoeven)) {
digit *= 2;
if (digit > 9)
digit -= 9;
}
sum += digit;
}
if (sum % 10 == 0)
return true;
else
return false;
}

/*************************************************************************\
ArrayObject makeArray(int size)
return the array object in the size specified.
\*************************************************************************/
function makeArray(size) {
this.size = size;
return this;
}

/*************************************************************************\
CardType setCardNumber(cardnumber)
return the CardType object.
\*************************************************************************/
function setCardNumber(cardnumber) {
this.cardnumber = cardnumber;
return this;
}

/*************************************************************************\
CardType setCardType(cardtype)
return the CardType object.
\*************************************************************************/
function setCardType(cardtype) {
this.cardtype = cardtype;
return this;
}

/*************************************************************************\
CardType setExpiryDate(year, month)
return the CardType object.
\*************************************************************************/
function setExpiryDate(year, month) {
this.year = year;
this.month = month;
return this;
}

/*************************************************************************\
CardType setLen(len)
return the CardType object.
\*************************************************************************/
function setLen(len) {
// Create the len array.
if (len.length == 0 || len == null)
len = "13,14,15,16,19";

var tmplen = len;
n = 1;
while (tmplen.indexOf(",") != -1) {
tmplen = tmplen.substring(tmplen.indexOf(",") + 1, tmplen.length);
n++;
}
this.len = new makeArray(n);
n = 0;
while (len.indexOf(",") != -1) {
var tmpstr = len.substring(0, len.indexOf(","));
this.len[n] = tmpstr;
len = len.substring(len.indexOf(",") + 1, len.length);
n++;
}
this.len[n] = len;
return this;
}

/*************************************************************************\
CardType setRules()
return the CardType object.
\*************************************************************************/
function setRules(rules) {
// Create the rules array.
if (rules.length == 0 || rules == null)
rules = "0,1,2,3,4,5,6,7,8,9";

var tmprules = rules;
n = 1;
while (tmprules.indexOf(",") != -1) {
tmprules = tmprules.substring(tmprules.indexOf(",") + 1, tmprules.length);
n++;
}
this.rules = new makeArray(n);
n = 0;
while (rules.indexOf(",") != -1) {
var tmpstr = rules.substring(0, rules.indexOf(","));
this.rules[n] = tmpstr;
rules = rules.substring(rules.indexOf(",") + 1, rules.length);
n++;
}
this.rules[n] = rules;
return this;
}
//  End -->

</SCRIPT></b></DIV></ul></BODY></html>
