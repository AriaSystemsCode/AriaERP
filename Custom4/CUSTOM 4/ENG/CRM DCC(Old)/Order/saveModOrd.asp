<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %> 
<%Response.Buffer = true%>

<%

if session("UseOnlinePayment") = "T" then 'use online payment - wma
	if trim(session("GateWay")) = "PlugNPay.com (MemberShip Managment)" then  'use PNP Membership
		If trim(Session("PO"))="" Then
			'Session("PO")=request("txttemp")
			Session("PO")=Request.QueryString("txttemp")
		End If
	end if					
else	'don't use online payment
	If trim(Session("PO"))="" Then
		Session("PO")=request("txttemp")
	End If
end if

If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 


' To handle the case of the rep.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF%>

<%
IF NOT Session("RSLine").EOF OR NOT Session("RSLine").BOF THEN
	'Variable definition
	Dim strsql ' as string
	Dim strOrder ' as string
	Dim intCount, intOrdWidth ' as int

	'Creat the Connection
	set conne=server.CreateObject("ADODB.connection")
	conne.Open Application("DataConnectionString")
	Dim rsHdrInfo
	Set rsHdrInfo = server.CreateObject("ADODB.recordset")
	'rsHdrInfo.Open "Select Book,Complete,Disc,Bookamt,Open,OpenAmt,cedit_user, cedit_time, dedit_date FROM Ordhdr WHERE ordhdr.cordtype+ordhdr.order = '" & trim(session("Type"))& trim(session("OrdNo")) &  "'",conne,2,4
	strSQL="Select Book,Complete,Disc,Bookamt,Open,OpenAmt,cedit_user, cedit_time, dedit_date,Account,Season FROM Ordhdr WHERE ordhdr.cordtype+ordhdr.order = '" & trim(session("Type"))& trim(session("OrdNo")) &  "'"
	'Response.Write strSQL 
	rsHdrInfo.Open strSQL ,conne,2,4
	dim strUpdateSql
	strUpdateSql = "Update ordhdr set " 
	'Response.Write "<br>"& conne.Version 
	'Response.End 
	Session("RSLine").Sort = "Complete desc"
	rsHdrInfo("Complete") = Session("RSLine")("Complete")'Session("completed")
	strUpdateSql = strUpdateSql & "Complete = {"&  Session("RSLine")("Complete") &"} " 

	if session("UseOnlinePayment") = "T" then 'use online payment - wma
		if trim(session("GateWay")) = "PlugNPay.com (MemberShip Managment)" then  'use PNP Membership
			rsHdrInfo("Disc") = Request.QueryString("txtDisc")'Session("RSCust").fields("disc")
			strUpdateSql = strUpdateSql & ", Disc = "& Request.QueryString("txtDisc")+0 &" " 
		end if					
	else	'don't use online payment
		rsHdrInfo("Disc") = Request.Form ("txtDisc")'Session("RSCust").fields("disc")
		strUpdateSql = strUpdateSql & ", Disc = "& Request.Form ("txtDisc")+0 &" " 	
	end if	
	

	if trim(Session("ordQty"))= "" then
		rsHdrInfo("Book") = 0
		strUpdateSql = strUpdateSql & ", Book = 0 " 
		Session("ordQty") = 0
		strUpdateSql = strUpdateSql & ", ordQty = 0 " 
	else
		rsHdrInfo("Book") = cdbl(Session("ordQty"))
		strUpdateSql = strUpdateSql & ", Book = "& cdbl(Session("ordQty")) &" " 
	end if
	if trim(Session("ordAmount"))= "" then
		rsHdrInfo("Book")= 0
		strUpdateSql = strUpdateSql & ", Book = 0 " 
		Session("ordAmount") = 0
	else
		rsHdrInfo("Bookamt") = Session("ordAmount")
		strUpdateSql = strUpdateSql & ", Bookamt = "& Session("ordAmount") &" " 		
	end if
		
	if trim(Session("ordQty"))= "" then
		rsHdrInfo("Open") = 0
		strUpdateSql = strUpdateSql & ", Open = 0 " 
	else
		rsHdrInfo("Open") = Session("ordQty")
		strUpdateSql = strUpdateSql & ", Open = "& Session("ordQty") &" " 		
	end if

	'Response.Write Session("RSLine")("Season") & "<hr>"
	'Response.Write rsHdrInfo("Season") & "<hr>"
	'Response.End 	
	'wma multiple seasons
	if rsHdrInfo("Season") <> Session("RSLine")("Season") then 'Session("completed")
		rsHdrInfo("Season") = "*"
		strUpdateSql = strUpdateSql & ", Season = '*' " 				
	end if		
	rsHdrInfo("OpenAmt") = cdbl(Session("ordAmount"))
	strUpdateSql = strUpdateSql & ", OpenAmt = "& cdbl(Session("ordQty")) &" " 
	rsHdrInfo("cedit_user") = session("rep")
	strUpdateSql = strUpdateSql & ", cedit_user = '"& session("rep") &"' " 
	rsHdrInfo("cedit_time") = time()
	strUpdateSql = strUpdateSql & ", cedit_time = '"& time() &"' " 
	rsHdrInfo("dedit_date") = date()
	strUpdateSql = strUpdateSql & ", dedit_date = {"& date() &"} "
	strUpdateSql = strUpdateSql & "WHERE ordhdr.cordtype+ordhdr.order = '" & trim(session("Type"))& trim(session("OrdNo")) &  "'  "  

	'Response.Write "<br>"& strUpdateSql
	'Response.End 

	'rsHdrInfo.UpdateBatch ()
	conne.Execute(strUpdateSql)

	Session("RSLine").UpdateBatch()
'	do while not Session("RSLine").eof
'		'Response.Write Session("RSLine").Fields("style")& "<hr>"
'		strSQL = ""
'		for each item in Session("RSLine").Fields
'			Response.Write "[Field]: " & Item.name & " [type]: "& Item.type & " [Value]: " 
'			Response.Write Item.value & "<hr>"
'		next
'		Session("RSLine").movenext
'	loop
'	Response.End 


	'add record in order/charge table[start]
	Dim rsCharges
	set rsCharges = server.CreateObject ("ADODb.Recordset")
	rsCharges.Open "Select * from ordCharg where cOrder='" &trim(session("OrdNo"))& "'",conne,1,3
		rsCharges("nShipAmt") = session("rsCharges")("nShipAmt")'round(Session("ordAmount"),2)
		rsCharges("nCOD") =  session("rsCharges")("nCOD")'Request.Form ("txtCOD")
		rsCharges("nFreight") = session("rsCharges")("nFreight")'Request.Form ("txtFreight")
		If Trim(session("rsCharges")("nInsur")) = "" Then
			rsCharges("nInsur") = 0
		Else
			rsCharges("nInsur") = session("rsCharges")("nInsur")
		End If
		rsCharges("nDiscount") = session("rsCharges")("nDiscount")'Request.Form ("txtDisc")
		rsCharges("ntax_rate") = cdbl(session("rsCharges")("ntax_rate"))/100
		rsCharges("ntax_amt") = session("rsCharges")("ntax_amt")
		rsCharges("ntotChg")  = session("rsCharges")("ntotChg")


		if session("UseOnlinePayment") = "T" then 'use online payment - wma
			if trim(session("GateWay")) = "PlugNPay.com (MemberShip Managment)" then  'use PNP Membership
				'if Request.form("txtDep") = "" then
				if Request.QueryString("txtDep") = "" then
					'intDep = Request.Form ("hidDep")
					Session("intDep") = Request.QueryString("hidDep")+0
				else
					'intDep = Request.Form ("txtDep")
					Session("intDep") = Request.QueryString("txtDep")+0
				end if
			end if
			
		else	'don't use online payment
			if Request.form("txtDep") = "" then
				Session("intDep") = Request.Form ("hidDep")+0
			else
				Session("intDep") = Request.Form ("txtDep")+0
			end if		
		end if
				

		
		'Response.Write Session("intDep")&cdbl(rsCharges("ndeposit"))
		'Response.End 
		'if cdbl(Session("intDep")) > cdbl(rsCharges("ndeposit")) then
		'	rsCharges("ndeposit") = cdbl(Session("intDep"))'session("rsCharges")("ndeposit")
		'else
		''	rsCharges("ndeposit") = cdbl(rsCharges("ndeposit")) + cdbl(Session("intDep"))'session("rsCharges")("ndeposit")
		'end if

		rsCharges("ndeposit") = cdbl(rsCharges("ndeposit")) + cdbl(Session("intDep"))'session("rsCharges")("ndeposit")
		
		'rsCharges("method_pay") = cdbl(Session("intDep"))'session("rsCharges")("ndeposit")
		rsCharges("method_pay") = Session("method_pay")
		
		rsCharges("nweight")  = session("rsCharges")("nweight")
		rsCharges("n_cartons") = session("rsCharges")("n_cartons")
		rsCharges("nprichg")  = session("rsCharges")("nprichg")
		rsCharges("lprichg")  = session("rsCharges")("lprichg")
		rsCharges("ndropchg") = session("rsCharges")("ndropchg")
		rsCharges("ldropchg") = session("rsCharges")("ldropchg")
	
	rsCharges.Update ()
'add record in order/charge table[end]		
		
session("Disc") = ""
session("Discount") = ""
Session("DefTax")  = ""
session("Tax") = ""
Session("Taxable") = ""
Session("getstyle")=""
Session("OrderFlag") = ""
Session("LongDesc1") = ""
Session("LongDesc") = ""
Session("ShortDesc") = ""
Session("text1") = ""
Session("text2") = ""
Session("text3") = ""
Session("text4") = ""
Session("text5") = ""
Session("text6") = ""
Session("text7") = ""
Session("text8") = ""
Session("ordAmount") = ""
Session("ordQty") = ""

%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<Title>CRM - Remote Order</Title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</head>
<body>
<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>
<link REL="stylesheet" HREF="../images/<%=session("Theme")%>/order.css" TYPE="text/css">
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
	<TD colspan=13>
	<%IF Trim(Session("customerid")) <> ""  Then%>
		<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=session("rscust").fields("btname").value%></P>
	<%else%>
		&nbsp
	<%end if%>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>

</TR>
</table>
<%End IF%>

<%
if session("UseOnlinePayment") = "T" then 'use online payment - wma
	if trim(session("GateWay")) = "PlugNPay.com (MemberShip Managment)" then  'use PNP Membership
%>
		<table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
		  <tr><td width="100%" class=MessageFont>Your order has been modified!</td></tr>  
		    <% if Session("PNPCase")= "DepositAdd" and Session("PNPFinalStatus") = "success" and Session("PNPcard-amount")<>0 then  %>
		    	<script language="javascript">
				window.alert("You have been Charged $ <%=Session("PNPcard-amount")%>")
				</script>	
		    <% elseif (Session("PNPCase")= "DepositRefund" or Session("PNPCase")="DepositCancel") and Session("PNPFinalStatus") = "success" and Session("PNPcard-amount")<>0  then  %>
		    	<script language="javascript">
				window.alert("You have been Refunded $ <%=Session("PNPcard-amount")%>")
				</script>	      
		    <%end if%>
		    <%Session("PNPcard-amount")=0 %>
		  </tr>
		</table>
<%
	end if 
else 'don't use online payment
%>	
	<table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
	  <tr><td width="100%" class=MessageFont>Your order has been modified!</td></tr>  
	</table>		
<%end if%>		
  
</body>
</html>
<%
'Response.Write strAddMemo &"<br>"&Session("Division")
ELSE
	Response.Redirect("modifyorder.asp")
End if

%>
