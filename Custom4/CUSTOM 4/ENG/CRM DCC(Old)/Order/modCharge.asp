<%@ Language=VBScript %>
<%

Response.CacheControl  = "no-cache"

Response.AddHeader  "Pragma", "no-cache"

Response.Expires = -1
Response.Buffer = true

%>

<%If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" and trim(Request.QueryString ("Save")) = "" Then%>
	<script language="javascript">
	alert('Session Expired!');
	window.close ();
	</script>
<%elseIf Trim(Session("ID")) = "" and Trim(Session("rep"))= "" then%>
	<script language="javascript">
	parent.location.href ="../login.asp"
	</script>
<%End if%>
<%IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
	strSalesRep = Ucase(Session("RSCust").fields("salesrep"))
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	'IF Trim(Session("customerid")) = "" Then
	'	Response.Redirect("../repcust.asp")
	'END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
	strSalesRep = Ucase(Trim(Session("rep")))
End IF
'setup var to the metohd of payment
Session("COD") = "F"
%>
<%
'charge calculations [start]
Set conn = server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
Set cnnFox = server.CreateObject ("ADODB.connection")
cnnFox.open Application("SystemConnectionString")

'get comp date
if not isobject(Session("RSLine")) or Session("RSLine") is nothing then%>
	<script language="javascript">
	parent.location.href ="../login.asp"
	</script>
<%
end if
Session("RSLine").Sort = "Complete desc"
dtComp = Session("RSLine")("Complete")'Session("completed")
'get order charge info saved

'WMA 
IF Trim(Session("customerid")) <> ""  Then
	'You have ready Customer Session
	set rsTempCust = Session("RSCust")
else'generate new customer recordset
	'get customer recordset in case select all customers
	strSQL = "select * from customer where account = '"& Session("tempCustomer") &"'"
'	Response.Write strSQL
	'Response.End 
	set rsTempCust = server.CreateObject ("ADODb.Recordset")
	rsTempCust.Open strSQL, conn, 1,3
end if

set rsCharges = server.CreateObject ("ADODb.Recordset")
rsCharges.Open "Select * from ordCharg where cOrder='" &trim(session("OrdNo"))& "'",conn,1,3	
if not 	rsCharges.EOF then	
	intPay = rsCharges("ndeposit")
else
	intPay =0
end if
'1- freight calc
'get ship to customer zip code
'strSplit = split(Session("Add4"),", ")
strShipTo = mid(trim(Session("Add43")),1,3)'mid(strSplit(2),1,3)
'get ship from code
dim rsLoc
set rsLoc = server.CreateObject ("ADODB.Recordset")
strWareHous = Session("WareHous")
rsLoc.Open "select * from warehous where cwarecode like '" &trim(strWareHous)& "%'",conn

if trim(rsLoc("Ups")) = "" then
	strShipFrom = mid(rsTempCust("Caddress5"),1,3)
else
	strShipFrom = rsLoc("UPS")
end if
'check if company uses taxes or not
dim rsSetup
set rsSetup = server.CreateObject ("ADODB.Recordset")
strVal = "M_TAX"&space(5)
rsSetup.Open "select mdata_def, cfld_name from setups where cfld_name like '" & strVal & "' or cfld_name like 'M_TAX_METH%'",conn

rsSetup.Filter = "cfld_name like '" & strVal & "'"
lTax = rsSetup("mdata_def")

rsSetup.Filter = ""
rsSetup.Filter = "cfld_name='M_TAX_METH'"
strMethod = rsSetup("mdata_def")
rsSetup.Filter = ""

'get tax rate ship to address[start]
strCustZip = trim(Session("Add43"))'trim(strSplit(2))'&space(30-len(trim(Session("RSCust").fields("caddress5"))))
strState = trim(Session("Add42"))'trim(strSplit(1))'&space(30-len(trim(Session("RSCust").fields("caddress4"))))
strCity  = trim(Session("Add41"))'trim(strSplit(0))
strCountry = trim(Session("Add5"))
'check if there is already saved taxes
if trim(session("Tax"))  = "" then
	Dim rsTax
	set rsTax = server.CreateObject ("ADODB.Recordset")

	Response.Write  rsCharges("ntax_rate")
	if len(rsCharges("ntax_rate")) = 0 then
		rsTax.Open "Select ntotsaltax from taxes where cstat+czipcode like '"& strState &"%"& strCustZip &"%' ",conn

		if rsTax.EOF and rsTax.BOF then
			strTax = 0
			Session("DefTax") = 0
		else
			strTax = rsTax("ntotsaltax")
			Session("DefTax") = rsTax("ntotsaltax")
		end if
	else
		strTax = cdbl(rsCharges("ntax_rate"))
		Session("DefTax") = cdbl(rsCharges("ntax_rate"))
	end if 
else
	strTax = trim(session("Tax"))/100
	Session("DefTax") = trim(session("Tax"))/100
end if
'get tax rate ship to address[end]
'get shipType code
strShipType = rsTempCust.fields("shipvia")
'get def values for other charges[start]
Dim rsShipCode
set rsShipCode = server.CreateObject ("ADODB.Recordset")
rsShipCode.Open "select crltd_vlu ,crltd_nam from codes where cfld_name='SHIPVIA' and ccode_no='"& strShipType &"' and (crltd_nam = 'NCODCHARGE' or crltd_nam = 'NINSCHARGE' or crltd_nam = 'CUPS')",conn
if not rsShipCode.EOF then
	rsShipCode.Filter = "crltd_nam = 'CUPS'"
	strShip = rsShipCode("crltd_vlu")
	rsShipCode.Filter = ""
end if
'calc frieght[start]
dim rsZone
set rsZone = server.CreateObject ("ADODB.Recordset")
dim rsRate
set rsRate = server.CreateObject ("ADODB.Recordset")
rsZone.Open "Select zone from syszones where shiptype+shipfrom+shipto like '" & trim(strShip) & "%" & trim(strShipFrom) & "%" & trim(strShipTo) &"%'",cnnFox
'Response.Write "Select zone from syszones where shiptype+shipfrom+shipto like '" & trim(strShip) & "%" & trim(strShipFrom) & "%" & trim(strShipTo) &"%'"
'Response.End 
'calc frieght[end]
'check if i'm just submitting to calc disc amount
if trim(Request.QueryString ("type")) = "" then
	
	'get weigtht/carton for each style
	dim rsStyle
	set rsstyle = server.CreateObject ("ADODB.Recordset")
	Session("RSLine").sort = "style"
	Session("RSLine").movefirst
	strStyle = Session("RSLine")("style")'get first style
	Session("RSLine").movefirst
	'get first style info
	'Response.Write 
	rsStyle.Open "select * from style where style like '"& trim(strStyle) &"%' ",conn
	intWght  = rsStyle("nstyweight")
	intQty   = rsStyle("qty_ctn")
	lTaxable = rsStyle("ltaxable")
	intTotQty  = 0
	intTotAmt = 0
	intTotWght = 0
	intTotCtn = 0
	intTotalCtn = 0
	intNetAmt = 0
	intTaxAmt = 0
	intTotTax = 0
	intFreightAmt = 0
	intInsurAmt= 0
	intCODAmt = 0
	'intDisc = 0
	'Response.End 
	do while not Session("RSLine").eof
	'Response.Write strStyle&"<br>"
		'check if its the same style
		if strStyle = Session("RSLine")("style") then
			'sum up tot qty for that style
			intTotQty = cdbl(intTotQty)+cdbl(Session("RSLine")("totqty"))
			intTotAmt = cdbl(intTotAmt)+(cdbl(Session("RSLine")("totqty")) * cdbl(Session("RSLine")("price")))
		else
			'get style info
			intTotWght = cdbl(intTotWght) + (cdbl(intWght)*cdbl(intTotQty))
			intWgtCtn  = (cdbl(intWght)*cdbl(intQty))
			intTotCtn  = 0
			if int(intWgtCtn) = cdbl(intWgtCtn) then
				intWgtCtn = intWgtCtn
			else
				intWgtCtn = cint(intWgtCtn) + 1
			end if
			if cdbl(intQty) <> 0 then
				
				intNo = cdbl(intTotQty)/cdbl(intQty)
				if int(intNo) = cdbl(intNo) then
					intTotCtn = intTotCtn + int(intNo)
				else
					intTotCtn = cint(intTotCtn) + (int(intNo)+1)
				end if
			end if
			
			'calc freight/style
			if not rsZone.EOF then
				strShip = trim(strShip)&space(7 - len(trim(strShip)))
				strZone = trim(rsZone("zone"))&space(3 - len(trim(rsZone("zone"))))
				intWgtCtn = space(6 - len(trim(intWgtCtn)))&trim(intWgtCtn)
				rsRate.Open "select cfrtrate from sysrates where shiptype+zone+cshpweight = '" & strShip &  strZone & intWgtCtn &"' ",cnnFox
				
				if not rsRate.EOF then
					intFreightAmt = intFreightAmt + (cdbl(rsRate("cfrtrate")) * cint(intTotCtn))
				end if
				rsRate.Close ()
			end if
			'check if style is taxable then get amount
			if lTaxable = "True" then
				intTaxAmt = intTaxAmt + cdbl(intTotAmt)
				Session("Taxable") = "T"
			end if
			strStyle = Session("RSLine")("style")
			rsStyle.Close()
			rsStyle.Open "select * from style where style like '"& trim(strStyle) &"%' ",conn
			intWght = rsStyle("nstyweight")
			intQty  = rsStyle("qty_ctn")
			lTaxable  = rsStyle("ltaxable")
			intTotQty = Session("RSLine")("totqty")
			intTotAmt = cdbl(Session("RSLine")("totqty")) * cdbl(Session("RSLine")("price"))
			intTotalCtn = intTotalCtn + intTotCtn
		end if
		intNetAmt = intNetAmt + (cdbl(Session("RSLine")("totqty")) * cdbl(Session("RSLine")("price")))
		
	Session("RSLine").movenext
	loop
	'get info for last style[start]
	'Response.Write "<font size=2>"&intTotalCtn&"<br>"
	Session("RSLine").filter = "style like '" &strStyle& "'"
	
	rsStyle.Close()
	rsStyle.Open "select * from style where style like '"& trim(strStyle) &"%' ",conn
	intWght = rsStyle("nstyweight")
	intQty  = rsStyle("qty_ctn")
	
	lTaxable  = rsStyle("ltaxable")
	intTotQty = 0
	intTotAmt = 0
	intTotCtn = 0
	do while not Session("RSLine").eof
	
		intTotQty = cdbl(intTotQty)+cdbl(Session("RSLine")("totqty"))
		intTotAmt = cdbl(intTotAmt)+(cdbl(Session("RSLine")("totqty")) * cdbl(Session("RSLine")("price")))
		
	Session("RSLine").movenext
	loop
	intTotWght = cdbl(intTotWght) + (cdbl(intWght)*cdbl(intTotQty))
	intWgtCtn  = (cdbl(intWght)*cdbl(intQty))
	if int(intWgtCtn) = cdbl(intWgtCtn) then
		intWgtCtn = intWgtCtn
	else
		intWgtCtn = cint(intWgtCtn) + 1
	end if
	if cdbl(intQty) <> 0 then
		intNo = cdbl(intTotQty)/cdbl(intQty)
		if int(intNo) = cdbl(intNo) then
			intTotCtn = intTotCtn + int(intNo)
		else
			intTotCtn = cint(intTotCtn) + (int(intNo)+1)
		end if
	end if
	'calc freight/style
	if not rsZone.EOF then
		strShip = trim(strShip)&space(7 - len(trim(strShip)))
		strZone = trim(rsZone("zone"))&space(3 - len(trim(rsZone("zone"))))
		intWgtCtn = space(6 - len(trim(intWgtCtn)))&trim(intWgtCtn)
		rsRate.Open "select cfrtrate from sysrates where shiptype+zone+cshpweight like '" & strShip &  strZone & intWgtCtn &"' ",cnnFox

		if not rsRate.EOF then
			intFreightAmt = intFreightAmt + (cdbl(rsRate("cfrtrate")) * cint(intTotCtn))
		end if
		rsRate.Close ()
	end if
	'Response.Write intTotQty&"__"&intFreightAmt&"<br>"
	'Response.End 
	'check if style is taxable then get amount
	if lTaxable = "True" then
		intTaxAmt = intTaxAmt + cdbl(intTotAmt)
		Session("Taxable") = "T"
	end if
	
	Session("RSLine").filter = ""
	intTotalCtn = intTotalCtn + intTotCtn

	'get info for last style[end]
	
	if not rsShipCode.EOF then
	'COD amount
		'check to add COD amount or not
		if Session("COD") = "T" then
			rsShipCode.Filter = "crltd_nam = 'NCODCHARGE'"
			intCODAmt = cdbl(rsShipCode("crltd_vlu")) * intTotalCtn
			rsShipCode.Filter = ""
		else
			intCODAmt = 0
		end if
	'INSUR amount
		rsShipCode.Filter = "crltd_nam = 'NINSCHARGE'"
		'wma
		if rsShipCode.EOF then
			intInsurAmt = 0 
		else 
			intInsurAmt = cdbl(rsShipCode("crltd_vlu")) * int(intNetAmt/100)		
		end if
		rsShipCode.Filter = ""
	end if
	
	'get def values for other charges[end]
	'check if there is discount
	if trim(session("Discount")) <> "" then
		intDisc = trim(session("Discount"))
		intDiscVal = trim(session("Discount"))
	else
		
		intDisc = cdbl(rsCharges("nDiscount"))
		intDiscVal = cdbl(rsCharges("nDiscount"))
	
	end if
	'check if to calc tax and of what type (A/M)
	'check if there is a drop ship charge
	if session("AddShip") <> "" or session("ShipChg") = true then
		intDropChg = Session("DropChg")
	else
		intDropChg = cdbl(rsCharges("ndropchg"))
	end if
	'check if there is a priority ship charge
	if session("AddPri") <> "" then
		intPriChg = Session("PriChg")
	else
		intPriChg = cdbl(rsCharges("nPrichg"))
	end if
	if lTax = "Y" and Session("Taxable") = "T" then
		if cdbl(intTaxAmt) > 0 then
			intDiscAmt = cdbl(intTaxAmt) * cdbl(intDisc/100)
			'check tax method
			if trim(strMethod) = "A" then'calc on totamount wz charges
				
				intTotTax = (cdbl(intTaxAmt)-cdbl(intDiscAmt)+cdbl(intCODAmt)+cdbl(intInsurAmt)+ cdbl(intFreightAmt) + cdbl(intDropChg) + cdbl(intPriChg)) * cdbl(strTax)
				
			else'on style amount only
				intTotTax = (cdbl(intTaxAmt)-cdbl(intDiscAmt)) * cdbl(strTax)
			end if
		else
			intTotTax = 0
		end if
	end if
	
	intTotDisc = cdbl(intNetAmt) * cdbl(intDisc/100)
	intTotChg  = round(intNetAmt,2) - round(intTotDisc,2) + round(intTotTax,2) + round(intCODAmt,2) + round(intInsurAmt,2) + round(intFreightAmt,2)+ cdbl(intDropChg) + cdbl(intPriChg)
	
	
	'wma use deposit perc.
	'intDep = round(intTotChg,2)* 0.5 - cdbl(intPay)
	intDep = round(intTotChg,2)* (Session("DepositPercent")/100) - cdbl(intPay)
	'if cdbl(intDep) <= 0 then
	'	intDep = 0
	'end if
	
	'get charges record
	set Session("rsCharges") = server.CreateObject ("ADODb.Recordset")
	Session("rsCharges").Open "Select * from ordCharg where corder='" &trim(session("OrdNo"))& "'",conn,1,3
		
	Session("rsCharges")("nShipAmt") = Session("ordAmount")
	Session("rsCharges")("nCOD") = round(intCODAmt,2) 'Request.Form ("txtCOD")
	Session("rsCharges")("nFreight") = round(intFreightAmt,2)'Request.Form ("txtFreight")
	If Trim(intInsurAmt) = "" Then
		Session("rsCharges")("nInsur") =0
	Else
		Session("rsCharges")("nInsur") = round(intInsurAmt,2)
	End If
	Session("rsCharges")("nDiscount") = round(intDisc,2)'Request.Form ("txtDisc")
	Session("rsCharges")("ntax_rate") = FormatNumber(cdbl(strTax)*100,3)'Request.Form ("txtTax")
	Session("rsCharges")("ntax_amt")  = round(intTotTax,2)'cdbl(Request.Form ("txtTaxAmt"))
	Session("rsCharges")("ntotChg")   = round(intTotChg,2)' cdbl(Request.Form ("txtTotChg"))
	Session("rsCharges")("ndeposit")  = round(intDep,2)'cdbl(Request.Form ("txtDep"))
	Session("rsCharges")("nweight")   = cdbl(intTotWght)
	Session("rsCharges")("n_cartons")  = intTotalCtn'Request.Form ("txtTotCtns")
	if intDropChg = 0 then
		Session("rsCharges")("ndropchg") = 0
		Session("rsCharges")("ldropchg") = 0
		session("AddShip") = ""
	else
		Session("rsCharges")("ndropchg") = round(intDropChg,2)
		Session("rsCharges")("ldropchg") = 1
		Session("DropChg") = round(intDropChg,2)
		session("AddShip") = 1
	end if
	Session("chkDrop")  = cdbl(intDropChg)
	if intPriChg = 0 then
		Session("rsCharges")("nPrichg") = 0
		Session("rsCharges")("lPrichg") = 0
		session("AddPri") = ""
	else
		Session("rsCharges")("nPrichg") = round(intPriChg,2)
		Session("rsCharges")("lPrichg") = 1
		Session("PriChg") = round(intPriChg,2)
		session("AddPri") = 1
	end if
	Session("Discount") = round(intDisc,2)'Request.Form ("txtDisc")
	Session("Tax") = FormatNumber(cdbl(strTax)*100,3)'Request.Form ("txtTax")
			
	'charge calculations [end]
else'calc disc amount or tax 
	intNetAmt = Request.QueryString ("netChg")
	intInsurAmt = Request.QueryString ("InsChg")
	intCODAmt = Request.QueryString ("CODChg")
	intFreightAmt = Request.QueryString ("FreightChg")
	intTaxAmt   = Request.QueryString ("taxAmt")
	intTotWght  = Request.QueryString ("Wght")
	intTotalCtn = Request.QueryString ("noCtn")
	intTotTax   =  Request.QueryString ("totTax")
	intDisc = Request.Form ("txtDisc")
	intDiscVal = Request.Form ("txtDisc")
	if Request.QueryString ("Type") = "D" then'calc disc
		intTotDisc = cdbl(Request.Form ("txtDisc"))
		intDisc = cdbl(intNetAmt) * cdbl((cdbl(Request.Form ("txtDisc"))/100))
		intTotChg = cdbl(Request.QueryString ("totChg")) - cdbl(intDisc)
		if lTax = "Y" and cdbl(intTaxAmt) > 0 then
			strTax = cdbl(Request.Form ("txtTax")/100)
			intDiscAmt = cdbl(intTaxAmt) * cdbl((cdbl(Request.QueryString ("val"))/100))
			'check tax method
			if trim(strMethod) = "A" then'calc on totamount wz charges
				
				intTotTax  = (cdbl(intTaxAmt)-cdbl(intDiscAmt)+cdbl(intCODAmt)+cdbl(intInsurAmt)+ cdbl(intFreightAmt) + cdbl(Session("chkDrop")) +cdbl(Session("chkPri"))) * cdbl(strTax)
			else'on style amount only
				intTotTax = (cdbl(intTaxAmt)-cdbl(intDiscAmt)) * cdbl(strTax)
			end if
		end if
		intTotChg  = round(intNetAmt,2) - round(intDisc,2) + round(intTotTax,2) + round(intCODAmt,2) + round(intInsurAmt,2) + round(intFreightAmt,2)+ round(Session("chkDrop"),2) +round(Session("chkPri"),2)
		

		
		'wma use deposit perc.
		'intDep = round(intTotChg,2)* 0.5 - cdbl(intPay)
		intDep = round(intTotChg,2)* (Session("DepositPercent")/100) - cdbl(intPay)
		'if cdbl(intDep) <= 0 then
		'	intDep = 0
		'end if
			if isobject(Session("rsCharges")) then
			Session("rsCharges")("cOrder") = strOrder
			Session("rsCharges")("nShipAmt") = Session("ordAmount")
			Session("rsCharges")("nCOD") = round(intCODAmt,2) 'Request.Form ("txtCOD")
			Session("rsCharges")("nFreight") = round(intFreightAmt,2)'Request.Form ("txtFreight")
			If Trim(intInsurAmt) = "" Then
				Session("rsCharges")("nInsur")= 0
			Else
				Session("rsCharges")("nInsur")= round(intInsurAmt,2)
			End If
			Session("rsCharges")("nDiscount") = round(intTotDisc,2)'Request.Form ("txtDisc")
			Session("rsCharges")("ntax_rate") = FormatNumber(cdbl(strTax)*100,3)'Request.Form ("txtTax")
			Session("rsCharges")("ntax_amt")  = round(intTotTax,2)'cdbl(Request.Form ("txtTaxAmt"))
			Session("rsCharges")("ntotChg")   = round(intTotChg,2)' cdbl(Request.Form ("txtTotChg"))
			Session("rsCharges")("ndeposit")  = round(intDep,2)'cdbl(Request.Form ("txtDep"))
			Session("rsCharges")("nweight")   = cdbl(intTotWght)
			Session("rsCharges")("n_cartons")  = intTotalCtn'Request.Form ("txtTotCtns")
			Session("Discount") = round(intTotDisc,2)'Request.Form ("txtDisc")
			Session("Tax") = FormatNumber(cdbl(strTax)*100,3)'Request.Form ("txtTax")
		end if
		'intTotChg = cdbl(intNetAmt) + cdbl(intTotTax) + cdbl(intCODAmt) + cdbl(intInsurAmt) + cdbl(intFreightAmt) - cdbl(intDisc)
	elseif Request.QueryString ("Type") = "P" then'add priority charge
		'check if i'm adding the value or cancelling
		if trim(Request.QueryString ("val")) = "" then
			'intPriChg = -cdbl(Sesion("PriChg"))
			intAdd = 0
		else
			'intPriChg = cdbl(Request.QueryString ("val"))
			intAdd = cdbl(Request.QueryString ("val"))
		end if
		if lTax = "Y" and cdbl(intTaxAmt) > 0 then
			strTax = cdbl(Request.Form ("txtTax")/100)
			intDiscAmt = cdbl(intTaxAmt) * cdbl(intDisc/100)
			'check tax method
			if trim(strMethod) = "A" then'calc on totamount wz charges
				
				intTotTax  = (cdbl(intTaxAmt)-cdbl(intDiscAmt)+cdbl(intCODAmt)+cdbl(intInsurAmt)+ cdbl(intFreightAmt)+ cdbl(intAdd)+ cdbl(Session("chkDrop"))) * cdbl(strTax)
			else'on style amount only
				intTotTax = (cdbl(intTaxAmt)-cdbl(intDiscAmt)) * cdbl(strTax)
			end if
		end if
		inttotDisc = cdbl(intNetAmt) * cdbl(intDisc/100)
		intTotChg  = round(intNetAmt,2) - round(inttotDisc,2) + round(intTotTax,2) + round(intCODAmt,2) + round(intInsurAmt,2) + round(intFreightAmt,2)+ round(Session("chkDrop"),2)+ round(intAdd,2)
		
		'wma use deposit perc.
		'intDep = round(intTotChg,2)* 0.5 - cdbl(intPay)
		intDep = round(intTotChg,2)* (Session("DepositPercent")/100) - cdbl(intPay)

	
		'if cdbl(intDep) <= 0 then
		'	intDep = 0
		'end if
		if isobject(Session("rsCharges")) then
			Session("rsCharges")("cOrder") = strOrder
			Session("rsCharges")("nShipAmt") = Session("ordAmount")
			Session("rsCharges")("nCOD") = round(intCODAmt,2) 'Request.Form ("txtCOD")
			Session("rsCharges")("nFreight") = round(intFreightAmt,2)'Request.Form ("txtFreight")
			If Trim(intInsurAmt) = "" Then
				Session("rsCharges")("nInsur") =0
			Else
				Session("rsCharges")("nInsur") = round(intInsurAmt,2)
			End If
			Session("rsCharges")("nDiscount") = Request.Form ("txtDisc")
			Session("rsCharges")("ntax_rate") = FormatNumber(cdbl(strTax)*100,3)'Request.Form ("txtTax")
			Session("rsCharges")("ntax_amt")  = round(intTotTax,2)'cdbl(Request.Form ("txtTaxAmt"))
			Session("rsCharges")("ntotChg")   = round(intTotChg,2)' cdbl(Request.Form ("txtTotChg"))
			Session("rsCharges")("ndeposit")  = round(intDep,2)'cdbl(Request.Form ("txtDep"))
			Session("rsCharges")("nweight")   = cdbl(intTotWght)
			Session("rsCharges")("n_cartons")  = intTotalCtn'Request.Form ("txtTotCtns")
			if intAdd = 0 then
				Session("rsCharges")("nPrichg") = 0
				Session("rsCharges")("lPrichg") = 0
				session("AddPri") = ""
			else
				Session("rsCharges")("nPrichg") = round(intAdd,2)
				Session("rsCharges")("lPrichg") = 1
				Session("PriChg") = round(intAdd,2)
				session("AddPri") = 1
			end if
			Session("chkPri")   = cdbl(intAdd)
			Session("Discount") = round(intDisc,2)'Request.Form ("txtDisc")
			Session("Tax") = FormatNumber(cdbl(strTax)*100,3)'Request.Form ("txtTax")
		end if	
	elseif Request.QueryString ("Type") = "S" then'add drop ship charge
		'check if i'm adding the value or cancelling
		if trim(Request.QueryString ("val")) = "" then
			'intDropChg = -cdbl(Session("DropChg"))
			intAdd = 0
		else
			'intDropChg = cdbl(Request.QueryString ("val"))
			intAdd = cdbl(Request.QueryString ("val"))
		end if
		if lTax = "Y" and cdbl(intTaxAmt) > 0 then
			strTax = cdbl(Request.Form ("txtTax")/100)
			intDiscAmt = cdbl(intTaxAmt) * cdbl(intDisc/100)
			'check tax method
			if trim(strMethod) = "A" then'calc on totamount wz charges
				
				intTotTax  = (cdbl(intTaxAmt)-cdbl(intDiscAmt)+cdbl(intCODAmt)+cdbl(intInsurAmt)+ cdbl(intFreightAmt)+ cdbl(intAdd)+cdbl(Session("chkPri"))) * cdbl(strTax)
			else'on style amount only
				intTotTax = (cdbl(intTaxAmt)-cdbl(intDiscAmt)) * cdbl(strTax)
			end if
		end if
		inttotDisc = cdbl(intNetAmt) * cdbl(intDisc/100)
		intTotChg  = round(intNetAmt,2) - round(inttotDisc,2) + round(intTotTax,2) + round(intCODAmt,2) + round(intInsurAmt,2) + round(intFreightAmt,2)+ round(intAdd,2)+round(Session("chkPri"),2)
		
		
		'wma use deposit perc.
		'intDep = round(intTotChg,2)* 0.5 - cdbl(intPay)
		intDep = round(intTotChg,2)* (Session("DepositPercent")/100) - cdbl(intPay)

		'if cdbl(intDep) <= 0 then
		'	intDep = 0
		'end if
		if isobject(Session("rsCharges")) then
			Session("rsCharges")("cOrder") = strOrder
			Session("rsCharges")("nShipAmt") = Session("ordAmount")
			Session("rsCharges")("nCOD") = round(intCODAmt,2) 'Request.Form ("txtCOD")
			Session("rsCharges")("nFreight") = round(intFreightAmt,2)'Request.Form ("txtFreight")
			If Trim(intInsurAmt) = "" Then
				Session("rsCharges")("nInsur") =0
			Else
				Session("rsCharges")("nInsur") = round(intInsurAmt,2)
			End If
			Session("rsCharges")("nDiscount") = Request.Form ("txtDisc")
			Session("rsCharges")("ntax_rate") = FormatNumber(cdbl(strTax)*100,3)'Request.Form ("txtTax")
			Session("rsCharges")("ntax_amt")  = round(intTotTax,2)'cdbl(Request.Form ("txtTaxAmt"))
			Session("rsCharges")("ntotChg")   = round(intTotChg,2)' cdbl(Request.Form ("txtTotChg"))
			Session("rsCharges")("ndeposit")  = round(intDep,2)'cdbl(Request.Form ("txtDep"))
			Session("rsCharges")("nweight")   = cdbl(intTotWght)
			Session("rsCharges")("n_cartons")  = intTotalCtn'Request.Form ("txtTotCtns")
			if intAdd = 0 then
				Session("rsCharges")("ndropchg") = 0
				Session("rsCharges")("ldropchg") = 0
				session("AddShip") = ""
			else
				Session("rsCharges")("ndropchg") = round(intAdd,2)
				Session("rsCharges")("ldropchg") = 1
				Session("DropChg") = round(intAdd,2)
				session("AddShip") = 1
			end if
			Session("chkDrop")  = cdbl(intAdd)
			Session("Discount") = round(intDisc,2)'Request.Form ("txtDisc")
			Session("Tax") = FormatNumber(cdbl(strTax)*100,3)'Request.Form ("txtTax")
		end if	
	else'calc tax
		inttotDisc = cdbl(intNetAmt) * cdbl(intDisc/100)

		if trim(Request.QueryString ("val")) = "" then
			strTax = 0
		else
			strTax = cdbl(Request.Form ("txtTax")/100)'cdbl(Request.QueryString ("val")/100)
		end if
		if lTax = "Y" and cdbl(intTaxAmt) > 0 then
			intDiscAmt = cdbl(intTaxAmt) * cdbl(intDisc/100)
			'strTax = cdbl(Request.QueryString ("val")/100)
			'check tax method
			if trim(strMethod) = "A" then'calc on totamount wz charges
				
				intTotTax  = (cdbl(intTaxAmt)-cdbl(intDiscAmt)+cdbl(intCODAmt)+cdbl(intInsurAmt)+ cdbl(intFreightAmt)+ cdbl(Session("chkDrop")) +cdbl(Session("chkPri"))) * cdbl(strTax)
			else'on style amount only
				intTotTax = (cdbl(intTaxAmt)-cdbl(intDiscAmt)) * cdbl(strTax)
			end if
		end if
		intTotChg  = round(intNetAmt,2) - round(inttotDisc,2) + round(intTotTax,2) + round(intCODAmt,2) + round(intInsurAmt,2) + round(intFreightAmt,2)+ round(Session("chkDrop"),2) +round(Session("chkPri"),2)
		 
		'wma use deposit perc.
		'intDep = round(intTotChg,2)* 0.5 - cdbl(intPay)
		intDep = round(intTotChg,2)* (Session("DepositPercent")/100) - cdbl(intPay)

		'if cdbl(intDep) <= 0 then
		'	intDep = 0
		'end if
		if isobject(Session("rsCharges")) then
			Session("rsCharges")("cOrder") = strOrder
			Session("rsCharges")("nShipAmt") = Session("ordAmount")
			Session("rsCharges")("nCOD") = round(intCODAmt,2) 'Request.Form ("txtCOD")
			Session("rsCharges")("nFreight") = round(intFreightAmt,2)'Request.Form ("txtFreight")
			If Trim(intInsurAmt) = "" Then
				Session("rsCharges")("nInsur") =0
			Else
				Session("rsCharges")("nInsur") = round(intInsurAmt,2)
			End If
			Session("rsCharges")("nDiscount") = round(intDisc,2)'Request.Form ("txtDisc")
			Session("rsCharges")("ntax_rate") = FormatNumber(cdbl(strTax)*100,3)'Request.Form ("txtTax")
			Session("rsCharges")("ntax_amt")  = round(intTotTax,2)'cdbl(Request.Form ("txtTaxAmt"))
			Session("rsCharges")("ntotChg")   = round(intTotChg,2)' cdbl(Request.Form ("txtTotChg"))
			Session("rsCharges")("ndeposit")  = round(intDep,2)'cdbl(Request.Form ("txtDep"))
			Session("rsCharges")("nweight")   = cdbl(intTotWght)
			Session("rsCharges")("n_cartons")  = intTotalCtn'Request.Form ("txtTotCtns")
			Session("Discount") = round(intDisc,2)'Request.Form ("txtDisc")
			Session("Tax") = FormatNumber(cdbl(strTax)*100,3)'Request.Form ("txtTax")
		end if
		'intTotChg = cdbl(intNetAmt) + cdbl(intTotTax) + cdbl(intCODAmt) + cdbl(intInsurAmt) + cdbl(intFreightAmt)
		
	end if
	
end if
Session("RSLine").MoveFirst()
if trim(Request.QueryString ("add")) <> "" then
	
%>
		<script language="javascript">
			window.location.href = 'modifyorder.asp?come=U';
		</script>
<%
end if
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<Title>CRM - Remote Order</Title>


</head>
<body >
<SCRIPT LANGUAGE=javascript>
<!--


function doSubmit(strSubmitTo,nValue,type)
{
	if (strSubmitTo=="SAVE")
	{
		//check deposit value	
		/*depVal = document.frmChg.txtDep.value;
		var re;
		re = /,/g;
		depVal = depVal.replace(re,'');
		depVal = parseFloat(depVal);
		//alert(isNaN(depVal));	
		//if (isNaN(document.frmChg.txtDep.value))
		if (isNaN(depVal))
		{
			alert("Please enter numbers only!");
			document.frmChg.txtDep.focus();
			return false;
		}
		if (document.frmChg.txtDep.value != '')
		{
			if (document.frmChg.txtDep.value > <%=intTotChg%>)
			{
				alert("Unexcepted deposit amount;Amount must be less then full amount!");
				return false;
			}
			/*if (document.frmChg.txtDep.value < <%=intTotChg*0.5%>)
			{
				alert("Unexcepted deposit amount;Amount must be more than 50%!");
				return false;
			}*/
		//}
				//wma pnp credit card payment interface [Start]
		//document.frmChg.action="saveord.asp";
		/*if (document.all("card-amount").value != 0)
		{
	    	//alert(document.all("card-amount").value);
	    	//alert(document.all("pnpcustomerid").value);
	    	alert("You will be redirect to pay your deposit amount.");
	    	document.frmChg.target = "_top"
			document.frmChg.action="https://pay1.plugnpay.com/payment/billblasspay.cgi";							
		}
		else
		{*/
		
		
		<%if session("UseOnlinePayment") = "T" then %>  //use online payment
			<%if trim(session("GateWay")) = "PlugNPay.com (MemberShip Managment)" then %> //use PNP Membership
				//wma pnp credit card payment interface [Start]
				//document.frmChg.action="saveModOrd.asp";
				//document.frmChg.submit ();
				document.all("card-amount").value = document.frmChg.txtDep.value
				if (parseFloat(document.frmChg.txtDep.value) == 0.00)
				{
					document.frmChg.action="saveModOrd.asp";
				}
				// Deposit Adding Amount
				if (parseFloat(document.all("card-amount").value) > 0)
				{
					document.frmChg.target = "_top"
					document.frmChg.action="../PnpCom/paymentform.asp?PNPCase=DepositAdd";														
				}
				// Deposit Refund
				if (parseFloat(document.all("card-amount").value) < 0)
				{
					document.frmChg.target = "_top"
					document.frmChg.action="../PnpCom/paymentform.asp?PNPCase=DepositRefund";														
				}
				document.frmChg.submit ();
				//wma pnp credit card payment interface [End]
			<%end if %>
			
		<%else %> //not use online payment
			document.frmChg.action="saveModOrd.asp";
			document.frmChg.submit ();
		<%end if%>
		
	}
	else
	{
		document.frmChg.action = 'modCharge.asp?save=<%=trim(Request.QueryString ("Save"))%>&type='+type+'&val='+nValue+'&taxAmt=<%=intTaxAmt%>&totTax=<%=intTotTax%>&totChg=<%=intTotChg%>&netChg=<%=intNetAmt%>&InsChg=<%=intInsurAmt%>&CODChg=<%=intCODAmt%>&FreightChg=<%=intFreightAmt%>&wght=<%=intTotWght%>&noCtn=<%=intTotalCtn%>';
		document.frmChg.submit ();
	}
}

function HandleEnterKey(nValue,type)

{
	if(window.event.keyCode==13)
	{
		if (ValidatePercent(nValue,type))
		{
			doSubmit("UPDATE",nValue,type)
			return false;
		}
		else
		{
			return false;
		}
	}
	return true;
}

function ValidatePercent(nValue,type)
{
	if (nValue > 100)
	{
		alert("Value cannot exceed 100%");
		return false;
	}
	return true;
}
function Update(nValue,type)
{
	if (nValue > 100)
	{
		alert("Value cannot exceed 100%");
		return false;
	}
	else
	{
		document.frmChg.action = 'modCharge.asp?save=<%=trim(Request.QueryString ("Save"))%>&type='+type+'&val='+nValue+'&taxAmt=<%=intTaxAmt%>&totTax=<%=intTotTax%>&totChg=<%=intTotChg%>&netChg=<%=intNetAmt%>&InsChg=<%=intInsurAmt%>&CODChg=<%=intCODAmt%>&FreightChg=<%=intFreightAmt%>&wght=<%=intTotWght%>&noCtn=<%=intTotalCtn%>';
		document.frmChg.submit ();
	}
}
function addChg(obj,type)
{
	if (obj.checked)
	{
		if (type == 'P')
			doSubmit("UPDATE","<%=session("PriChg")%>",type);
		else if (type == 'S')
			doSubmit("UPDATE","<%=session("DropChg")%>",type);
	}
	else
		doSubmit("UPDATE","",type);
}
function payFull(obj,amount)
{
	if (obj.checked)
	{
		document.frmChg.txtDep.value = amount;
		document.frmChg.hidDep.value = amount;
		document.all("card-amount").value =amount;
		document.frmChg.txtDep.disabled  = true;
	}
	else
	{
		<%
			'wma use deposit
			'intFullAmt = cdbl(intTotChg*0.5) - cdbl(intPay)
			intFullAmt = cdbl(intTotChg * Session("DepositPercent")/100) - cdbl(intPay)
		%>
		document.frmChg.txtDep.value = <%=round(intFullAmt,2)%>;
		document.frmChg.hidDep.value = <%=round(intFullAmt,2)%>;
		document.all("card-amount").value = <%=round(intFullAmt,2)%>;
		document.frmChg.txtDep.disabled = false;
	}
}
function saveord()
{
	document.frmChg.action  = 'saveord.asp';
	document.frmChg.submit ();
}
function back()
{
	document.frmChg.action = 'modCharge.asp?add=T'
	document.frmChg.submit ();
}
//-->
</SCRIPT>
<%if trim(Request.QueryString ("Save")) = "" then%>
<!-- Table width=100% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<tr>
<td height=72>
< old header >
</td>
</tr>
</table -->
<%end if%>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
<%IF strFile = "cust" Then%>
	<SCRIPT LANGUAGE="JavaScript1.2"
	        SRC="../HM_Loader_Cust.js"
	        TYPE='text/javascript'>
	</SCRIPT>
	<p><BR><BR><br></p>
<%Else%>
	<SCRIPT LANGUAGE="JavaScript1.2"
	        SRC="../HM_Loader_Sales.js"
	        TYPE='text/javascript'>
	</SCRIPT>
	<p><br><br><br></p>
	<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
	<TR>
	<TD colspan=13>
		<%IF Trim(Session("customerid")) <> ""  Then%>
			<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=session("rscust").fields("btname").value%></P>
		<%else%>
			&nbsp
		<%end if%>
		</TD>
		<!--TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD-->
	</TR>
	</table>
<%End IF%>

<Table width=85% align=center height="50" border="1">
<TR>
	<TD class="title">Order Charges</TD>
</TR>
</Table>
<br><br>
<%
If Trim(Session("Rep")) = "" Then
	strAppUserVar = Session("ID")
Else
	strAppUserVar = Session("Rep")
End If

%>

<form name=frmChg method=post >
<input type="hidden" name="card-amount" value=<%=round(intTotChg*(Session("DepositPercent")/100),2)%>>



<div align="center">
<TABLE  border=1  width="85%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<%if Session("COD") = "T" then%>
	<TR>
		<TD width=20% class="dark_cell">Method of Payment</TD>
		<TD colspan=3 class="light_cell">
			<input type=radio name=radPay value="C">Cash on Delivary
			<input type=radio name=radPay value="D">Credit Card
		</TD>
		
	</TR>
	<TR>
		<TD colspan=4 class="light_cell" height=1></TD>
	</TR>
<%end if%>
<TR>
	<TD width=20% class="dark_cell">Total Quantity</TD>
	<TD width=15% class="light_cell" align=right>
		<%=Session("ordQty")%>
	</TD>
	<TD width=25% class="dark_cell">Merchandise Amount</TD>
	<TD class="light_cell" align=right>
		<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
			<%=Session("Currency") & FormatNumber(Session("ordAmount"),2)%>
		<%else%>
			<%=FormatNumber(Session("ordAmount"),2) & Session("Currency")%>
		<%end if %>
	</TD>
	
</TR>
<TR>
	<TD class="dark_cell">Total Weight</TD>
	<TD class="light_cell" align=right>
		<%=intTotWght%>
		<input type=hidden name=txtWght value="<%=intTotWght%>">
	</TD>
	<TD class="dark_cell">Discount</TD>
	<TD class="light_cell" align=right>
		<img border=0 src="../images/<%=Session("theme")%>/cancel.gif" onclick="doSubmit('UPDATE','0','D')"title=Reset>
		<img border=0 src="../images/<%=Session("theme")%>/ok.gif" title="Apply" onClick="return doSubmit('UPDATE',document.frmChg.txtDisc.value,'D')">
		
		<input type=text name=txtDisc size=4 
			
		<%if isobject(Session("rsCharges")) then
			if trim(Session("rsCharges")("nDiscount")) <> "" then%> 
				value="<%=FormatNumber(trim(Session("rsCharges")("nDiscount")))%>" 
			<%end if%>
			
		<%else%> 
			value="<%=FormatNumber(0)%>" 
		<%end if%>
		<%if trim(Request.QueryString ("Save")) = "" then%> readonly <%end if%> onkeypress="return HandleEnterKey(this.value,'D')" onChange="return Update(this.value,'D')">%
	</TD>
</TR>
<TR>
	<TD  class="dark_cell">No. of Cartons</TD>
	<TD class="light_cell" align=right>
		<%=intTotalCtn%>
		<input type=hidden name=txtTotCtns value="<%=intTotalCtn%>">
	</TD>
	<%'if trim(Request.Form("txtDisc")) <> "" then
		'intDisc = cdbl(Session("ordAmount")) * cdbl(trim(Request.Form("txtDisc")))/100
	  if isobject(Session("rsCharges")) then
		if trim(Session("rsCharges")("nDiscount")) <> "" then
			 intDisc = cdbl(Session("ordAmount")) * cdbl(trim(Session("rsCharges")("nDiscount")))/100
		end if
	  else
		intDisc = 0
	  end if
	%>
	<TD class="dark_cell">Discount Amount</TD>
	<TD class="light_cell" align=right>
		<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
			<%=Session("Currency") & formatnumber(round(intDisc,2),2)%>
		<%else%>
			<%=formatnumber(round(intDisc,2),2) & Session("Currency")%>
		<%end if %>
	</TD>
	
	
</TR>
<%if lTax = "Y" and Session("Taxable") = "T" then'and cdbl(intTaxAmt) > 0 then%>
	<tr>
	<TD  class="dark_cell"></TD>
	<TD class="light_cell" align=right></TD>
	<TD class="dark_cell">Tax</TD>
	<TD class="light_cell" align=right>
		<img border=0  src="../images/<%=Session("theme")%>/cancel.gif" onclick="doSubmit('UPDATE',<%=FormatNumber(cdbl(Session("DefTax"))*100,3)%>,'T')" title="Reset">
		<img border=0  src="../images/<%=Session("theme")%>/ok.gif" onclick="doSubmit('UPDATE',document.frmChg.txtTax.value,'T')" title='Apply'>
		<input type=text name=txtTax size=4  
		
		<%if isobject(Session("rsCharges")) then 
			if trim(Session("rsCharges")("ntax_rate")) <> "" then%> 
				value="<%=FormatNumber(cdbl(trim(Session("rsCharges")("ntax_rate"))),3)%>"
			<%end if%>
		<%else%> 
			value="<%=FormatNumber(cdbl(strTax)*100,3)%>" 
		<%end if%>
		<%if trim(Request.QueryString ("Save")) = "" then%> readonly <%end if%> onkeypress="return HandleEnterKey(this.value,'T')" onChange="return Update(this.value,'T')" >%
		<!--input type=hidden name=txtTaxAmt value="<%=intTotTax%>"-->
	</TD>
	</tr>
<%end if%>
<%if lTax = "Y" and Session("Taxable") = "T" then'and cdbl(intTaxAmt) > 0 then
	if isobject(Session("rsCharges")) then
		if trim(Session("rsCharges")("ntax_amt")) <> "" then
			intTotTax=trim(Session("rsCharges")("ntax_amt"))
		end if
   end if
%>
<tr>	
	<TD  class="dark_cell"></TD>
	<TD class="light_cell" align=right></TD>
	<TD class="dark_cell">Tax Amount</TD>
	<TD class="light_cell" align=right>
		<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
			<%=Session("Currency") & formatnumber(round(intTotTax,2),2)%>
		<%else%>
			<%=formatnumber(round(intTotTax,2),2) & Session("Currency")%>
		<%end if %>
		<input type=hidden name=txtTaxAmt value="<%=formatnumber(round(intTotTax,2),2)%>">
	</TD>
</tr>
<%end if%>
<TR>
	<TD  class="dark_cell">Expected Ship Date</TD>
	<TD class="light_cell" align=right><%=dtComp%></TD>
	<TD class="dark_cell">Freight</TD>
	<TD class="light_cell" align=right>
		<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
			<%=Session("Currency") & formatnumber(round(intFreightAmt,2),2)%>
		<%else%>
			<%=formatnumber(round(intFreightAmt,2),2) & Session("Currency")%>
		<%end if %>
		<input type=hidden name=txtFreight value="<%=formatnumber(round(intFreightAmt,2),2)%>">
	</TD>
</TR>
<%If Session("PriChg")>0 then'wma%>
	<TR>
		<TD  class="dark_cell"></TD>
		<TD class="light_cell" align=right></TD>
		<TD class="dark_cell">Priority Shipping</TD>
		<TD class="light_cell" align=right>
			<input type=checkbox name=chkPri <%if Session("rsCharges")("lPrichg")= true then%>checked<%end if%> onclick="addChg(this,'P');">
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(Session("PriChg"),2),2)%>
			<%else%>
				<%=formatnumber(round(Session("PriChg"),2),2) & Session("Currency")%>
			<%end if %>
			<input type=hidden name=txtPri value="<%=formatnumber(round(Session("PriChg"),2),2)%>">
		</TD>
	</TR>
<%end if 'wma%>	
<%If Session("DropChg")>0 then'wma%>
	<TR>
		<TD  class="dark_cell"></TD>
		<TD class="light_cell" align=right></TD>
		<TD class="dark_cell">Drop Ship</TD>
		<TD class="light_cell" align=right>
			<input type=checkbox name=chkDrop <%if Session("rsCharges")("lDropchg")= true then%>checked<%end if%> onclick="addChg(this,'S');">
			<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
				<%=Session("Currency") & formatnumber(round(Session("DropChg"),2),2)%>
			<%else%>
				<%=formatnumber(round(Session("DropChg"),2),2) & Session("Currency")%>
			<%end if %>
			<input type=hidden name=txtDrop value="<%=formatnumber(round(Session("DropChg"),2),2)%>">
		</TD>
	</TR>
<%end if 'wma%>
<TR>
	<TD  class="dark_cell"></TD>
	<TD class="light_cell" align=right></TD>
	<TD class="dark_cell">UPS Insur</TD>
	<TD class="light_cell" align=right>
		<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
			<%=Session("Currency") & formatnumber(round(intInsurAmt,2),2)%>
		<%else%>
			<%=formatnumber(round(intInsurAmt,2),2) & Session("Currency")%>
		<%end if %>
		<input type=hidden name=txtInsur value="<%=formatnumber(round(intInsurAmt,2),2)%>">
	</TD>
</TR>
<%if Session("COD") = "T" then%>
	<TR>
		<TD  class="dark_cell"></TD>
		<TD class="light_cell" align=right></TD>
		<TD class="dark_cell">C.O.D Charge</TD>
		<TD class="light_cell" align=right>
			<%=formatnumber(round(intCODAmt,2),2)%>
			<input type=hidden name=txtCOD value="<%=formatnumber(round(intCODAmt,2),2)%>">
		</TD>
	</TR>
<%else%>
	<input type=hidden name=txtCOD value="0">
<%end if%>
<TR>
	<TD colspan=4 class="light_cell" height=1></TD>
</TR>
<%if isobject(Session("rsCharges")) then
	if trim(Session("rsCharges")("ntotChg")) <> "" then
		intTotChg=trim(Session("rsCharges")("ntotChg"))
		intDep =trim(Session("rsCharges")("nDeposit")) 
		intFull = cdbl(intTotChg) - cdbl(intpay)
		'if cdbl(intFull) <= 0 then
		'	intFull = cdbl(intTotChg)
		'end if
	end if
  end if
%>
<TR>
	<TD colspan=2 class="dark_cell" align=right></TD>
	<TD class="dark_cell">Balance to Pay</TD>
	<TD class="light_cell" align=right>
		
		<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
			<%=Session("Currency") & formatnumber(round(intTotChg,2),2)%>
		<%else%>
			<%=formatnumber(round(intTotChg,2),2) & Session("Currency")%>
		<%end if %>
		<input type=hidden name=txtTotChg  value="<%=formatnumber(round(intTotChg,2),2)%>">
	</TD>
</TR>
<TR>
	<TD colspan=2 class="dark_cell" align=right></TD>
	<TD class="dark_cell">Paid Amount</TD>
	<TD class="light_cell" align=right>
		<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
			<%=Session("Currency") & formatnumber(round(intPay,2),2)%>
		<%else%>
			<%=formatnumber(round(intPay,2),2) & Session("Currency")%>
		<%end if %>
	</TD>
</TR>

<TR>
	<TD colspan=2 class="dark_cell" align=right></TD>
	<TD class="dark_cell">Deposit Amount</TD>
	<TD class="light_cell" align=right>
		<input type=checkbox name=chkFull <%if Request.Form ("chkFull") <> "" then%>checked<%end if%> onclick="payFull(this,'<%=round(intFull,2)%>');">Pay in Full
		<%if Trim(Session("CurrencyAlign")) = "LEFT" then%>
			<%=Session("Currency")%><input type=text name=txtDep size=5 <%if Request.Form ("chkFull") <> "" then%>value="<%=formatnumber(round(intFull,2),2)%>" disabled <%else%>value="<%=formatnumber(round(intDep,2),2)%>"<%end if%> onchange="document.all('card-amount').value = document.all('txtDep').value" <%if session("AllowEditDeposit") = "F" then Response.Write "readonly" %>>
		<%else%>
			<input type=text name=txtDep size=5 <%if Request.Form ("chkFull") <> "" then%>value="<%=formatnumber(round(intFull,2),2)%>" disabled <%else%>value="<%=formatnumber(round(intDep,2),2)%>"<%end if%> onchange="document.all('card-amount').value = document.all('txtDep').value" <%if session("AllowEditDeposit") = "F" then Response.Write "readonly" %>><%=Session("Currency")%> 
		<%end if %>
		<input type=hidden name=hidDep size=5 value="<%=formatnumber(round(intFull,2),2)%>">
	</TD>
</TR>

<!--WMA Payment Information [Start]-->
<%
if session("UseOnlinePayment") = "T" then 'use online payment - wma
	if trim(session("GateWay")) = "PlugNPay.com (MemberShip Managment)" then  'use PNP Membership
%>
		<TR><TD colspan=4 class="light_cell" height=1></TD></TR>
		<TR>
			<TD colspan=2 class="dark_cell" align=right></TD>
			<TD class="dark_cell">Payment Method:</TD>
			<TD class="light_cell" align=right>
				<select name="paymethod">
					<option value="credit">Credit Card</option>
					<option value="onlinecheck">Online Check</option>
				</select>
			</TD>
		</TR>
		<TR><TD colspan=4 class="light_cell" height=1></TD></TR>
<%
	end if
else	'don't use online payment
end if
%>		
<!--WMA Payment Information [End]-->

</TABLE>

<%if Request.QueryString("FinalStatus")= "badcard" OR Request.QueryString("FinalStatus")= "problem" then%>
	<script language="javascript">
		alert('The Payment Procedures were not completed successfully, Because of <%="(( "& Request.QueryString("MErrMsg") &" ))" %> , please try again');
	</script>	
<%end if%>

<%if trim(Request.QueryString ("Save")) <> "" OR Request.QueryString("FinalStatus")= "badcard" OR Request.QueryString("FinalStatus")= "problem" then %>
	<table width=85%>
	<tr>
		<td align=right>
		<INPUT name=btnback type="button" value="Back to Order Lines" onclick="javascript:back();">
		<INPUT type="button" value="Save Order" style="HEIGHT: 24px; WIDTH: 100px" onClick ="return doSubmit('SAVE','','')" id=button1 name=button1></td>
	</tr>
	</table>
<%end if%>

</div>
</form>
</BODY>
</HTML>