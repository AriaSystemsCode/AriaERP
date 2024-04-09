<%@ Language=VBScript %>
<!--#include file="../common/Add2Log.asp"-->
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>

<%
Response.Buffer = true
'set oui = server.CreateObject ("CustomerUI.CustomUI")
set ouiCustomer = server.CreateObject ("CustomerUI.CustomUI")

'oui.ConParameter = Application("DataConnectionString")
ouiCustomer.ConParameter = Application("DataConnectionString")
If ouiCustomer.Load (uCase(Trim(Session("customerid")))) then
	If Trim(Request("Store")) = "" Then
		Set oui = ouiCustomer
		'Response.Write("Store Not found in request")
		'Response.End 
	Else
		ouiCustomer.ChildFind 2,"Store='" & Trim(Request("Store")) & "'"
		If Not(ouiCustomer.ChildEOF(2)) Then
			Set oui = ouiCustomer.ChildGet(2)
			'Response.Write(oui.cust_store)
			'Response.End 
		Else
			'Set oui =  ouiCustomer
			Response.Write("Can't Load Store Data")
			Response.End 
		End If

	End If


	strEditCust = "Edit!#!Customer!#!Old!#!" & trim(oui.cust_dba)& "!#!" &trim(oui.cust_phone1)& "!#!" &trim(oui.keeper)& "!#!" &trim(oui.buyer)& "!#!" &trim(oui.cust_phone2)& "!#!" &trim(oui.cust_fax)& "!#!" &trim(oui.cust_caddress1)& "!#!" &trim(oui.cust_caddress2)& "!#!" &trim(oui.cust_caddress3)& "!#!" &trim(oui.cust_caddress4)& "!#!" &trim(oui.cust_caddress5)& "!#!" &trim(oui.cust_caddress6)& "!#!New!#!" &trim(cstr(Request.Form ("DBA"))) & "!#!" & trim(cstr(Request.Form ("Phone1")))& "!#!" &trim(cstr(Request.Form ("Keeper")))& "!#!" &trim(cstr(Request.Form ("Buyer")))& "!#!" &trim(cstr(Request.Form ("Phone2")))& "!#!" &trim(cstr(Request.Form ("Fax")))& "!#!" &trim(cstr(Request.Form ("Addr1")))& "!#!" &trim(cstr(Request.Form ("Addr2")))& "!#!" &trim(cstr(Request.Form ("Addr3")))& "!#!" &trim(cstr(Request.Form ("Addr4")))& "!#!" &trim(cstr(Request.Form ("Addr5")))& "!#!" &trim(cstr(Request.Form ("Addr6")))
	oui.cust_dba = trim(cstr(Request.Form ("DBA")))
	Response.Write(oui.cust_dba)
	Set Conn = server.CreateObject("ADODB.Connection")
	Conn.Open Application("SystemConnectionString")
	Set RSComp = server.CreateObject("ADODB.Recordset")
	Set RSFormat = server.CreateObject("ADODB.Recordset")
			    
	strsql = "select * from syccomp where ccomp_id='" & Session("CompanyID") & "'"
	RSComp.Open strsql,conn
	IF Not(RSComp.EOF And RSComp.BOF) Then
		strCountryCode = Trim(RSComp.Fields("ccont_code").Value )
	End IF
	RSComp.Close 
	Set RSComp = Nothing
	strsql = "select * from sycint where ccont_code='" & strCountryCode & "'"
			    
	RSFormat.Open strsql,conn
	IF Not(RSFormat.EOF And RSFormat.BOF) Then
		strFormat = RSFormat.Fields("CPHONETEMP").Value 
	End IF

	intTotLegth = Len(strFormat)
	intOldPos = 1
	intPos1 = 1
	strOutPut = ""
	intpos = 1
	intcount = 1
	
	Do while Not intPos1 = 0
		intpos1 = instr(intOldPos, strFormat, "-",1)
		intpos2 = instr(intOldPos, strFormat, "/",1)
		IF intpos1 > intpos2 OR intpos1 = 0 Then
			intpos1 = intpos2
		End IF
		intpos = intpos + intpos1 - intOldPos
		intOldPos = intpos1 + 1
		intcount = intcount + 1
	Loop

	strtempfax = ""
	strphone2 = ""
	For inttemp=1 To  intcount-1
		strtemp = strtemp & request("phone1" & inttemp)
		strtempfax = strtempfax & request("Fax" & inttemp)
		strphone2 = strphone2 & request("phone2" & inttemp)
	Next
	oui.cust_phone1 = strtemp
	
	oui.cust_phone2 = strphone2
	'Response.Write(oui.cust_phone2)
	'Response.End 
	oui.cust_fax = strtempfax
	oui.cust_shipvia = trim(Request.Form ("selShip"))
	'WMA Replace only potential by ordinary 4 status
	Response.Write  trim(Request.Form ("lstStatus"))
	'Response.End 
	oui.cust_status = trim(Request.Form ("lstStatus"))
	oui.keeper = trim(cstr(Request.Form ("txtKeeper")))
	oui.buyer = trim(cstr(Request.Form ("txtBuyer")))
	oui.cust_caddress1 = trim(cstr(Request.Form ("Addr1")))
	oui.cust_caddress2 = trim(cstr(Request.Form ("Addr2")))
	oui.cust_caddress3 = trim(cstr(Request.Form ("Addr3")))
	oui.cust_caddress4 = trim(cstr(Request.Form ("Addr4")))
	oui.cust_caddress5 = trim(cstr(Request.Form ("Addr5")))
	oui.cust_caddress6 = trim(cstr(Request.Form ("Addr6")))
	If Trim(Request("Store"))="" Then
		'Response.Write("Mod. in main account")
		'Response.End 
		If oui.Save () Then
			Add2Log "", session("CustomerID"),"Editing customer",session("CustomerID"),strEditCust
		End If
	Else
	'	Response.Write("Mod. in Store account")
	'	Response.End 

		ouiCustomer.ChildSet 2,oui
		If ouiCustomer.Save () Then
			Add2Log "", session("CustomerID"),"Editing Store",session("CustomerID"),strEditCust
		End If
	End If
Else
	'Response.Write("Cann't load")
	'Response.End 
End If
Set oui = Nothing
Response.Redirect "custprof.asp?store="&Request("Store")
%>