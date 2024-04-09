<%@ Language=VBScript %>
<!--#include file="../common/Add2Log.asp"-->
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>
<%
Response.Buffer=true
If trim(Session("PO"))="" Then
	Session("PO")=request("txttemp")
Else
End If
response.write  Session("PO")
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
	</script>	
<%End if

' To handle the case of the rep.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF


IF NOT Session("RSLine").EOF OR NOT Session("RSLine").BOF THEN

'Variable definition
Dim strsql ' as string
Dim strOrder ' as string
Dim intCount, intOrdWidth ' as int

'Creat the Connection
set conne=server.CreateObject("ADODB.connection")
conne.Open Application("DataConnectionString")

'Creat the Recordset for the Sequence file to get the order No.
set RSSequence = server.CreateObject("ADODB.recordset")
strsql="select * from sequence where cseq_type='ORDER' And cfile_nam='ORDHDR'"

RSSequence.CursorLocation = 3
RSSequence.CursorType = adOpenStatic
RSSequence.LockType = 4

' Get The Sequence Number.
RSSequence.open strsql,conne




' Creating Objects.
Dim objUIOrder 
Set objUIOrder = Server.CreateObject("UIOrder.UIOrd")

Dim objUIOrdLine
Set objUIOrdLine = server.CreateObject("UIOrder.UIOrdChld")


'hdm[start]
'objUIOrder.ConParameter = "Provider=MSDATASHAPE;DSN=CRM;SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=YES;"
objUIOrder.ConParameter = Application("DataConnectionString")
'hdm[end]
' creating a new recordset for order object.
Dim bolTemp ' as boolean
bolTemp = objUIOrder.Add()

IF bolTemp Then
	strOrder = RSSequence("nseq_no")
	RSSequence("nseq_no") = cdbl(RSSequence("nseq_no")) + 1
	intOrdWidth = RSSequence("nfld_wdth")

	intLong = Len(strOrder)
	intDif = cint(intOrdWidth) - cint(intLong)

	For i = 1 to intDif 
		strOrder = "0" & strOrder
	Next


	RSSequence.UpdateBatch 
	RSSequence.Close()
	Set RSSequence = Nothing

	objuiorder.Order_type = "T"
	objuiorder.Ordered = strOrder
objuiorder.stats = "B" 'Status Should be Bid because it's from web
	objuiorder.Acount = CustID

	IF Session("Has_Store")="Y" Then
		Set RSCustomer = Server.CreateObject("ADODB.RecordSet")
		strsql="select * from customer where type='S' And store='" & Session("store") & "' And account='" & CustID & "'"
		RSCustomer.open strsql,conne
		Session("Store") = RSCustomer("store")
	Else
		Session("Store") = ""
	End IF
	
	objuiorder.stor = Trim(Session("Store"))
	objUIOrder.Multi_POrder = False
	objUIOrder.Cust_po =trim(Session("PO"))
	objUIOrder.Note_1 = ""
	objUIOrder.Note_2 = ""
	objUIOrder.Priorities = Session("RSCust").fields("priority")
	objUIOrder.Order_Cat = ""
	objUIOrder.Seasons = Session("Season")
	objUIOrder.Divisions = Session("Division")
	objUIOrder.Bulk_Order = "N"
	objUIOrder.Re_Order = "N"
	objUIOrder.Consols = ""
	objUIOrder.Multi_Store = "N"
	objUIOrder.Term_Code = Session("RSCust").fields("ctermcode")
	objUIOrder.Ship_Via = Session("RSCust").fields("shipvia")
	objUIOrder.Spc_Inst = Session("RSCust").fields("spcinst")
	objUIOrder.St_Name = Session("RSCust").fields("stname")
	objUIOrder.Do_Insurance = Session("RSCust").fields("cinsur")
	objUIOrder.Buyer_Name = Session("RSCust").fields("buyer")
	objUIOrder.Phone_No = Session("RSCust").fields("phone1")
	objUIOrder.Fac_Code = Session("RSCust").fields("cfaccode")
	'objUIOrder.FactAcct=Session("RSCust").fields("factacct") 'Need to recompile the object again
	objUIOrder.Do_Approval =""
	objUIOrder.Approval_Amount = 0
	'objUIOrder.Decl_Date = ""
	objUIOrder.Decl_Code =""
	objUIOrder.Rep_1 = Session("RSCust").fields("salesrep")
	objUIOrder.Com_1 = Session("RSCust").fields("comm")
	objUIOrder.Rep_2 = Session("RSCust").fields("rep2")
	objUIOrder.Com_2 = Session("RSCust").fields("comm2")
	objUIOrder.Entered = date()
	objUIOrder.Started = Session("start")
	objUIOrder.Completed = Session("completed")
	'objUIOrder.Canceled = ""
	objUIOrder.Discount = Session("RSCust").fields("disc")
	objUIOrder.Book = Session("ordQty")
	objUIOrder.Book_Amount = Session("ordAmount")
	objUIOrder.sCancel = 0
	objUIOrder.CancelAmnt = 0
	objUIOrder.Ship = 0
	objUIOrder.Ship_Amount = 0
	objUIOrder.OpenOrd = Session("ordQty")
	objUIOrder.OpenAmnt = Session("ordAmount")
	objUIOrder.TotCut = 0
	objUIOrder.Flag =""
	objUIOrder.From_web = true
	set RSSetups = server.CreateObject("ADODB.recordset")
	strsql="select mdata_def from setups where cfld_name='M_DIV_LINK'"
	RSSetups.open strsql,conne 

'	IF RSSetups("mdata_def")="N" Then
	IF NOT RSSetups.eof Then
		objUIOrder.Link_Code = Session("RSCust").fields("link_code")
		objUIOrder.GL_Sales = Session("RSCust").fields("link_code")
	Else
		set RSCodes = server.CreateObject("ADODB.recordset")
		'strsql="select crltd_vlu,crltd_nam from codes where cfld_name='CDIVISION' And crltfield='Y' And (crltd_nam='LINK_CODE' OR crltd_nam='CSLSGLLINK')"
		strsql = "select crltd_vlu,crltd_nam from codes where crltfield+cfld_name like 'YCDIVISION%' And (crltd_nam='LINK_CODE' OR crltd_nam='CSLSGLLINK')"
		RSCodes.open strsql,conne 
		if not(rscodes.EOF and rscodes.BOF) then
			RSCodes.MoveFirst 
			If RSCodes("crltd_nam")="LINK_CODE" Then
				objUIOrder.Link_Code = RSCodes("crltd_vlu")
				RSCodes.MoveNext 
				objUIOrder.GL_Sales =RSCodes("crltd_vlu")
			Else
				objUIOrder.GL_Sales = RSCodes("crltd_vlu")
				RSCodes.MoveNext 
				objUIOrder.Link_Code = RSCodes("crltd_vlu")
			End if
		End if
		RSCodes.Close 
		Set RSCodes = Nothing
	End IF
	'objUIOrder.Int_Vend = Session("RSCust").fields("ccusvend")
	objUIOrder.EventCode = ""
	objUIOrder.BillNo = ""
	objUIOrder.Merc_Type = ""
	objUIOrder.BlankOrder = ""
	objUIOrder.Distrb_No = ""
	objUIOrder.Class = ""
	objUIOrder.Mon_Flag = ""
	objUIOrder.Labels = 0
	objUIOrder.Alt_ShpTo = false

	set RSWarecode = server.CreateObject("ADODB.recordset")
	strsql="select cwarecode from warehous"
	RSWarecode.open strsql,conne 
	RSWarecode.MoveFirst 
	objUIOrder.CwareCode = RSWarecode("cwarecode")
	objUIOrder.CancelReason = ""
	objUIOrder.Address1 = Session("RSCust").fields("caddress1")
	objUIOrder.Address2 = Session("RSCust").fields("caddress2")
	objUIOrder.Address3 = Session("RSCust").fields("caddress3")
	objUIOrder.Address4 = Session("RSCust").fields("caddress4")
	objUIOrder.address5 = Session("RSCust").fields("caddress5")
	objUIOrder.CurrCode = Session("RSCust").fields("ccurrcode")
	objUIOrder.Exch_Rate = 1
	'objUIOrder.CurrUnit = ""
	objUIOrder.FromOrder = ""
	'objUIOrder.Direct_Inv = ""
	objUIOrder.Has_Notes = false

	objUIOrder.Add_User = "WEB"
	objUIOrder.Add_Time = Time()
	objUIOrder.Add_Date = Date()
	'objUIOrder.Lok_Stat = ""
	'objUIOrder.Lok_User = ""
	'objUIOrder.Lok_Date = ""
	'objUIOrder.Lok_Time = "" 
	objUIOrder.EDI_Order = False
	objUIOrder.Owner = ""



	'creating Order Lines.
	intLineno = 1
	Session("RSLine").movefirst
	Do While Not Session("RSLine").Eof
		Set objUIOrdLine = objuiorder.ChildAddNew(OrderLine)

		objUIOrdLine.Account_line = CustID
		objUIOrdLine.OrderType_line = "T"
		objUIOrdLine.Style_line = Session("RSLine").fields("style")
		objUIOrdLine.Season_line = Session("season")
		objUIOrdLine.Desc1_line = Session("RSLine").fields("desc1")
		objUIOrdLine.Scale_line = Session("RSLine").fields("scale")
		objUIOrdLine.PrePak_line = Session("RSLine").fields("prepak")
		objUIOrdLine.Price_line = Session("RSLine").fields("price")
		objUIOrdLine.Start_line = Session("start")
		objUIOrdLine.Complete_line = Session("completed")
		objUIOrdLine.WareCode_line = objUIOrder.CwareCode 
		objUIOrdLine.Qty1_line = Session("RSLine").fields("qty1")
		objUIOrdLine.Qty2_line = Session("RSLine").fields("qty2")
		objUIOrdLine.Qty3_line = Session("RSLine").fields("qty3")
		objUIOrdLine.Qty4_line = Session("RSLine").fields("qty4")
		objUIOrdLine.Qty5_line = Session("RSLine").fields("qty5")
		objUIOrdLine.Qty6_line = Session("RSLine").fields("qty6")
		objUIOrdLine.Qty7_line = Session("RSLine").fields("qty7")
		objUIOrdLine.Qty8_line = Session("RSLine").fields("qty8")
		objUIOrdLine.Total_Qty_line = Session("RSLine").fields("totqty")
		objUIOrdLine.Book1_line = Session("RSLine").fields("qty1")
		objUIOrdLine.Book2_line = Session("RSLine").fields("qty2")
		objUIOrdLine.Book3_line = Session("RSLine").fields("qty3")
		objUIOrdLine.Book4_line = Session("RSLine").fields("qty4")
		objUIOrdLine.Book5_line = Session("RSLine").fields("qty5")
		objUIOrdLine.Book6_line = Session("RSLine").fields("qty6")
		objUIOrdLine.Book7_line = Session("RSLine").fields("qty7")
		objUIOrdLine.Book8_line = Session("RSLine").fields("qty8")
		objUIOrdLine.Total_Book_line = Session("RSLine").fields("totqty")	
		objUIOrdLine.Picked_line = false
		objUIOrdLine.shipments_line = 0
		objUIOrdLine.Line_No_line = intLineno
		intLineno = intLineno + 1
		objUIOrdLine.Store_line = Session("Store")


		if objuiorder.ChildSet(1,objUIOrdLine) then
		end if
		Session("RSLine").movenext
	Loop
	objUIOrder.LastLine = intLineno - 1


	bolTemp = objuiorder.Save()
	strAddMemo = "Add!#!Remote Order!#!" & objuiorder.Acount & "!#!" & objuiorder.stor & "!#!" & objUIOrder.Cust_po & "!#!" &objUIOrder.Divisions& "!#!" &objUIOrder.Seasons& "!#!" &objUIOrder.Started& "!#!" &objUIOrder.Completed& "!#!" &Session("ordQty")& "!#!" &Session("ordAmount")& "!#!" &objuiorder.Ordered
		
End IF		

	Set objUIOrder = Nothing



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

	Session("RSLine").close
	Set Session("RSLine") = Nothing

	Dim strAccount
	strAccount = CustID
	strTemp = "compSave.asp?order=" & strOrder & "&ID=" & strAccount

	'''''''''''''''''''''''''
		Add2Log "", strAccount,"Adding Remote Order",trim(cstr(strOrder)),strAddMemo
	'''''''''''''''''''''''''

	Response.Redirect(strTemp)
	'Response.Write strAddMemo &"<br>"&Session("Division")
	ELSE
		Response.Redirect("custorder.asp")
	end if

'Creat the Recordset for the ORDHDR file.
'Set RSOrdHdr = Server.CreateObject("ADODB.RecordSet")
'strsql="select * from ordhdr where .f."

'RSOrdHdr.CursorLocation = 3
'RSOrdHdr.CursorType = adOpenStatic
'RSOrdHdr.LockType = 4

'RSOrdHdr.open strsql,conne

'RSOrdHdr.AddNew			'Add New Record.


'start Saving the data
'RSOrdHdr("cordtype") = "O"

'intCount = cdbl(intOrdWidth) - Len(strOrder)
'Do While intCount > 0

'	intCount = intCount - 1
'	strOrder = "0" & strOrder
'Loop

'RSOrdHdr("order") = strOrder


'RSOrdHdr("status") = "B"
'RSOrdHdr("account") = Session("ID")

'IF Session("Has_Store")="Y" Then

'	Set RSCustomer = Server.CreateObject("ADODB.RecordSet")
'	strsql="select * from customer where type='S' And store='" & Session("store") & "' And account='" & Session("ID") & "'"

'	RSCustomer.open strsql,conne
'	Session("Store") = RSCustomer("store")
'Else
'	Session("Store") = ""
'End IF


'RSOrdHdr("store")= Session("Store")
''RSOrdHdr("dept")=
'RSOrdHdr("multipo") = False
''RSOrdHdr("custpo")=
'RSOrdHdr("note1")= ""
'RSOrdHdr("note2")= ""
'RSOrdHdr("priority") = Session("RSCust").fields("priority")
''RSOrdHdr("cordercat")=
'RSOrdHdr("season") = Session("Season")
'RSOrdHdr("cdivision") = Session("Division")
'RSOrdHdr("Bulk") = "N"
'RSOrdHdr("creorder") = "N"
''RSOrdHdr("consol") = 
'RSOrdHdr("multi") = "N"
'RSOrdHdr("ctermcode") = Session("RSCust").fields("ctermcode")
'RSOrdHdr("shipvia")= Session("RSCust").fields("shipvia")
'RSOrdHdr("spcinst")= Session("RSCust").fields("spcinst")
'RSOrdHdr("stname")=Session("RSCust").fields("stname")
'RSOrdHdr("cinsur")=Session("RSCust").fields("cinsur")
'RSOrdHdr("buyer")=Session("RSCust").fields("buyer")
'RSOrdHdr("phone")=Session("RSCust").fields("phone1")
'RSOrdHdr("cfaccode")=Session("RSCust").fields("cfaccode")
'RSOrdHdr("factacct")=Session("RSCust").fields("factacct")
'RSOrdHdr("approval")=""
'RSOrdHdr("appramt")=0
''RSOrdHdr("decl_date")=
''RSOrdHdr("decl_code")=
'RSOrdHdr("rep1")=Session("RSCust").fields("salesrep")
'RSOrdHdr("comm1")=Session("RSCust").fields("comm")
'RSOrdHdr("rep2")=Session("RSCust").fields("rep2")
'RSOrdHdr("comm2")=Session("RSCust").fields("comm2")
'RSOrdHdr("entered") = date()
'RSOrdHdr("start") = Session("Completed")
''RSOrdHdr("cancelled")=""
'RSOrdHdr("complete")= Session("Completed")
'RSOrdHdr("disc")=Session("RSCust").fields("disc")
'RSOrdHdr("book")= Session("ordQty")
'RSOrdHdr("bookamt")= Session("ordAmount")
''RSOrdHdr("cancel")=
''RSOrdHdr("cancelamt")=
''RSOrdHdr("ship")=
''RSOrdHdr("shipamt")=
'RSOrdHdr("open")= Session("ordQty")
'RSOrdHdr("openamt")= Session("ordAmount")
'RSOrdHdr("totcut")=0
''RSOrdHdr("flag")=

'set RSSetups = server.CreateObject("ADODB.recordset")
'strsql="select mdata_def from setups where cfld_name='M_DIV_LINK'"
'RSSetups.open strsql,conne 

'IF RSSetups("mdata_def")="N" Then
'	RSOrdHdr("link_code")= Session("RSCust").fields("link_code")
'  RSOrdHdr("gl_sales") = Session("RSCust").fields("link_code")
'Else
'	set RSCodes = server.CreateObject("ADODB.recordset")
'	strsql="select crltd_vlu,crltd_nam from codes where cfld_name='CDIVISION' And crltfield='Y' And (crltd_nam='LINK_CODE' OR crltd_nam='CSLSGLLINK')"
'	RSCodes.open strsql,conne 
'	RSCodes.MoveFirst 
'	If RSCodes("crltd_nam")="LINK_CODE" Then
'		RSOrdHdr("link_code")=RSCodes("crltd_vlu")
'		RSCodes.MoveNext 
'		RSOrdHdr("gl_sales")=RSCodes("crltd_vlu")
'	Else
'		RSOrdHdr("gl_sales")=RSCodes("crltd_vlu")
'		RSCodes.MoveNext 
'		RSOrdHdr("link_code")=RSCodes("crltd_vlu")
'	End if
'	RSCodes.Close 
'	Set RSCodes = Nothing
'End IF


''RSOrdHdr("int_vend")=Session("RSCust").fields("ccusvend")
''RSOrdHdr("event_code")=
''RSOrdHdr("bill_no")=
''RSOrdHdr("merc_type")=
''RSOrdHdr("blank_ord")=
''RSOrdHdr("distrb_no")=
''RSOrdHdr("cclass")=
''RSOrdHdr("mon_flg")=
''RSOrdHdr("labels")=
'RSOrdHdr("alt_shpto")= False

'set RSWarecode = server.CreateObject("ADODB.recordset")
'strsql="select cwarecode from warehous"
'RSWarecode.open strsql,conne 
'RSWarecode.MoveFirst 
'RSOrdHdr("cwarecode")= RSWarecode("cwarecode")

''RSOrdHdr("ccancreason")=
'RSOrdHdr("caddress1")=Session("RSCust").fields("caddress1")
'RSOrdHdr("caddress2")=Session("RSCust").fields("caddress2")
'RSOrdHdr("caddress3")=Session("RSCust").fields("caddress3")
'RSOrdHdr("caddress4")=Session("RSCust").fields("caddress4")
'RSOrdHdr("caddress5")=Session("RSCust").fields("caddress5")
'RSOrdHdr("ccurrcode")=Session("RSCust").fields("ccurrcode")
''RSOrdHdr("nexrate")=Session("RSCust").fields("nexrate")
''RSOrdHdr("ncurrunit")=
''RSOrdHdr("fromorder")=
''RSOrdHdr("direct_inv")=
'RSOrdHdr("lhasnotes")= False

'RSOrdHdr("cadd_user") = "WEB"
'RSOrdHdr("cadd_time") = Time()
'RSOrdHdr("dadd_date") = Date()
''RSOrdHdr("llok_stat") = 
''RSOrdHdr("clok_user") = 
''RSOrdHdr("dlok_date") = 
''RSOrdHdr("clok_time") = 

''RSOrdHdr("lediorder")=
''RSOrdHdr("cowner")=
'dim intLineno
'intLineno = 1

'IF Session("RSLine").BOF AND Session("RSLine").EOF Then
'Else
'	Session("RSLine").MoveFirst()
'	Do While Not Session("RSLine").Eof
'		Session("RSLine").fields("order") = strOrder
'		Session("RSLine").fields("cwarecode") = RSOrdHdr("cwarecode")
'		Session("RSLine").fields("book1") = Session("RSLine").fields("qty1")
'		Session("RSLine").fields("book2") = Session("RSLine").fields("qty2")
'		Session("RSLine").fields("book3") = Session("RSLine").fields("qty3")
'		Session("RSLine").fields("book4") = Session("RSLine").fields("qty4")
'		Session("RSLine").fields("book5") = Session("RSLine").fields("qty5")
'		Session("RSLine").fields("book6") = Session("RSLine").fields("qty6")
'		Session("RSLine").fields("book7") = Session("RSLine").fields("qty7")
'		Session("RSLine").fields("book8") = Session("RSLine").fields("qty8")
'		Session("RSLine").fields("totbook") = Session("RSLine").fields("totqty")
'		Session("RSLine").fields("picked") = False
'		Session("RSLine").fields("shipments") = 0
'		Session("RSLine").fields("lineno")= intLineno
'		intLineno = intLineno + 1
'		Session("RSLine").fields("store")=Session("Store")
'		'Session("RSLine").update()
'		Session("RSLine").MoveNext()
'	Loop
'	Session("RSLine").updatebatch
'	
'	RSOrdHdr("lastline")= intLineno - 1
'
'END IF


''	Session("Season") = request("slctSeason")
''	Session("Division") = request("slctDivision")
''	Session("Start") = request("txtStart")
''	Session("Completed") = request("txtCompleted")
''	Session("Store") = request("slctStore")


''Session("RSLine").Update()

'RSOrdHdr.UpdateBatch 
'RSOrdHdr.Close()
'Set RSOrdHdr = Nothing

'Session("getstyle")=""
'Session("OrderFlag") = ""
'Session("LongDesc1") = ""
'Session("LongDesc") = ""
'Session("ShortDesc") = ""
'Session("text1") = ""
'Session("text2") = ""
'Session("text3") = ""
'Session("text4") = ""
'Session("text5") = ""
'Session("text6") = ""
'Session("text7") = ""
'Session("text8") = ""
'Session("ordAmount") = ""
'Session("ordQty") = ""

'Session("RSLine").close
'Set Session("RSLine") = Nothing
'strTemp = "compSave.asp?order=" & strOrder
'Response.Redirect(strTemp)
'ELSE
'	Response.Redirect("custorder.asp")
'END IF

%>
