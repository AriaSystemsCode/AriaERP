<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %> 
<% Response.Buffer = true%>
<!--#include file="../common/Add2Log.asp"-->
<!--#include file="addcredit.asp"-->
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

<%'Response.End 

IF NOT Session("RSLine").EOF OR NOT Session("RSLine").BOF THEN
	'Variable definition
	Dim strsql ' as string
	Dim strOrder ' as string
	Dim intCount, intOrdWidth ' as int

	'Creat the Connection
	set conne=server.CreateObject("ADODB.connection")
	conne.Open Application("DataConnectionString")

	'XXXXXX WMA 5/20/2004 START: Rmove and add following line after app.lock
	'Creat the Recordset for the Sequence file to get the order No.
	'set RSSequence = server.CreateObject("ADODB.recordset")
	'strsql="select * from sequence where cseq_type='ORDER' And cfile_nam='ORDHDR'"

	''RSSequence.CursorLocation = 3 'adUseClient 
	''RSSequence.CursorType = adOpenStatic	'Static
	''RSSequence.LockType = 4		'adLockBatchOptimistic
	
	'RSSequence.CursorLocation = 3 'adUseServer  
	'RSSequence.CursorType = 1	'adOpenKeyset
	'RSSequence.LockType = 3		'adLockOptimistic 

	' Get The Sequence Number.
	'RSSequence.open strsql,conne
   'XXXXXX WMA 5/20/2004 END


	' Creating Objects.
	Dim objUIOrder 
	Set objUIOrder = Server.CreateObject("UIOrder.UIOrd")

	Dim objUIOrdLine
	Set objUIOrdLine = server.CreateObject("UIOrder.UIOrdChld")

	'objUIOrder.ContractID 
	'hdm[start]
	'objUIOrder.ConParameter = "Provider=MSDATASHAPE;DSN=CRM;SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=YES;"
	objUIOrder.ConParameter = Application("DataConnectionString")
	'hdm[end]
	' creating a new recordset for order object.
	Dim bolTemp ' as boolean
	bolTemp = objUIOrder.Add()

	IF bolTemp Then
		Application.Lock 
		
		'XXXXXX WMA 5/20/2004 START: Rmove and add following line after app.lock
		'Creat the Recordset for the Sequence file to get the order No.
		set RSSequence = server.CreateObject("ADODB.recordset")
		'strsql="select * from sequence where cseq_type='ORDER' And cfile_nam='ORDHDR'"
		strsql="select nseq_no, nfld_wdth from sequence where cseq_type='ORDER' And cfile_nam='ORDHDR'"
		

		'RSSequence.CursorLocation = 3 'adUseClient 
		'RSSequence.CursorType = adOpenStatic	'Static
		'RSSequence.LockType = 4		'adLockBatchOptimistic
	
		RSSequence.CursorLocation = 3 'adUseServer  
		RSSequence.CursorType = 1	'adOpenKeyset
		RSSequence.LockType = 3		'adLockOptimistic 

		' Get The Sequence Number.
		RSSequence.open strsql,conne
			
		strOrder = RSSequence("nseq_no")

'		RSSequence("nseq_no") = cdbl(RSSequence("nseq_no")) + 1
		intOrdWidth = RSSequence("nfld_wdth")

		intLong = Len(strOrder)
		intDif = cint(intOrdWidth) - cint(intLong)

		For i = 1 to intDif 
			strOrder = "0" & strOrder
		Next

		RSSequence.Close()
		Set RSSequence = Nothing

'		strSeqU = cint(strOrder) + 1
		strSeqU = strOrder + 1
		conne.Execute ("UPDATE sequence set nseq_no=" & strSeqU & " where cseq_type='ORDER' And cfile_nam='ORDHDR'")
				
		Application.UnLock 
		'XXXXXX WMA 5/20/2004 END	
		
		objuiorder.Order_type = "T"
		objuiorder.Ordered = strOrder
		objuiorder.stats = "B" 'Status Should be Bid because it's from web
		objuiorder.Acount = CustID
		'Response.Write Session("Has_Store")
		'NOT USED ANYMORE
		IF Session("Has_Store")="Y" Then
			Set RSCustomer = Server.CreateObject("ADODB.RecordSet")
			strsql="select * from customer where type+account+store ='S"&CustID&Session("store")&"'"
			RSCustomer.open strsql,conne
			Session("Store") = RSCustomer("store")
		Else
			Session("Store") = ""
		End IF
		
		
		objuiorder.stor = Trim(Session("Store"))
		objUIOrder.Multi_POrder = False
		objUIOrder.Cust_po =trim(Session("PO"))
		'wal_128815 save Contract ID
		objUIOrder.ContractID  = session("ContractID")
		'wal_127343 add saving of the 2 notes feilds
		objUIOrder.Note_1 = session("Note1")
		objUIOrder.Note_2 = session("Note2")
		objUIOrder.Priorities = Session("RSCust").fields("priority")
		objUIOrder.Order_Cat = ""
		if instr(Session("Season"),",") <> 0 then 'wma multiple seasons
			objUIOrder.Seasons = "*"
		else
			objUIOrder.Seasons = Session("Season")
		end if
		'Response.Write objUIOrder.Seasons
		
		objUIOrder.Divisions = Session("Division")
		objUIOrder.Bulk_Order = "N"
		objUIOrder.Re_Order = "N"
		objUIOrder.Consols = ""
		objUIOrder.Multi_Store = "N"
		objUIOrder.Term_Code = Session("TermCode")'Session("RSCust").fields("ctermcode")
		objUIOrder.Ship_Via = Session("RSCust").fields("shipvia")
		objUIOrder.Spc_Inst = Session("RSCust").fields("spcinst")
		objUIOrder.St_Name =  Session("RSCust").fields("stname")
		objUIOrder.Do_Insurance = Session("RSCust").fields("cinsur")
		objUIOrder.Buyer_Name = Session("RSCust").fields("buyer")
		objUIOrder.Phone_No = Session("RSCust").fields("phone1")
		objUIOrder.Fac_Code = Session("RSCust").fields("cfaccode")
		'objUIOrder.Term_Code = "89"
		'objUIOrder.FactAcct=Session("RSCust").fields("factacct") 'Need to recompile the object again
		objUIOrder.Do_Approval =""
		objUIOrder.Approval_Amount = 0
		'objUIOrder.Decl_Date = ""
		objUIOrder.Decl_Code =""
		objUIOrder.Rep_1 = session("rep")'Session("RSCust").fields("salesrep")
		if Session("RSCust").fields("SalesRep") = session("Rep") then
			session("DefComm") = Session("RSCust").fields("comm")
			
	   else
			session("DefComm") = Session("RSCust").fields("comm2")
	   end if
		objUIOrder.Com_1 = session("DefComm")'Session("RSCust").fields("comm")
		'objUIOrder.Rep_2 = Session("RSCust").fields("rep2")
		'objUIOrder.Com_2 = Session("RSCust").fields("comm2")
		objUIOrder.Entered = date()
		objUIOrder.Started = Session("start")
		Session("RSLine").Sort = "Complete desc"
		objUIOrder.Completed = Session("RSLine")("Complete")'Session("completed")
		'objUIOrder.Canceled = ""
		
		
		if session("UseOnlinePayment") = "T" then 'use online payment - wma
			if trim(session("GateWay")) = "PlugNPay.com (MemberShip Managment)" then  'use PNP Membership
				'objUIOrder.Discount = Request.Form ("txtDisc")'Session("RSCust").fields("disc")
				objUIOrder.Discount = Request.QueryString("txtDisc")+0'Session("RSCust").fields("disc")
			end if					
		else	'don't use online payment
			objUIOrder.Discount = Request.Form ("txtDisc")+0'Session("RSCust").fields("disc")
		end if
		
		if trim(Session("ordQty"))= "" then
			objUIOrder.Book = 0
			Session("ordQty") = 0
		else
			objUIOrder.Book = cdbl(Session("ordQty"))
		end if
		if trim(Session("ordAmount"))= "" then
			objUIOrder.Book = 0
			Session("ordAmount") = 0
		else
			objUIOrder.Book_Amount = Session("ordAmount")
		end if
		
		objUIOrder.sCancel = 0
		objUIOrder.CancelAmnt = 0
		objUIOrder.Ship = 0
		objUIOrder.Ship_Amount = 0
		if trim(Session("ordQty"))= "" then
			objUIOrder.Book = 0
		else
			objUIOrder.OpenOrd = Session("ordQty")
		end if
		
		objUIOrder.OpenAmnt = cdbl(Session("ordAmount"))
		objUIOrder.TotCut = 0
		objUIOrder.Flag =""
		objUIOrder.From_web = True

		set RSSetups = server.CreateObject("ADODB.recordset")
		strsql="select mdata_def from setups where cfld_name='M_DIV_LINK'"
		RSSetups.open strsql,conne ,1,3
		'if RSSetups.EOF then
		'	Response.Write "SETUP EMPTY"
		'end if 
		'Response.End 
		'IF RSSetups("mdata_def")="N" Then
		if NOT RSSetups.EOF then
			objUIOrder.Link_Code = Session("RSCust").fields("link_code")
			objUIOrder.GL_Sales = Session("RSCust").fields("link_code")
		Else
			set RSCodes = server.CreateObject("ADODB.recordset")
			strsql="select crltd_vlu,crltd_nam from codes where cdefcode+crltfield+cfld_name='NYCDIVISION' And (crltd_nam='LINK_CODE' OR crltd_nam='CSLSGLLINK')"
			RSCodes.open strsql,conne 
			if rscodes.EOF then
				objUIOrder.Link_Code = "DEF"
			else
				Response.Write "noooo data found"
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
				RSCodes.Close 
				Set RSCodes = Nothing
			end if 

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
		'Response.Write Session("ShipAdd")
		'Response.End 
		objUIOrder.Alt_ShpTo = Session("AddChg")
		'get def ware house saved in setup file
		'first check if there is a warehous for the customer
		if trim(application("WareCode")) = "" then
			strWareHous = Session("WareHous")
		else
			strWareHous = trim(application("WareCode"))
		end if
		if trim(strWareHous) = "" then
			set RSWarecode = server.CreateObject("ADODB.recordset")
			strsql="select cwarecode from warehous"
			RSWarecode.open strsql,conne 
			RSWarecode.MoveFirst 
			objUIOrder.CwareCode = RSWarecode("cwarecode")
		else
			objUIOrder.CwareCode = trim(strWareHous)
		end if
		objUIOrder.CancelReason = ""
		'check if address changed then save it
		
		if Session("AddChg") = true then
			objUIOrder.Address1 = Session("Add2")
			objUIOrder.Address2 = Session("Add3")
			'wma  # 038155 avoid strings more than 30 characters start
			'objUIOrder.Address3 = Session("Add41")&","&Session("Add42")&","&Session("Add43")
			objUIOrder.Address3 = left(Session("Add41")&","&Session("Add42")&","&Session("Add43"),30)
			'wma  # 038155 avoid strings more than 30 characters end
			objUIOrder.Address4 = Session("Add5")
			objUIOrder.St_Name =  Session("Add1")
			'objUIOrder.Address1 = Session("RSCust").fields("caddress1")
			'objUIOrder.Address2 = Session("RSCust").fields("caddress2")
			'objUIOrder.Address3 = Session("Add3")'Session("RSCust").fields("caddress3")
			'objUIOrder.Address4 = Session("RSCust").fields("caddress6")
			'objUIOrder.address5 = Session("RSCust").fields("caddress5")
		end if
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


		Session("RSLine").Sort = "lineNo"
		'creating Order Lines.
		intLineno = 1
		Session("RSLine").movefirst
		Do While Not Session("RSLine").Eof
			Set objUIOrdLine = objuiorder.ChildAddNew(OrderLine)

			objUIOrdLine.Account_line = CustID
			objUIOrdLine.OrderType_line = "T"
			objUIOrdLine.Style_line = Session("RSLine").fields("style")
			objUIOrdLine.Season_line = Session("RSLine").fields("season") 'Session("season") 'wma multiple seasons
			objUIOrdLine.Desc1_line = Session("RSLine").fields("desc1")
			objUIOrdLine.Scale_line = Session("RSLine").fields("scale")
			objUIOrdLine.PrePak_line = Session("RSLine").fields("prepak")
			objUIOrdLine.Price_line = Session("RSLine").fields("price")
			objUIOrdLine.Gros_price_line = Session("RSLine").fields("Gros_price")
			objUIOrdLine.Disc_pcnt_line = Session("RSLine").fields("Disc_pcnt")
			objUIOrdLine.Comm1_line = session("DefComm")'Session("RSLine").fields("comm1")
			'WAL_ update group[start]
			strGrp = "txtGrp"&trim(Session("RSLine").fields("style"))
			
			objUIOrdLine.Group_Line = Session("RSLine").Fields("Group")'Request.Form (strGrp)
			'WAL_ update group[end]
			objUIOrdLine.Start_line = Session("start")
			objUIOrdLine.Complete_line = Session("RSLine").fields("complete")
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
			if NOT ucase(trim(Session("StoreID")))="MAIN" then
				objUIOrdLine.Store_line = trim(Session("StoreID"))
			END IF


			if objuiorder.ChildSet(1,objUIOrdLine) then
			end if
			Session("RSLine").movenext
		Loop
		objUIOrder.LastLine = intLineno - 1

		bolTemp = objuiorder.Save()
		'wal_1313300 update 2 new fildes manually
		set rsOrder = server.CreateObject ("Adodb.recordset")

		'rsorder.Open "select cwebid, cwebName from ordhdr where order='"&trim(strOrder)&"'",conne,1,3
		
		'rsorder("cwebid") = trim(Application("userID"))
		'rsorder("cwebName") = trim(Application("userName"))
		'rsorder.Update()
		'rsorder.Close

		strAddMemo = "Add!#!Remote Order!#!" & objuiorder.Acount & "!#!" & objuiorder.stor & "!#!" & objUIOrder.Cust_po & "!#!" &objUIOrder.Divisions& "!#!" &objUIOrder.Seasons& "!#!" &objUIOrder.Started& "!#!" &objUIOrder.Completed& "!#!" &Session("ordQty")& "!#!" &Session("ordAmount")& "!#!" &objuiorder.Ordered
		'add record in order/charge table[start]
		Dim rsCharges
		set rsCharges = server.CreateObject ("ADODb.Recordset")
		rsCharges.Open "Select * from ordCharg where 1=0",conne,1,3
		rsCharges.AddNew 
			rsCharges("cOrder") = strOrder
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
				Session("intDep") = Request.Form ("hidDep")
			else
				Session("intDep") = Request.Form ("txtDep")
			end if		
		end if
			rsCharges("ndeposit") = cdbl(Session("intDep"))'session("rsCharges")("ndeposit")
			rsCharges("method_pay") = Session("method_pay")
			rsCharges("nweight")  = session("rsCharges")("nweight")
			rsCharges("n_cartons") = session("rsCharges")("n_cartons")
			rsCharges("nprichg")  = session("rsCharges")("nprichg")
			rsCharges("lprichg")  = session("rsCharges")("lprichg")
			rsCharges("ndropchg") = session("rsCharges")("ndropchg")
			rsCharges("ldropchg") = session("rsCharges")("ldropchg")
	
		rsCharges.Update ()
		'add record in order/charge table[end]	
		'wal_127795 add record in order/notepad table[start]
		if session("ShowNotes") = "T"  then
			Dim rsNotes
			set rsNotes = server.CreateObject ("ADODb.Recordset")
			rsNotes.Open "Select * from notepad where 1=0",conne,1,3
			rsNotes.AddNew 
				rsNotes("Type") = "B"
				rsNotes("Key") = "T"& strOrder
				rsnotes("cDesc") = "Notes for Order Number: " & strOrder
				rsNotes("mNotes")= Request.Form ("txtNotes")
			rsNotes.Update ()
		end if
		'wal_127795 add record in order/notepad table[end]
		'call function to add record in credit tables

		if session("UseOnlinePayment") = "T" then 'use online payment
			if trim(session("GateWay")) = "PlugNPay.com (MemberShip Managment)" then  'use PNP Membership
				'WMA Update order field in PNPtrans file [start]	
				'call function to add record in credit tables
				'call addCredit(strOrder,Session("intDep"))
				IF cdbl(Session("intDep")) <>0 THEN
					call addCredit("Order# "&strOrder,Session("intDep"),Session("RSCust").fields("Account"),Session("RSCust").fields("btname"))
					Dim rsPNPTrans
						set rsPNPTrans = server.CreateObject ("ADODb.Recordset")
						rsPNPTrans.Open "Select * from pnptrans where cTransid='"& Session("PNPorderID") &"'",conne,1,3	
						rsPNPTrans("Order")= strOrder
						rsPNPTrans("lBasic")= true
						rsPNPTrans.Update  
						rsPNPTrans.Close 
						set rsPNPTrans = nothing
				END IF
				'WMA Update order field in PNPtrans file [start]	
						
			end if
		end if
		'''''''''''''''''''''''''
		Add2Log "", CustID,"Adding Remote Order",trim(cstr(strOrder)),strAddMemo
		'''''''''''''''''''''''''
	End IF		

		Set objUIOrder = Nothing
		
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
		session("Note1") = ""
		session("Note2") = ""
		Session("PO") = ""
		Session("RSLine").close
		set Session("RSLine") = nothing
		
		Dim strAccount
		strAccount = CustID
		'wma SSL Frame visibility [start]
		'strTemp = "compSave.asp?order=" & strOrder & "&ID=" & strAccount
		Session("orderVal")= strOrder
		Session("IDVal")= strAccount
		
		if session("UseOnlinePayment") = "T" then 'use online payment - wma
			if trim(session("GateWay")) = "PlugNPay.com (MemberShip Managment)" then  'use PNP Membership
				if Session("intDep")<>0 then 'from pnp
					strTemp = "compSaveFrame.htm"
				else 'from crm
					strTemp = "compSave.asp"
				end if			
				'wma SSL Frame visibility [end]
				'Response.Redirect(strTemp)
			end if					
		else	'don't use online payment
				strTemp = "compSave.asp"
		end if
		 
		%>
		
	<SCRIPT LANGUAGE=javascript>
	<!--
	document.location.replace('<%=strTemp%>');
	//-->
	</SCRIPT>

<%
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


'Set Session("RSLine") = Nothing
'strTemp = "compSave.asp?order=" & strOrder
'Response.Redirect(strTemp)
'ELSE
'	Response.Redirect("custorder.asp")
'END IF

%>
