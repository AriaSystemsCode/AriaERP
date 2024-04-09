<%@ Language=VBScript %>
<!--#include file="../common/Add2Log.asp"-->
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>
<% Response.Buffer=true%>
<%'Response.Write "RecordsetEmptyFlag    " &Request("RecordsetEmptyFlag")
'Response.End

		IF trim(session("ID"))="" And trim(session("rep"))="" Then
			'Response.redirect "../default.asp"%>
			<script language="javascript">
				parent.location.href ="../login.asp"
			</script>	
		<%END IF 


    set connSys=server.CreateObject("ADODB.connection")
	  connSys.Open Application("SystemConnectionString")

	
		Dim objUIRA 
		Set objUIRA = Server.CreateObject("UIReturn.UIRAParent")
		objUIRA.ConParameter= Application("DataConnectionString")
		
		TotalQtyOfAllLines = session("totalqty")
		'ARD - Aug. 21,2001 [Start]
		'TotalAmountOfAllLines = session(totalqty)
		TotalAmountOfAllLines = Session("TotalAmount")
		'ARD - Aug. 21,2001 [Start]
		if len(trim(session("rep")))>0 then
			UserID =session("customerid")
		else
			UserID =session("id")
		end if
		'WAL_check value of return invoice flag
		if Ucase(Session("RetInvoice")) = "YES" and trim(Session("Invoice")) <> "" then
			Intr_inv ="Y"
		else
			Intr_inv ="N"
		end if
		NoOfCartons = Session("CartonsNo")
		Status="E"
		RetOpenAmount = Session("totalAmount")
		txtlocation=session("selectlocation")
		division=Session("Division")'session("selectdivision")
		entered=session("txtentered")
		voiddate=session("txtvoid")
		if trim(Request.Form ("selectReason")) = "" then
			selectreason=Trim(Session("selectReason2"))
		else
			selectreason=Request.Form ("selectReason")
		end if
		'Response.Write Request.Form ("selectReason")
		'Response.End 
		txtamount=Request.form("txtamount")
		RetaBud = Session("TotalQty")
		RetaOpen  = Session("TotalQty")
		ExRate = 1
	
		if Ucase(Trim(Session("selectStore"))) = "MAIN" then 
			Store = ""
		else 
			Store = Session("selectStore")
		end if 
	
		Set conn=server.createobject("ADODB.connection")
		Set RS = server.createobject("ADODB.RecordSet")
		conn.Open Application("DataConnectionString")
		sqls="select * from retauth"
		RS.Open sqls,conn,2,3,1
		Set RSline = server.createobject("ADODB.RecordSet")
		sqlstat="select * from raline"
		RSLine.Open sqlstat,conn,2,3,1
	
		set rsCompCurrency = server.CreateObject("ADODB.RecordSet") 
		'Response.write "Company ID"&Session("CompanyID")
		'Response.End
		rsCompCurrency = connSys.Execute ("Select Ccurrcode From Syccomp Where Ccomp_id ='"&Session("CompanyID")&"'")
		CompCurrency = rsCompCurrency("Ccurrcode") 


		set rsCurrUnit = server.CreateObject("ADODB.RecordSet") 
		strsql = "Select Ncurrunit From Syccurr Where CcurrCode = '" & CompCurrency &"'"
		'rsCurrUnit = connSys.Execute ("Select Ncurrunit From Syccurr Where CcurrCode = '" & CompCurrency &"'")
		rsCurrUnit.open strsql,ConnSys

		if not(rsCurrUnit.EOF and rsCurrUnit.BOF) then
		  CurrentUnit = rsCurrUnit("Ncurrunit")
		else
		  CurrentUnit = 1
		end if



'		if objUIRA.Add() then
'		
'			'change the sequince object WMA 5/25/2004 [START]: 
'			'Dim objSeq 
'			'Set objSeq = server.createobject("DBGenSeq.DBSequence")
'			'Dim strSeq 
'			'strSeq = objSeq.GetSequence(objuira.ConParameter, "RANO")
'			'session("strseq")=strSeq	
'		
'	
'			'Creat the Recordset for the Sequence file to get the Return No.
'			Application.Lock 
'			set RSSequence = server.CreateObject("ADODB.recordset")
'			strsql="select nseq_no, nfld_wdth from sequence where cseq_type='RANO' And cfile_nam='RETAUTH'"
'			
'			RSSequence.CursorLocation = 3 'adUseServer  
'			RSSequence.CursorType = 2	'adOpenKeyset
'			RSSequence.LockType = 4		'adLockOptimistic 
'
'			' Get The Sequence Number.
'			RSSequence.open strsql,conn		
'			strSeq = RSSequence("nseq_no")
'
'	
'			'RSSequence("nseq_no") = cdbl(RSSequence("nseq_no")) + 1
'			'on error resume next
'			'RSSequence.Updatebatch
'			'on error goto 0
'			intOrdWidth = RSSequence("nfld_wdth")
'
'			intLong = Len(strSeq)
'			intDif = cint(intOrdWidth) - cint(intLong)
'
'			For i = 1 to intDif 
'				strSeq = "0" & strSeq
'			Next
'			
'			RSSequence.Close()
'			Set RSSequence = Nothing
'			
'		    strSeqU = cint(strSeq) + 1
'			conn.Execute ("UPDATE sequence set nseq_no=" & strSeqU & " where cseq_type='RANO' And cfile_nam='RETAUTH'")
'		
'			Application.UnLock 
'			session("strseq")=strSeq
'			'change the sequince object WMA 5/25/2004 [end] 
'			
'			objuira.RANO = strSeq
'			objUIRA.account=userid	
'			objUIRA.store=store
'			if trim(entered) <> "" then
'				objUIRA.radate=entered
'			end if
'			if trim(voiddate) <> "" then
'				objUIRA.VOID= voiddate
'			end if
'			objUIRA.reason=selectreason
'			objUIRA.CWARECODE= txtlocation
'			objUIRA.cdivision=Division
'			objUIRA.STATUS=status
'			objUIRA.CINTR_INV=Intr_inv 
'			objUIRA.AUTH = TotalQtyOfAllLines
'			objUIRA.AUTHAMT = TotalAmountOfAllLines
'			objUIRA.CARTONS=NoOfCartons
'			objUIRA.NRTOPNAMT=RetOpenAmount
'			objUIRA.NRETA_BUD=RetaBud
'			objUIRA.NRETA_OPN=RetaOpen
'			objUIRA.CCURRCODE=CompCurrency
'			objUIRA.NCURRUNIT=CurrentUnit
'			objUIRA.NEXRATE=ExRate
'			objUIRA.from_web = True
'			'HDM
'			objUIRA.INVOICE = Session("Invoice")
'			'HDM
'			'WAL E302075,4 add info of order and P/O # [start]
'			objUIRA.Order  = Session("Order")
'			objUIRA.CustPO = session("P/O")
'			'WAL E302075,4 add info of order and P/O # [end]
'			session("rsreturnline").movefirst
'			index =1
'			dim objchild1
'	  	do while not session("rsreturnline").EOF
'		  	'ARD - [Start]
'		  	'set objchild1 = objUIRA.childaddnew(ralinedetail)
'		  	set objchild1 = objUIRA.childaddnew(1)
'		  	'ARD - [End]
'		  	
'				objchild1.RANOchild = strSeq
'				session("rsreturnline").fields("amount")=cdbl(Session("rsReturnLine").fields("totqty")) * cdbl(Session("rsReturnLine").fields("price"))
'				'Session("rsReturnLine").Fields("cret_linno").Value = Cstr(index)
'				Session("rsReturnLine").Fields("cra_linno").Value = Cstr(index)
'				objchild1.cra_linnochild=session("rsreturnline").fields("cra_linno")
'				objchild1.reasonchild=session("rsreturnline").fields("reason")
'				objchild1.accountchild=session("rsreturnline").fields("account")
'				objchild1.stylechild=session("rsreturnline").fields("style")
'				objchild1.totqtychild=session("rsreturnline").fields("totqty")
'				objchild1.amountchild=Session("rsreturnline").fields("amount")
'				objchild1.pricechild=session("rsreturnline").fields("price")
'				objchild1.qty1child=session("rsreturnline").fields("qty1")
'		        objchild1.qty2child=session("rsreturnline").fields("qty2")
'				objchild1.qty3child=session("rsreturnline").fields("qty3")
'				objchild1.qty4child=session("rsreturnline").fields("qty4")
'				objchild1.qty5child=session("rsreturnline").fields("qty5")
'				objchild1.qty6child=session("rsreturnline").fields("qty6")
'				objchild1.qty7child=session("rsreturnline").fields("qty7")
'				objchild1.qty8child=session("rsreturnline").fields("qty8")
'			
'				'ARD - [Start]
'				'call objUIRA.Childset(ralinedetail,objchild1 )
'				call objUIRA.Childset(1,objchild1 )
'				'ARD - [End]
'				index=index+1
'				Session("rsReturnLine").movenext
'				Set objchild1 = Nothing
'			loop
'
'			if objUIRA.Save()then
'			else
'				Response.Write objUIRA.error
'				Response.End 
'			end if
'			'Append the transaction to log file
'			strAddMemo = "Add!#!RA Request!#!" &UserID&"!#!"& objUIRA.store & "!#!" & objUIRA.CWARECODE & "!#!" & objUIRA.reason & "!#!" &objUIRA.cdivision& "!#!" &objUIRA.radate& "!#!" &objUIRA.VOID& "!#!" &Session("TotalQty")& "!#!" &Session("totalAmount")& "!#!" &objuira.RANO
'			Add2Log "", UserID,"Adding RA Request",trim(cstr(objuira.RANO)),strAddMemo
'			'------------------
'			Session("txtEntered") = ""
'			Session("txtVoid") = ""
'			Session("selectDivision") = ""
'			Session("selectStore") = ""
'			Session("selectReason") = ""
'			Session("selectLocation") = ""
'			Session("CartonsNo") = ""
'			'Session("selectReason2") = ""
'			Session("getstyle")=""
'			Session("TotalQty")= ""
'			Session("TotalAmount") = ""
'			Session("rsReturnline")=""
'			Session("Order") = ""
'			Session("P/O") = ""
'			rs.Close
'			rsline.Close
'			conn.Close
'			connsys.Close
'
'			set objchild1=nothing
'			'Set objSeq =nothing
'			Set objUIRA=nothing
'			Set RS=nothing
'			Set RSLine=nothing
'			set rsCompCurrency=nothing
'			set rsCurrUnit=nothing
'			set conn=nothing
'			Set connsys=nothing
'		end if 
'	  Response.Redirect "returnauthsave.asp"




		'WMA 4/24/2005 change RA Object model to ordinary ADO  [START]:
		dim strRetSQL 
		dim strRetLinesSQL 
		
		
		'if objUIRA.Add() then
			'change the sequince object WMA 5/25/2004 [START]: 
			'Dim objSeq 
			'Set objSeq = server.createobject("DBGenSeq.DBSequence")
			'Dim strSeq 
			'strSeq = objSeq.GetSequence(objuira.ConParameter, "RANO")
			'session("strseq")=strSeq	
		
	
			'Creat the Recordset for the Sequence file to get the Return No.
			Application.Lock 
			set RSSequence = server.CreateObject("ADODB.recordset")
			strsql="select nseq_no, nfld_wdth from sequence where cseq_type='RANO' And cfile_nam='RETAUTH'"
			
			RSSequence.CursorLocation = 3 'adUseServer  
			RSSequence.CursorType = 2	'adOpenKeyset
			RSSequence.LockType = 4		'adLockOptimistic 

			' Get The Sequence Number.
			RSSequence.open strsql,conn		
			strSeq = RSSequence("nseq_no")

	
			'RSSequence("nseq_no") = cdbl(RSSequence("nseq_no")) + 1
			'on error resume next
			'RSSequence.Updatebatch
			'on error goto 0
			intOrdWidth = RSSequence("nfld_wdth")

			intLong = Len(strSeq)
			intDif = cint(intOrdWidth) - cint(intLong)

			For i = 1 to intDif 
				strSeq = "0" & strSeq
			Next
			
			RSSequence.Close()
			Set RSSequence = Nothing
			
		    strSeqU = cint(strSeq) + 1
			conn.Execute ("UPDATE sequence set nseq_no=" & strSeqU & " where cseq_type='RANO' And cfile_nam='RETAUTH'")
		
			Application.UnLock 
			session("strseq")=strSeq
			'change the sequince object WMA 5/25/2004 [end] 
			
			
			strRetSQL = "Insert into RETAUTH ( "
			strRetSQL = strRetSQL & "RANO, account, store, radate , VOID, reason, CWARECODE, cdivision, STATUS "
			strRetSQL = strRetSQL & ", CINTR_INV, AUTH, AUTHAMT, CARTONS, NRTOPNAMT, NRETA_BUD, NRETA_OPN "
			strRetSQL = strRetSQL & ", CCURRCODE, NCURRUNIT, NEXRATE, lfromweb, INVOICE, Order, CustPO) "
			strRetSQL = strRetSQL & " values("
			
			'objuira.RANO = strSeq
			strRetSQL = strRetSQL & "'"& strSeq &"'"
			'objUIRA.account=userid	
			strRetSQL = strRetSQL & ", '"& userid &"'"
			'bjUIRA.store=store
			strRetSQL = strRetSQL & ", '"& store &"'"
			'if trim(entered) <> "" then
				'bjUIRA.radate=entered
			strRetSQL = strRetSQL & ", {"& entered &"}"
			'end if
			'if trim(voiddate) <> "" then
				'objUIRA.VOID= voiddate
			strRetSQL = strRetSQL & ", {"& voiddate &"}"
			'end if
			'objUIRA.reason=selectreason
			strRetSQL = strRetSQL & ", '"& selectreason &"'"
			'objUIRA.CWARECODE= txtlocation
			strRetSQL = strRetSQL & ", '"& txtlocation &"'"
			'objUIRA.cdivision=Division
			strRetSQL = strRetSQL & ", '"& Division &"'"
			'objUIRA.STATUS=status
			strRetSQL = strRetSQL & ", '"& status &"'"
			
			'objUIRA.CINTR_INV=Intr_inv 
			strRetSQL = strRetSQL & ", '"& Intr_inv &"'"
			'objUIRA.AUTH = TotalQtyOfAllLines
			strRetSQL = strRetSQL & ", "& TotalQtyOfAllLines &""
			'objUIRA.AUTHAMT = TotalAmountOfAllLines
			strRetSQL = strRetSQL & ", "& TotalAmountOfAllLines &""
			'objUIRA.CARTONS=NoOfCartons
			strRetSQL = strRetSQL & ", "& NoOfCartons &""
			'objUIRA.NRTOPNAMT=RetOpenAmount
			strRetSQL = strRetSQL & ", "& RetOpenAmount &""
			'objUIRA.NRETA_BUD=RetaBud
			strRetSQL = strRetSQL & ", "& RetaBud &""
			'objUIRA.NRETA_OPN=RetaOpen
			strRetSQL = strRetSQL & ", "& RetaOpen &""
			
			'objUIRA.CCURRCODE=CompCurrency
			strRetSQL = strRetSQL & ", '"& CompCurrency &"'"
			'objUIRA.NCURRUNIT=CurrentUnit
			strRetSQL = strRetSQL & ", "& CurrentUnit &""
			'objUIRA.NEXRATE=ExRate
			strRetSQL = strRetSQL & ", "& ExRate &""
			'objUIRA.from_web = True
			strRetSQL = strRetSQL & ", .T."
			'objUIRA.INVOICE = Session("Invoice")
			strRetSQL = strRetSQL & ", '"& Session("Invoice") &"'"
			'objUIRA.Order  = Session("Order")
			strRetSQL = strRetSQL & ", '"& Session("Order") &"'"
			'objUIRA.CustPO = session("P/O")
			strRetSQL = strRetSQL & ", '"& session("P/O") &"'"
			
			strRetSQL = strRetSQL & ")"
			
			'Response.Write strRetSQL 
			'Response.End 
			'Add Ret Auth Record Here
			conn.Execute(strRetSQL)
			
			
		session("rsreturnline").movefirst
		index =1
		'dim objchild1
	  	do while not session("rsreturnline").EOF
	  	
				session("rsreturnline").fields("amount")=cdbl(Session("rsReturnLine").fields("totqty")) * cdbl(Session("rsReturnLine").fields("price"))
				Session("rsReturnLine").Fields("cra_linno").Value = Cstr(index)

				strRetLinesSQL = "Insert into RALine ("
				strRetLinesSQL = strRetLinesSQL & " RANO, account, cra_linno, reason, style "
				strRetLinesSQL = strRetLinesSQL & ", totqty, amount, price, qty1, qty2, qty3, qty4, qty5, qty6, qty7, qty8) "
				strRetLinesSQL = strRetLinesSQL & " values("
		  	
				'objchild1.RANOchild = strSeq
				strRetLinesSQL = strRetLinesSQL & " '"& strSeq &"'"
				
				'objchild1.accountchild=session("rsreturnline").fields("account")
				strRetLinesSQL = strRetLinesSQL & ", '"& session("rsreturnline").fields("account") &"'"
				
				'objchild1.cra_linnochild=session("rsreturnline").fields("cra_linno")
				strRetLinesSQL = strRetLinesSQL & ", '"& session("rsreturnline").fields("cra_linno") &"'"
				
				'objchild1.reasonchild=session("rsreturnline").fields("reason")
				strRetLinesSQL = strRetLinesSQL & ", '"& session("rsreturnline").fields("reason") &"'"
						
				'objchild1.stylechild=session("rsreturnline").fields("style")
				strRetLinesSQL = strRetLinesSQL & ", '"& session("rsreturnline").fields("style") &"'"
				
				'objchild1.totqtychild=session("rsreturnline").fields("totqty")
				strRetLinesSQL = strRetLinesSQL & ","& session("rsreturnline").fields("totqty") &""
				
				'objchild1.amountchild=Session("rsreturnline").fields("amount")
				strRetLinesSQL = strRetLinesSQL & ", "& session("rsreturnline").fields("amount") &""
				
				'objchild1.pricechild=session("rsreturnline").fields("price")
				strRetLinesSQL = strRetLinesSQL & ", "& session("rsreturnline").fields("price") &""
				
				'objchild1.qty1child=session("rsreturnline").fields("qty1")
				strRetLinesSQL = strRetLinesSQL & ", "& session("rsreturnline").fields("qty1") &""
		        
		        'objchild1.qty2child=session("rsreturnline").fields("qty2")
		        strRetLinesSQL = strRetLinesSQL & ", "& session("rsreturnline").fields("qty2") &""
				
				'objchild1.qty3child=session("rsreturnline").fields("qty3")
				strRetLinesSQL = strRetLinesSQL & ", "& session("rsreturnline").fields("qty3") &""
				
				'objchild1.qty4child=session("rsreturnline").fields("qty4")
				strRetLinesSQL = strRetLinesSQL & ", "& session("rsreturnline").fields("qty4") &""
				
				'objchild1.qty5child=session("rsreturnline").fields("qty5")
				strRetLinesSQL = strRetLinesSQL & ", "& session("rsreturnline").fields("qty5") &""
				
				'objchild1.qty6child=session("rsreturnline").fields("qty6")
				strRetLinesSQL = strRetLinesSQL & ", "& session("rsreturnline").fields("qty6") &""
				
				'objchild1.qty7child=session("rsreturnline").fields("qty7")
				strRetLinesSQL = strRetLinesSQL & ", "& session("rsreturnline").fields("qty7") &""
				
				'objchild1.qty8child=session("rsreturnline").fields("qty8")
				strRetLinesSQL = strRetLinesSQL & ", "& session("rsreturnline").fields("qty8") &""
			
				strRetLinesSQL = strRetLinesSQL & ")"
				
				'Response.Write strRetLinesSQL 
				'Response.end 
				
				'Add Ret Auth line Record Here
				conn.Execute(strRetLinesSQL)
				
				
				''ARD - [Start]
				''call objUIRA.Childset(ralinedetail,objchild1 )
				'call objUIRA.Childset(1,objchild1 )
				''ARD - [End]
				index=index+1
				Session("rsReturnLine").movenext
				'Set objchild1 = Nothing
			loop

			'if objUIRA.Save()then
			'else
			'	Response.Write objUIRA.error
			'	Response.End 
			'end if
			
			'Update Order Line to DB
			
			
			
			'Append the transaction to log file
			'strAddMemo = "Add!#!RA Request!#!" &UserID&"!#!"& objUIRA.store & "!#!" & objUIRA.CWARECODE & "!#!" & objUIRA.reason & "!#!" &objUIRA.cdivision& "!#!" &objUIRA.radate& "!#!" &objUIRA.VOID& "!#!" &Session("TotalQty")& "!#!" &Session("totalAmount")& "!#!" &objuira.RANO
			'Add2Log "", UserID,"Adding RA Request",trim(cstr(objuira.RANO)),strAddMemo
			'------------------
			Session("txtEntered") = ""
			Session("txtVoid") = ""
			Session("selectDivision") = ""
			Session("selectStore") = ""
			Session("selectReason") = ""
			Session("selectLocation") = ""
			Session("CartonsNo") = ""
			'Session("selectReason2") = ""
			Session("getstyle")=""
			Session("TotalQty")= ""
			Session("TotalAmount") = ""
			Session("rsReturnline")=""
			Session("Order") = ""
			Session("P/O") = ""
			rs.Close
			rsline.Close
			conn.Close
			connsys.Close

			'set objchild1=nothing
			'Set objUIRA=nothing
			Set RS=nothing
			Set RSLine=nothing
			set rsCompCurrency=nothing
			set rsCurrUnit=nothing
			set conn=nothing
			Set connsys=nothing
			
'		end if 
	  Response.Redirect "returnauthsave.asp"
	  
%>
