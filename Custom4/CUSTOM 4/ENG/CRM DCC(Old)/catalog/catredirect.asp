g230
<%@ Language=VBScript %>
<%
Response.Buffer = true
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%End if



Set conn = server.CreateObject("ADODB.connection")
Conn.Open Application("DataConnectionString")
'session var to allow adding size per line
'WAL_get company weekend days[start]
Set rsWeekEnd = server.CreateObject("ADODB.Recordset")
sqlWeek = "Select * from fishd where Cfisystat = 'C' "
	
rsWeekEnd.Open sqlWeek,conn
'WAL_get company weekend days[end ]
'Session("SizePerLine") = "T"	
Set RSStyle = Server.CreateObject("ADODB.RecordSet")
strSql = "select * from style where style='" & request("lststyle") & "'"
RSStyle.Open strSql,conn

	
Dim rsCompdate
set rsCompdate = server.CreateObject ("ADODB.Recordset")	


'wal_039365 add the lines per scale
if isobject(session("rsSty")) then
	session("rsSty").movefirst
	'filter on the selected color only
	session("rsSty").filter = "style like '%"& Trim(mid(trim(request("lststyle")),session("styleSize")+2,session("colorSize"))) & "%'"
	'loop into the stlyle session to get its scales
	Set RSScale = Server.CreateObject("ADODB.RecordSet")
	do while not session("rsSty").EOF 
		StyScale = session("rsSty").Fields("scale").Value 
		strsql = "SELECT * FROM SCALE WHERE TYPE+scale+prepak like 'S" & StyScale & "%'"
		RSScale.Open strsql,conn
		SzCount = RSScale("cnt")
		'check condtion for size per line
		If trim(Session("SizePerLine")) = "T" then
			if Session("LineNo") = "" then
				Session("LineNo") = 1
			end if
			'add line per size
			for i= 1 to 8
				strOrd = "txtord"&trim(RSScale("cDim1")) & trim(RSScale("sz"&i))
				strQty = "qty"&i
				strType = "txtType"&trim(RSScale("sz"&i))
				'Response.Write request(strOrd)&"<br>"
			
				if request(strOrd) <> "" then
					'WMA avoid empty RSLine in every click cataloge in the menu [start]
					'if not IsObject(Session("RSLine")) then		
					if Session("OrderFlag") = "" then
						Set session("RSLine") = server.CreateObject("ADODB.recordset")
						strsql = "select * from ordline where .f."
						Session("RSLine").open  strSql, conn, 2, 4
						Session("OrderFlag")="X"
					end if
					
					If not Session("RSLine").EOF AND not Session("RSLine").BOF then 
						Session("RSLine").MoveFirst()
						Session("RSLine").filter = "style='"&session("rsSty")("Style")&"' and "& strQty &" > 0"

						if Session("ordQty")="" then
							Session("ordQty") = 0
						End IF
						if Session("ordAmount")="" then
							Session("ordAmount")=0
						End IF
						'Response.Write RSStyle("Style") &"<br>"
						'Response.Write cdbl(request(strOrd)) &"<br>"				
						'Response.End 
												
						if Session("RSLine").eof then
							Session("RSLine").addnew
							Session("RSLine").Fields("lineno").Value = Session("LineNo")
							Session("LineNo") = Session("LineNo")+1				
						else
							strFound = "T"
							val1 = cdbl(Session("RSLine").fields("gros_price")) - cdbl(cdbl(Session("RSLine").fields("gros_price")) * cdbl(Session("RSLine").fields("Disc_pcnt"))/100)
						end if
					else
						Session("RSLine").addnew
						Session("RSLine").Fields("lineno").Value = Session("LineNo")
						Session("LineNo") = Session("LineNo")+1
					end if
						
					'WMA	
					CUSTID = Session("RSCust").Fields("account").value
							
					Session("RSLine").Fields("cordtype").Value = "H"
					Session("RSLine").Fields("account").Value = Session("RSCust").Fields("account").value
					Session("RSLine").Fields("style").value = session("rsSty")("Style")
					Session("RSLine").Fields("desc1").value = session("rsSty")("desc1")
					Session("RSLine").Fields("scale").value = session("rsSty")("scale")
					Session("RSLine").Fields("prepak").value= session("rsSty")("prepak")
					Session("RSLine").Fields("nsugretpri").value = 0
					Session("RSLine").Fields("season").value = Session("Season")
					'Session("RSLine").Fields("Group").value = Request.Form ("txtGrp")
							
					Session("RSLine").fields(strQty) = cdbl(request(strOrd))' + cdbl(intVal)

					inttemp = cdbl(request(strOrd)) '+ cdbl(intVal)
							
					Session("RSLine").fields("Disc_pcnt") = Session("RSCust").Fields("Disc").value'Request.Form ("txtDisc")
					Session("RSLine").fields("comm1") = Session("RSCust").Fields("Comm").value'Request.Form ("txtComm")	
							
					if session("PriceCode")="" then
						Session("RSLine").fields("gros_price") = GetStyPrice(RSStyle("Style"),1)
					else
						Session("RSLine").fields("gros_price") = GetPriceCode(RSStyle("Style"),session("PriceCode"),i)
					end if			
					'Session("RSLine").fields("gros_price") = GetStyPrice(RSStyle("Style"),1)
					val = cdbl(Session("RSLine").fields("gros_price")) - cdbl(cdbl(Session("RSLine").fields("gros_price")) * cdbl(Session("RSLine").fields("Disc_pcnt"))/100)
					Session("RSLine").fields("price") = val 'FormatNumber(cdbl(val),2)
					if strFound = "T" then
						Session("ordQty") = Cdbl(Session("ordQty")) - Cdbl(Session("RSLine").fields("totqty"))
						Session("ordAmount") = Session("ordAmount") - (cdbl(Session("RSLine").fields("totqty")) * cdbl(val1))
								
						Session("ordQty") = Cdbl(Session("ordQty")) + cdbl(inttemp)
						Session("ordAmount") = Session("ordAmount") + (cdbl(inttemp) * cdbl(val))
					else
						if Session("ordQty")="" then
							ordqty = 0
							'Response.Write ordqty
							'Response.End 
						Else
							ordqty = Session("ordQty")
						End IF
						if Session("ordAmount")="" then
							ordAmount = 0
						Else
							ordAmount = Session("ordAmount")
						End IF

						Session("ordQty") = Cdbl(ordQty) + cdbl(inttemp)
						Session("ordAmount") = ordAmount + (cdbl(inttemp) * cdbl(val))
					end if
					Session("RSLine").fields("totqty") = inttemp
					 '+ cdbl(intVal)
					'calc val od comp date
					if request(strType) = "green" then
						Session("RSLine").fields("complete") = dateadd("ww",1,date())
					elseif request(strType) = "orange" then
						if session("make") = false then'get value from PO
							strSql = "Select PosHdr.Complete from PosHdr, PosLn where PosLn.style+PosLn.cstytype+PosLn.po+STR(PosLn.lineno,6)+PosLn.trancd like '" &RSStyle("Style")& "%' And (PosHdr.status = 'O' OR PosHdr.status = 'H') And qty"& i &" > 0 And PosHdr.PO = PosLn.PO order by PosHdr.Complete desc"
									
							rsCompdate.Open strSql,conn
						else'get value from cuttkt
							strSql = "Select Cuttkth.Complete from Cuttkth, Cuttktl where Cuttktl.style+Cuttktl.cuttkt+Cuttktl.trancd like '" &RSStyle("Style")& "%' And (Cuttkth.status = 'O' OR Cuttkth.status = 'H') And Cuttktl.qty"& i &" > 0 And Cuttktl.cuttkt = Cuttkth.cuttkt order by Cuttkth.complete desc"
							rsCompdate.Open strSql,conn
						end if
								
						if not rsCompdate.EOF then
							rsCompdate.MoveFirst ()
							'get the # of working days to add to get expected ship date[start]
							if rsWeekEnd.eof then
								strfirts = "6"
								strSecond = "7"
							else
								today = Month(now()) & "/" & Day(now()) & "/" & Year(now())
								y = year(today)

								if cdate(rsWeekEnd.Fields("cfisfyear"))= cdate(y) then
									for x=1 to len(trim(rsWeekEnd.Fields("Cfisnonwd").Value))
										strNwd = Trim(rsWeekEnd.Fields("Cfisnonwd").Value)
										strfirts = Mid(strNwd,1,1)
										strSecond = Mid(strNwd,2,1)
									next
								end if
								if strfirts = "" and strSecond = "" then
									strfirts = "6"
									strSecond = "7"
								end if
							end if
							strNoOfDays = 0
									
							'get the # of working days to add to get expected ship date[start]
							'check that its valid date(bigger than todays date)
							if date() > rsCompdate("Complete") then
								dBeg = date()
								dEnd = dateadd("d",cint(Session("Days")),dBeg)
								'Session("RSLine").fields("complete") = dateadd("ww",1,date())
							else
								dBeg = rsCompdate("Complete")
								dEnd = dateadd("d",cint(Session("Days")),dBeg)
								'Session("RSLine").fields("complete") = dateadd("ww",1,rsCompdate("Complete"))
							end if
							do while (dBeg <= dEnd)
								chkDate = WeekDay(cdate(dBeg),2)		
								if chkDate = cint(strfirts) or chkDate = cInt(strSecond) then
									strNoOfDays = strNoOfDays+1
								end if
								dBeg = dateadd("d",1,dBeg)'Month(xbeg) & "/" & Day(xbeg)+1 & "/" & Year(xbeg)	
							loop
							dExpected = dateadd("d",int(strNoOfDays),dEnd)
							chkDate = WeekDay(cdate(dExpected),2)	
							if chkDate = cint(strfirts) or chkDate = cInt(strSecond) then
								dExpected = dateadd("d",1,dExpected)
							end if
							chkDate = WeekDay(cdate(dExpected),2)	
							if chkDate = cint(strfirts) or chkDate = cInt(strSecond) then
								dExpected = dateadd("d",1,dExpected)
							end if
							Session("RSLine").fields("complete") = dExpected
							'Added By HDM to close the recordset in order to allow the code to re-open it
							rsCompdate.Close()
						end if
					else
						Session("RSLine").fields("complete") = Session("Completed")
					end if
							
					Session("RSLine").Update
						
					'if session("SelLine") <> "" then
					Session("RSLine").filter = ""
					session("SelLine") = ""
					'end if
				end if
			next
			'Response.End 
		Else
			strQty = 0
			for i= 1 to 8
				IF Len(request("txtord"&trim(RSScale("cDim1")) & trim(RSScale("sz"&i)))) > 0 and instr(1,request("txtord"&trim(RSScale("cDim1")) & trim(RSScale("sz"&i))),",") <= 0 Then
				'IF Len(request("txtord"&trim(RSScale("cDim1")) & trim(RSScale("sz"&i)))) > 0 Then
				'Session("RSLine").fields("qty1") = 0
					'strQty = strQty + 0
				'Else
				strOrd = "txtord"&trim(RSScale("cDim1")) & trim(RSScale("sz"&i))
				'Response.Write request()
				'Response.End
				
					'Session("RSLine").fields("qty"&i) = request("txtord"&trim(RSScale("cDim1")) & trim(RSScale("sz"&i)))
					strQty = cdbl(strQty) +  cdbl(request("txtord"&trim(RSScale("cDim1")) & trim(RSScale("sz"&i))))
				END IF
				'strOrd = "txtord"&trim(RSScale("sz"&i))
			next
			if strQty <> 0 then
		
				if Session("OrderFlag") = "" then
					Set session("RSLine") = server.CreateObject("ADODB.recordset")
					strsql = "select * from ordline where .f."
					Session("RSLine").open  strSql, conn, 2, 4
					Session("OrderFlag")="X"
				end if
				If Session("RSLine").EOF AND Session("RSLine").BOF Then 
			
					Session("RSLine").addnew
				Else
					Session("RSLine").MoveFirst()
						
					Do while not Session("RSLine").EOF
						if session("rsSty")("Style") = Trim(Session("RSLine").Fields("style")) then
							foundFlag ="YES"
							Exit Do
						end if 
						Session("RSLine").MoveNext()
					Loop
							
					if foundFlag<>"YES" Then
						Session("RSLine").AddNew			
					End IF
				End if ' first time add
						

				Session("RSLine").Fields("cordtype").Value = "H"
				Session("RSLine").Fields("account").Value = Session("RSCust").Fields("account").value
				Session("RSLine").Fields("style").value = session("rsSty")("Style")
						  
				Session("RSLine").Fields("desc1").value = session("rsSty")("desc1")
				Session("RSLine").Fields("scale").value = session("rsSty")("scale")
				Session("RSLine").Fields("prepak").value = session("rsSty")("prepak")
				Session("RSLine").Fields("nsugretpri").value = 0
				Session("RSLine").Fields("start").value = date()
				'Response.Write  "Date"&Session("Completed")
				Session("RSLine").Fields("complete").value = Session("Completed")
				Session("RSLine").Fields("season").value = Session("Season")
				'get qty entered
				for i= 1 to 8
					
					IF Len(request("txtord"&trim(RSScale("cDim1")) & trim(RSScale("sz"&i))))=0 Then
					'Session("RSLine").fields("qty1") = 0
					Else
						Session("RSLine").fields("qty"&i) = request("txtord"&trim(RSScale("cDim1")) & trim(RSScale("sz"&i)))
					END IF
					'strOrd = "txtord"&trim(RSScale("sz"&i))
				next

				'IF Len(request("txtord1"))=0 Then
					'Session("RSLine").fields("qty1") = 0
				'Else
				'	Session("RSLine").fields("qty1") = request("txtord1")
				'END IF
							
				'IF Len(request("txtord2"))=0 Then
				'	'Session("RSLine").fields("qty2") = 0
				'Else
				'	Session("RSLine").fields("qty2") = request("txtord2")
				'END IF

				'IF Len(request("txtord3"))=0 Then
				'	'Session("RSLine").fields("qty3") = 0
				'Else
				'	Session("RSLine").fields("qty3") = request("txtord3")
				'END IF

				'IF Len(request("txtord4"))=0 Then
					'Session("RSLine").fields("qty4") = 0
				'Else
				'	Session("RSLine").fields("qty4") = request("txtord4")
				'END IF

				'IF Len(request("txtord5"))=0 Then
					'Session("RSLine").fields("qty5") = 0
				'Else
				'	Session("RSLine").fields("qty5") = request("txtord5")
				'END IF

				'IF Len(request("txtord6"))=0 Then
					'Session("RSLine").fields("qty6") = 0
				'Else
				'	Session("RSLine").fields("qty6") = request("txtord6")
				'END IF

				'IF Len(request("txtord7"))=0 Then
				'	'Session("RSLine").fields("qty7") = 0
				'Else
				'	Session("RSLine").fields("qty7") = request("txtord7")
				'END IF

				'IF Len(request("txtord8"))=0 Then
					'Session("RSLine").fields("qty8") = 0
				'Else
				'	Session("RSLine").fields("qty8") = request("txtord8")
				'END IF

				Dim inttemp ' as integer

				inttemp = cdbl(Session("RSLine").fields("qty1")) + cdbl(Session("RSLine").fields("qty2"))
				inttemp = inttemp + cdbl(Session("RSLine").fields("qty3")) + cdbl(Session("RSLine").fields("qty4"))
				inttemp = inttemp + cdbl(Session("RSLine").fields("qty5")) + cdbl(Session("RSLine").fields("qty6"))
				inttemp = inttemp + cdbl(Session("RSLine").fields("qty7")) + cdbl(Session("RSLine").fields("qty8"))
				'Response.Write(cdbl(GetStyPrice(RSStyle("Style"),inttemp)))
				'Response.End 

				IF foundFlag="YES" Then
					Session("ordQty") = Cdbl(Session("ordQty")) - Cdbl(Session("RSLine").fields("totqty"))
					if session("PriceCode")="" then
						Session("ordAmount") = Session("ordAmount") - (cdbl(Session("RSLine").fields("totqty")) * cdbl(GetStyPrice(RSStyle("Style"),inttemp)))
						Session("ordAmount") = Session("ordAmount") + (cdbl(inttemp) * cdbl(GetStyPrice(RSStyle("Style"),inttemp)))
					else
						Session("ordAmount") = Session("ordAmount") - (cdbl(Session("RSLine").fields("totqty")) * cdbl(getpricecode(RSStyle("Style"),session("PriceCode"),"")))
						Session("ordAmount") = Session("ordAmount") + (cdbl(inttemp) * cdbl(getpricecode(RSStyle("Style"),session("PriceCode"),"")))
					end if	
					
					'Session("ordAmount") = Session("ordAmount") - (cdbl(Session("RSLine").fields("totqty")) * cdbl(RSStyle("pricea")))
					Session("ordQty") = Cdbl(Session("ordQty")) + cdbl(inttemp)
					
					'Session("ordAmount") = Session("ordAmount") + (cdbl(inttemp) * cdbl(RSStyle("pricea")))
				Else
					if Session("ordQty")="" then
						ordqty = 0
					Else
						ordqty = Session("ordQty")
					End IF
					if Session("ordAmount")="" then
						ordAmount = 0
					Else
						ordAmount = Session("ordAmount")
					End IF

					Session("ordQty") = Cdbl(ordQty) + cdbl(inttemp)
					if session("PriceCode")="" then
						Session("ordAmount") = ordAmount + (cdbl(inttemp) * cdbl(GetStyPrice(RSStyle("Style"),inttemp)))
					else
						Session("ordAmount") = ordAmount + (cdbl(inttemp) * cdbl(getpricecode(RSStyle("Style"),session("PriceCode"),"")))
					end if
				End IF

				Session("RSLine").fields("totqty") = inttemp
				Session("RSLine").fields("Disc_pcnt") = Session("RSCust").Fields("Disc").value'Request.Form ("txtDisc")
				Session("RSLine").fields("comm1") = Session("RSCust").Fields("Comm").value'Request.Form ("txtComm")	
				Session("RSLine").fields("complete") = Session("Completed")	
				if session("PriceCode")="" then
					Session("RSLine").fields("gros_price") = GetStyPrice(RSStyle("Style"),1)
				else
					Session("RSLine").fields("gros_price") = GetPriceCode(RSStyle("Style"),session("PriceCode"),"")
				end if			
				'Session("RSLine").fields("gros_price") = GetStyPrice(RSStyle("Style"),1)
				val = cdbl(Session("RSLine").fields("gros_price")) - cdbl(cdbl(Session("RSLine").fields("gros_price")) * cdbl(Session("RSLine").fields("Disc_pcnt"))/100)
				Session("RSLine").fields("price") = FormatNumber(cdbl(val),2)
			end if			
			'Session("RSLine").fields("price") = cdbl(GetStyPrice(RSStyle("Style"),inttemp))'RSStyle("pricea")
		End if
	RSScale.close
	session("rsSty").movenext
	loop
End if
Set Session("RSStyStruct") = server.CreateObject("ADODB.recordset")
strsql = "select * from icistru where citemrecty='U'"
Session("RSStyStruct").open strsql,Conn

Session("OrderFlag")="X"

IF Session("M_STYVIEW") = "P" Then
	Response.Redirect("catpage.asp")
Else
	If Request.QueryString("PageID")="C" Then
		Response.Redirect("Newcat.asp")
	Else
		Response.Redirect("CatSearch.asp")
	End if
End IF
'*!*************************************************************
'*! Name      : GetStyPrice
'*! Developer : Ahmed M. Reda
'*! Date      : 06/21/2001
'*! Purpose   : Get style Price if there a contract or not
'*!*************************************************************
'*! Calls       : None
'*!*************************************************************
'*! Passed Parameters : strStyle    : Style code
'*!                     Qty         : Quantity
'*!*************************************************************
'*! Return      : Style price
'*!*************************************************************


FUNCTION GetStyPrice(strStyle,Qty)
Dim StyConn
Dim RSStyOrdHdr
Set StyConn = server.CreateObject("ADODB.Connection")
StyConn.Open Application("DataConnectionString")
Set RSStyOrdHdr = server.CreateObject("ADODB.RECOrdSET")
strtempsql = ""
'strtempsql = strtempsql & "SELECT ORDER FROM ORDHDR WHERE ACCOUNT='" & CUSTID & "' AND CORDTYPE='C'"
'strtempsql = strtempsql & " and status <> 'X' and status <> 'B' and start >= {" & Date() & "} and complete <= {" & Date() & "}"
''nek-Optimization
strtempsql = strtempsql & "Select ORDER from ORDHDR where Account+CORDTYPE+order like '"&CUSTID&"C%' "
strtempsql = strtempsql & " and status <> 'X' and status <> 'B' and start >= {" & Date() & "} and complete <= {" & Date() & "}"
''end of Optimization
RSStyOrdHdr.open strtempsql,StyConn
Response.Write(RSStyOrdHdr.recordcount)
GetStyPrice = 0
IF Not(RSStyOrdHdr.EOF And RSStyOrdHdr.BOF) Then ' Contract Exist
	Dim RSStyLine
	Set RSStyLine = server.CreateObject("adodb.recordset")
	strtempsql = ""
	strtempsql = strtempsql & "SELECT * FROM ORDLINE WHERE ORDER = '" & RSSTYORDHDR.FIELDS("ORDER").VALUE & "' and style='" & style & "'"
	RSStyLine.Open strtempsql,StyConn
	If Not(RSStyLine.EOF And RSStyLine.Bof)then 
		GetStyPrice = RSStyLine.Fields("Price").Value 
	End IF
Else ' Contract Not Exist
	strtempsql = ""
	strtempsql = strtempsql & "Select * from customer where type+account+store like 'M"&custid&"%'"
	'Response.Write(strtempsql)
	Set RSSTYCustomer = server.CreateObject("ADODB.Recordset")
	RSSTYCustomer.Open strtempsql,StyConn
	IF Not(RSSTYCustomer.EOF And RSSTYCustomer.BOF) Then
	
		strtempsql = "select * from style where style='" & strStyle & "'"
		'Response.Write(strtempsql)
		Set RSSTYStyle = server.CreateObject("adodb.recordset")
		RSSTYStyle.Open strtempsql,StyConn
		IF Not(RSSTYStyle.EOF And RSSTYStyle.BOF) Then
	'Response.Write("ok")
			Select Case RSSTYCustomer.Fields("pricelvl").Value 
			Case "A"
				GetStyPrice = RSSTYStyle.Fields("Pricea").Value 
			Case "B"
				GetStyPrice = RSSTYStyle.Fields("Priceb").Value 
			Case "C"
				GetStyPrice = RSSTYStyle.Fields("Pricec").Value 
			Case "Q"
				IF qty < RSSTYStyle.Fields("natqtyb").Value then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Pricea").Value)
				End if
				
				IF qty > RSSTYStyle.Fields("natqtyb").Value and qty < RSSTYStyle.Fields("natqtyc").Value then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Priceb").Value)
				End if
				
				IF qty > RSSTYStyle.Fields("natqtyc").Value then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Pricec").Value) 
				End if
			End select
		End IF
	End IF
End IF
GetStyPrice = cdbl(GetStyPrice)
End Function	
'wal_131300 [Start] new function get the price in price code file
Function GetPriceCode(strStyle,strCode,strSize)
	Set StyConn = server.CreateObject("ADODB.Connection")
	StyConn.Open Application("DataConnectionString")
	
	set rsStyPrice = server.CreateObject("ADODB.Recordset")
	set rsPriceCode = server.CreateObject("ADODB.Recordset")
	if session("CcurrCode")  = "" then
		rsstyprice.Open "select * from CSTPRICE where priccode = '"&trim(strCode)&"' and stydv = '"&strStyle&"' ",StyConn,1,3
	else
		rsstyprice.Open "select * from CSTPRICE where priccode = '"&trim(strCode)&"' and stydv = '"&strStyle&"' and ccurrcod = '"& trim(session("CcurrCode")) &"' ",StyConn,1,3
	end if
	'Response.Write "<br> select * from CSTPRICE where priccode = '"&trim(strCode)&"' and stydv = '"&strStyle&"' "
	
	if not rsstyprice.EOF then
		'validate the profile date
		rspricecode.Open "select * from cstprich where priccode = '"&trim(strCode)&"'",StyConn,1,3
		if not isnull(rspricecode("dvldprfr")) and not isnull(rspricecode("dvldprto")) and rspricecode("dvldprto") <> "" and rspricecode("dvldprfr") <> "" then
			'check that there are value
			if (date() > rspricecode("dvldprfr") and date() < rspricecode("dvldprto")) then
				'first check on the price for the selected size
				if strSize <> "" then'get the price for that size
					if cdbl(rsstyprice("price"&i)) <> 0 then
						GetPriceCode = rsstyprice("price"&i)
					end if
				end if
				if GetPriceCode = "" or GetPriceCode = 0 then'no price per size then gte the defualt size
					if isnull(rsstyprice("pricedv")) or cdbl(rsstyprice("pricedv")) = 0 or rsstyprice("pricedv") = ""then
						GetPriceCode = GetStyPrice(trim(strStyle),1)
					else
						GetPriceCode = rsstyprice("pricedv")
					end if
				end if
			else
				GetPriceCode = GetStyPrice(trim(strStyle),1)
			end if
		else'not valid date then get the default price
			'first check on the price for the selected size
			if strSize <> "" then'get the price for that size
				if cdbl(rsstyprice("price"&i)) <> 0 then
					GetPriceCode = rsstyprice("price"&i)
				end if
			end if
			if GetPriceCode = "" or GetPriceCode = 0 then'no price per size then gte the defualt size
				if isnull(rsstyprice("pricedv")) or cdbl(rsstyprice("pricedv")) = 0 or rsstyprice("pricedv") = ""then
					GetPriceCode = GetStyPrice(trim(strStyle),1)
				else
					GetPriceCode = rsstyprice("pricedv")
				end if
			end if
			'GetPriceCode = rsstyprice("pricedv")'GetStyPrice(trim(strStyle),1)
		end if
		
		'if not isnull(rsstyprice("dcrt_date")) and rsstyprice("dcrt_date") <> "" and date() < rsstyprice("dcrt_date") then
			'check that there are value
			'first check on the price for the selected size
		'	if strSize <> "" then'get the price for that size
		'		if cdbl(rsstyprice("price"&i)) <> 0 then
		'			GetPriceCode = rsstyprice("price"&i)
		'		end if
		'	end if
		'	if GetPriceCode = "" or GetPriceCode = 0 then'no price per size then gte the defualt size
		'		if isnull(rsstyprice("pricedv")) or cdbl(rsstyprice("pricedv")) = 0 or rsstyprice("pricedv") = ""then
		'			GetPriceCode = GetStyPrice(trim(strStyle),1)
		'		else
		'			GetPriceCode = rsstyprice("pricedv")
		'		end if
		'	end if
		'else'not valid date then get the defualt price
		'	GetPriceCode = GetStyPrice(trim(strStyle),1)
		'end if
	else
		GetPriceCode = GetStyPrice(trim(strStyle),1)
	end if
end function	
%>
