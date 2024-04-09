<%@ Language=VBScript %>
<%
Response.Buffer = True
IF LEN(Request("SlctColor"))<>0 THEN
IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" and Request.QueryString ("From") <> "M" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF

set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
'Session("SizePerLine") = "T"
'WAL_get company weekend days[start]
Set rsWeekEnd = server.CreateObject("ADODB.Recordset")
sqlWeek = "Select * from fishd where Cfisystat = 'C' "
	
rsWeekEnd.Open sqlWeek,conn
if len(Session("Days")) = 0 then
	Session("Days") = 7
end if
'WAL_get company weekend days[end ]
set RSStyle=server.CreateObject("ADODB.recordset")
strSql="select * from style where style='" & Ucase(Request("SlctColor")) & "'"
RSStyle.open strSql,conn 
'
Session("text1") = ""
Session("text2") = ""
Session("text3") = ""
Session("text4") = ""
Session("text5") = ""
Session("text6") = ""
Session("text7") = ""
Session("text8") = ""
Dim rsCompdate
set rsCompdate = server.CreateObject ("ADODB.Recordset")
'Response.Write trim(Session("SizePerLine"))
'Response.End 
Select Case request("Typeline")
Case "U":
	Dim inttemp ' as integer
	'WAL_4/12/03[start]
	'check condtion for size per line
	
	If trim(Session("SizePerLine")) = "T" then
		if Session("LineNo") = "" then
			Session("LineNo") = 1
		end if
		
		'add line per size
		for i= 1 to 8
			strOrd = "txtord"&i
			strQty = "qty"&i
			strType = "txtType"&i
			'Response.Write request(strOrd)&"<br>"
			if Len(request(strOrd)) <> 0 then
				
				If not Session("RSLine").EOF AND not Session("RSLine").BOF then 
					Session("RSLine").MoveFirst()
					 
					'check if its update in existing line
					'if session("SelLine") <> "" then
					'	Session("RSLine").filter = "lineNo = '" &session("SelLine")& "'"
					'	intVal = 0
					'else
						'check if this style- size already exits
						'Response.Write "style='"&Request("SlctColor")&"' and "& strQty &" <> ''"
						Session("RSLine").filter = "style='"&Request("SlctColor")&"' and "& strQty &" > 0"
						
						if Session("RSLine").eof then
							Session("RSLine").addnew
							Session("RSLine").Fields("lineno").Value = Session("LineNo")
							Session("LineNo") = cint(Session("LineNo"))+1
						else
							strFound = "T"
							val1 = cdbl(Session("RSLine").fields("price"))' - cdbl(cdbl(Session("RSLine").fields("gros_price")) * cdbl(Session("RSLine").fields("Disc_pcnt"))/100)
						end if
					'end if
				else
					Session("RSLine").addnew
					Session("RSLine").Fields("lineno").Value = Session("LineNo")
					
					Session("LineNo") = Session("LineNo")+1
				end if
				
					'wma 
					if Request.QueryString ("From") = "M" then 'updates
						strSql = "select * from OrdHdr where Order='"& session("OrdNo") &"'"
						set rsCurrOrdHdr = conn.Execute(strSql)
					end if
				
					Session("RSLine").Fields("order").Value  = session("OrdNo")
					Session("RSLine").Fields("cordtype").Value = session("Type")
					if Request.QueryString ("From") <> "M" then
						Session("RSLine").Fields("account").Value = Session("RSCust").Fields("account").value
					else
						'Session("RSLine").Fields("account").Value = Session("RSLine").Fields("account").value
						Session("RSLine").Fields("account").Value = rsCurrOrdHdr("Account")
						Session("RSLine").Fields("cwarecode").Value = rsCurrOrdHdr("cwarecode")
					end if
					Session("RSLine").Fields("style").value = Request("SlctColor")
  
					Session("RSLine").Fields("desc1").value = RSStyle("desc1")
					Session("RSLine").Fields("scale").value = RSStyle("scale")
					Session("RSLine").Fields("prepak").value = RSStyle("prepak")
					Session("RSLine").Fields("nsugretpri").value = 0
					'wma
					if Request.QueryString ("From") <> "M" then
						Session("RSLine").Fields("season").value = Session("seasonCode") 'Session("Season") 'wma multiple seasons
					else
						Session("RSLine").Fields("season").value = RSStyle("season") 'Session("Season") 'wma multiple seasons
					end if

					Session("RSLine").Fields("Group").value = Request.Form ("txtGrp")
					
					Session("RSLine").fields(strQty) = cdbl(request(strOrd))' + cdbl(intVal)

					inttemp = cdbl(request(strOrd)) '+ cdbl(intVal)
					if trim(Request.Form ("txtDisc")) <> "" then
						Session("RSLine").fields("Disc_pcnt") = Request.Form ("txtDisc")
					end if
					Session("RSLine").fields("comm1") = Request.Form ("txtComm")	
					
					if session("PriceCode")="" then
						Session("RSLine").fields("gros_price") = GetStyPrice(Request("SlctColor"),1)
					else
						Session("RSLine").fields("gros_price") = GetPriceCode(Request("SlctColor"),session("PriceCode"),i)
					end if
					val = cdbl(Session("RSLine").fields("gros_price")) - cdbl(cdbl(Session("RSLine").fields("gros_price")) * cdbl(Session("RSLine").fields("Disc_pcnt"))/100)
					Session("RSLine").fields("price") = cdbl(val)
					if strFound = "T" then
						Session("ordQty") = Cdbl(Session("ordQty")) - Cdbl(Session("RSLine").fields("totqty"))
						Session("ordAmount") = Session("ordAmount") - (cdbl(Session("RSLine").fields("totqty")) * cdbl(val1))'formatnumber(round(Session("ordAmount"),2),2)
						
						Session("ordQty") = Cdbl(Session("ordQty")) + cdbl(inttemp)
						Session("ordAmount") = Session("ordAmount") + (cdbl(inttemp) * cdbl(Session("RSLine").fields("price")))
					else
						if Session("ordQty")="" then
							ordqty = 0
						Else
							ordqty = Session("ordQty")
						End IF
						if Session("ordAmount")="" then
							ordAmount = 0
						Else
							ordAmount = Session("ordAmount")'formatnumber(round(Session("ordAmount"),2),2)
						End IF
						Session("ordQty") = Cdbl(ordQty) + cdbl(inttemp)
						Session("ordAmount") = ordAmount + (cdbl(inttemp) * cdbl(Session("RSLine").fields("price")))
					end if
					'Response.Write cdbl(Session("RSLine").fields("price")) &"--"& cdbl(val)&"--"& cdbl(inttemp)&"<br>"&Session("ordAmount")
					Session("RSLine").fields("desc1") = Session("LongDesc")
					Session("RSLine").fields("totqty") = inttemp
					'Response.End 
					 '+ cdbl(intVal)
					'calc val od comp date
					if request(strType) = "green" then
						Session("RSLine").fields("complete") = dateadd("ww",1,date())
					elseif request(strType) = "orange" then
						if session("make") = false then'get value from PO
							strSql = "Select PosHdr.Complete from PosHdr, PosLn where PosLn.style+PosLn.cstytype+PosLn.po+STR(PosLn.lineno,6)+PosLn.trancd like '" &Session("getstyle")& "%' And (PosHdr.status = 'O' OR PosHdr.status = 'H') And qty"& i &" > 0 And PosHdr.PO = PosLn.PO order by PosHdr.Complete desc"
							
							rsCompdate.Open strSql,conn
						else'get value from cuttkt
							strSql = "Select Cuttkth.Complete from Cuttkth, Cuttktl where Cuttktl.style+Cuttktl.cuttkt+Cuttktl.trancd like '" &Session("getstyle")& "%' And (Cuttkth.status = 'O' OR Cuttkth.status = 'H') And Cuttktl.qty"& i &" > 0 And Cuttktl.cuttkt = Cuttkth.cuttkt order by Cuttkth.complete desc"
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
							
							'get the # of working days to add to get expected ship date[end]
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
					
				Session("RSLine").Update()
				
				'if session("SelLine") <> "" then
				Session("RSLine").filter = ""
				session("SelLine") = ""
				'end if
			end if
		next
		'Response.End 
	Else
		if Session("LineNo") = "" then
			Session("LineNo") = 1
		end if
		If Session("RSLine").EOF AND Session("RSLine").BOF then 
			Session("RSLine").addnew
			Session("RSLine").Fields("lineno").Value = Session("LineNo")
			Session("LineNo") = cint(Session("LineNo"))+1
		else
			Session("RSLine").MoveFirst()
			
			Do while not Session("RSLine").EOF
				if (Request("SlctColor") = Trim(Session("RSLine").Fields("style")) ) then
					foundFlag ="YES"
					Exit Do
				end if 
				Session("RSLine").MoveNext()
			Loop
			if foundFlag<>"YES" Then
				Session("RSLine").AddNew
				Session("RSLine").Fields("lineno").Value = Session("LineNo")
				Session("LineNo") = cint(Session("LineNo"))+1			
			End IF
		End if ' first time add

		Session("RSLine").Fields("cordtype").Value = "O"
		if Request.QueryString ("From") <> "M" then
			Session("RSLine").Fields("account").Value = Session("RSCust").Fields("account").value
		else
			Session("RSLine").Fields("account").Value = Session("RSLine").Fields("account").value
		end if
		Session("RSLine").Fields("style").value = Request("SlctColor")
  
		'Response.Write ("<br>|" & GetStyPrice(Request("SlctColor"),1) & "|<br>")
  
		Session("RSLine").Fields("desc1").value = RSStyle("desc1")
		Session("RSLine").Fields("scale").value = RSStyle("scale")
		Session("RSLine").Fields("prepak").value = RSStyle("prepak")
		Session("RSLine").Fields("nsugretpri").value = 0
		'Session("RSLine").Fields("start").value = Session("Start")
		'Session("RSLine").Fields("complete").value = Session("Completed")
		'wma
		if Request.QueryString ("From") <> "M" then
			Session("RSLine").Fields("season").value = Session("seasonCode") 'Session("Season") 'wma multiple seasons
		else
			Session("RSLine").Fields("season").value = RSStyle("Style") 'Session("Season") 'wma multiple seasons
		end if
		'Response.Write Session("RSLine").Fields("season") & "<hr>"
		'Response.End 
		Session("RSLine").Fields("Group").value = Request.Form ("txtGrp")

		IF Len(request("txtord1"))=0 Then
			'Session("RSLine").fields("qty1") = 0
		Else
			Session("RSLine").fields("qty1") = request("txtord1")
		END IF
	
		IF Len(request("txtord2"))=0 Then
			'Session("RSLine").fields("qty2") = 0
		Else
			Session("RSLine").fields("qty2") = request("txtord2")
		END IF

		IF Len(request("txtord3"))=0 Then
			'Session("RSLine").fields("qty3") = 0
		Else
			Session("RSLine").fields("qty3") = request("txtord3")
		END IF

		IF Len(request("txtord4"))=0 Then
			'Session("RSLine").fields("qty4") = 0
		Else
			Session("RSLine").fields("qty4") = request("txtord4")
		END IF

		IF Len(request("txtord5"))=0 Then
			'Session("RSLine").fields("qty5") = 0
		Else
			Session("RSLine").fields("qty5") = request("txtord5")
		END IF

		IF Len(request("txtord6"))=0 Then
			'Session("RSLine").fields("qty6") = 0
		Else
			Session("RSLine").fields("qty6") = request("txtord6")
		END IF

		IF Len(request("txtord7"))=0 Then
			'Session("RSLine").fields("qty7") = 0
		Else
			Session("RSLine").fields("qty7") = request("txtord7")
		END IF

		IF Len(request("txtord8"))=0 Then
			'Session("RSLine").fields("qty8") = 0
		Else
			Session("RSLine").fields("qty8") = request("txtord8")
		END IF



		inttemp = cdbl(Session("RSLine").fields("qty1")) + cdbl(Session("RSLine").fields("qty2"))
		inttemp = inttemp + cdbl(Session("RSLine").fields("qty3")) + cdbl(Session("RSLine").fields("qty4"))
		inttemp = inttemp + cdbl(Session("RSLine").fields("qty5")) + cdbl(Session("RSLine").fields("qty6"))
		inttemp = inttemp + cdbl(Session("RSLine").fields("qty7")) + cdbl(Session("RSLine").fields("qty8"))
		
		Session("RSLine").fields("Disc_pcnt") = Request.Form ("txtDisc")	
		Session("RSLine").fields("comm1") = Request.Form ("txtComm")	
		Session("RSLine").fields("totqty") = inttemp
		if session("PriceCode")="" then
			Session("RSLine").fields("gros_price") = GetStyPrice(Request("SlctColor"),1)
		else
			Session("RSLine").fields("gros_price") = GetPriceCode(Request("SlctColor"),session("PriceCode"),"")
		end if			
		'Session("RSLine").fields("gros_price") = GetStyPrice(Request("SlctColor"),1)
		val = cdbl(Session("RSLine").fields("gros_price")) - cdbl(cdbl(Session("RSLine").fields("gros_price")) * cdbl(Session("RSLine").fields("Disc_pcnt"))/100)
		Session("RSLine").fields("price") = cdbl(val)
		IF foundFlag="YES" Then
			Session("ordQty") = Cdbl(Session("ordQty")) - Cdbl(Session("RSLine").fields("totqty"))
			Session("ordAmount") = Session("ordAmount") - (cdbl(Session("RSLine").fields("totqty")) * cdbl(val))
			'Session("ordAmount") = Session("ordAmount") - (cdbl(Session("RSLine").fields("totqty")) * cdbl(RSStyle("pricea")))
			Session("ordQty") = Cdbl(Session("ordQty")) + cdbl(inttemp)
			Session("ordAmount") = Session("ordAmount") + (cdbl(inttemp) * cdbl(val))
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
			Session("ordAmount") = ordAmount + (cdbl(inttemp) * cdbl(val))	
		End IF
		
		Session("RSLine").fields("complete") = Session("Completed")
		Session("RSLine").fields("desc1") = Session("LongDesc")
	End if'end condtion for size per line

Case "R":
	If Session("RSLine").EOF AND Session("RSLine").BOF then 
	Else
		Session("RSLine").MoveFirst()
					
		Do while not Session("RSLine").EOF
			If (Request("SlctColor") = Trim(Session("RSLine").Fields("style")) ) then
				Session("ordQty") = Cdbl(Session("ordQty")) - cdbl(Session("RSLine").fields("totqty"))
				Session("ordAmount") = Cdbl(Session("ordAmount")) - (cdbl(Session("RSLine").fields("totqty")) * cdbl(RSStyle("pricea")))
				Session("RSLine").Delete()
				Exit Do
			End If 
			Session("RSLine").MoveNext()
		Loop
	End if ' first time add
	'WAL_4/12/03[end]
End Select


Session("getstyle")=""
Session("OrderFlag") = "X"
Session("LongDesc1") = Session("LongDesc")
Session("LongDesc") = ""
Session("ShortDesc") = ""
Session("Price") = ""
Session("seasonCode") = ""
Session("Disc") = ""
Session("Comm") = ""
Session("Grp") = ""
if Request.QueryString ("From") <> "M" then
	Response.Redirect"custorder.asp?From="&Request.QueryString ("From")
else
	Response.Redirect"modifyorder.asp?Come=U&From="&Request.QueryString ("From")
end if

Session("text1") = ""
Session("text2") = ""
Session("text3") = ""
Session("text4") = ""
Session("text5") = ""
Session("text6") = ""
Session("text7") = ""
Session("text8") = ""
Session("Disc") = ""
Session("Comm") = ""
Session("Grp") = ""
'Response.Write(inttemp)
ELSE
	if Request.QueryString ("From") <> "M" then
		Response.Redirect"custorder.asp?From="&Request.QueryString ("From")
	else
		Response.Redirect"modifyorder.asp?Come=U&From="&Request.QueryString ("From")
	end if
	'Response.Redirect"custorder.asp?From="&Request.QueryString ("From")
END IF
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
	strtempsql = strtempsql & "SELECT * FROM ORDLINE WHERE ORDER = '" & RSSTYORDHDR.FIELDS("ORDER").VALUE & "' and style='" & strStyle & "'"
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
				IF cint(qty) < cint(RSSTYStyle.Fields("natqtyb").Value) then
					GetStyPrice = cint(qty) * cdbl(RSSTYStyle.Fields("Pricea").Value)
				End if
				
				IF cint(qty) > cint(RSSTYStyle.Fields("natqtyb").Value) and cint(qty) < cint(RSSTYStyle.Fields("natqtyc").Value) then
					GetStyPrice = cint(qty) * cdbl(RSSTYStyle.Fields("Priceb").Value)
				End if
				
				IF cint(qty) > cint(RSSTYStyle.Fields("natqtyc").Value) then
					GetStyPrice = cint(qty) * cdbl(RSSTYStyle.Fields("Pricec").Value) 
				End if
			Case Else 'Use price level "A"
				GetStyPrice = RSSTYStyle.Fields("Pricea").Value 
			End select
		End IF
	End IF
End IF
GetStyPrice = cdbl(GetStyPrice)
End Function
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