<%@ Language=VBScript %>
<%Response.Buffer = true
Response.Expires=-1


'*************************************************************************************
'* OTS Report From CRM
'*************************************************************************************
%>

<%
if Trim(Session("ID")) = "" AND Trim(Session("rep")) = "" then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 
Set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
if Trim(Session("ID")) = "" then
	custID = Session("customerid")
else
	custID = Session("ID")
end if
'get value of low availability qty
if trim(Session("LowQty")) = "" then
	intQty = 2
else
	intQty = Session("LowQty")
end if
'WAL_get company weekend days[start]
Set rsWeekEnd = server.CreateObject("ADODB.Recordset")
sqlWeek = "Select * from fishd where Cfisystat = 'C' "
	
rsWeekEnd.Open sqlWeek,conn
if len(Session("Days")) = 0 then
	Session("Days") = 7
end if
'WAL_get company weekend days[end ]
Dim rsCompdate
set rsCompdate = server.CreateObject ("ADODB.Recordset")
set session("rsRecords") = server.CreateObject("ADODB.RecordSet")
	Call session("rsRecords").fields.append("style",129,19)
	Call session("rsRecords").fields.append("major",129,19)
	Call session("rsRecords").fields.append("desc",129,60)
	Call session("rsRecords").fields.append("complete1",7)
	Call session("rsRecords").fields.append("complete2",7)
	Call session("rsRecords").fields.append("complete3",7)
	Call session("rsRecords").fields.append("complete4",7)
	Call session("rsRecords").fields.append("complete5",7)
	Call session("rsRecords").fields.append("complete6",7)
	Call session("rsRecords").fields.append("complete7",7)
	Call session("rsRecords").fields.append("complete8",7)
	Call session("rsRecords").fields.append("Sz1",129,5)
	Call session("rsRecords").fields.append("Sz2",129,5)
	Call session("rsRecords").fields.append("Sz3",129,5)
	Call session("rsRecords").fields.append("Sz4",129,5)
	Call session("rsRecords").fields.append("Sz5",129,5)
	Call session("rsRecords").fields.append("Sz6",129,5)
	Call session("rsRecords").fields.append("Sz7",129,5)
	Call session("rsRecords").fields.append("Sz8",129,5)
	Call session("rsRecords").fields.append("Ava1",129,1)
	Call session("rsRecords").fields.append("Ava2",129,1)
	Call session("rsRecords").fields.append("Ava3",129,1)
	Call session("rsRecords").fields.append("Ava4",129,1)
	Call session("rsRecords").fields.append("Ava5",129,1)
	Call session("rsRecords").fields.append("Ava6",129,1)
	Call session("rsRecords").fields.append("Ava7",129,1)
	Call session("rsRecords").fields.append("Ava8",129,1)
session("rsRecords").open

Dim rsOtsChk
set rsOtsChk = server.CreateObject ("ADODB.Recordset")
'If Not Session("RSStyle").Eof Then 
if request("Select") = "A" then
	Session("RSSTyResult").movefirst()
	Do While Not Session("RSSTyResult").Eof
		session("rsRecords").addnew()
		session("rsRecords")("style") = Session("RSSTyResult")("style")
		session("rsRecords")("major") = Session("RSSTyResult")("cstymajor")
		session("rsRecords")("desc")  = Session("RSSTyResult")("desc")
		Set RSScale = Conn.Execute("SELECT * FROM Scale WHERE Type+scale+prepak = 'S"&Session("RSSTyResult")("Scale")&"'") 
	    SzCount = RSScale("cnt") 
		For inti = 1 to 8
		  if trim(RSScale("Sz"&inti)) = "ASST" then
			session("rsRecords")("Sz"&inti) = ""
		  else
			session("rsRecords")("Sz"&inti) = RSScale("Sz"&inti)
		  end if
		Next 
	
		'WAL_calc of ots display[start]
	    
		strSQl = "Select (style.stk1 - sum(ordline.qty1)) as sum1, (style.stk2 - sum(ordline.qty2)) as sum2, "& _
				 "(style.stk3 - sum(ordline.qty3)) as sum3, (style.stk4 - sum(ordline.qty4)) as sum4, "& _ 
				 "(style.stk5 - sum(ordline.qty5)) as sum5, (style.stk6 - sum(ordline.qty6)) as sum6, "& _
				 "(style.stk7 - sum(ordline.qty7)) as sum7, (style.stk8 - sum(ordline.qty8)) as sum8, "& _
				 "(style.stk1 + style.wip1 - sum(ordline.qty1)) as sum11, (style.stk2 + style.wip2 - sum(ordline.qty2)) as sum22, "& _
				 "(style.stk3 + style.wip3 - sum(ordline.qty3)) as sum33, (style.stk4 + style.wip4 - sum(ordline.qty4)) as sum44, "& _ 
				 "(style.stk5 + style.wip5 - sum(ordline.qty5)) as sum55, (style.stk6 + style.wip6 - sum(ordline.qty6)) as sum66, "& _
				 "(style.stk7 + style.wip7 - sum(ordline.qty7)) as sum77, (style.stk8 + style.wip8 - sum(ordline.qty8)) as sum88, "& _
				 "style.make from ordline, ordHdr, style where ordline.style+DTOS(ordline.complete)+ordline.cordtype+ordline.order+ordline.store+STR(ordline.lineno,6) like '" &Session("RSSTyResult")("Style")& "%' "& _
				 "and ordHdr.status <> 'C' and ordHdr.Status <> 'X' and ordline.order = ordHdr.order and ordline.style = style.style"
		rsOtsChk.Open strSQL, conn

		if rsOtsChk.EOF then'no orders for this style then get value in stock
			rsOtsChk.Close ()
			strSQl = "Select style.stk1 as sum1, style.stk2 as sum2, "& _
					 "style.stk3 as sum3, style.stk4 as sum4, "& _ 
					 "style.stk5 as sum5, style.stk6 as sum6, "& _
					 "style.stk7 as sum7, style.stk8 as sum8, "& _
					 "sum(style.stk1 + style.wip1) as sum11, sum(style.stk2 + style.wip2) as sum22, "& _
					 "sum(style.stk3 + style.wip3) as sum33, sum(style.stk4 + style.wip4) as sum44, "& _ 
					 "sum(style.stk5 + style.wip5) as sum55, sum(style.stk6 + style.wip6) as sum66, "& _
					 "sum(style.stk7 + style.wip7) as sum77, sum(style.stk8 + style.wip8) as sum88, "& _
					 "style.make from style where style like '" &Session("RSSTyResult")("Style")& "' "
			
			rsOtsChk.Open strSQL, conn
		end if
		if not rsOtsChk.eof then
			Session("Make") = rsOtsChk("make")
			For i = 1 to 8'Cdbl(SzCount)
			 if trim(RSScale("Sz"&i))<> "" and trim(RSScale("Sz"&i)) <> "ASST" then
				if cdbl(rsOtsChk("sum"&i)) > cint(intQty) then
					session("rsRecords")("Ava"&i) = "A"
				elseif cdbl(rsOtsChk("sum"&i)) <= cint(intQty) and cdbl(rsOtsChk("sum"&i)) > 0 then
					session("rsRecords")("Ava"&i) = "L"
				elseif cdbl(rsOtsChk("sum"&i&i)) > cint(intQty) then
					session("rsRecords")("Ava"&i) = "A"
					'get expected date
					if session("make") = false then'get value from PO
						strSql = "Select PosHdr.Complete from PosHdr, PosLn where PosLn.style+PosLn.cstytype+PosLn.po+STR(PosLn.lineno,6)+PosLn.trancd like '" &Session("RSSTyResult")("Style")& "%' And (PosHdr.status = 'O' OR PosHdr.status = 'H') And qty"& i &" > 0 And PosHdr.PO = PosLn.PO order by PosHdr.Complete desc"
							
						rsCompdate.Open strSql,conn
					else'get value from cuttkt
						strSql = "Select Cuttkth.Complete from Cuttkth, Cuttktl where Cuttktl.style+Cuttktl.cuttkt+Cuttktl.trancd like '" &Session("RSSTyResult")("Style")& "%' And (Cuttkth.status = 'O' OR Cuttkth.status = 'H') And Cuttktl.qty"& i &" > 0 And Cuttktl.cuttkt = Cuttkth.cuttkt order by Cuttkth.complete desc"
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
						end if
						strNoOfDays = 0
							
						'get the # of working days to add to get expected ship date[end]
						'check that its valid date(bigger than todays date)
						if date() > rsCompdate("Complete") then
							dBeg = date()
							dEnd = dateadd("d",cint(Session("Days")),dBeg)
						else
							dBeg = rsCompdate("Complete")
							dEnd = dateadd("d",cint(Session("Days")),dBeg)
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
						session("rsRecords")("complete"&i) = dExpected
						'Added By HDM to close the recordset in order to allow the code to re-open it
						'rsCompdate.Close()
						'check that its valid date(bigger than todays date)
						
						'if date() > rsCompdate("Complete") then
						'	session("rsRecords")("complete"&i) = dateadd("ww",1,date())
						'else
						'	session("rsRecords")("complete"&i) = dateadd("ww",1,rsCompdate("Complete"))
						'end if
						'Added By HDM to close the recordset in order to allow the code to re-open it
						'rsCompdate.Close()
					end if
					rsCompdate.Close()
				elseif cdbl(rsOtsChk("sum"&i&i)) <= cint(intQty) and cdbl(rsOtsChk("sum"&i&i)) > 0 then
					session("rsRecords")("Ava"&i) = "L"
					'get expected date
					if session("make") = false then'get value from PO
						strSql = "Select PosHdr.Complete from PosHdr, PosLn where PosLn.style+PosLn.cstytype+PosLn.po+STR(PosLn.lineno,6)+PosLn.trancd like '" &Session("RSSTyResult")("Style")& "%' And (PosHdr.status = 'O' OR PosHdr.status = 'H') And qty"& i &" > 0 And PosHdr.PO = PosLn.PO order by PosHdr.Complete desc"
							
						rsCompdate.Open strSql,conn
					else'get value from cuttkt
						strSql = "Select Cuttkth.Complete from Cuttkth, Cuttktl where Cuttktl.style+Cuttktl.cuttkt+Cuttktl.trancd like '" &Session("RSSTyResult")("Style")& "%' And (Cuttkth.status = 'O' OR Cuttkth.status = 'H') And Cuttktl.qty"& i &" > 0 And Cuttktl.cuttkt = Cuttkth.cuttkt order by Cuttkth.complete desc"
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
						end if
						strNoOfDays = 0
							
						'get the # of working days to add to get expected ship date[end]
						'check that its valid date(bigger than todays date)
						if date() > rsCompdate("Complete") then
							dBeg = date()
							dEnd = dateadd("d",cint(Session("Days")),dBeg)
						else
							dBeg = rsCompdate("Complete")
							dEnd = dateadd("d",cint(Session("Days")),dBeg)
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
						session("rsRecords")("complete"&i) = dExpected
						'Added By HDM to close the recordset in order to allow the code to re-open it
						'rsCompdate.Close()
					end if
					rsCompdate.Close()
				else
					session("rsRecords")("Ava"&i) = "O"
				end if
			 end if
			next
		end if
	rsOtsChk.Close ()
	session("rsRecords").update
    Session("RSSTyResult").MoveNext 
    loop
Else
	Session("RSStyle").movefirst()
	Do While Not Session("RSStyle").Eof
		session("rsRecords").addnew()
		session("rsRecords")("style") = Session("RSStyle")("style")
		session("rsRecords")("major") = Session("RSStyle")("cstymajor")
		session("rsRecords")("desc") = Session("RSStyle")("desc")
		Set RSScale = Conn.Execute("SELECT * FROM Scale WHERE Type+scale+prepak = 'S"&Session("RSStyle")("Scale")&"'") 
	    SzCount = RSScale("cnt") 
		For inti = 1 to 8
		  if trim(RSScale("Sz"&inti)) = "ASST" then
			session("rsRecords")("Sz"&inti) = ""
		  else
			session("rsRecords")("Sz"&inti) = RSScale("Sz"&inti)
		  end if
		Next 
	
		'WAL_calc of ots display[start]
		strSQl = "Select (style.stk1 - sum(ordline.qty1)) as sum1, (style.stk2 - sum(ordline.qty2)) as sum2, "& _
				 "(style.stk3 - sum(ordline.qty3)) as sum3, (style.stk4 - sum(ordline.qty4)) as sum4, "& _ 
				 "(style.stk5 - sum(ordline.qty5)) as sum5, (style.stk6 - sum(ordline.qty6)) as sum6, "& _
				 "(style.stk7 - sum(ordline.qty7)) as sum7, (style.stk8 - sum(ordline.qty8)) as sum8, "& _
				 "(style.stk1 + style.wip1 - sum(ordline.qty1)) as sum11, (style.stk2 + style.wip2 - sum(ordline.qty2)) as sum22, "& _
				 "(style.stk3 + style.wip3 - sum(ordline.qty3)) as sum33, (style.stk4 + style.wip4 - sum(ordline.qty4)) as sum44, "& _ 
				 "(style.stk5 + style.wip5 - sum(ordline.qty5)) as sum55, (style.stk6 + style.wip6 - sum(ordline.qty6)) as sum66, "& _
				 "(style.stk7 + style.wip7 - sum(ordline.qty7)) as sum77, (style.stk8 + style.wip8 - sum(ordline.qty8)) as sum88, "& _
				 "style.make from ordline, ordHdr, style where ordline.style+DTOS(ordline.complete)+ordline.cordtype+ordline.order+ordline.store+STR(ordline.lineno,6) like '" &Session("RSStyle")("Style")& "%' "& _
				 "and ordHdr.status <> 'C' and ordHdr.Status <> 'X' and ordline.order = ordHdr.order and ordline.style = style.style"
		rsOtsChk.Open strSQL, conn

		if rsOtsChk.EOF then'no orders for this style then get value in stock
			rsOtsChk.Close ()
			strSQl = "Select style.stk1 as sum1, style.stk2 as sum2, "& _
					 "style.stk3 as sum3, style.stk4 as sum4, "& _ 
					 "style.stk5 as sum5, style.stk6 as sum6, "& _
					 "style.stk7 as sum7, style.stk8 as sum8, "& _
					 "sum(style.stk1 + style.wip1) as sum11, sum(style.stk2 + style.wip2) as sum22, "& _
					 "sum(style.stk3 + style.wip3) as sum33, sum(style.stk4 + style.wip4) as sum44, "& _ 
					 "sum(style.stk5 + style.wip5) as sum55, sum(style.stk6 + style.wip6) as sum66, "& _
					 "sum(style.stk7 + style.wip7) as sum77, sum(style.stk8 + style.wip8) as sum88, "& _
					 "style.make from style where style like '" &Session("RSStyle")("Style")& "' "
			
			rsOtsChk.Open strSQL, conn
		end if
		if not rsOtsChk.eof then
			Session("Make") = rsOtsChk("make")
			For i = 1 to 8'Cdbl(SzCount)
			 if trim(RSScale("Sz"&i))<> "" and trim(RSScale("Sz"&i)) <> "ASST" then
				if cdbl(rsOtsChk("sum"&i)) > cint(intQty) then
					session("rsRecords")("Ava"&i) = "A"
				elseif cdbl(rsOtsChk("sum"&i)) <= cint(intQty) and cdbl(rsOtsChk("sum"&i)) > 0 then
					session("rsRecords")("Ava"&i) = "L"
				elseif cdbl(rsOtsChk("sum"&i&i)) > cint(intQty) then
					session("rsRecords")("Ava"&i) = "A"
					'get expected date
					if session("make") = false then'get value from PO
						strSql = "Select PosHdr.Complete from PosHdr, PosLn where PosLn.style+PosLn.cstytype+PosLn.po+STR(PosLn.lineno,6)+PosLn.trancd like '" &Session("RSStyle")("Style")& "%' And (PosHdr.status = 'O' OR PosHdr.status = 'H') And qty"& i &" > 0 And PosHdr.PO = PosLn.PO order by PosHdr.Complete desc"
							
						rsCompdate.Open strSql,conn
					else'get value from cuttkt
						strSql = "Select Cuttkth.Complete from Cuttkth, Cuttktl where Cuttktl.style+Cuttktl.cuttkt+Cuttktl.trancd like '" &Session("RSStyle")("Style")& "%' And (Cuttkth.status = 'O' OR Cuttkth.status = 'H') And Cuttktl.qty"& i &" > 0 And Cuttktl.cuttkt = Cuttkth.cuttkt order by Cuttkth.complete desc"
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
						end if
						strNoOfDays = 0
							
						'get the # of working days to add to get expected ship date[end]
						'check that its valid date(bigger than todays date)
						if date() > rsCompdate("Complete") then
							dBeg = date()
							dEnd = dateadd("d",cint(Session("Days")),dBeg)
						else
							dBeg = rsCompdate("Complete")
							dEnd = dateadd("d",cint(Session("Days")),dBeg)
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
						session("rsRecords")("complete"&i) = dExpected
						'Added By HDM to close the recordset in order to allow the code to re-open it
						'rsCompdate.Close()
					end if
					rsCompdate.Close()
				elseif cdbl(rsOtsChk("sum"&i&i)) <= cint(intQty) and cdbl(rsOtsChk("sum"&i&i)) > 0 then
					session("rsRecords")("Ava"&i) = "L"
					'get expected date
					if session("make") = false then'get value from PO
						strSql = "Select PosHdr.Complete from PosHdr, PosLn where PosLn.style+PosLn.cstytype+PosLn.po+STR(PosLn.lineno,6)+PosLn.trancd like '" &Session("RSStyle")("Style")& "%' And (PosHdr.status = 'O' OR PosHdr.status = 'H') And qty"& i &" > 0 And PosHdr.PO = PosLn.PO order by PosHdr.Complete desc"
							
						rsCompdate.Open strSql,conn
					else'get value from cuttkt
						strSql = "Select Cuttkth.Complete from Cuttkth, Cuttktl where Cuttktl.style+Cuttktl.cuttkt+Cuttktl.trancd like '" &Session("RSStyle")("Style")& "%' And (Cuttkth.status = 'O' OR Cuttkth.status = 'H') And Cuttktl.qty"& i &" > 0 And Cuttktl.cuttkt = Cuttkth.cuttkt order by Cuttkth.complete desc"
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
						end if
						strNoOfDays = 0
							
						'get the # of working days to add to get expected ship date[end]
						'check that its valid date(bigger than todays date)
						if date() > rsCompdate("Complete") then
							dBeg = date()
							dEnd = dateadd("d",cint(Session("Days")),dBeg)
						else
							dBeg = rsCompdate("Complete")
							dEnd = dateadd("d",cint(Session("Days")),dBeg)
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
						session("rsRecords")("complete"&i) = dExpected
						'Added By HDM to close the recordset in order to allow the code to re-open it
						'rsCompdate.Close()
					end if
					rsCompdate.Close()
				else
					session("rsRecords")("Ava"&i) = "O"
				end if
			 end if
			next
		end if
	rsOtsChk.Close ()
	session("rsRecords").update
    Session("RSStyle").MoveNext 
    loop
End if

session("rsRecords").updatebatch
%>
<!--
Sending data To the Crystal Report
-->
<html>
<head>
<meta NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<title>Check Order Status - Order # <%=request("orderNo")%></title>
<!--LINK rel="stylesheet" type="text/css" href="../reports/report.css"-->
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</head>
<body bgcolor="#aecae6" topmargin="0" leftmargin="0">
<br>
<%
reportname = "otsReport.rpt"
If Not IsObject (session("oApp")) Then                              
'	Set session("oApp") = Server.CreateObject("Crystal.CRPE.Application")
Set session("oApp") = Server.CreateObject("CrystalRuntime.Application")
End If                                                                
Path = Request.ServerVariables("PATH_TRANSLATED")        

While (Right(Path, 1) <> "\" And Len(Path) <> 0)                      
iLen = Len(Path) - 1                                                  
Path = Left(Path, iLen)                                               
Wend                                                                  

If IsObject(session("oRpt")) then
	Set session("oRpt") = nothing
End if

Set session("oRpt") = session("oApp").OpenReport(path & reportname, 1)


set Database = session("oRpt").Database
set Tables = Database.Tables
set Table1 = Tables.Item(1)
Table1.SetPrivateData 3, session("rsRecords")
On Error Resume Next                                                  
err.Clear 
session("oRpt").ReadRecords


If Err.Number <> 0 Then                                               
  Response.Write "An Error has occured on the server in attempting to access the data source --" & err.Description
Else
  If IsObject(session("oPageEngine")) Then                              
  	set session("oPageEngine") = nothing
  End If
set session("oPageEngine") = session("oRpt").PageEngine
End If                                                                

viewer = Request.QueryString ("viewer")
'viewer = Request.form("viewer")

viewer="ActiveX"
'viewer = "Java-Plug-in"
'This line collects the value passed for the viewer to be used, and stores
'it in the "viewer" variable.

IF cstr(viewer) = "ActiveX" THEN
%>
<!-- #include file="../crystal/SmartViewerActiveX.asp" -->
<%
ElseIf cstr(viewer) = "Netscape Plug-in" then
%>
<!-- #include file="../crystal/ActiveXPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "JVM" then
%>
<!-- #include file="../crystal/SmartViewerJava.asp" -->
<%
ElseIf cstr(viewer) = "Java-Plug-in" then
%>
<!-- #include file="../crystal/JavaPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "HTML Frame" then
	Response.Redirect("htmstart.asp")
Else
	Response.Redirect("rptserver.asp")
End If
'The above If/Then/Else structure is designed to test the value of the "viewer" varaible
'and based on that value, send down the appropriate Crystal Smart Viewer.
%>
<br>
<br>
<BR>
<CENTER>
<a href = "ots.asp"><Img border=0 src="../Images/<%=session("Theme")%>/back.gif"></a>
</CENTER>
<br>
<br>
</body>
</html>
