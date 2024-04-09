<%
'******************************************************************
'Page Name:  addCredit.asp
'Date     :  01/05/2004
'Developer:  wal _ Walaa Ahmed
'Purpose  :  function to add credit record in credit files
'******************************************************************
%>
<%Response.Buffer  = true%>
<%
''''''''''''''Database Connections''''''''''''''''''''''
Set conn = server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
%>
<%
'******************************************************************
'function Name:  addCredit
'Date     :  01/05/2004
'Developer:  wal _ Walaa Ahmed
'Purpose  :  function to add credit record in credit files
     
'******************************************************************
'function addCredit(ByVal ordNo, ByVal Amount)
function addCredit(ByVal Reference, ByVal Amount, ByVal CustId, ByVal CustName)
	'get bank info
	Dim rsBank
	set rsBank = server.CreateObject ("ADODB.Recordset")
	rsBank.Open "select * from ApChecks where cBnkCode='" &trim(session("BankCode"))& "'",conn
	'get date year and period
	Dim rsYear
	set rsYear = server.CreateObject ("ADODB.Recordset")
	rsYear.Open "select * from fsprd where dfsppbgdt < {" & date() & "} and dfsppendt > {" & date() & "}",conn
	
	'get AR GL account
	Dim rsAR
	set rsAR = server.CreateObject ("ADODB.Recordset")
	 
	rsAR.Open "select * from codes where (cdefcode+ccode_no+crltfield+cfld_name='N" &trim(session("ARPTYPE"))& "YCARPTYPE' and crltd_nam='CARGLACC') or (cdefcode+ccode_no+crltfield+cfld_name='N" &trim(session("ARPTYPE"))& "NCARPTYPE')",conn
	'get Tran and Batch seq values[start]
	Dim rsSeq
	set rsSeq = server.CreateObject ("ADODB.recordset")
	strsql="select cseq_type, nseq_no, nfld_wdth from sequence where cseq_type='TRAN' or cseq_type='BATCH'"

	rsSeq.CursorLocation = 3
	rsSeq.CursorType = adOpenStatic
	rsSeq.LockType = 4
	' Get The Sequence Number.
	rsSeq.open strsql,conn
	rsSeq.Filter  = "cseq_type = 'BATCH'"
		strBatch = rsSeq("nseq_no")
		rsSeq("nseq_no") = cdbl(rsSeq("nseq_no")) + 1
		intWidth = rsSeq("nfld_wdth")

		intLong = Len(strBatch)
		intDif  = cint(intWidth) - cint(intLong)

		For i = 1 to intDif 
			strBatch = "0" & strBatch
		Next
	rsSeq.UpdateBatch 
	rsSeq.Filter  = ""
	rsSeq.Filter  = "cseq_type='TRAN'"
		strTran = rsSeq("nseq_no")
		rsSeq("nseq_no") = cdbl(rsSeq("nseq_no")) + 1
		intWidth = rsSeq("nfld_wdth")

		intLong = Len(strTran)
		intDif  = cint(intWidth) - cint(intLong)

		For i = 1 to intDif 
			strTran = "0" & strTran
		Next
	rsSeq.UpdateBatch
	rsSeq.Filter  = "" 
	rsSeq.Close()
	Set rsSeq = Nothing
	'get Tran and Batch seq values[end]
	'add new record in credit file [start]
	Dim rsCredit
	set rsCredit = server.CreateObject ("ADODB.Recordset")
	rsCredit.Open "select * from credit where 1=0",conn,1,3
	
	rsCredit.AddNew ()
		'rsCredit("Account")  = trim(Session("customerid"))
		rsCredit("Account")  = trim(CustId)
		rsCredit("TranType") = "4"
		rsCredit("Tran")   = strTran
		rsCredit("Batch")  = strBatch
		rsCredit("TranDate")  = date()
		rsCredit("dPostDate") = date()
		'rsCredit("Reference") = "Order# "&ordNo
		rsCredit("Reference") = Reference
		rsCredit("Amount")    = -cdbl(Amount)
		rsCredit("cBnkCode")  = session("BankCode")
		if not rsBank.EOF then
			rsCredit("cChkacct")  = rsBank("cChkAcct")
			rsCredit("cAdjacct")  = rsBank("cchkglacc")
			rsCredit("cCurrcode") = rsBank("cCurrcode")
		end if
		rsCredit("cArpType")  = Session("ARPTYPE")
		if not rsAR.EOF then
			rsAR.Filter = "crltd_nam='CARGLACC'"
			if not rsAR.EOF then
				rsCredit("cArglacc")  = trim(rsAR("crltd_vlu"))
			end if
			rsAR.Filter = ""
			rsAR.Filter = "crltfield='N'"
			if not rsAR.EOF then
				rsCredit("Desc")  = trim(rsAR("cdiscrep"))
			end if
			rsAR.Filter = ""
		end if
		rsCredit("lnonar") = false
		rsCredit("nexrate") = 1
		rsCredit("ncurrunit") = 1
		rsCredit("cAdd_ver") = "WEB"
		rsCredit("cAdd_user") = Trim(Session("rep"))
		rsCredit("cAdd_time") = time()
		rsCredit("dAdd_date") = date()
	rsCredit.UpdateBatch ()
	'add new record in credit file [end]
	'add new record in Appayment file [start]
	Dim rsAppay
	set rsAppay = server.CreateObject ("ADODB.Recordset")
	rsAppay.Open "select * from appaymnt where 1=0",conn,1,3
	rsAppay.AddNew ()
		rsAppay("cPayType")  = "A"
		rsAppay("cPayMeth")  = "A"
		rsAppay("dPayDate")  = date()
		'rsAppay("cpayclno")  = trim(Session("customerid"))
		'rsAppay("cpaycomp")  = trim(Session("RSCust").fields("btname"))
		rsAppay("cpayclno")  = trim(CustId)
		rsAppay("cpaycomp")  = trim(CustName)
		rsAppay("npayamnt")  = -cdbl(Amount)
		rsAppay("cBnkCode")  = session("BankCode")
		if not rsBank.EOF then
			rsAppay("cChkacct")  = rsBank("cChkAcct")
			rsAppay("cCurrcode") = rsBank("cCurrcode")
		end if
		if not rsYear.EOF then
			rsAppay("cFisfyear")  = rsYear("cFisfyear")
			rsAppay("cfspprdid")  = rsYear("cfspprdid")
		end if
		rsAppay("Batch")  = strBatch
		rsAppay("nexrate")   = 1
		rsAppay("ncurrunit") = 1
		rsAppay("cAdd_ver")  = "WEB"
		rsAppay("cAdd_user") = Trim(Session("rep"))
		rsAppay("cAdd_time") = time()
		rsAppay("dAdd_date") = date()
	rsAppay.AddNew ()
	
	'add new record in Appayment file [end]
End function

%>