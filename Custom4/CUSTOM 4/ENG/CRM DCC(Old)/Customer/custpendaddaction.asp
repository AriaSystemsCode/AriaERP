<%Response.Buffer=True
'on error resume next
Dim objSeq 
'Set objSeq = server.createobject("DBGenSeq.DBSequence")
set conn=server.CreateObject("ADODB.connection")
set connsys=server.CreateObject("ADODB.connection")
set RS=server.CreateObject("ADODB.Recordset")
set RSphone=server.CreateObject("ADODB.Recordset")
conn.Open Application("DataConnectionString")
connsys.Open Application("SystemConnectionString")
sqls="select * from syschdul"
sqlphone="select * from contact where contact='"&trim(request("lstcontact"))&"'"
RS.open sqls,connsys,3,3
RSphone.Open sqlphone,conn,3,3
If not RSphone.EOF And not RSphone.BOF  Then
	phone=RSphone("phone")
Else
	phone=""
End IF
lstreason1=trim(request("lstreason"))
lstact=trim(request("lstact"))
dtrandate=cdate(trim(request("dtrandate")))
txttime=trim(request("txttime"))
lstuserid=trim(Session("rep"))
lstcontact=trim(request("lstcontact"))
txtsubject=trim(request("txtsubject"))
txtnotes=trim(request("txtnotes"))
txtpriority=trim(request("txtpriority"))
txtduration=trim(request("txtduration"))
RS.AddNew 
		RS("ctrantype")=lstact
		If txtduration="" Then
			RS("nestdur")="0"
		Else
			RS("nestdur")=txtduration
		End if
			RS("cpriority")=txtpriority
			RS("mnotes")=txtnotes
			RS("dtrandate")=dtrandate
			RS("ctrantime")=txttime
			RS("cuser_id")=lstuserid
			RS("contact")=lstcontact
			
			
			RS("ctranreson")=lstreason1
			IF trim(RS("ctranreson"))="" Then
				RS("ctranreson")="000092"
			End IF
			RS("csubject")=txtsubject
			RS("ccompleted")="N"
			RS("ccomp_id")="99"
			RS("cadd_user")=lstuserid
			RS("ccont_id")=Session("customerid")
			RS("cphone")=phone
			RS("cconttype")="C"
			RS("cadd_time")=time()
			RS("dadd_date")=date()
			RS("cconttype")="C"
			RS("cresultcd")="000097"
			
			'Dim strSeq 
			'strSeq = objSeq.GetSequence(conn,"CSEQNUMBER")
			strSeq = GetSequence("CSEQNUMBER")
			RS("cseqnumber")=strseq
			response.write RS("ctranreson")
RS.Update 
	
	set RS=nothing
	set RSphone=nothing
	conn.Close
	connsys.Close 
	set conn=nothing
	set connsys=nothing
	
response.redirect "custpending.asp"

Function GetSequence(strSeqType)
	'Dim objSeq
	'Set objSeq = Server.CreateObject("DBGenSeq.DBSequence")
	'GetSequence = objSeq.GetSequence("dsn=webtrack1;uid=aria;pwd=aria;BackgroundFetch=No" , strSeqType)
	'Set objSeq = Nothing
	'this code has been added to solve the problem of the ODBC driver [Start]
	Dim rsSequence
	Set rsSequence = server.CreateObject("ADODB.Recordset")

	'rsSequence.Open "Select * from Sequence where cSeq_type ='"&trim(strSeqType)&"'",conn ,2,4
	strsql="select * from sequence where cseq_type='"&trim(strSeqType)&"'"

	RSSequence.CursorLocation = 3
	RSSequence.CursorType = adOpenStatic
	RSSequence.LockType = 4
	' Get The Sequence Number.
	RSSequence.open strsql,conn
	if not (rsSequence.EOF and rsSequence.BOF ) then
		'intSeq = Cdbl(rsSequence.Fields("nseq_no").Value) + 1
		intSeq = Cdbl(rsSequence.Fields("nseq_no").Value)
		'GetSequence = String(6 - Len(CStr(intSeq)), "0") & CStr(intSeq)
		GetSequence = String(6 - Len(CStr(intSeq)), "0") & CStr(intSeq)
		
		rsSequence.Fields("nseq_no").Value = GetSequence + 1
		'Response.Write(rsSequence.Fields("nseq_no").Value)
		'Response.End 
		rsSequence.Updatebatch
	'this code has been added to solve the problem of the ODBC driver [End]
	end if
End Function

%>