<%response.buffer=true
On Error resume next

txtnotes=trim(request("txtnotes"))
txtpriority=trim(request("txtpriority"))
dttrandate=trim(cdate(request("dttrandate")))
txtduration=trim(request("txtduration"))
lstcontact=trim(request("lstcontact"))
txtsubject=trim(request("txtsubject"))
txttime=trim(request("txttime"))
lstreason=trim(request("lstreason"))
lstresult=trim(request("lstresult"))

Set conn=server.CreateObject("ADODB.connection")
Set connSys=server.CreateObject("ADODB.connection")

Set rs=server.CreateObject("ADODB.recordset")
Set RStemp=server.CreateObject("ADODB.recordset")
Set RSs=server.CreateObject("ADODB.recordset")
Set RSphone=server.CreateObject("ADODB.Recordset")

connSys.Open Application("systemConnectionString")
conn.Open Application("DataConnectionString")

sqltemp="select * from syschdul where cseqnumber='"&request("SEQ")&"'"
sqlstat="select * from syschdul where cseqnumber='"&request("SEQ")&"'"

sqls="select  * from codes where cdefcode+crltfield+cfld_name = 'NNCTRANRESON' and cdiscrep='"&request("lstreason")&"'"


rs.open sqlstat,connSys ,1,3
rsTemp.Open sqltemp,connsys,1,3
Rss.Open sqls,conn,1,3

Tempreason=RsTemp("ctranreson")
Tempresult=RsTemp("cresultcd")
Tempcode=RSS("ccode_no")
sqlphone="select * from contact where contact='"&trim(request("lstcontact"))&"'"
RSphone.Open sqlphone,conn,3,3
If trim(RSphone("phone"))<>"" Then
	phone=RSphone("phone")
Else
	phone=""
End IF
RS("cphone")=phone
RS("mnotes")=txtnotes
RS("cpriority")=txtpriority
RS("dtrandate")=cdate(dttrandate)
RS("ctrantime")=txttime
If txtduration="" Then
	RS("nestdur")="0"
Else
	RS("nestdur")=txtduration
End if
RS("contact")=lstcontact
RS("csubject")=txtsubject
RS("ctranreson")=lstreason
RS("cresultcd")=lstresult
RS("ctrantype") = request("radio1")

If request("submit")<>"Complete Activity" Then
	RS.Update
	set RStemp=nothing
	set RS=nothing
	set RSs=nothing
	set RSphone=nothing
	conn.Close
	connsys.Close
	set conn=nothing
	set connsys=nothing
 response.redirect "custpending.asp"
	
Else
	
	RS("CCompleted")="Y"
	RS.Update
	set RStemp=nothing
	set RS=nothing
	set RSs=nothing
	set RSphone=nothing
	conn.Close
	connsys.Close 
	set connsys=nothing
	set conn=nothing
	response.redirect "custpending.asp"
End IF
%>