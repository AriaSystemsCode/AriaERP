<%
Response.Buffer = True
Dim foxcon
Set Foxcon = server.CreateObject("ADODB.Connection")
foxcon.Open "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Session("Data") &";SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=Yes"

Dim SqlCon
Set SqlCon = server.CreateObject("ADODB.Connection")
SqlCon.Open Application("SqlServer")

Set RSGroup = server.CreateObject("ADODB.RecordSet")

strsql = "select * from custGroup where groupid = " & Session("CustGrp")
RSGroup.Open strsql,SqlCon

Const NumPerPage = 1
Dim CurPage

IF Request.QueryString("CurPage") = "" Then
	CurPage = 1
Else
	CurPage = Request.QueryString("CurPage")
End IF
Response.Write(CurPage)
Set RSCustomer = Server.CreateObject("ADODB.Recordset")
RSCustomer.CursorLocation = 3


IF request("txtCust") = "" Then
	'HDM [Start] 9/18/2002 force rushmore
	'strCustomer = "Select account,Btname From Customer where type='M' and status='A' order by account"
	strCustomer = "Select account,Btname From Customer where type+account+store Like 'M%' and Status='A' Order By Account"
	'HDM [End] 9/18/2002 force rushmore
Else
	'HDM [Start] 9/18/2002 force rushmore
	'strCustomer = "Select account,Btname From Customer where type='M' and status='A' and account like '" & Ucase(request("txtCust")) & "%' order by account "
	strCustomer = "Select account,Btname From Customer where type+account+store Like 'M" & Ucase(request("txtCust")) & "%' and Status='A' order by account"
	'HDM [end] 9/18/2002 force rushmore
End IF
Response.Write(strCustomer)
Response.End 
RSCustomer.Open strCustomer,foxcon

RSCustomer.CacheSize  = NumPerPage
RSCustomer.PageSize = NumPerPage
TotalPages = RSCustomer.PageCount 
RSCustomer.AbsolutePage = CurPage

Dim Count
Count = 0

Do while Not RSCustomer.EOF and count < NumPerPage
	
	IF request(RSCustomer.Fields("account").Value) = "ON" Then
	
		IF Session("StrAccount") = "" Then
			Session("StrAccount") = RSCustomer.Fields("account").Value 
		Else
			IF instr(1,Session("StrAccount"),RSCustomer.Fields("account").Value) = 0 Then
				Session("StrAccount") = Session("StrAccount") & "," & RSCustomer.Fields("account").Value
			End IF
		End IF
	Else
		Session("StrAccount") = replace(Session("StrAccount"),RSCustomer.Fields("account").Value,"")
	End IF
	count = count + 1
	RSCustomer.MoveNext 
Loop
Response.Write(Session("StrAccount"))

Set RSCustClas = server.CreateObject("Adodb.recordset")
strsql = "select * from CustClassification where CustGroup=" & request("GrpID")
RSCustClas.Open strsql,SqlCon,2,4

IF Session("StrAccount") = "" Then
	Do While Not RSCustClas.EOF 
		RSCustClas.Delete 
		RSCustClas.MoveFirst
	Loop
	'RSCustClas.UpdateBatch 
Else
	ArrCust = Split(Session("StrAccount"),",")
	
	Do While Not RSCustClas.EOF 
		IF Not(RSCustClas.EOF And RSCustClas.BOF) Then
			RSCustClas.Delete 
			RSCustClas.MoveFirst
		End IF
	Loop
	RSCustClas.UpdateBatch 
	For intCount = 0 to ubound(ArrCust)
		RSCustClas.AddNew 
		RSCustClas.Fields("custID").Value = ArrCust(intcount)
		RSCustClas.Fields("CustGroup").Value = request("GrpID")
	Next
	RSCustClas.UpdateBatch 
End IF
Session("StrAccount") = ""
Response.Redirect("default.asp")

%>