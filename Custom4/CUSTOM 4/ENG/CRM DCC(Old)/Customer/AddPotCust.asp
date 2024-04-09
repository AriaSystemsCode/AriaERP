<%@ Language=VBScript %>
<!--#include file="../common/Add2Log.asp"-->
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%End if
Dim Conn 'as connection
Dim rsCustomer ' as recordset

'HDM Use the customer UI object to add new customer
Dim objNewCust
set objNewCust = server.CreateObject ("CustomerUI.CustomUI")
objNewCust.ConParameter = Application("DataConnectionString")
'open DB connections
Set Conn = server.CreateObject("ADODB.Connection")
Conn.Open Application("SystemConnectionString")
Set cnnDB = server.CreateObject("ADODB.connection")
cnnDB.Open Application("DataConnectionString")
Set cnnSQL = server.CreateObject("ADODB.Connection")
cnnSQL.Open Application("SqlServer")

Set RSComp = server.CreateObject("ADODB.Recordset")
Set RSFormat = server.CreateObject("ADODB.Recordset")

'Creat the Recordset for the Sequence file to get the customer ID[start]
set RSSequence = server.CreateObject("ADODB.recordset")
'strsql="select * from sequence where cseq_type='CUSTSEQ'"
strsql="select nseq_no,nfld_wdth  from sequence where cseq_type='CUSTSEQ'"

RSSequence.CursorLocation = 3
RSSequence.CursorType = adOpenStatic
RSSequence.LockType = 4
' Get The Sequence Number.
RSSequence.open strsql,cnnDB	
strCustID = RSSequence("nseq_no")
RSSequence("nseq_no") = cdbl(RSSequence("nseq_no")) + 1
intWidth = RSSequence("nfld_wdth")

intLong = Len(strCustID)
intDif  = cint(intWidth) - cint(intLong)

For i = 1 to intDif 
	strCustID = "0" & strCustID
Next
RSSequence.UpdateBatch 
RSSequence.Close()
Set RSSequence = Nothing
'Creat the Recordset for the Sequence file to get the customer ID[end]	    

'Prepare the user password[start]
Dim objPriv
set objPriv = Server.CreateObject("AriaWebSecurity.Privileges")
strPass = objPriv.GetPassword(Trim(Request.Form("txtPWD")))
'Prepare the user password[end]
' Get the Phone Format[start]
strsql = "select * from syccomp where ccomp_id='" & Session("CompanyID") & "'"
RSComp.Open strsql,conn
IF Not(RSComp.EOF And RSComp.BOF) Then
  strCountryCode = Trim(RSComp.Fields("ccont_code").Value )
End IF

RSComp.Close 
Set RSComp = Nothing
strsql = "select * from sycint where ccont_code='" & strCountryCode & "'"
			    
RSFormat.Open strsql,conn
IF Not(RSFormat.EOF And RSFormat.BOF) Then
	strFormat = RSFormat.Fields("CPHONETEMP").Value 
End IF
		  
intTotLegth = Len(strFormat)
intOldPos = 1
intPos1 = 1
strOutPut = ""
intpos = 1
intcount = 1
Do while Not intPos1 = 0
	intpos1 = instr(intOldPos, strFormat, "-",1)
	intpos2 = instr(intOldPos, strFormat, "/",1)
	IF intpos1 > intpos2 OR intpos1 = 0 Then
		intpos1 = intpos2
	End IF
	intpos = intpos + intpos1 - intOldPos
	intOldPos = intpos1 + 1
	intcount = intcount + 1
Loop

strtempfax = ""
strphone2 = ""
for inttemp=1 to  intcount-1
	strtemp = strtemp &  request("phone1" & inttemp)
	strContPhone = strContPhone &  request("ContPhone" & inttemp)
	strtempfax = strtempfax & request("Fax" & inttemp)
	strContFax = strContFax &  request("ContFax" & inttemp)
	strphone2 = strphone2 & request("phone2" & inttemp)
next
' Get the Phone Format[end]
'Wal_ check that there is a phone entered
if trim(strtemp) = "" then
	Response.Write("<font color=#ff0000><b>Please you must enter a phone no.</b></font>")
	Response.Write("<font color=#ff0000><br><b><a href='javascript:window.history.back();'>Back</a></b></font>")
	Response.End 
end if

'WAL_ check that this # doesn't exist[start]
Dim rsChk
set rsChk = server.CreateObject ("ADODB.Recordset")

'wma
if session("CustomerCodeType") = "Mannual" then 'mannualy
	rsChk.Open "select * from customer where type+account+store like 'M"& ucase(request("txtAccount")) & "%'",cnnDB
else 'phone
	rsChk.Open "select * from customer where type+phone1 = 'M"& trim(strtemp) &"' ",cnnDB
end if
'wma

if rsChk.RecordCount <= 0 then
	If objNewCust.Add Then
		objNewCust.Cust_type = "M"
		objNewCust.Bill_To   = "M"
		
		if session("CustomerCodeType") = "Mannual" then 'mannualy
			strCustID = Ucase(Trim(Request("txtAccount")))
			objNewCust.cust_account = strCustID
		else 'phone
			objNewCust.cust_account = strCustID'Ucase(Trim(Request("txtAccount")))
		end if		
		
		objNewCust.cust_btname  = Trim(Request("txtName"))
		objNewCust.cust_stname  = Trim(Request("txtName"))
		objNewCust.cust_caddress1 = trim(Request("txtaddr1"))
		
		objNewCust.cust_caddress12 =trim(Request("txtaddr1"))
		
		objNewCust.cust_caddress2 = trim(Request("txtaddr2"))
		objNewCust.cust_caddress22 = trim(Request("txtaddr2"))
		
		objNewCust.cust_caddress3 = trim(Request("txtaddr3"))
		objNewCust.cust_caddress32 = trim(Request("txtaddr3"))
		
		objNewCust.cust_caddress4 = trim(Request("txtaddr4"))
		objNewCust.cust_caddress42 = trim(Request("txtaddr4"))
		
		objNewCust.cust_caddress5 = trim(Request("txtaddr5"))
		objNewCust.cust_caddress52 = trim(Request("txtaddr5"))
		
		objNewCust.cust_caddress6 = trim(Request("txtaddr6"))
		objNewCust.cust_caddress62 = trim(Request("txtaddr6"))
		
		'WMA Replace only potential by ordinary 4 status
		'objNewCust.cust_status = "P"
		objNewCust.cust_status = trim(Request.Form ("lstStatus"))
		objNewCust.cust_dba = trim(Request("txtDBA"))
		objNewCust.cust_phone1 = trim(strtemp)
		objNewCust.cust_phone2 = trim(strphone2)
		objNewCust.cust_fax  = trim(strtempfax)
		objNewCust.buyer  =Trim(Request("txtBuyer"))
		objNewCust.keeper =Trim(Request("txtKeeper"))
		objNewCust.cust_shipvia = trim(Request.Form ("selShip"))
		objNewCust.cust_salesrep  = Trim(Session("rep"))
		'get salesrep comm value[start]
		set rsComm = server.CreateObject ("ADODB.Recordset")
		rsComm.Open "select comm from salesrep where repcode = '" &Trim(Session("rep"))& "'",cnnDB
		if not rsComm.EOF then
			if trim(rsComm("comm")) <> "" then 
				objNewCust.cust_comm = cdbl(rsComm("comm"))
			end if
		end if
		'get salesrep comm value[end]
		objNewCust.cust_cadd_user = Trim(Session("rep"))
		objNewCust.cust_cadd_time = Time()
		objNewCust.cust_dadd_date = Date()
		objNewCust.cust_ctermcode = Session("TermCode")
		If objNewCust.Save Then
			boladd = True
			'Add user in syuuser table[start]
			Dim rsUser
			set rsUser = Server.CreateObject("ADODB.RecordSet")
			rsUser.Open "select top 0 * from syuuser",cnnSQL,1,4
			rsUser.AddNew
			rsUser("cuser_id")  = strCustID
			rsUser("cusr_name") = Trim(Request("txtName"))
			rsUser("cusr_pass") = strPass
			'rsUser("cwe_mail") = strEmail
			rsUser("cusr_levl") = "A"
			rsUser("profile")   = "C"
			rsUser.UpdateBatch
			'Add user in syuuser table[end]
			'WAL - add record in contact file[start]
			if Trim(Request("txtContact")) <> "" and Trim(Request("txtMail")) <> "" then
				set objAddCont = objNewCust.ChildAddNew (1)
				objAddCont.cconttype="C"
				objAddCont.Ccont_id = strCustID'Ucase(Trim(Request("txtAccount")))
				objAddCont.contact  = Trim(Request("txtContact"))
				objAddCont.ccontttl = trim(Request.Form("Title"))
				objAddCont.phone = trim(strContPhone)
				objAddCont.fax = trim(strContFax)
				objAddCont.cemail_add = Trim(Request("txtMail"))
				objNewCust.childset 1,objAddCont
				objNewCust.save
				'if Request.Form ("hidName") <> "" and Request.Form ("hidMail") <> "" then
				'	Dim rsContact
				'	set rsContact  = server.CreateObject ("ADODB.Recordset")
				'	rsContact.Open "Select * from contact where 1=0",Application("DataConnectionString"),1,3
				''	rsContact.AddNew 
				'		rsContact("cCont_id") = Ucase(Trim(Request("txtAccount")))
				'		rsContact("contact") =  Trim(Request("hidName"))
				'		rsContact("cemail_add")= Trim(Request("hidMail"))
				'		rsContact("phone") = trim(strtemp)
				'		rsContact("fax") = trim(strtempfax)
				'	rsContact.Update 
			end if
			'WAL - add record in contact file[end]
			Session("CustomerID") = objNewCust.cust_account 
			strEditCust = "Add!#!Customer!#!New!#!" &trim(cstr(Request.Form ("DBA"))) & "!#!" &_
										trim(cstr(Request.Form ("Phone1")))& "!#!" &trim(cstr(Request.Form ("Keeper")))& "!#!" &_
										trim(cstr(Request.Form ("Buyer")))& "!#!" &trim(cstr(Request.Form ("Phone2")))& "!#!" &_
										trim(cstr(Request.Form ("Fax")))& "!#!" &trim(cstr(Request.Form ("Addr1")))& "!#!" &_
										trim(cstr(Request.Form ("Addr2")))& "!#!" &trim(cstr(Request.Form ("Addr3")))& "!#!" &_
										trim(cstr(Request.Form ("Addr4")))& "!#!" &trim(cstr(Request.Form ("Addr5")))& "!#!" &_
										trim(cstr(Request.Form ("Addr6")))
			Add2Log "", Session("CustomerID"),"Add Customer",trim(cstr(objNewCust.cust_account)),strEditCust
		Else
			Response.Write("<font color=#ff0000><b>Error, cannot save!</b></font>")
			Response.End 
		End If
	End If
Else
	boladd = false
End If
%>
<Html>
<Head>
<title>CRM - Customer Profile</title>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/customer.css">
</Head>
<Body>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><BR><BR><br></p>

<table border="1" width="95%" height=50 cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width=100% height=50 class="Title"><p><%=session("CustField")%> Profile</p></td>
    </tr>
</table>
<br><br><br>
<table border="0" width="95%" height=50 cellspacing="0" cellpadding="0" align=center>
	<TR><TD class="dark_cell">
	<%
	if boladd = "True" then
		'if customer added then load its data in the session recordset
		Dim strsql ' as string
		Set conn = server.CreateObject("Adodb.connection")
		Conn.Open Application("DataConnectionString")

		Set rsCustomer = server.CreateObject("Adodb.recordset")
		strsql = "select * from customer where type+Account+store like 'M" & Trim(strCustID) & "%'"
		
		rsCustomer.Open strsql,conn,1,3

		Response.Write("Your Customer has been added succefully to the database.")
		Session("CustomerID") = rsCustomer("Account")
		set Session("rscust") = rsCustomer
		Response.Write("<br><a href='custprof.asp'>Click here to Edit this customer</a>")
	else
		Response.Write("<font color=#ff0000><b>This account already exist.</b></font>")
		Response.Write("<font color=#ff0000><br><b><a href='javascript:window.history.back();'>Back</a></b></font>")
	end if
	%>
	
	</TD></TR>
</table>
</body>
</Html>
















