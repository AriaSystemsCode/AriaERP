<%@ Language=VBScript %>
<!--#include file="../common/Add2Log.asp"-->
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>
<%
Dim Conn 'as connection
Dim rsCustomer ' as recordset

'HDM Use the customer UI object to add new customer

Dim objCust
set objCust = server.CreateObject ("CustomerUI.CustomUI")
objCust.ConParameter = Application("DataConnectionString")


'If Can Load Customer then add new child
If objCust.Load (Ucase(Trim(Request("txtAccount")))) Then
	objCust.ChildFilter 2,"Account='" & Request("txtAccount") & "' And Store='" & Request("txtStore") & "'"
	'This Store Not Found for this customer the you can aad it
	If objCust.ChildEOF(2) Then
		Set objNewCust = objCust.ChildAddNew(2)
		' Get the Phone Format
		Set Conn = server.CreateObject("ADODB.Connection")
		Conn.Open Application("SystemConnectionString")
		Set RSComp = server.CreateObject("ADODB.Recordset")
		Set RSFormat = server.CreateObject("ADODB.Recordset")
				    
		strsql = "select * from syccomp where ccomp_id='" & Session("CompanyID") & "'"
		RSComp.Open strsql,conn
		If Not(RSComp.EOF And RSComp.BOF) Then
		  strCountryCode = Trim(RSComp.Fields("ccont_code").Value )
		End If

		RSComp.Close 
		Set RSComp = Nothing
		strsql = "select * from sycint where ccont_code='" & strCountryCode & "'"
				    
		RSFormat.Open strsql,conn
		If Not(RSFormat.EOF And RSFormat.BOF) Then
			strFormat = RSFormat.Fields("CPHONETEMP").Value 
		End If
		  
		intTotLegth = Len(strFormat)
		intOldPos = 1
		intPos1 = 1
		strOutPut = ""
		intpos = 1
		intcount = 1
		Do while Not intPos1 = 0
			intpos1 = instr(intOldPos, strFormat, "-",1)
			intpos2 = instr(intOldPos, strFormat, "/",1)
			If intpos1 > intpos2 Or intpos1 = 0 Then
				intpos1 = intpos2
			End If
			intpos = intpos + intpos1 - intOldPos
			intOldPos = intpos1 + 1
			intcount = intcount + 1
		Loop
		strtempfax = ""
		strphone2 = ""
		For inttemp=1 To  intcount-1
			strtemp = strtemp & request("phone1" & inttemp)
			strtempfax = strtempfax & request("Fax" & inttemp)
			strphone2 = strphone2 & request("phone2" & inttemp)
		Next
		If objNewCust.Add Then
			objNewCust.Cust_type = "S"
			objNewCust.Bill_To = "M"
			objNewCust.cust_account = Ucase(Trim(Request("txtAccount")))
			objNewCust.cust_store = Trim(Request("txtStore"))
			'objNewCust.cust_btname = Trim(Request("txtName"))
			objNewCust.cust_stname = Trim(Request("txtName"))
			objNewCust.cust_caddress1 = trim(Request("txtaddr1"))
			objNewCust.cust_caddress2 = trim(Request("txtaddr2"))
			objNewCust.cust_caddress3 = trim(Request("txtaddr3"))
			objNewCust.cust_caddress4 = trim(Request("txtaddr4"))
			objNewCust.cust_caddress5 = trim(Request("txtaddr5"))
			objNewCust.cust_caddress6 = trim(Request("txtaddr6"))
			objNewCust.cust_status = Request("txtStatus")
			objNewCust.cust_dba = trim(Request("txtDBA"))
			objNewCust.cust_phone1 = strtemp
			objNewCust.cust_phone2 = strphone2
			objNewCust.cust_fax  = strtempfax
			objNewCust.buyer =Trim(Request("txtBuyer"))
			objNewCust.keeper =Trim(Request("txtKeeper"))
			objNewCust.cust_salesrep = objCust.cust_salesrep
			'Get codes from the parent account
			objNewCust.cust_class = objCust.cust_class 'Class
			objNewCust.cust_ctermcode = objCust.cust_ctermcode 'terms
			objNewCust.cust_cdivision = objCust.cust_cdivision 'Division
			objNewCust.cust_region = objCust.cust_region 'region
			objNewCust.cust_shipvia = trim(Request("selShip"))'objCust.cust_shipvia 'ShipVia
			objNewCust.cust_spcinst = objCust.cust_spcinst 'Special instructions
			objNewCust.cust_cbackord = objCust.cust_cbackord 'Back Order
			objNewCust.cust_btname = objCust.cust_btname 'BTName
			
			
			
			objNewCust.cust_cadd_user = Trim(Session("rep"))
			objNewCust.cust_cadd_time = Time()
			objNewCust.cust_dadd_date = Date()
			objCust.ChildSet 2,objNewCust
			If objCust.Save Then
				boladd = True
				strEditCust = "Add!#!Customer!#!New!#!" &trim(cstr(Request.Form ("DBA"))) & "!#!" &_
											trim(cstr(Request.Form ("Phone1")))& "!#!" &trim(cstr(Request.Form ("Keeper")))& "!#!" &_
											trim(cstr(Request.Form ("Buyer")))& "!#!" &trim(cstr(Request.Form ("Phone2")))& "!#!" &_
											trim(cstr(Request.Form ("Fax")))& "!#!" &trim(cstr(Request.Form ("Addr1")))& "!#!" &_
											trim(cstr(Request.Form ("Addr2")))& "!#!" &trim(cstr(Request.Form ("Addr3")))& "!#!" &_
											trim(cstr(Request.Form ("Addr4")))& "!#!" &trim(cstr(Request.Form ("Addr5")))& "!#!" &_
											trim(cstr(Request.Form ("Addr6")))
				Add2Log "", Session("CustomerID"),"Add Customer",trim(cstr(objCust.cust_account)),strEditCust
			Else
				boladd = false
			End If
		End If
	End If
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
	<TR><TD>
	<%
	if boladd then
		'if customer added then load its data in the session recordset
		Dim strsql ' as string
		Set conn = server.CreateObject("Adodb.connection")
		Conn.Open Application("DataConnectionString")

		Set rsCustomer = server.CreateObject("Adodb.recordset")
		strsql = "select * from customer where type+Account+store = 'M" & Ucase(Trim(Request("txtAccount"))) & "'"
		rsCustomer.Open strsql,conn,1,3

		Response.Write("Your "& Session("StoreField") &" has been added succefully to the database.")
		Session("CustomerID") = rsCustomer("Account")
		set Session("rscust") = rsCustomer
		Response.Write("<br><a href='custprof.asp?store=" & objNewCust.cust_store & "'>Click here to Edit this  " & Session("StoreField") & "</a>")
	else
		Response.Write("<font color=#ff0000><b>This account already exist.</b></font>")
	end if
	%>
	
	</TD></TR>
</table>
</body>
</Html>
















