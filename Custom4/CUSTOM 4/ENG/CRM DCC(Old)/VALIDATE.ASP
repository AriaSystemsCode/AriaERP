<%
Response.Buffer = true
session("rep")=""
Session("customerid")=""
Application("Store") =""
application("StyGroup") = ""
application("WareCode")  = ""
application("styProfile")=""
application("PriceCode")=""
application("strStyles") = ""
Application("userName") = ""

session("Store") =""
session("StyGroup") = ""
session("WareCode")  = ""
session("styProfile")=""
session("PriceCode")=""
session("strStyles") = ""
session("userName") = ""

set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

Function CheckLogin(User) 'NEK
		SR_textfile = server.mappath("Admin/CrmSetup/Setup/SR.txt")
		Set FileSystem = server.CreateObject("Scripting.FileSystemObject")
		FileIsExists=FileSystem.FileExists(SR_textfile) 
		if (FileIsExists) then 'File Exists
			set File=FileSystem.OpenTextFile(SR_textfile,1)
			ss = File.readAll
			Arrusers=split(ss,chr(13)+chr(10))
			foundInFile = false
			for i=lbound(Arrusers) to ubound(Arrusers)
				if User = trim(Arrusers(i)) then
					foundInFile = true
				end if	 
			next
			if (foundInFile) then
				CheckLogin = true
			else
				CheckLogin = false
			end if
		else 'File Doesn't Exists
			CheckLogin = false
		end if		
End Function
'wal_127795 add function to get color code size
function getColor()
	Set rsStyStr = server.CreateObject("ADODB.RECORDSET")
	strsql = "select * from icistru where citemrecty='U'"
	rsStyStr.Open strsql,Conn,2,4
	if not rsStyStr.EOF then
	    rsStyStr.MoveFirst
	    'get color code size
		rsstystr.Filter  = "cisegtype='C'"
		if not rsStyStr.EOF then
			session("colorSize") = cdbl(RSStyStr("nisegsize"))
		end if
		rsstystr.Filter = ""
		rsStyStr.MoveFirst
		'get style(major field) code size
		rsstystr.Filter = "lsegendmaj=1"
		if not rsStyStr.EOF then
			Dim intCount
			intCount = 0
				
			Do while Not rsStyStr.Eof
				intCount = intCount + cdbl(RSStyStr("nisegsize"))
				rsStyStr.MoveNext 
			Loop
			session("styleSize") = cdbl(intCount)
		end if
		rsstystr.Filter = ""
	end if
	rsStyStr.Close ()
end function
%>

<input type="hidden" name="UserName" value="<%=request("txtuser")%>">
<input type="hidden" name="Pswd" value="<%=request("txtpw")%>">
<%



'Moghazy Jan 23 2004 START
if Ucase(Request("txtpw"))="ARIA" AND  Ucase(Request("txtuser"))="ARIAREP" then
	'wma get first rep insteed of tan
	set rsAdminSales = conn.Execute("Select * from salesrep")
	if not rsAdminSales.eof then
		Session("Rep") = rsAdminSales("repcode")
	else
		Session("Rep") ="TAN" 'Ucase(Request("txtuser"))
	end if		
	rsAdminSales.close
	set rsAdminSales = nothing

	Session("Authority")="Full"
	Response.redirect("salesfrm.htm")
end if
'Moghazy Jan 23 2004 START



set Session("RSCust")=server.CreateObject("ADODB.recordset")

Dim objUserPriv
Set objUserPriv = Server.CreateObject("AriaWebSecurity.Privileges")
If Session("DBType") = "ORACLE" then
	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=OraOLEDB.Oracle;Data Source="&Trim(session("SQLServer"))&";user id=" & Trim(Session("SqlUserName")) & ";password=" & Trim(Session("SqlPassWord")) & ""	
Else
	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog="&Trim(session("DBName"))&";Data Source="&Trim(session("SQLServer"))&";uid=" & Trim(Session("SqlUserName")) & ";pwd=" & Trim(Session("SqlPassWord")) & ""
End If
objUserPriv.ConnString = Application("SqlServer")
'Response.Write Application("SqlServer")
'Response.End 
'validate login user
if request("lstType")= "C" then'case customer
	Dim strsql ' as string
	'wal_127795 check if i m using multi user login
	if Session("CustomerLoginUsing")= "User"  then
		session("STRCount") = 0
		Session("Store") = ""
		'Check if the user is valid
		objUserPriv.Profile = Trim(Request("txtCust"))
		objUserPriv.UserID  = trim(request("txtuser"))
		objUserPriv.UserPWD = Trim(Request("txtpw"))
		'reset this application variable
		Application(trim(request("txtuser"))) = ""
		If objUserPriv.ValidateUser() Then
			Application(trim(request("txtuser")) & "Lvl") = objUserPriv.UserLvl
			If objUserPriv.UserLvl = "O" Then
				objUserPriv.GetUserTokens()
				'Fill Session string with user tokens
				For intTokenLoop = 1 To objUserPriv.Tokens.Count
					Set objToken = objUserPriv.Tokens.Item(intTokenLoop)
					'get the count of stores applied with this user
					if instr(objToken.TokenID,"STR") > 0 then
						session("STRCount") = session("STRCount")+1
						'save the store IDs in an application var to work on with in the application
						if Session("Store") = "" then
							'Application("Store") =  mid(objToken.TokenID,4)
							Session("Store") =  mid(objToken.TokenID,4)
						else
							Session("Store") = trim(Session("Store")) & "," & mid(objToken.TokenID,4)
						end if						
					end if
					Application(trim(request("txtuser"))) = Application(trim(request("txtuser"))) & "|" & objToken.TokenID
				Next
			End If
		Else
'			Response.Write("Your account not activated yet.<br>")
			Response.Write("Please check your user ID and password!<br>")
			Response.Write ("<a href='javascript:window.history.back();'>Back</a>")
			Response.End 
		End If
		
		strsql="select * from customer where type+account+store like 'M"& ucase(request("txtCust")) &"%'"
		
		Session("RSCust").open strsql,conn,2,4
		
		session("ID") = ucase(Session("RSCust").Fields("Account"))
		'wal make it ann application var not to expire
		Application("ID") = ucase(Session("RSCust").Fields("Account"))
		Session("Cust_Name") = Session("RSCust").Fields("Btname")
		Session("customerid") = ucase(Session("RSCust").Fields("Account"))
		Session("userID") = trim(request("txtuser"))
		Application("userID") = trim(request("txtuser"))
		'Application("Store") = mid(Application("Store"),2)
		'Response.Write  Application("Store") 
		'Response.End 
		'check if the user has one store then get its name and save it to display it into the application
		if cint(session("STRCount")) = 1 then
			dim rsstore
			set rsstore= server.CreateObject ("ADODB.recordset")
			
			rsstore.open "Select * from Customer where type+account+store like 'S" & ucase(request("txtCust")) & Application("Store") & "%'" ,Conn,1,3
			 
			if not rsstore.EOF then
				Session("Cust_Name") = rsstore("stname")
			end if
			'Response.Write Session("Cust_Name")  '"Select * from Customer where type+account+store like 'S" & ucase(request("txtCust")) & Application("Store") & "%'" 
			'Response.End
		end if
	else
		'select customer with his phone no.
		if Session("CustomerLoginUsing")= "PhoneNumber"  then 'wma 
			strsql="select * from customer where type+phone1 = 'M"& trim(request("txtuser")) &"'"
		else 'using Id
			strsql="select * from customer where type+account+store like 'M"& ucase(request("txtuser")) &"%'"
		end if		
		'wma
'Response.Write strsql
		Session("RSCust").open strsql,conn,2,4
		'If user not found in the customer file then display error message
		If Session("RSCust").EOF AND  Session("RSCust").BOF then
			Response.Write("Invalid user ID!<br>")
			Response.Write ("<a href='javascript:window.history.back();'>Back</a>")
			Response.End
		Else
			'Check if the user is valid
			objUserPriv.UserID  = Session("RSCust").Fields("Account")
			objUserPriv.UserPWD = Trim(Request("txtpw"))
			'reset this application variable
			Application(Trim(Session("RSCust").Fields("Account"))) = ""
			If objUserPriv.ValidateUser() Then
				Application(Trim(Session("RSCust").Fields("Account")) & "Lvl") = objUserPriv.UserLvl
				If objUserPriv.UserLvl = "O" Then
					objUserPriv.GetUserTokens()
					'Fill Session string with user tokens
					For intTokenLoop = 1 To objUserPriv.Tokens.Count
						Set objToken = objUserPriv.Tokens.Item(intTokenLoop)
						Application(Trim(Session("RSCust").Fields("Account"))) = Application(Session("RSCust").Fields("Account")) & "|" & objToken.TokenID
					Next
				End If
			Else
				'Response.Write("Your account not activated yet.<br>")
				Response.Write("Please check your user ID and password!<br>")
				Response.Write ("<a href='javascript:window.history.back();'>Back</a>")
				Response.End 
			End If
			Application("ID") = ucase(Session("RSCust").Fields("Account"))
			session("ID") = ucase(Session("RSCust").Fields("Account"))
			Session("Cust_Name") = Session("RSCust").Fields("Btname")
			Session("customerid") = ucase(Session("RSCust").Fields("Account"))
		end if
	End If
	'wal_039385 get defualt warehouse for customer if exists
	set rsware = server.CreateObject ("ADODB.Recordset")
	rsware.Source = "select * from customerwarecode where ccustid='"& trim(session("ID")) &"'"
	rsware.ActiveConnection = Application("SqlServer")
	rsware.Open 
	if not rsware.EOF then
		if not isnull(rsware("cwarecode"))then 
			application("WareCode") = rsware("cwarecode")
			session("WareCode") = rsware("cwarecode")
		end if
		'wal_130731 get style group too
		if not isnull(rsware("cStyleGroup")) then
			application("StyGroup") = rsware("cStyleGroup")
			session("StyGroup") = rsware("cStyleGroup")
		end if
	end if
	rsware.Close
	
	'Response.Write len(application("StyGroup"))
	'Response.End 
	'wal_131300 check if the user has a style profile saved case multi user login
	if trim(session("CustomerLoginUsing")) = "User" then
		dim rsuser
		set rsuser = server.CreateObject ("adodb.recordset")
		rsuser.Source = "select * from syuuser where cuser_id = '"&trim(request("txtuser"))&"' and profile='" & Trim(Request("txtCust")) &"'"
		rsuser.ActiveConnection = Application("SqlServer")
		rsuser.Open
		'save the user name
		Application("userName") = rsuser("cusr_name")
		session("userName") = rsuser("cusr_name")
		if rsuser("cStyleProfileCode") <> "" then
			'get the info for thAT profile
			dim rsStyProfileHdr,rsStyProfileDt,cnnSQL, rsStyGroup
			set cnnSQL  = server.CreateObject ("Adodb.connection")
			cnnSQL.Open Application("SqlServer") 
	
			set rsStyProfileHdr = server.CreateObject ("Adodb.recordset")
			set rsStyProfileDt = server.CreateObject ("Adodb.recordset")
			set rsStyGroup = server.CreateObject ("Adodb.recordset")
	'Response.Write "select * StyleProfileHeader where cstyleprofilecode='"& trim(rsuser("cStyleProfileCode")) &"'"
	'Response.End 
			rsStyProfileHdr.Open "select * from StyleProfileHeader where cStyleProfileCode='"& trim(rsuser("cStyleProfileCode")) &"'",cnnSQL,1,3
			rsStyProfileDt.Open "select * from styleprofiledetail where cStyleProfileCode='"&trim(rsuser("cStyleProfileCode"))&"'",cnnSQL,1,3
			do while not rsStyProfileDt.EOF
				if session("strStyles") = "" then
					session("strStyles") = "'" & Trim(rsStyProfileDt("cstyle"))& "'"
				else
					session("strStyles") = application("strStyles") & ", " & "'" & Trim(rsStyProfileDt("cstyle")) & "'"
				end if
			rsStyProfileDt.MoveNext 
			loop
			session("styProfile")=rsuser("cStyleProfileCode")
			session("PriceCode")=rsuser("priccode")
		end if
	end if
	Response.Redirect("crmfrm.htm")
Else
	Set SYSConn = server.CreateObject("ADODB.connection")
	SYSConn.Open Application("SystemConnectionString")
			
	Set rsCheckRep = server.CreateObject ("ADODB.recordset")
	strSQL = "SELECT * FROM Salesrep WHERE Repcode='" & Ucase(Request("txtuser")) & "'"
	'strSQL = "SELECT * FROM syuuser WHERE cuser_id='" & Ucase(Request("txtuser")) & "'"	
	'Response.Write strSQL
	'Response.End 
	rsCheckRep.Open strSQL,conn
	'rsCheckRep.Open strSQL,SYSConn
	
	If rsCheckRep.EOF And rsCheckRep.BOF Then
		Response.Write("Invalid user ID!<br>")
		Response.Write ("<a href='javascript:window.history.back();'>Back</a>")
		Response.End 
	Else
	
		'wma
		'validate user against SQL Server sysuser
		objUserPriv.UserID  = Ucase(Request("txtuser")) 'Session("RSCust").Fields("Account")
		objUserPriv.UserPWD = Trim(Ucase(Request("txtpw")))
		'reset this application variable
		Application(Trim(Ucase(Request("txtuser")))) = ""
		Application(Trim(Ucase(Request("txtuser"))) & "Lvl") = ""
		If objUserPriv.ValidateUser() Then
			Application(Trim(Ucase(Request("txtuser"))) & "Lvl") = objUserPriv.UserLvl
			If objUserPriv.UserLvl = "O" Then
				objUserPriv.GetUserTokens()
				'Fill Session string with user tokens
				For intTokenLoop = 1 To objUserPriv.Tokens.Count
					Set objToken = objUserPriv.Tokens.Item(intTokenLoop)
					Application(Trim(Ucase(Request("txtuser")))) = Application(Ucase(Request("txtuser"))) & "|" & objToken.TokenID
				Next
			End If
			
			Session("Rep") = Ucase(Request("txtuser"))
			Session("Authority")=""
			'getColor()
			Response.redirect("salesfrm.htm")
		Else
			'Response.Write("Your account not activated yet.<br>")
			'Response.Write ("<a href='javascript:window.history.back();'>Back</a>")
			'Response.End 
			'End If

			'at last validate user against fox sysuser
			rsCheckRep.Close
			Set objFpcmd = server.CreateObject("FPCMD.FPCMD")
			
			
			strSQL = "SELECT * FROM syuuser WHERE cuser_id='" & Ucase(Request("txtuser")) & "'"
			rsCheckRep.Open strSQL,SYSConn
			Dim strLogPassWord
		
			strLogPassWord = objfpcmd.docmd("return sys(2007,'" & Ucase(Request("txtpw")) & "')")
		
			If rsCheckRep.EOF Then
				Response.Write("Invalid User ID!<br>")
				Response.Write ("<a href='javascript:window.history.back();'>Back</a>")
				Response.End 
			Else
				if Trim(rsCheckRep("cusr_pass")) = strLogPassWord then
					isSpecial = CheckLogin(Ucase(Request("txtuser")))
					if(isSpecial) then
						Session("Rep") = Ucase(Request("txtuser"))
						Session("Authority")="Full"
						Response.redirect("salesfrm.htm")
					Else
						Session("Rep") = Ucase(Request("txtuser"))
						Session("Authority")=""
						Response.redirect("salesfrm.htm")'("main.asp")
					End IF
				else
					Response.Write("Invalid Password!<br>")
					Response.Write ("<a href='javascript:window.history.back();'>Back</a>")
					Response.End 
				end if
			End If

		end if
	End IF
End IF
%>
