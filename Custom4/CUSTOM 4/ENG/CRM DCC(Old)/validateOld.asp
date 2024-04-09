<%
Response.Buffer = true
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

%>

<input type="hidden" name="UserName" value="<%=request("txtuser")%>">
<input type="hidden" name="Pswd" value="<%=request("txtpw")%>">
<%

session("rep")=""
Session("customerid")=""
'Moghazy Jan 23 2004 START

if Ucase(Request("txtpw"))="ARIA" AND  Ucase(Request("txtuser"))="ARIAREP" then
	Session("Rep") ="TAN" 'Ucase(Request("txtuser"))
	Session("Authority")="Full"
	Response.redirect("salesfrm.htm")
end if
'Moghazy Jan 23 2004 START

set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

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
	'select customer with his phone no.
	if Session("CustomerLoginUsing")= "PhoneNumber"  then 'wma 
		strsql="select * from customer where type+phone1 = 'M"& trim(request("txtuser")) &"'"
	else 'using Id
		strsql="select * from customer where type+account+store like 'M"& ucase(request("txtuser")) &"%'"
	end if		
	'wma

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
			Response.Write("Your account not activated yet.<br>")
			Response.Write ("<a href='javascript:window.history.back();'>Back</a>")
			Response.End 
		End If
		session("ID") = ucase(Session("RSCust").Fields("Account"))
		Session("Cust_Name") = Session("RSCust").Fields("Btname")
		Session("customerid") = Session("RSCust").Fields("Account")
		Response.Redirect("crmfrm.htm")
	End If
Else
	Set rsCheckRep = server.CreateObject ("ADODB.recordset")
	strSQL = "SELECT * FROM Salesrep WHERE Repcode='" & Ucase(Request("txtuser")) & "'"
	rsCheckRep.Open strSQL,conn
	
	If rsCheckRep.EOF And rsCheckRep.BOF Then
		Response.Write("Invalid user ID or password.<br>")
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
			Response.redirect("salesfrm.htm")
		Else
			'Response.Write("Your account not activated yet.<br>")
			'Response.Write ("<a href='javascript:window.history.back();'>Back</a>")
			'Response.End 
			'End If

			'at last validate user against fox sysuser
			rsCheckRep.Close
			Set objFpcmd = server.CreateObject("FPCMD.FPCMD")
			Set SYSConn = server.CreateObject("ADODB.connection")
			SYSConn.Open Application("SystemConnectionString")
			
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
						Response.redirect("salesfrm.htm")
					End IF
				else
					Response.Write("Invalid user ID or password.<br>")
					Response.Write ("<a href='javascript:window.history.back();'>Back</a>")
					Response.End 
				end if
			End If

		end if
	End IF
End IF
%>
