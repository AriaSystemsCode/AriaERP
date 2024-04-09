<%@LANGUAGE="VBSCRIPT"%>
<%Response.Buffer = True %>
<HTML>
<HEAD>
<!--#include file="Connections/cnConn.asp" -->
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<TITLE></TITLE>
</HEAD>
<BODY>

<P>
<%

Dim strTokens
strTokens = Trim(Request("Token"))

Dim arTokens
arTokens = Split(strTokens,",")

  Dim objPriv
  Set objPriv = Server.CreateObject("AriaWebSecurity.Privileges")
  
  objPriv.ConnString = MM_cnConn_STRING
  objPriv.Parent = "CRMCUST"
  objPriv.UserID = Request("lstUsers")
  'check if i ve a profile (customer ID selected)
  if request("CustID") <> "" then
	objPriv.Profile = request("CustID")
  end if
  Dim objToken
  Dim intLoop
  For intLoop = 0 To uBound(arTokens)
    Set objToken = Server.CreateObject("AriaWebSecurity.SecurityToken")
    objToken.TokenID = Trim(arTokens(intLoop))
    'Response.Write Trim(arTokens(intLoop))
    objPriv.Tokens.Add objToken
  Next
  'wal_127795 save stores if exists as tokens in the privilege table[start]
  if session("strStores") <> "" then
	'Response.Write session("strRmvStr")
	'Response.End 
	'Dim cnConnection 
	'Set cnConnection = server.CreateObject ("ADODB.Connection")
	'cnConnection.Open Application("SqlServer")	
	'delte any record exist before for the user
	'cnConnection.Execute ("Delete From [Privileges] where cUser_ID='" & Request("lstUsers") & "' and Profile='STORE'")
    'get stores secelted into man array
	arTokens = Split(session("strStores"),", ")
	'Response.Write session("strStores")
	'Response.End 
	For intLoop = 0 To uBound(arTokens)  'Each objToken In Tokens
	  if Trim(arTokens(intLoop)) = "" then
	  else
		'check that this store is not removed first
		if instr(session("strRmvStr"),Trim(arTokens(intLoop))) > 0 then
		else
			'cnConnection.Execute ("INSERT INTO [Privileges] (cUser_ID, cTokenID,Profile) VALUES ('" & Request("lstUsers") & "', '" & Trim(arTokens(intLoop)) & "', 'STORE')")
			 Set objToken = Server.CreateObject("AriaWebSecurity.SecurityToken")
			 'Response.Write session("strRmvStr")
			 'check that its saved before and save the value with a prefix "STR" to know that these values are related to the store value
			 if mid(Trim(arTokens(intLoop)),1,3) = "STR" then
				objToken.TokenID = Trim(arTokens(intLoop))
			 else
				objToken.TokenID = "STR" & Trim(arTokens(intLoop))
			 end if
			 objPriv.Tokens.Add objToken
		end if
	  end if
	Next
  'Response.End 
	'cnConnection.Close
  end if
  'wal_127795 save stores if exists as tokens in the privilege table[end]
 objPriv.SaveTokens
 set objPriv  = nothing
 
 session("strStores") = "" 
 session("strRmvStr") = ""
'Response.Redirect("UserPriv.asp?lstUsers=" & Trim(Request("lstUsers")))
'check where i m coming from to check where to redirect to
if trim(Request("custID")) = "" then
	Response.Redirect("Users.asp?strUserType="& Request("strUserType") &"")
else
	Response.Redirect("custUsers.asp?UserID="& Request.QueryString ("custID") &"")
end if

%>
</P>

</BODY>
</HTML>
