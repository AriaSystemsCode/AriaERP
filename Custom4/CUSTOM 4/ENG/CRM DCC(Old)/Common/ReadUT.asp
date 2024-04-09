<%@ Language=VBScript %>
<%
'Check if the user is valid
Dim objUserPriv
Set objUserPriv = Server.CreateObject("AriaWebSecurity.Privileges")
objUserPriv.ConnString = Application("SqlServer")
objUserPriv.UserID = Session("ID")

If objUserPriv.ValidateUser() Then
	If objUserPriv.UserLvl = "O" Then
		objUserPriv.GetUserTokens()
		'Fill Session string with user tokens
		For intTokenLoop = 1 To objUserPriv.Tokens.Count
			Set objToken = objUserPriv.Tokens.Item(intTokenLoop)
			Session("UserTokens") = Session("UserTokens") & "|" & objToken.TokenID
		Next
	End If
	'Response.Write("User Ready:" & Session("UserTokens"))
	'Response.End 
Else
	Response.Write(" Invalid user ID or password.")
	Response.End 
End If
%>