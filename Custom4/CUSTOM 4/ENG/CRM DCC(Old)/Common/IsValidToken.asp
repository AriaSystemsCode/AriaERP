<%

Function IsValidToken(strUID, strConnString, strTokenID)
	Dim objUserPriv
	Set objUserPriv = Server.CreateObject("AriaWebSecurity.Privileges")
	objUserPriv.ConnString = strConnString
	objUserPriv.UserID = strUID
	IsValidToken =  objUserPriv.IsValidToken(strTokenID)
End Function
	
%>