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
  
  Dim objToken
  Dim intLoop
  For intLoop = 0 To uBound(arTokens)
    Set objToken = Server.CreateObject("AriaWebSecurity.SecurityToken")
    objToken.TokenID = Trim(arTokens(intLoop))
    objPriv.Tokens.Add objToken
  Next
  objPriv.SaveTokens
'Response.Redirect("UserPriv.asp?lstUsers=" & Trim(Request("lstUsers")))
Response.Redirect("Users.asp?strUserType="& Request("strUserType") &"")

%>
</P>

</BODY>
</HTML>
