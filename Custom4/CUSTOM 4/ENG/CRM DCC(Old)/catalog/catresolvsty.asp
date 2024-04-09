<%@ Language=VBScript %>
<%
'Response.Write(request("sty"))
'strTemp = "<br>"
'Response.Write(strTemp)

IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
Else
	Session("RSStyStruct").MoveFirst
	Dim intStart, intEnd ' as integer
	intStart = 1 
	Session("getstyle") = Request("sty")
	'Response.Write(Request("desc1"))
	Session("LongDesc") = Request("desc1")
	'Response.Write(Session("LongDesc"))
	DO While Not Session("RSStyStruct").Eof
		intEnd = Session("RSStyStruct").fields("nisegsize")
		
		Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) = Mid(Request("sty"),intStart,intEnd)
		intStart = 2 + cdbl(intEnd)
		'Response.Write(Session(Trim(Session("RSStyStruct").fields("cisegsdes"))))
		Session("RSStyStruct").MoveNext
	Loop
End IF
Response.Redirect("catcustord.asp")

%>
