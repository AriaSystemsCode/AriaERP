<%@ Language=VBScript %>
<%
Response.Buffer=true

Session("ConnectionString") =Application("DataConnectionString")
Set Connect = Server.CreateObject("ADODB.Connection")
Connect.Open(Session("ConnectionString"))

set Session("RSStyStruct")= nothing
Set Session("RSStyStruct")=server.CreateObject("ADODB.recordset")
	strSql="select * from icistru where citemrecty='U'"
	Session("RSStyStruct").open strSql,connect

Session("RSStyStruct").MoveFirst

IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
Else
	Dim strStyle, strcoll, strasd
	strStyle = ""
	DO While Not Session("RSStyStruct").Eof
		strcoll = request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value))
		Session(Trim(Session("RSStyStruct").Fields("cisegsdes").Value)) = strcoll
		intTemp = Session("RSStyStruct").Fields("nisegsize")
		intCount = cdbl(intTemp) - Len(strcoll)
		Do While intCount > 0
			intCount = intCount - 1
			strcoll = strcoll & " "
		Loop
		strStyle = strStyle & strcoll & Trim(Session("RSStyStruct").Fields("cisegsepr").Value)
		Session("RSStyStruct").MoveNext
	Loop
End IF


'Session("selectReason2") = Request.Form("selectReason")
Set RSGetSty = Server.CreateObject("ADODB.Recordset")
strSql = "select style, desc, desc1 from style where style='" & Ucase(strStyle) & "'"  'Ucase(Trim(request("txtstyle"))) & "'"
RSGetSty.open  strSql, connect

if RSGetSty.eof And RSGetSty.bof then
	'Response.Write()
	'Session("firsttime")=Request("firsttime")

	Session("FindStyleBtnPressed") = "NO"
	Response.Redirect("../common/FindStyle.asp?logintype=R")
'	Response.Redirect("../common/findstyle.asp?logintype=O")
else
	Session("getstyle") = Ucase(strStyle)
	Session("ShortDesc") = RSGetSty("desc")
	Session("LongDesc") = RSGetSty("desc1")
	Session("FindStyleBtnPressed") = "YES"
'	Response.Redirect("ReturnDetail.asp")


	Response.Redirect("../common/FindStyle.asp?logintype=R")
'	Response.Redirect("../common/findstyle.asp?logintype=O")
end if



%>
