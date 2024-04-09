<%@ Language=VBScript %>

<%
Response.Buffer = true

	Session("Store") = ""
	'Session("StoreID")="MAIN"
	Set conn=server.CreateObject("ADODB.connection")
	conn.Open Application("DataConnectionString")
			

'ARD - Modifay the Catalog [Start]
	IF Session("CatDivision") <> "NONE" and Session("StyleColor") <> "NONE" Then
		Response.Write("ok")
		Session("Season") = request("slctSeason")
		Session("Division") = request("slctDivision")
	Else
		'Response.Write "ok"
		'Response.End 
		Session("Season") = Session("catSeason")
		Session("Division") = session("catDivision")
		IF request("slctSeason") <> "" Then
			Session("season")= request("slctSeason")
		End IF
	End IF
	
	Set RSSetups = Server.CreateObject("ADODB.RecordSet")
	RSSetups.Open "SELECT Mdata_def FROM setups where Capp_id = 'SO' AND Cfld_name = 'M_COMPDATE'",Conn
	
	intCompDate = RSSetups("Mdata_def")
	Session("Start") =date()
	Session("Completed") = date() + intCompDate

	'ARD - Modifay the Catalog [End]	


	Set Session("RSStyStruct")=server.CreateObject("ADODB.recordset")
	strSql="select * from icistru where citemrecty='U'"
	Session("RSStyStruct").open strSql,conn 

	'WMA avoid empty RSLine in every click cataloge in the menu [start]
	'Set session("RSLine") = server.CreateObject("ADODB.recordset")
	'strsql = "select * from ordline where 1=0"
	'Session("RSLine").open  strSql, conn, 2, 4
	'WMA avoid empty RSLine in every click cataloge in the menu [start]
	
'	Response.Write "M_STYVIEW===="&Session("M_STYVIEW")
'	Response.End 
	IF Session("M_STYVIEW") = "P" Then
		Response.Redirect("catpage.asp")	
	Else
		If Request.QueryString("PageID")="C" Then
			Response.Redirect("Newcat.asp")
		Else
			'Response.Write "ok"
			'Response.End 
			Response.Redirect("CatSearch.asp")		
		End if
	End IF
	

%>


