<%@ Language=VBScript %>
<%

If Session("M_STYVIEW") <> "P" Then
	'wma multiple seasons
'	IF request("slctSeason") <> "" Then
'		Session("Season") = request("slctSeason")
'	Else
'		Session("Season") = Trim(Session("StyleColor"))
'	End If
	'if Trim(Session("selectSeason")) = "All" then
	
	IF request("slctSeason") = "ALL" Then
	
		Session("Season") = Trim(Session("StyleColor"))
	Else
		Session("Season") = request("slctSeason")
	End If

	IF request("slctDivision") <> "" Then
		Session("Division") = request("slctDivision")
	Else
		Session("Division") = Trim(Session("CatDivision"))
	End If
End If
'Response.Write "<font size=2 color=red>"&Session("StoreID")
'Response.End 
	if trim(Request.QueryString ("From")) = "" then
		Session("Start") = request("txtStart")
		Session("Completed") = request("txtCompleted")
	end if
	if Trim(request("slctStore"))="" then
		Session("Store") = ""
		'Session("StoreID")="MAIN"
	else
		Session("Store") = request("slctStore")
		Session("StoreID")=request("slctStore")
	end if 
	Session("PO") = Request("txtpo")
	'wal_127343 add saving of 2 notes fields
	'Response.Write Request.Form ("txtNote1")
	session("Note1") = Request("txtNote1")
	session("Note2") = Request("txtNote2")
	'wal_128815 save Contract ID
	session("ContractID") = Request("txtContID")
	'Response.Write session("Note2")
	'Response.End 
	'update ship address if changed[start]
	if Request.form ("selShipAdd") = "A" then
		Session("Add1") = Request.Form ("txtadd1")
			
		Session("Add2") = Request.Form ("txtadd2")
		
		Session("Add3") = Request.Form ("txtadd3")
			
		Session("Add41") = Request.Form ("txtAdd41")
		Session("Add42") = Request.Form ("txtAdd42")
		Session("Add43") = Request.Form ("txtAdd43")
				
		Session("Add5") = Request.Form ("txtadd5")
			
		Session("chgAdd") = "A"
		Session("Type")   = "A"
		Session("AddChg") = true
	end if
	

'	set RSC=server.CreateObject("ADODB.recordset")
'	RSC.Open strsql,conn , 1, 3
'	RSC.CursorType=1
'	RSC.LockType=3
	if trim(Request.Form ("selShip")) <> "" then
		Session("RSCust").fields("shipvia")=Request.Form ("selShip")
	end if
'	Response.Write Session("RSCust").fields("shipvia")
'	Response.End 
'

	'update ship address if changed[end]
	Set conn=server.CreateObject("ADODB.connection")
	conn.Open Application("DataConnectionString")

	
	Set Session("RSStyStruct")=server.CreateObject("ADODB.recordset")
	strSql="select * from icistru where citemrecty='U'"
	Session("RSStyStruct").open strSql,conn 
	
	'WAL_add var of where i'm coming from
	if Request.QueryString ("From") = "Ord" then
		Response.Redirect "ordCharge.asp?save=T"
	else
		Response.Redirect "custorder.asp?From="&Request.QueryString ("From")
	end if
%>
