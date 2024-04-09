<%@ Language=VBScript %>
<%Response.Buffer=true%>

<%
	
' check to see if there is something missing in the previous page .. 
	missingFlag="NO"
	if Trim( Request.Form("selectStore")) = "" then 
		Session("StoreValue") = "MAIN"
	else 
		Session("StoreValue") = Trim (Request.Form("selectStore"))
	end if 
	
	if Trim(Request.Form("txtCartonNo")) ="" then
		Response.Write (" You didn't choose number of cartons <BR>")
		if len(trim(session("rep")))>0 then
		Response.Write ("<A HREF=""repreturnadd.asp"">Go back and choose a valid Number.</A>")
		else
		Response.Write ("<A HREF=""returnadd.asp"">Go back and choose a valid Number.</A>")
		end if
		missingFlag = "YES"
	end if 

if missingFlag ="YES" then
else
'Save values of the form in the session..

	Session("txtEntered") = Request("txtEntered")
	Session("txtVoid") = Request.Form("txtVoid")
	if Request("selectDivision") <> "" then
		Session("Division") = Trim(Request("selectDivision"))
	Else
		Session("Division") = Session("CatDivision")
	end if
	'Session("Division") = request("selectDivision")
	Session("selectStore")    = Session("StoreValue")
	Session("selectReason")   = Request.Form("selectReason")
	Session("selectReason2")  = Request.Form("selectReason")
	strSplit = split(Request.Form("selectLocation"),";")
	if instr(1,Request.Form("selectLocation"),";") > 0 then
		strSplit = split(Request.Form("selectLocation"),";")
		'Response.Write Request.Form("selectLocation")
		'Response.End 
		Session("selectLocation") =strSplit(0)
		Session("LocDesc") =strSplit(1)
	else
		Session("selectLocation") =Request.Form("selectLocation")
		Session("LocDesc") = Request.Form("selectLocation")
	end if
	Session("CartonsNo") = Request.Form("txtCartonNo")
	'HDM E302075,1 12/15/2002 [Start] add invoice info to the session variables needed
	Session("Invoice") = Request.Form("Invoice")
	Session("RetInvoice") = Request.Form("RetInvoice")
	'HDM E302075,1 [End]
	'WAL E302075,4 add info of order and P/O # [start]
	Session("Order") = Request.Form ("txtOrder")
	Session("P/O") = Request.Form ("txtPO")
	'WAL E302075,4 add info of order and P/O # [end]
' Remove values from session
	Session("getstyle")=""


	Set conn=server.CreateObject("ADODB.connection")
	conn.Open Application("DataConnectionString")

	Set Session("rsRetStyStruct")=server.CreateObject("ADODB.recordset")
	strSql="select * from icistru where citemrecty='U'"
	Session("rsRetStyStruct").open strSql,conn 

	Response.Redirect("ReturnRedirect.asp")
	end if ' of missing flag ..
	
	
%>

