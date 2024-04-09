 <%@ Language=VBScript %>
<%Response.Buffer = true
Response.Expires=-1


'*************************************************************************************
'* Order Confirmation Report From CRM
'* Done By ARD - 29/05/2001
'*************************************************************************************
%>

<%
if Trim(Session("ID")) = "" AND Trim(Session("rep")) = "" then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 

if Trim(Session("ID")) = "" then
	custID = Session("customerid")
else
	custID = Session("ID")
end if

orderNo = Request("OrderNo")'"030010"

'****************************************************************************************
'* Creating The record Sets
'****************************************************************************************
'Data Connection
set conn = server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")


'SysFiles Connection
Set ConnSys = server.CreateObject("ADODB.connection")
ConnSys.ConnectionString  = Application("SystemConnectionString")
'ConnSys.Open Application("SystemConnectionString")
ConnSys.Open

Set RSordHdr = server.CreateObject("ADODB.Recordset")
strsql = "select * from ordhdr where order='" & orderNo & "'"
RSordHdr.Open strsql,Conn 
if RSordHdr.EOF and RSordHdr.BOF then
	Response.Write "No record exists!"
	Response.End 
end if
'WMA Adding ORder Charge Data [Start]
Set rsOrdcharg = server.CreateObject("ADODB.Recordset")
strsql = "select * from Ordcharg where corder='" & orderNo & "'"
rsOrdcharg.Open strsql,Conn 
'WMA Adding ORder Charge Data [End]

Set RSCompName = server.CreateObject("ADODB.Recordset")
strsql = "select * from syccomp where ccomp_id='" & Session("CompanyID") & "'"
RSCompName.Open strsql,ConnSys

strCompanyName = RSCompName.Fields("ccom_name").Value 

Dim RSTemp
Set RSTemp = server.CreateObject("ADODB.Recordset")
strsql = "select * from customer where account='" & custID & "' and type='M'"
RSTemp.Open strsql,Conn

Dim RSCodes
Set RSCodes = server.CreateObject("ADODB.Recordset")

Dim RSLine
Set RSLine = Server.CreateObject("ADODB.Recordset")
strsql = "select * from ordline where order='" & orderNo & "' order by lineno"
RSLine.open strsql,conn

Dim RSScale
Set RSScale = server.CreateObject("ADODB.Recordset")

Dim RSStyimage
Set RSStyimage = server.CreateObject("ADODB.recordset")

Dim RSTemp01
Set RSTemp01 = server.CreateObject("ADODB.recordset")

'****************************************************************************************
'* Checking The Store
'****************************************************************************************
IF Trim(RSordHdr.Fields("store").Value) <> "" then
	dim RSstore
	Set RSStore = server.CreateObject("ADODB.Recordset")
	strsql = "select * from customer where store='" & Trim(RSordHdr.Fields("store").Value) & "' and account='" & custID & "' and type='S'"
	RSstore.Open strsql,conn
End IF


'****************************************************************************************
'* Creating the header RecordSet That will be send to the Crestal Report
'****************************************************************************************
'RecordSets that will send to the Report
'session("rsOrdersMatch").close 
'set session("rsOrdersMatch") = nothing
set session("rsOrdersMatch") = server.CreateObject("ADODB.RecordSet")

'session("rsOrdersMatch").ActiveConnection  = ConnSys
Call session("rsOrdersMatch").fields.append("CompanyName",129,30)
Call session("rsOrdersMatch").fields.append("OrderType",129,1)
Call session("rsOrdersMatch").fields.append("Address1",129,30)
Call session("rsOrdersMatch").fields.append("Address2",129,30)
Call session("rsOrdersMatch").fields.append("Address3",129,30)
Call session("rsOrdersMatch").fields.append("Address4",129,30)
Call session("rsOrdersMatch").fields.append("Address5",129,30)
Call session("rsOrdersMatch").fields.append("order",129,6)
Call session("rsOrdersMatch").fields.append("Phone",129,20)
Call session("rsOrdersMatch").fields.append("Fax",129,20)
Call session("rsOrdersMatch").fields.append("Entered",7)
Call session("rsOrdersMatch").fields.append("Location",129,6)
Call session("rsOrdersMatch").fields.append("Start",7)
Call session("rsOrdersMatch").fields.append("Complete",7)
Call session("rsOrdersMatch").fields.append("Status",129,1)
Call session("rsOrdersMatch").fields.append("Approval",129,10)
Call session("rsOrdersMatch").fields.append("sAddress1",129,30)
Call session("rsOrdersMatch").fields.append("sAddress2",129,30)
Call session("rsOrdersMatch").fields.append("sAddress3",129,100)
Call session("rsOrdersMatch").fields.append("sAddress4",129,30)
Call session("rsOrdersMatch").fields.append("sAddress5",129,30)
Call session("rsOrdersMatch").fields.append("SName",129,30)
Call session("rsOrdersMatch").fields.append("Store",129,8)
Call session("rsOrdersMatch").fields.append("StoreName",129,30)
Call session("rsOrdersMatch").fields.append("shAddress1",129,30)
Call session("rsOrdersMatch").fields.append("shAddress2",129,30)
Call session("rsOrdersMatch").fields.append("shAddress3",129,100)
Call session("rsOrdersMatch").fields.append("shAddress4",129,30)
Call session("rsOrdersMatch").fields.append("shAddress5",129,30)
Call session("rsOrdersMatch").fields.append("ShipVia",129,30)
Call session("rsOrdersMatch").fields.append("SpcInst",129,30)
Call session("rsOrdersMatch").fields.append("Terms",129,30)
Call session("rsOrdersMatch").fields.append("Account",129,30)
Call session("rsOrdersMatch").fields.append("POrder",129,15)
Call session("rsOrdersMatch").fields.append("dept",129,5)
Call session("rsOrdersMatch").fields.append("rep1",129,3)
Call session("rsOrdersMatch").fields.append("rep2",129,3)
Call session("rsOrdersMatch").fields.append("Season",129,30)
Call session("rsOrdersMatch").fields.append("StyleCode",129,19)
Call session("rsOrdersMatch").fields.append("TotQty",5)
Call session("rsOrdersMatch").fields.append("TotAmount",5)
Call session("rsOrdersMatch").fields.append("Disc",5)
'wal_128815 add Contract ID field
Call session("rsOrdersMatch").fields.append("ContRef",129,30)
'WMA Adding ORder Charge Data [Start]
Call session("rsOrdersMatch").fields.append("nShipAmt",5)
Call session("rsOrdersMatch").fields.append("nfreight",5)
Call session("rsOrdersMatch").fields.append("nweight",5)
Call session("rsOrdersMatch").fields.append("ninsur",5)
Call session("rsOrdersMatch").fields.append("ntax_rate",5)
Call session("rsOrdersMatch").fields.append("ntax_amt",5)
Call session("rsOrdersMatch").fields.append("ntotchg",5)
Call session("rsOrdersMatch").fields.append("ndiscount",5)
Call session("rsOrdersMatch").fields.append("nCod",5)
Call session("rsOrdersMatch").fields.append("ndeposit",5)
Call session("rsOrdersMatch").fields.append("n_cartons",5)
Call session("rsOrdersMatch").fields.append("nPriChg",5)
Call session("rsOrdersMatch").fields.append("nDropChg",5)
Call session("rsOrdersMatch").fields.append("lDropChg",11)
Call session("rsOrdersMatch").fields.append("lPriChg",11)
'WMA Adding ORder Charge Data [End]

session("rsOrdersMatch").open

'****************************************************************************************
'* Creating the Lines RecordSet That will be send to the Crestal Report
'****************************************************************************************
' Create Lines Record Set
set session("rsOrdersMatch_lines") = server.CreateObject("ADODB.RecordSet")

Call session("rsOrdersMatch_lines").fields.append("orderType",129,1)
Call session("rsOrdersMatch_lines").fields.append("order",129,6)
Call session("rsOrdersMatch_lines").fields.append("lineno",129,6)
Call session("rsOrdersMatch_lines").fields.append("Style",129,19)
Call session("rsOrdersMatch_lines").fields.append("Group",129,1)
Call session("rsOrdersMatch_lines").fields.append("TotQty",5)
Call session("rsOrdersMatch_lines").fields.append("price",5)
Call session("rsOrdersMatch_lines").fields.append("desc",129,60)
Call session("rsOrdersMatch_lines").fields.append("qty1",5)
Call session("rsOrdersMatch_lines").fields.append("qty2",5)
Call session("rsOrdersMatch_lines").fields.append("qty3",5)
Call session("rsOrdersMatch_lines").fields.append("qty4",5)
Call session("rsOrdersMatch_lines").fields.append("qty5",5)
Call session("rsOrdersMatch_lines").fields.append("qty6",5)
Call session("rsOrdersMatch_lines").fields.append("qty7",5)
Call session("rsOrdersMatch_lines").fields.append("qty8",5)
Call session("rsOrdersMatch_lines").fields.append("sz1",129,5)
Call session("rsOrdersMatch_lines").fields.append("sz2",129,5)
Call session("rsOrdersMatch_lines").fields.append("sz3",129,5)
Call session("rsOrdersMatch_lines").fields.append("sz4",129,5)
Call session("rsOrdersMatch_lines").fields.append("sz5",129,5)
Call session("rsOrdersMatch_lines").fields.append("sz6",129,5)
Call session("rsOrdersMatch_lines").fields.append("sz7",129,5)
Call session("rsOrdersMatch_lines").fields.append("sz8",129,5)
Call session("rsOrdersMatch_lines").fields.append("Complete",7)
Call session("rsOrdersMatch_lines").fields.append("Note1",12)
Call session("rsOrdersMatch_lines").fields.append("Note2",12)
Call session("rsOrdersMatch_lines").fields.append("bolNote1",11)
Call session("rsOrdersMatch_lines").fields.append("bolNote2",11)
Call session("rsOrdersMatch_lines").fields.append("StyImage",205,2147483647,234)
'WAL_ add new fields[start]
Call session("rsOrdersMatch_lines").fields.append("gros_price",5)
Call session("rsOrdersMatch_lines").fields.append("disc",5)
Call session("rsOrdersMatch_lines").fields.append("comm",5)
'WAL_ add new fields[end]
session("rsOrdersMatch_lines").open 


intTotQty = 0 
totAmount = 0 


'*************************************************************************************
'* Collecting the data For The Lines Recordset [Start] 
'*************************************************************************************
Do  Until RSLine.Eof 
	session("rsOrdersMatch_lines").addnew
	session("rsOrdersMatch_lines").fields("orderType").value = RSordHdr.Fields("cordtype").Value
	session("rsOrdersMatch_lines").fields("order").value = RSordHdr.Fields("order").value
	session("rsOrdersMatch_lines").fields("lineno").value = RSLine.Fields("lineno").value
	session("rsOrdersMatch_lines").fields("Style").value = RSLine.Fields("Style").value
	session("rsOrdersMatch_lines").fields("Group").value = RSLine.Fields("Group").value
	session("rsOrdersMatch_lines").fields("Totqty").value = RSLine.Fields("totqty").value
	session("rsOrdersMatch_lines").fields("price").value = RSLine.Fields("price").value
	'WMA changing this line for sub totals
	'totAmount = cdbl(totAmount) + cdbl(RSLine.Fields("gros_price").value) * cdbl(RSLine.Fields("totqty").value)
	totAmount = cdbl(totAmount) + cdbl(RSLine.Fields("price").value) * cdbl(RSLine.Fields("totqty").value)
	'WMA [End]
	session("rsOrdersMatch_lines").fields("desc").value = RSLine.Fields("desc1").value
	session("rsOrdersMatch_lines").fields("qty1").value = RSLine.Fields("qty1").value
	session("rsOrdersMatch_lines").fields("qty2").value = RSLine.Fields("qty2").value
	session("rsOrdersMatch_lines").fields("qty3").value = RSLine.Fields("qty3").value
	session("rsOrdersMatch_lines").fields("qty4").value = RSLine.Fields("qty4").value
	session("rsOrdersMatch_lines").fields("qty5").value = RSLine.Fields("qty5").value
	session("rsOrdersMatch_lines").fields("qty6").value = RSLine.Fields("qty6").value
	session("rsOrdersMatch_lines").fields("qty7").value = RSLine.Fields("qty7").value
	session("rsOrdersMatch_lines").fields("qty8").value = RSLine.Fields("qty8").value
	
	strsql = "select * from scale where type='S' and scale='" & RSLine.Fields("scale").value & "'"
	RSScale.Open strsql,conn 
	
	session("rsOrdersMatch_lines").fields("sz1").value = RSScale.Fields("sz1").value
	session("rsOrdersMatch_lines").fields("sz2").value = RSScale.Fields("sz2").value
	session("rsOrdersMatch_lines").fields("sz3").value = RSScale.Fields("sz3").value
	session("rsOrdersMatch_lines").fields("sz4").value = RSScale.Fields("sz4").value
	session("rsOrdersMatch_lines").fields("sz5").value = RSScale.Fields("sz5").value
	session("rsOrdersMatch_lines").fields("sz6").value = RSScale.Fields("sz6").value
	session("rsOrdersMatch_lines").fields("sz7").value = RSScale.Fields("sz7").value
	session("rsOrdersMatch_lines").fields("sz8").value = RSScale.Fields("sz8").value
	intTotQty = intTotQty + session("rsOrdersMatch_lines").Fields("qty1").value + session("rsOrdersMatch_lines").Fields("qty2").value +session("rsOrdersMatch_lines").Fields("qty3").value + session("rsOrdersMatch_lines").Fields("qty4").value + session("rsOrdersMatch_lines").Fields("qty5").value + session("rsOrdersMatch_lines").Fields("qty6").value + session("rsOrdersMatch_lines").Fields("qty7").value + session("rsOrdersMatch_lines").Fields("qty8").value
	RSScale.Close 
	
	'WAL_stop reading reading style image from object file 
	'get image from "STYLE" folder in CRM[start]
	strsql = "select * from style where style='" & RSLine.Fields("Style").value & "'"
	RSStyimage.Open strsql,conn
	
	StrStyMajor = RSStyimage.Fields("cstymajor").Value 
	 
	dim obj
	set obj = server.CreateObject ("ADODB.Stream")
	obj.Type = 1
	obj.Open 
	'load image
	imgPath = server.MapPath ("../styimg/"&trim(StrStyMajor)&".jpg")
	Set fs = CreateObject("Scripting.FileSystemObject")
    If (fs.FileExists(trim(imgPath))) Then
		obj.LoadFromFile (imgPath)
		if not obj.EOS then
			session("rsOrdersMatch_lines").fields("StyImage").value = obj.Read
		end if
	end if
	RSStyimage.Close
	obj.Close ()
	set obj = nothing
	set fs=nothing
	'strsql = "select * from objlink where cobjlink='" & Trim(StrStyMajor) & "' and cobjlnktyp='S'"
	'RSStyimage.Open strsql,conn
	
	'IF  Not(RSStyimage.EOF And RSStyimage.BOF) Then
	'	strsql = "select * from objects where cobject_id='" & RSStyimage.Fields("cobject_id").Value & "'"
	'	RSTemp01.Open strsql,conn 
	'	
	'	IF Not(RSTemp01.Eof And RSTemp01.BOF) Then
	'		'wal_ check if the img obj is saved or the path
	'		if trim(RSTemp01("Gobject")) = "" then
	'			
	'		else
	'			session("rsOrdersMatch_lines").fields("StyImage").value = RSTemp01.Fields("Gobject").Value 
	'		end if
	'	End IF
	'	RSTemp01.Close
	'Else
	'Here we should go and check the style image and append it to the temp recordset
	'open the file for binary
	'Dim flImage
	'flImage = Server.MapPath("../STYIMG")
	'flImage = flImage&"\"&Trim(StrStyMajor)&".JPG"
	
	'Response.Write(flImage)
	'Response.End 
	'End IF
	'RSStyimage.Close 
	'WAL_stop reading reading style image from object file 
	'get image from "STYLE" folder in CRM[end]
	'Response.Write RSLine.Fields("complete").value&"<br>"
	session("rsOrdersMatch_lines").fields("complete").value = RSLine.Fields("complete").value
	IF Len(Trim(RSordHdr.Fields("Note1").value)) > 0 then
		session("rsOrdersMatch_lines").fields("bolNote1").value = True
	Else
		session("rsOrdersMatch_lines").fields("bolNote1").value = False
	End IF
	session("rsOrdersMatch_lines").fields("Note1").value = RSordHdr.Fields("Note1").value
	
	IF Len(Trim(RSordHdr.Fields("Note2").value)) > 0 then
		session("rsOrdersMatch_lines").fields("bolNote2").value = True
	Else
		session("rsOrdersMatch_lines").fields("bolNote2").value = False
	End IF
	session("rsOrdersMatch_lines").fields("Note2").value = RSordHdr.Fields("Note2").value
	'WAL_ add new fields[start]
	session("rsOrdersMatch_lines").fields("gros_price").value = RSLine.Fields("gros_price").value
	session("rsOrdersMatch_lines").fields("Disc").value = RSLine.Fields("disc_pcnt").value
	totDisc = cdbl(totDisc) + cdbl(RSLine.Fields("disc_pcnt").value)
	session("rsOrdersMatch_lines").fields("Comm").value = RSLine.Fields("comm1").value
	'WAL_ add new fields[end]
	session("rsOrdersMatch_lines").updatebatch
	
	RSLine.MoveNext 
Loop
'sResponse.End 
'*************************************************************************************
'* Collecting the data For The Lines Recordset [End] 
'*************************************************************************************


'*************************************************************************************
'* Collecting the data For The Header Recordset [Start] 
'*************************************************************************************
IF  Not (RSordHdr.EOF And RSordHdr.BOF)  then
	session("rsOrdersMatch").addnew
	session("rsOrdersMatch").fields("OrderType").value = RSordHdr.Fields("cordtype").Value 
	session("rsOrdersMatch").fields("CompanyName").value = strCompanyName
	session("rsOrdersMatch").fields("Address1").value =  RSCompName.Fields("caddress1").value
	session("rsOrdersMatch").fields("Address2").value =  RSCompName.Fields("caddress2").value
	session("rsOrdersMatch").fields("Address3").value =  RSCompName.Fields("caddress3").value
	session("rsOrdersMatch").fields("Address4").value =  RSCompName.Fields("caddress4").value
	session("rsOrdersMatch").fields("Address5").value =  RSCompName.Fields("caddress5").value
	session("rsOrdersMatch").fields("order").value = RSordHdr.Fields("order").value
	session("rsOrdersMatch").fields("phone").value = RSCompName.Fields("ccom_phon")
	session("rsOrdersMatch").fields("fax").value = RSCompName.Fields("ccom_fax")
	session("rsOrdersMatch").fields("Entered").value = RSordHdr.Fields("Entered").value
	session("rsOrdersMatch").fields("Location").value = RSordHdr.Fields("cwarecode").value
	session("rsOrdersMatch").fields("Start").value = RSordHdr.Fields("start").value
	session("rsOrdersMatch").fields("complete").value = RSordHdr.Fields("complete").value
	session("rsOrdersMatch").fields("Status").value = RSordHdr.Fields("status").value
	session("rsOrdersMatch").fields("Approval").value = RSordHdr.Fields("Approval").value
	'Response.Write Trim(RSordHdr.Fields("store").Value)
	'Response.End 
	IF Trim(RSordHdr.Fields("store").Value) <> "" or Trim(RSordHdr.Fields("store").Value) = "MAIN" then
		session("rsOrdersMatch").fields("sAddress1").value = RSstore.Fields("caddress12").Value
		session("rsOrdersMatch").fields("sAddress2").value = RSstore.Fields("caddress22").Value 
		session("rsOrdersMatch").fields("sAddress3").value = Trim(RSstore.Fields("caddress32").Value) & ", " & Trim(RSstore.Fields("caddress42").Value) & "  " & Trim(RSstore.Fields("caddress52").Value)
		session("rsOrdersMatch").fields("sAddress4").value = RSstore.Fields("caddress62").Value
		session("rsOrdersMatch").fields("Sname").value = RSstore.Fields("stName").Value 
	Else
		session("rsOrdersMatch").fields("sAddress1").value = RSTemp.Fields("caddress12").Value
		session("rsOrdersMatch").fields("sAddress2").value = RSTemp.Fields("caddress22").Value 
		session("rsOrdersMatch").fields("sAddress3").value = Trim(RSTemp.Fields("caddress32").Value) & ", " & Trim(RSTemp.Fields("caddress42").Value) & "  " & Trim(RSTemp.Fields("caddress52").Value)
		session("rsOrdersMatch").fields("sAddress4").value = RSTemp.Fields("caddress62").Value
		session("rsOrdersMatch").fields("Sname").value = RSTemp.Fields("btName").Value 
	End if	
	
	session("rsOrdersMatch").fields("Store").value = RSordhdr.Fields("store").Value 
	'WAL_ check if the ship address was changed then display new address
	if RSordHdr("Alt_ShpTo") = true then
		session("rsOrdersMatch").fields("StoreName").value  = RSordHdr.Fields("stName").Value 
		session("rsOrdersMatch").fields("shAddress1").value = RSordHdr.Fields("caddress1").Value
		session("rsOrdersMatch").fields("shAddress2").value = RSordHdr.Fields("caddress2").Value
		session("rsOrdersMatch").fields("shAddress3").value = RSordHdr.Fields("caddress3").Value
		session("rsOrdersMatch").fields("shAddress4").value = RSordHdr.Fields("caddress4").Value

	else
		IF Trim(RSordHdr.Fields("store").Value) <> "" then
			session("rsOrdersMatch").fields("StoreName").value = Trim(RSstore.Fields("stname").Value)
			session("rsOrdersMatch").fields("shAddress1").value = RSstore.Fields("caddress1").Value
			session("rsOrdersMatch").fields("shAddress2").value = RSstore.Fields("caddress2").Value 
			session("rsOrdersMatch").fields("shAddress3").value = Trim(RSstore.Fields("caddress3").Value) & ", " & Trim(RSstore.Fields("caddress4").Value) & "  " & Trim(RSstore.Fields("caddress5").Value)
			session("rsOrdersMatch").fields("shAddress4").value = RSstore.Fields("caddress6").Value
			session("rsOrdersMatch").fields("sAddress1").value = RSstore.Fields("caddress12").Value
			session("rsOrdersMatch").fields("sAddress2").value = RSstore.Fields("caddress22").Value 
			session("rsOrdersMatch").fields("sAddress3").value = Trim(RSstore.Fields("caddress32").Value) & ", " & Trim(RSstore.Fields("caddress42").Value) & "  " & Trim(RSstore.Fields("caddress52").Value)
			session("rsOrdersMatch").fields("sAddress4").value = RSstore.Fields("caddress62").Value
			session("rsOrdersMatch").fields("Sname").value = RSstore.Fields("stName").Value 
		Else
			session("rsOrdersMatch").fields("StoreName").value  = RSTemp.Fields("btName").Value 
			session("rsOrdersMatch").fields("shAddress1").value = RSTemp.Fields("caddress1").Value
			session("rsOrdersMatch").fields("shAddress2").value = RSTemp.Fields("caddress2").Value
			session("rsOrdersMatch").fields("shAddress3").value = Trim(RSTemp.Fields("caddress3").Value) & ", " & Trim(RSTemp.Fields("caddress4").Value) & "  " & Trim(RSTemp.Fields("caddress5").Value)
			session("rsOrdersMatch").fields("shAddress4").value = RSTemp.Fields("caddress6").Value
			session("rsOrdersMatch").fields("sAddress1").value = RSTemp.Fields("caddress12").Value
			session("rsOrdersMatch").fields("sAddress2").value = RSTemp.Fields("caddress22").Value 
			session("rsOrdersMatch").fields("sAddress3").value = Trim(RSTemp.Fields("caddress32").Value) & ", " & Trim(RSTemp.Fields("caddress42").Value) & "  " & Trim(RSTemp.Fields("caddress52").Value)
			session("rsOrdersMatch").fields("sAddress4").value = RSTemp.Fields("caddress62").Value
			session("rsOrdersMatch").fields("Sname").value = RSTemp.Fields("btName").Value 
		end if	
	End IF
	
	session("rsOrdersMatch").fields("totAmount").value = totAmount
	'session("rsOrdersMatch").fields("Disc").value = totDisc'RSordhdr.Fields("disc").Value
	session("rsOrdersMatch").fields("Disc").value = RSordhdr.Fields("disc").Value  
	
	Strsql = "select * from codes where cfld_name='SHIPVIA' and crltfield='N' and cdefcode='N' and ccode_no='" & RSordHdr.Fields("shipvia").Value  & "'"
	RSCodes.Open strsql,conn
	
	IF Not(RSCodes.EOF and RSCodes.BOF) Then
		strShipVia = RSCodes.Fields("cdiscrep").Value 
	End IF
	RSCodes.Close 
	session("rsOrdersMatch").fields("shipVia").value = strShipVia
	
	Strsql = "select * from codes where cfld_name='SPCINST' and crltfield='N' and cdefcode='N' and ccode_no='" & RSordHdr.Fields("spcinst").Value  & "'"
	RSCodes.Open strsql,conn
	
	IF Not(RSCodes.EOF and RSCodes.BOF) Then
		strSpcInst = RSCodes.Fields("cdiscrep").Value 
	End IF
	RSCodes.Close 
	session("rsOrdersMatch").fields("SpcInst").value = strSpcInst

	Strsql = "select * from codes where cfld_name='CTERMCODE' and crltfield='N' and cdefcode='N' and ccode_no='" & RSordHdr.Fields("ctermcode").Value  & "'"
	RSCodes.Open strsql,conn

	IF Not(RSCodes.EOF and RSCodes.BOF) Then
		strSpcInst = RSCodes.Fields("cdiscrep").Value 
	End IF
	RSCodes.Close 
	session("rsOrdersMatch").fields("Terms").value = strSpcInst
	session("rsOrdersMatch").fields("Account").value = RSordHdr.Fields("Account").Value
	if session("CustPO") = "T" and Trim(Session("rep")) = "" then
		session("rsOrdersMatch").fields("porder").value = RSordHdr.Fields("custpo").Value
	else
		session("rsOrdersMatch").fields("porder").value  = "F"
	end if
	'wal_ check if the value of contract ID is to be displayed or not
	if session("ContID") = "T" then
		session("rsOrdersMatch").fields("ContRef").value = RSordhdr.Fields("cContRef").Value 
	else
		session("rsOrdersMatch").fields("ContRef").value  = "F"
	end if
	session("rsOrdersMatch").fields("dept").value = RSordHdr.Fields("dept").Value
	session("rsOrdersMatch").fields("rep1").value = RSordHdr.Fields("rep1").Value
	'session("rsOrdersMatch").fields("rep2").value = RSordHdr.Fields("rep2").Value
	
	Strsql = "select * from codes where cfld_name='SEASON' and crltfield='N' and cdefcode='N' and ccode_no='" & RSordHdr.Fields("season").Value  & "'"
	RSCodes.Open strsql,conn

	IF Not(RSCodes.EOF and RSCodes.BOF) Then
		strSSeason = RSCodes.Fields("cdiscrep").Value 
		RSCodes.Close 
	End IF
	session("rsOrdersMatch").fields("Season").value = strSSeason
	
	Dim RSSTyStruct
	strsql = "select * from icistru where citemrecty='U' and cisegno='1'"
	Set RSSTyStruct = server.CreateObject("ADODB.Recordset")
	RSSTyStruct.Open strsql,conn 
	
	IF Not(RSSTyStruct.EOF and RSSTyStruct.BOF) Then
		strStyStruct = RSSTyStruct.Fields("ciseghead").Value 
		RSSTyStruct.Close
	End IF
	session("rsOrdersMatch").fields("StyleCode").value = strStyStruct
	session("rsOrdersMatch").fields("totqty").value = intTotQty
	
	'WMA Adding ORder Charge Data [Start]
	IF Not(rsOrdcharg.EOF and rsOrdcharg.BOF) Then
		session("rsOrdersMatch").fields("nShipAmt").value = rsOrdcharg.Fields("nShipAmt").Value
		session("rsOrdersMatch").fields("nfreight").value = rsOrdcharg.Fields("nfreight").Value
		session("rsOrdersMatch").fields("nweight").value = rsOrdcharg.Fields("nweight").Value
		session("rsOrdersMatch").fields("ninsur").value = rsOrdcharg.Fields("ninsur").Value
		session("rsOrdersMatch").fields("ntax_rate").value = rsOrdcharg.Fields("ntax_rate").Value
		session("rsOrdersMatch").fields("ntax_amt").value = rsOrdcharg.Fields("ntax_amt").Value
		session("rsOrdersMatch").fields("ntotchg").value = rsOrdcharg.Fields("ntotchg").Value
		session("rsOrdersMatch").fields("ndiscount").value = rsOrdcharg.Fields("ndiscount").Value
		session("rsOrdersMatch").fields("nCod").value = rsOrdcharg.Fields("nCod").Value
		session("rsOrdersMatch").fields("ndeposit").value = rsOrdcharg.Fields("ndeposit").Value
		session("rsOrdersMatch").fields("n_cartons").value = rsOrdcharg.Fields("n_cartons").Value
		session("rsOrdersMatch").fields("nPriChg").value = rsOrdcharg.Fields("nPriChg").Value
		session("rsOrdersMatch").fields("nDropChg").value = rsOrdcharg.Fields("nDropChg").Value
		session("rsOrdersMatch").fields("lPriChg").value = rsOrdcharg.Fields("lPriChg").Value
		session("rsOrdersMatch").fields("lDropChg").value = rsOrdcharg.Fields("lDropChg").Value
	end if
	'WMA Adding ORder Charge Data [End]
	
	session("rsOrdersMatch").updatebatch
End IF
'*************************************************************************************
'* Collecting the data For The Header Recordset [End] 
'*************************************************************************************


%>
<!--
Sending data To the Crystal Report
-->
<html>
<head>
<meta NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<title>Check Order Status - Order # <%=request("orderNo")%></title>
<!--LINK rel="stylesheet" type="text/css" href="../reports/report.css"-->
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</head>
<body bgcolor="#aecae6" topmargin="0" leftmargin="0">
<br>
<%
reportname = "orderconfA.rpt"
If Not IsObject (session("oApp")) Then                              
'	Set session("oApp") = Server.CreateObject("Crystal.CRPE.Application")
Set session("oApp") = Server.CreateObject("CrystalRuntime.Application")
End If                                                                
Path = Request.ServerVariables("PATH_TRANSLATED")        

While (Right(Path, 1) <> "\" And Len(Path) <> 0)                      
iLen = Len(Path) - 1                                                  
Path = Left(Path, iLen)                                               
Wend                                                                  

If IsObject(session("oRpt")) then
	Set session("oRpt") = nothing
End if

Set session("oRpt") = session("oApp").OpenReport(path & reportname, 1)
'wal_128815,1 set field values in the report for the contract label[Start]
Set fs = Session("oRpt").FormulaFields
Set f1 = fs.Item(19)
'session("ContractField") = ""
if trim(session("ContractField")) = "" then
	session("ContractField") = "Contract #"
end if
strRef = replace(session("ContractField"),"@","")
'Response.Write "<font size=2>" &"'" & trim(session("ContractField")) & "'"
'Response.End 
f1.Text = "'" & trim(session("ContractField")) & "'"
'"'" & trim(session("ContractField")) & "'"

' Send the Line recordset [Start] 
	set session("subRep")= session("oRpt").OpenSubReport("OrdLines")
	Set fs_sub = Session("subRep").FormulaFields
	'NEK[Start]1/9/2003
	set fs_sub1 = fs_sub.Item(4)
	set fs_sub2 = fs_sub.Item(3)
	fs_sub1.Text = "'"& session("Currency") &"'"
	fs_sub2.Text = "'"& session("CurrencyAlign") &"'"
	'NEK [End]1/9/2003
	set subDatabase = session("subRep").Database
	set subTables = subDatabase.Tables
	set subTable1 = subTables.Item(1)
	subTable1.SetPrivateData 3, session("rsOrdersMatch_lines")
' Send the Line recordset [End] 


' Send the header recordset [Start] 
session("oRpt").DiscardSavedData
Set fs = Session("oRpt").FormulaFields
Set f1 = fs.Item(11)
Set f2 = fs.Item(12)
Set f3 = fs.Item(13)
Set f4 = fs.Item(14)
f1.Text = "'"& session("CustField") &"'"
f2.Text = "'"& session("StoreField") &"'"
'NEK [Start]
f3.Text = "'"& session("Currency") &"'"
f4.Text = "'"& session("CurrencyAlign") &"'"
'NEK [End]
set Database = session("oRpt").Database
set Tables = Database.Tables
set Table1 = Tables.Item(1)
Table1.SetPrivateData 3, session("rsOrdersMatch")
' Send the header recordset [End] 
'Response.Write(session("rsOrdersMatch").recordCount)
'Response.End 


On Error Resume Next                                                  
err.Clear 
session("oRpt").ReadRecords


If Err.Number <> 0 Then                                               
  Response.Write "<font size=2>An Error has occured on the server in attempting to access the data source --" & err.Description & err.Source 
  'Response.End 
Else
  If IsObject(session("oPageEngine")) Then                              
  	set session("oPageEngine") = nothing
  End If
set session("oPageEngine") = session("oRpt").PageEngine
End If                                                                

viewer = Request.QueryString ("viewer")
'viewer = Request.form("viewer")
viewer="ActiveX"

'viewer = "Java-Plug-in"
'This line collects the value passed for the viewer to be used, and stores
'it in the "viewer" variable.

IF cstr(viewer) = "ActiveX" THEN
%>
<!-- #include file="../crystal/SmartViewerActiveX.asp" -->
<%
ElseIf cstr(viewer) = "Netscape Plug-in" then
%>
<!-- #include file="../crystal/ActiveXPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "JVM" then
%>
<!-- #include file="../crystal/SmartViewerJava.asp" -->
<%
ElseIf cstr(viewer) = "Java-Plug-in" then
%>
<!-- #include file="../crystal/JavaPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "HTML Frame" then
	Response.Redirect("htmstart.asp")
Else
	Response.Redirect("rptserver.asp")
End If
'The above If/Then/Else structure is designed to test the value of the "viewer" varaible
'and based on that value, send down the appropriate Crystal Smart Viewer.
%>
<br>
<br>
<BR>
<Center>
<%
if Trim(Request.QueryString("PageType"))="O" then
%>
<a href = "Ord_Conf_Resp.asp?OrderNo=<%=Trim(orderNo)%>"><Img border=0 src="../Images/<%=session("Theme")%>/back.gif"></a>
<%
Else
%>
<a href = "../catalog/ord_conf.asp?"><Img border=0 src="../Images/<%=session("theme")%>/back.gif"></a>
<%
End IF
%>
</CENTER>
<br>
<br>
</body>
</html>
<%
'session("rsOrdersMatch").close
'conn.Close 
%>