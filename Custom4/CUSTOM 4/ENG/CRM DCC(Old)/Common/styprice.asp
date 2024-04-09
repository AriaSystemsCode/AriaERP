
<%
Response.Buffer=true
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
	</script>	
<%End if

' To handle the case of the rep.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF



'*!*************************************************************
'*! Name      : GetStyPrice
'*! Developer : Ahmed M. Reda
'*! Date      : 06/21/2001
'*! Purpose   : Get style Price if there a contract or not
'*!*************************************************************
'*! Calls       : None
'*!*************************************************************
'*! Passed Parameters : lcStyle    : Style code
'*!                     lcGPrice   : Gross Price
'*!                     lcNPrice   : Net Price
'*!                     lcDiscount : Discount Percent
'*!*************************************************************
'*! Return      : Style price
'*!*************************************************************
FUNCTION GetStyPrice(strStyle,Qty)
Dim StyConn
Dim RSStyOrdHdr
Set StyConn = server.CreateObject("ADODB.Connection")
StyConn.Open Application("DataConnectionString")
Set RSStyOrdHdr = server.CreateObject("ADODB.RECOrdSET")
strtempsql = ""
strtempsql = strtempsql & "SELECT ORDER FROM ORDHDR WHERE ACCOUNT='" & CUSTID & "' AND CORDTYPE='C'"
strtempsql = strtempsql & " and status <> 'X' and status <> 'B' and start >= {" & Date() & "} and complete <= {" & Date() & "}"

RSStyOrdHdr.open strtempsql,StyConn

Response.Write(RSStyOrdHdr.recordcount)
GetStyPrice = 0
IF Not(RSStyOrdHdr.EOF And RSStyOrdHdr.BOF) Then ' Contract Exist
	Dim RSStyLine
	Set RSStyLine = server.CreateObject("adodb.recordset")
	strtempsql = ""
	strtempsql = strtempsql & "SELECT * FROM ORDLINE WHERE ORDER = '" & RSSTYORDHDR.FIELDS("ORDER").VALUE & "' and style='" & style & "'"
	RSStyLine.Open strtempsql,StyConn
	If Not(RSStyLine.EOF And RSStyLine.Bof)then 
		GetStyPrice = RSStyLine.Fields("Price").Value 
	End IF
Else ' Contract Not Exist
	strtempsql = ""
	strtempsql = strtempsql & "Select * from customer where type='M' and account='" & CustID & "'"
	Response.Write(strtempsql)
	Set RSSTYCustomer = server.CreateObject("ADODB.Recordset")
	RSSTYCustomer.Open strtempsql,StyConn
	IF Not(RSSTYCustomer.EOF And RSSTYCustomer.BOF) Then
		strtempsql = "select * from style where style='" & style & "'"
		Set RSSTYStyle = server.CreateObject("adodb.recordset")
		RSSTYStyle.Open strtempsql,StyConn
		IF Not(RSSTYStyle.EOF And RSSTYStyle.BOF) Then
			Select Case RSSTYCustomer.Fields("pricelvl").Value 
			Case "A"
				GetStyPrice = RSSTYStyle.Fields("Pricea").Value 
			Case "B"
				GetStyPrice = RSSTYStyle.Fields("Priceb").Value 
			Case "C"
				GetStyPrice = RSSTYStyle.Fields("Pricec").Value 
			Case "Q"
				IF qty < RSSTYStyle.Fields("natqtyb").Value then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Pricea").Value)
				End if
				
				IF qty > RSSTYStyle.Fields("natqtyb").Value and qty < RSSTYStyle.Fields("natqtyc").Value then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Priceb").Value)
				End if
				
				IF qty > RSSTYStyle.Fields("natqtyc").Value then
					GetStyPrice = cint(qty) * cint(RSSTYStyle.Fields("Pricec").Value) 
				End if
			End select
		End IF
	End IF
End IF

End Function
%>


