<%@ Language=VBScript %>
<%
Response.Buffer = True
IF LEN(Request("SlctColor"))<>0 THEN
IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF

set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")


set RSStyle=server.CreateObject("ADODB.recordset")
strSql="select * from style where style='" & Ucase(Request("SlctColor")) & "'"
RSStyle.open strSql,conn 

Session("text1") = ""
Session("text2") = ""
Session("text3") = ""
Session("text4") = ""
Session("text5") = ""
Session("text6") = ""
Session("text7") = ""
Session("text8") = ""

Select Case request("Typeline")
Case "U":
'IF request("Typeline")="U" Then

		if Session("RSLine").EOF AND Session("RSLine").BOF then 
		Session("RSLine").addnew
		else
			Session("RSLine").MoveFirst()
		
			Do while not Session("RSLine").EOF
				if (Request("SlctColor") = Trim(Session("RSLine").Fields("style")) ) then
					foundFlag ="YES"
					Exit Do
				end if 
				Session("RSLine").MoveNext()
			Loop
			if foundFlag<>"YES" Then
				Session("RSLine").AddNew			
			End IF
		End if ' first time add


	
	

	Session("RSLine").Fields("cordtype").Value = "O"
	Session("RSLine").Fields("account").Value = Session("RSCust").Fields("account").value
  Session("RSLine").Fields("style").value = Request("SlctColor")
  
  Response.Write ("<br>|" & GetStyPrice(Request("SlctColor"),1) & "|<br>")
  
  Session("RSLine").Fields("desc1").value = RSStyle("desc1")
  Session("RSLine").Fields("scale").value = RSStyle("scale")
  Session("RSLine").Fields("prepak").value = RSStyle("prepak")
  Session("RSLine").Fields("nsugretpri").value = 0
  'Session("RSLine").Fields("start").value = Session("Start")
  'Session("RSLine").Fields("complete").value = Session("Completed")
  Session("RSLine").Fields("season").value = Session("Season")
  

	IF Len(request("txtord1"))=0 Then
		Session("RSLine").fields("qty1") = 0
	Else
		Session("RSLine").fields("qty1") = request("txtord1")
	END IF
	
	IF Len(request("txtord2"))=0 Then
		Session("RSLine").fields("qty2") = 0
	Else
		Session("RSLine").fields("qty2") = request("txtord2")
	END IF

 IF Len(request("txtord3"))=0 Then
		Session("RSLine").fields("qty3") = 0
	Else
		Session("RSLine").fields("qty3") = request("txtord3")
	END IF

	IF Len(request("txtord4"))=0 Then
		Session("RSLine").fields("qty4") = 0
	Else
		Session("RSLine").fields("qty4") = request("txtord4")
	END IF

	IF Len(request("txtord5"))=0 Then
		Session("RSLine").fields("qty5") = 0
	Else
		Session("RSLine").fields("qty5") = request("txtord5")
	END IF

	IF Len(request("txtord6"))=0 Then
		Session("RSLine").fields("qty6") = 0
	Else
		Session("RSLine").fields("qty6") = request("txtord6")
	END IF

	IF Len(request("txtord7"))=0 Then
		Session("RSLine").fields("qty7") = 0
	Else
		Session("RSLine").fields("qty7") = request("txtord7")
	END IF

	IF Len(request("txtord8"))=0 Then
		Session("RSLine").fields("qty8") = 0
	Else
		Session("RSLine").fields("qty8") = request("txtord8")
	END IF

Dim inttemp ' as integer

inttemp = cdbl(Session("RSLine").fields("qty1")) + cdbl(Session("RSLine").fields("qty2"))
inttemp = inttemp + cdbl(Session("RSLine").fields("qty3")) + cdbl(Session("RSLine").fields("qty4"))
inttemp = inttemp + cdbl(Session("RSLine").fields("qty5")) + cdbl(Session("RSLine").fields("qty6"))
inttemp = inttemp + cdbl(Session("RSLine").fields("qty7")) + cdbl(Session("RSLine").fields("qty8"))
IF foundFlag="YES" Then
	Session("ordQty") = Cdbl(Session("ordQty")) - Cdbl(Session("RSLine").fields("totqty"))
	Session("ordAmount") = Session("ordAmount") - (cdbl(Session("RSLine").fields("totqty")) * cdbl(GetStyPrice(Request("SlctColor"),1)))
	'Session("ordAmount") = Session("ordAmount") - (cdbl(Session("RSLine").fields("totqty")) * cdbl(RSStyle("pricea")))
	Session("ordQty") = Cdbl(Session("ordQty")) + cdbl(inttemp)
	Session("ordAmount") = Session("ordAmount") + (cdbl(inttemp) * cdbl(GetStyPrice(Request("SlctColor"),1)))
	'Session("ordAmount") = Session("ordAmount") + (cdbl(inttemp) * cdbl(RSStyle("pricea")))
Else
	Session("ordQty") = Cdbl(Session("ordQty")) + cdbl(inttemp)
	Session("ordAmount") = Session("ordAmount") + (cdbl(inttemp) * cdbl(GetStyPrice(Request("SlctColor"),1)))
	'Session("ordAmount") = Session("ordAmount") + (cdbl(inttemp) * cdbl(RSStyle("pricea")))
End IF

Session("RSLine").fields("totqty") = inttemp
Session("RSLine").fields("price") = GetStyPrice(Request("SlctColor"),1)
'Session("RSLine").fields("price") = RSStyle("pricea")

Session("RSLine").fields("desc1") = Session("LongDesc")

'	IF Len(request("txtord"))=0 Then
'		objRSLine("totqty") = 0
'	Else
'		objRSLine("totqty") = request("txtord")
'	END IF

'Else
Case "R":
	If Session("RSLine").EOF AND Session("RSLine").BOF then 
	Else
		Session("RSLine").MoveFirst()
					
		Do while not Session("RSLine").EOF
			If (Request("SlctColor") = Trim(Session("RSLine").Fields("style")) ) then
				Session("ordQty") = Cdbl(Session("ordQty")) - cdbl(Session("RSLine").fields("totqty"))
				Session("ordAmount") = Cdbl(Session("ordAmount")) - (cdbl(Session("RSLine").fields("totqty")) * cdbl(RSStyle("pricea")))
				Session("RSLine").Delete()
				Exit Do
			End If 
			Session("RSLine").MoveNext()
		Loop
	End if ' first time add

End Select


Session("getstyle")=""
Session("OrderFlag") = "X"
Session("LongDesc1") = Session("LongDesc")
Session("LongDesc") = ""
Session("ShortDesc") = ""


Response.Redirect("catcustord.asp")

Session("text1") = ""
Session("text2") = ""
Session("text3") = ""
Session("text4") = ""
Session("text5") = ""
Session("text6") = ""
Session("text7") = ""
Session("text8") = ""

'Response.Write(inttemp)
ELSE
Response.Redirect("catcustord.asp")
END IF
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
</HEAD>
<BODY>

<P>&nbsp;</P>

</BODY>
</HTML>
<%'*!*************************************************************
'*! Name      : GetStyPrice
'*! Developer : Ahmed M. Reda
'*! Date      : 06/21/2001
'*! Purpose   : Get style Price if there a contract or not
'*!*************************************************************
'*! Calls       : None
'*!*************************************************************
'*! Passed Parameters : strStyle    : Style code
'*!                     Qty         : Quantity
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
'strtempsql = strtempsql & "SELECT ORDER FROM ORDHDR WHERE ACCOUNT='" & CUSTID & "' AND CORDTYPE='C'"
'strtempsql = strtempsql & " and status <> 'X' and status <> 'B' and start >= {" & Date() & "} and complete <= {" & Date() & "}"
''nek-Optimization
strtempsql = strtempsql & "Select ORDER from ORDHDR where Account+CORDTYPE+order like '"&CUSTID&"C%' "
strtempsql = strtempsql & " and status <> 'X' and status <> 'B' and start >= {" & Date() & "} and complete <= {" & Date() & "}"
''end of Optimization
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
	strtempsql = strtempsql & "Select * from customer where type+account+store like 'M"&custid&"%'"
	'Response.Write(strtempsql)
	Set RSSTYCustomer = server.CreateObject("ADODB.Recordset")
	RSSTYCustomer.Open strtempsql,StyConn
	IF Not(RSSTYCustomer.EOF And RSSTYCustomer.BOF) Then
	
		strtempsql = "select * from style where style='" & strStyle & "'"
		'Response.Write(strtempsql)
		Set RSSTYStyle = server.CreateObject("adodb.recordset")
		RSSTYStyle.Open strtempsql,StyConn
		IF Not(RSSTYStyle.EOF And RSSTYStyle.BOF) Then
	'Response.Write("ok")
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
GetStyPrice = cint(GetStyPrice)
End Function
%>