<%@ Language=VBScript %>
<%
Response.Buffer = true
foundFlag="YES"

'make fields in the table empty 
Session("text1") = ""
Session("text2") = ""
Session("text3") = ""
Session("text4") = ""
Session("text5") = ""
Session("text6") = ""
Session("text7") = ""
Session("text8") = ""
Session("TotalAmount") = 0
Session("TotalQty") = 0
'HDM E302075,1 12/15/2002 [Start] If There is an invoice and the user selected
' to fill with its lines then fill the lines before redirect
'Response.Write(UCase(Session("RetInvoice")))
'Response.End 
If Trim(Session("Invoice")) <> "" And UCase(Session("RetInvoice")) = "YES" Then
	'Build a connection to the database
	set Connt = server.CreateObject("ADODB.Connection")
	constr = Trim(Application("DataConnectionString"))
	connt.Open constr
	'Get the invoice details
	Dim rsInvoice
	Set rsInvoice = Server.CreateObject("ADODB.RecordSet")
	
	Dim strInvSQL
	'strInvSQL = "Select invline.*,invhdr.status From InvLine,invhdr Where InvLine.invoice+STR(InvLine.lineno,6) Like '" & Trim(Session("Invoice")) & "%' And invhdr.status='C' and invhdr.invoice=invline.invoice"
	strInvSQL = "Select invline.*,invhdr.status From InvLine,invhdr Where InvLine.invoice+STR(InvLine.lineno,6) Like '" & Trim(Session("Invoice")) & "%' And invhdr.status <> 'V' and invhdr.invoice=invline.invoice"
	'Response.Write strInvSQL&"<br>"
	rsInvoice.Open strInvSQL, connt, 3, 1
	'Response.Write rsInvoice.EOF
	'Response.End 
	If rsInvoice.EOF And rsInvoice.BOF Then
		'If no invoice found empty out the invoice variable
		Session("Invoice") = ""
	Else
	rsInvoice.MoveFirst 
		Set Session("rsReturnLine") = server.CreateObject("ADODB.recordset")
		Set rsReturnLine1 = server.CreateObject("ADODB.recordset")

		strSql = "select * from raline where rano+style+cra_linno=''"
		Session("rsReturnLine").open  strSql, connt, 2, 4
		'Start Filling the lines
		dim count
		count=1
		Do While Not rsInvoice.EOF 
			'Build a new empty recordset
		
			Session("rsReturnLine").AddNew
							
			'Session("rsReturnLine").Fields("cordtype").Value = "O"
			'Session("rsReturnLine").Fields("account").Value = Session("RSCust").Fields("account").value
			If Len(trim(session("rep")))>0 Then
				Session("rsReturnLine").Fields("account").Value =session("customerid")
			Else
				Session("rsReturnLine").Fields("account").Value =session("id")
			End If
			Session("rsReturnLine").Fields("style").value = rsInvoice("Style")
			Session("rsReturnLine").Fields("cra_linno").value = count
			'Session("rsReturnLine").Fields("scale").value = rsRetStyStruct("scale")
			'Session("rsReturnLine").Fields("prepak").value = rsRetStyStruct("prepak")
			'Session("rsReturnLine").Fields("nsugretpri").value = 0
			'Session("rsReturnLine").Fields("season").value = Session("Season")
			Session("rsReturnLine").Fields("Reason").value =Session("selectReason2")

			'If Len(request("txtord1"))=0 Then
			'	Session("rsReturnLine").fields("qty1") = 0
			'Else
				Session("rsReturnLine").fields("qty1") = rsInvoice("QTY1")
			'END IF
			Session("rsReturnLine").fields("qty2") = rsInvoice("QTY2")
			Session("rsReturnLine").fields("qty3") = rsInvoice("QTY3")
			Session("rsReturnLine").fields("qty4") = rsInvoice("QTY4")
			Session("rsReturnLine").fields("qty5") = rsInvoice("QTY5")
			Session("rsReturnLine").fields("qty6") = rsInvoice("QTY6")
			Session("rsReturnLine").fields("qty7") = rsInvoice("QTY7")
			Session("rsReturnLine").fields("qty8") = rsInvoice("QTY8")
								
							
			Dim inttemp ' as integer
							
			inttemp = cdbl(Session("rsReturnLine").fields("qty1")) + cdbl(Session("rsReturnLine").fields("qty2"))
			inttemp = inttemp + cdbl(Session("rsReturnLine").fields("qty3")) + cdbl(Session("rsReturnLine").fields("qty4"))
			inttemp = inttemp + cdbl(Session("rsReturnLine").fields("qty5")) + cdbl(Session("rsReturnLine").fields("qty6"))
			inttemp = inttemp + cdbl(Session("rsReturnLine").fields("qty7")) + cdbl(Session("rsReturnLine").fields("qty8"))
			Session("rsReturnLine").fields("totqty") = inttemp
			Session("rsReturnLine").fields("price") = cdbl(rsInvoice("price"))
			count=count+1
			rsInvoice.MoveNext
			Session("OrderFlag") = "X"
			
		Loop
		Session("Seq") = count - 1
		'Compute the Total Quantity and the total amount and put them in the session to let 
		'them be displayed in the Return Detail form. 
		'1- Total Account
		tempTotalQty = 0 
		tempTotalAmount = 0

		Session("rsReturnLine").MoveFirst()
		IF Session("rsReturnLine").EOF AND Session("rsReturnLine").BOF then 
			Session("TotalQty")= 0
			Session("TotalAmount") = 0
		ELSE
			DO While Not Session("rsReturnLine").EOF
				tempTotalQty =tempTotalQty + cdbl(Session("rsReturnLine").fields("totqty"))
				intAmount = cdbl(Session("rsReturnLine").fields("totqty")) * cdbl(Session("rsReturnLine").fields("price"))
				tempTotalAmount = tempTotalAmount + intAmount
				Session("rsReturnLine").MoveNext()
			LOOP
			Session("rsReturnLine").MoveFirst()
			Session("TotalQty")= tempTotalQty
			Session("TotalAmount") = tempTotalAmount
		END IF
	End If
	
	'Response.Write("Fill Lines Here")
	'Response.End 
Else
End If
'HDM E302075,1 [End]
Response.Redirect("ReturnDetail.asp")
%>
