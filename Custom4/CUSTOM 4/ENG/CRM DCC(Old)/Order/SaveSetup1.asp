<%@ Language=VBScript %>
<%
Response.Buffer = True
'Dim strAppPath
'Dim strFilePath

'strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))

'If Right(strAppPath,1) = "\" Then
'	strFilePath = "admin\crmsetup\setup\setup.txt"
'Else
'	strFilePath = "\admin\crmsetup\setup\setup.txt"
									
'End If
'Response.Write(strAppPath & strFilePath)

'Dim objFileSystem
'Dim objTextFile
'Response.Write(strAppPath & strFilePath)
'Response.End 
'Set objFileSystem = Server.CreateObject("Scripting.FileSystemObject")
'Set objTextFile = objFileSystem.CreateTextFile(strAppPath & strFilePath , True)
Dim objFile
Set objFile = Server.CreateObject("Scripting.FileSystemObject")
	
Dim strAppPath
Dim strFilePath

strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))

If Right(strAppPath,1) = "\" Then
	strFilePath = "admin\crmsetup\setup\setup.txt"
Else
	strFilePath = "\admin\crmsetup\setup\setup.txt"
End If

'Response.Write(strAppPath & strFilePath)

'on error resume next
'Set objTextFile objFile.CreateTextFile(strAppPath & strFilePath , True)
Set objTxtFile = objFile.OpenTextFile(strAppPath & strFilePath,1,true)

'On Error GoTo 0
Dim strLine


'wma save only in second page
'strFile = objTxtFile.ReadAll
'Session("strLine") = strFile

Session("strLine") = Session("strLine") & " AND CatDivision=" & Request("lstDivision")
'Session("strLine") = Session("strLine") & " AND StyleColor=" & Request("lstSeason")
Session("strLine") = Session("strLine") & " AND StyleColor=" & Request("txtSeason")
Session("strLine") = Session("strLine") & " AND ShowCatalogVal=" & Request("txtgrp")
'WAL_ add var for TERM CODE and BANK CODE[start]
Session("strLine") = Session("strLine") & " AND TermCode=" & Request("lstTerm")
Session("strLine") = Session("strLine") & " AND WareHous=" & Request("lstWareHous")
Session("strLine") = Session("strLine") & " AND BankCode=" & Request("lstBanks")
Session("strLine") = Session("strLine") & " AND ARPTYPE=" & Request("lstPay")
Session("strLine") = Session("strLine") & " AND PriChg=" & Request("txtPri")
Session("strLine") = Session("strLine") & " AND DropChg=" & Request("txtDrop")
Session("strLine") = Session("strLine") & " AND LowQty=" & Request("txtQty")
Session("strLine") = Session("strLine") & " AND Days=" & Request("txtDays")
'wal_127343 add Cust PO and UPS
Session("strLine") = Session("strLine") & " AND CustPO=" & Request("radCustPO")
Session("strLine") = Session("strLine") & " AND UPS=" & Request("radUPS")
'WAL_ add var for TERM CODE and BANK CODE[end]
'WAL_ add var for show avaliablity and add order size per line[start]
Session("strLine") = Session("strLine") & " AND SizePerLine=" & Request("radLine")
Session("strLine") = Session("strLine") & " AND ShowOTS=" & Request("radOTS")
'WAL_ add var for show avaliablity and add order size per line[end]
Session("strLine") = Session("strLine") & " AND UseTaxes=" & Request("radTaxes")
Session("strLine") = Session("strLine") & " AND ShowExpected=" & Request("radExpected")
Session("strLine") = Session("strLine") & " AND ShowShipVia=" & Request("radShipVia")
Session("strLine") = Session("strLine") & " AND UseCode=" & Request("radCode")
'WMA_Add  var for Online Payment Parameters [Start]
Session("strLine") = Session("strLine") & " AND UseOnlinePayment=" & Request("radUseOnlinePayment")
Session("strLine") = Session("strLine") & " AND GateWay=" & Request("lstGateWay")
Session("strLine") = Session("strLine") & " AND UseExtSSL=" & Request("radUseExtSSL")
Session("strLine") = Session("strLine") & " AND SSLURL=" & Request("txtSSLURL")
Session("strLine") = Session("strLine") & " AND UseDeposit=" & Request("radUseDeposit")
Session("strLine") = Session("strLine") & " AND DepositPercent=" & Request("txtDepositPercent")+0
Session("strLine") = Session("strLine") & " AND AllowEditDeposit=" & Request("radAllowEditDeposit")
Session("strLine") = Session("strLine") & " AND ShowNotes=" & Request("radNote")
'WMA_Add  var for Online Payment Parameters [End]

'wma
Session("strLine") = Session("strLine") & " AND CustomerLoginUsing=" & Request("lstCustomerLoginUsing")
Session("strLine") = Session("strLine") & " AND CustomerCodeType=" & Request("lstCustomerCodeType")
'wma


Session("strLine") = Session("strLine") & " AND THEME=" & Session("THEME")
'WAL_E302083,1 add parameter of Customer and Store Fields [start]
if trim(Session("CustField")) = "" then
	Session("CustField")  = "Customer"
end if
if trim(Session("StoreField")) = "" then
	Session("StoreField")  = "Store"
end if

if trim(Session("DBAField")) = "" then
	Session("DBAField")  = "DBA"
end if
if trim(Session("Phone2Field")) = "" then
	Session("Phone2Field")  = "Phone2"
end if
if trim(Session("PhoneField")) = "" then
	Session("PhoneField")  = "Phone"
end if
if trim(Session("FaxField")) = "" then
	Session("FaxField")  = "Fax"
end if
if trim(Session("keeperField")) = "" then
	Session("keeperField")  = "Bookkeeper"
end if
if trim(Session("BuyerField")) = "" then
	Session("BuyerField")  = "Buyer"
end if
'wal add new defualt labels
if trim(Session("CatalogField")) = "" then
	Session("CatalogField")  = "Catalog"
end if
if trim(Session("OTSField")) = "" then
	Session("OTSField")  = "OTS"
end if
if trim(Session("BillingField")) = "" then
	Session("BillingField")  = "Billing"
end if
if trim(Session("ShipToField")) = "" then
	Session("ShipToField")  = "Ship To"
end if
if trim(Session("ShippingField")) = "" then
	Session("ShippingField")  = "Shipping"
end if
if trim(Session("FreightField")) = "" then
	Session("FreightField")  = "Freight"
end if
if trim(Session("AmountField")) = "" then
	Session("AmountField")  = "Merchandise Amount"
end if
if trim(Session("BalanceField")) = "" then
	Session("BalanceField")  = "Balance To Pay"
end if
Session("strLine") = Session("strLine") & " AND CustField=" & Session("CustField")
Session("strLine") = Session("strLine") & " AND StoreField=" & Session("StoreField")
Session("strLine") = Session("strLine") & " AND DBAField=" & Session("DBAField")
Session("strLine") = Session("strLine") & " AND Phone2Field=" & Session("Phone2Field")
Session("strLine") = Session("strLine") & " AND PhoneField=" & Session("PhoneField")
Session("strLine") = Session("strLine") & " AND FaxField=" & Session("FaxField")
Session("strLine") = Session("strLine") & " AND keeperField=" & Session("keeperField")
Session("strLine") = Session("strLine") & " AND BuyerField=" & Session("BuyerField")
Session("strLine") = Session("strLine") & " AND CatalogField=" & Session("CatalogField")
Session("strLine") = Session("strLine") & " AND OTSField=" & Session("OTSField")
Session("strLine") = Session("strLine") & " AND BillingField=" & Session("BillingField")
Session("strLine") = Session("strLine") & " AND ShipToField=" & Session("ShipToField")
Session("strLine") = Session("strLine") & " AND ShippingField=" & Session("ShippingField")
Session("strLine") = Session("strLine") & " AND StyleField=" & Session("StyleField")
Session("strLine") = Session("strLine") & " AND FreightField=" & Session("FreightField")
Session("strLine") = Session("strLine") & " AND AmountField=" & Session("AmountField")
Session("strLine") = Session("strLine") & " AND BalanceField=" & Session("BalanceField")
'WAL_E302083,1 add parameter of Customer and Store Fields [start]
Session("strLine") = Session("strLine") & " AND End=End"
Set objTextFile = objFile.CreateTextFile(strAppPath & strFilePath , True)
objTextFile.WriteLine(Session("strLine"))
objTextFile.Close
Set objTextFile = Nothing

Set objFile = Nothing


Set objFile = Server.CreateObject("Scripting.FileSystemObject")



strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))

If Right(strAppPath,1) = "\" Then
	strFilePath = "admin\crmsetup\setup\setup.txt"
Else
	strFilePath = "\admin\crmsetup\setup\setup.txt"							
End If


Set objTxtFile = objFile.OpenTextFile(strAppPath & strFilePath,1)


strFile = objTxtFile.ReadAll
	
Dim strArSetups
strArSetups = Split(strFile," AND ", -1 , 1)

'Declare Vartiables To Hold the temporary key and values
Dim strKey
Dim strValue

For intLoop = 0 To UBound(strArSetups)
	strArKeyValue = Split(strArSetups(intLoop) , "=" , -1 , 1)
	Session(strArKeyValue(0)) = strArKeyValue(1)
Next
%>
<script language="javascript">
<!--
	alert("Please go to the _Layout Section_ to complete CRM Setup!");
	//-->
</script>
<%
'Response.End 
'Session.Abandon
 

Session.Abandon 
Response.Redirect("../default.asp?AdminLogin=true&userid="& session("strAppUserVar") &"")



%>

