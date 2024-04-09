<%
	Response.Buffer = True
	dim fso,pathCust,pathSales
	Set fso = CreateObject("Scripting.FileSystemObject")
	
	'''''''''''''Customer'''''''''''''''''''''''''
	pathCust = server.MapPath ("../../menu/HM_Arrays_Cust.js")
	
	'''''''''''''Sales Rep'''''''''''''''''''''''''
	pathSales = server.MapPath ("../../menu/HM_Arrays_Sales.js")
	
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

	on error resume next
	Set objTxtFile = objFile.OpenTextFile(strAppPath & strFilePath,1)
	
	If err.number<>0 then
			response.write "You must setup CRM first."
			response.write "<br><a href=""../CrmSetup\crmSetup.asp"">Setup CRM</a>"
			Response.End
	End If
	On Error GoTo 0
	Dim strLine

	strFile = objTxtFile.ReadAll
	'Response.Write (strFile)
	
	
	Dim strArSetups
	strArSetups = Split(strFile," AND ", -1 , 1)

	'Declare Vartiables To Hold the temporary key and values
	Dim strKey
	Dim strValue
	strtoSave = ""
	'Update Setup File
	For intLoop = 0 To UBound(strArSetups) 
		strTemp = split(strArSetups(intLoop),"=")
		select case (Trim(strTemp(0)))
			case "THEME"
				strTemp(1) = request("lstTheme")
			case "CustField"
				strTemp(1) = request("txtCust")
				flag = "true"
			case "StoreField"
				strTemp(1) = request("txtStore")
				flag = "true"
			case "keeperField"
				strTemp(1) = request("txtKeeper")
				flag = "true"
			case "BuyerField"
				strTemp(1) = request("txtBuyer")
				flag = "true"
			case "DBAField"
				strTemp(1) = request("txtDBA")
				flag = "true"
			case "PhoneField"
				strTemp(1) = request("txtPhone")
				flag = "true"
			case "Phone2Field"
				strTemp(1) = request("txtPhone2")
				flag = "true"
			case "FaxField"
				strTemp(1) = request("txtFax")
				flag = "true"
			case "CatalogField"
				strTemp(1) = request("txtCatalog")
				flag = "true"
			case "OTSField"
				strTemp(1) = request("txtOTS")
				flag = "true"
			case "BillingField"
				strTemp(1) = request("txtBilling")
				flag = "true"
			case "ShipToField"
				strTemp(1) = request("txtShipTo")
				flag = "true"
			case "ShippingField"
				strTemp(1) = request("txtShipping")
				flag = "true"
			case "StyleField"
				strTemp(1) = request("txtStyle")
				flag = "true"
			case "AmountField"
				strTemp(1) = request("txtAmount")
				flag = "true"
			case "FreightField"
				strTemp(1) = request("txtFreight")
				flag = "true"
			case "BalanceField"
				strTemp(1) = request("txtBalance")
				flag = "true"
			case "CityField"
				strTemp(1) = request("txtCity")
				flag = "true"
			case "StateField"
				strTemp(1) = request("txtState")
				flag = "true"
			case "ZipField"
				strTemp(1) = request("txtZip")
				flag = "true"
			case "ContractField"
				strTemp(1) = request("txtContract")
				flag = "true"
			case "Note1Field"
				strTemp(1) = request("txtNote1")
				flag = "true"
			case "Note2Field"
				strTemp(1) = request("txtNote2")
				flag = "true"
			
		end select
		
		IF strtoSave = "" Then
			strtoSave = strTemp(0) & "=" & strTemp(1)
		Else
			strtoSave = strtoSave & " AND " & strTemp(0) & "=" & strTemp(1)
		End IF
	Next
	'WAL_ check if the paramter fileds not found in the text file then add them[start]
	if instr("1", strFile, "CustField") <= 0 then
		strtoSave = strtoSave & " AND CustField=" & trim(request("txtCust"))
	end if
	if instr("1", strFile, "StoreField") <= 0 then
		strtoSave = strtoSave & " AND StoreField=" & trim(request("txtStore"))
	end if
	if instr("1", strFile, "DBAField") <= 0 then
		strtoSave = strtoSave & " AND DBAField=" & trim(request("txtDBA"))
	end if
	if instr("1", strFile, "Phone2Field") <= 0 then
		strtoSave = strtoSave & " AND Phone2Field=" & trim(request("txtPhone2"))
	end if
	if instr("1", strFile, "PhoneField") <= 0 then
		strtoSave = strtoSave & " AND PhoneField=" & trim(request("txtPhone"))
	end if
	if instr("1", strFile, "FaxField") <= 0 then
		strtoSave = strtoSave & " AND FaxField=" & trim(request("txtFax"))
	end if
	if instr("1", strFile, "keeperField") <= 0 then
		strtoSave = strtoSave & " AND keeperField=" & trim(request("txtKeeper"))
	end if
	if instr("1", strFile, "BuyerField") <= 0 then
		strtoSave = strtoSave & " AND BuyerField=" & trim(request("txtBuyer"))
	end if
	if instr("1", strFile, "CatalogField") <= 0 then
		strtoSave = strtoSave & " AND CatalogField=" & trim(request("txtCatalog"))
	end if
	
	if instr("1", strFile, "OTSField") <= 0 then
		strtoSave = strtoSave & " AND OTSField=" & trim(request("txtOTS"))
	end if
	if instr("1", strFile, "BillingField") <= 0 then
		strtoSave = strtoSave & " AND BillingField=" & trim(request("txtBilling"))
	end if
	if instr("1", strFile, "ShipToField") <= 0 then
		strtoSave = strtoSave & " AND ShipToField=" & trim(request("txtShipTo"))
	end if
	if instr("1", strFile, "ShippingField") <= 0 then
		strtoSave = strtoSave & " AND ShippingField=" & trim(request("txtShipping"))
	end if
	if instr("1", strFile, "StyleField") <= 0 then
		strtoSave = strtoSave & " AND StyleField=" & trim(request("txtStyle"))
	end if
	if instr("1", strFile, "FreightField") <= 0 then
		strtoSave = strtoSave & " AND FreightField=" & trim(request("txtFreight"))
	end if
	if instr("1", strFile, "AmountField") <= 0 then
		strtoSave = strtoSave & " AND AmountField=" & trim(request("txtAmount"))
	end if
	if instr("1", strFile, "BalanceField") <= 0 then
		strtoSave = strtoSave & " AND BalanceField=" & trim(request("txtBalance"))
	end if
	if instr("1", strFile, "CityField") <= 0 then
		strtoSave = strtoSave & " AND CityField=" & trim(request("txtCity"))
	end if
	if instr("1", strFile, "ZipField") <= 0 then
		strtoSave = strtoSave & " AND ZipField=" & trim(request("txtZip"))
	end if
	if instr("1", strFile, "StateField") <= 0 then
		strtoSave = strtoSave & " AND StateField=" & trim(request("txtState"))
	end if
	if instr("1", strFile, "ContractField") <= 0 then
		strtoSave = strtoSave & " AND ContractField=" & trim(request("txtContract"))
	end if
	if instr("1", strFile, "Note1Field") <= 0 then
		strtoSave = strtoSave & " AND Note1Field=" & trim(request("txtNote1"))
	end if
	if instr("1", strFile, "Note2Field") <= 0 then
		strtoSave = strtoSave & " AND Note2Field=" & trim(request("txtNote2"))
	end if
	
	strtoSave = replace(strtoSave, chr(13)&chr(10),"")
	'WAL_ check if the paramter fileds not founf in the text file then add them[end]
	'Response.End 
	'Update XML Files case the values are changed
	if flag = "true" then
		if fso.FileExists (pathCust) then
			fso.DeleteFile (pathCust)
		end if
		if fso.FileExists (pathSales) then
			fso.DeleteFile (pathSales)
		end if
		Set objCustXML = Server.CreateObject("Msxml2.DOMDocument")
		Set objCustLst = Server.CreateObject("Msxml2.DOMDocument")
		Set objCustHdl = Server.CreateObject("Msxml2.DOMDocument")
		objCustXML.async = False
		strCustXMLFile = "../../CustMENU.xml"
		objCustXML.Load (Server.MapPath(strCustXMLfile))
		Set objCustLst = objCustXML.getElementsByTagName("BAR")
		noOfCustHeadlines = objCustLst.length
		Set objSalesXML = Server.CreateObject("Msxml2.DOMDocument")
		Set objSalesLst = Server.CreateObject("Msxml2.DOMDocument")
		Set objSalesHdl = Server.CreateObject("Msxml2.DOMDocument")
		objSalesXML.async = False
		strSalesXMLFile = "../../SalesMENU.xml"
		objSalesXML.Load (Server.MapPath(strSalesXMLfile))
		Set objSalesLst = objSalesXML.getElementsByTagName("BAR")
		noOfSalesHeadlines = objSalesLst.length
		'Code to update Cust XML file
		For i=0 To noOfCustHeadlines - 1 ' Loop for all bar nodes 
		 Set objCustHdl = objCustLst.item(i)
		 'Check if value exist in file
		 
		 if objCustHdl.childNodes(0).text  = request("hidkeeper") or objCustHdl.childNodes(0).text  = request("hidBuyer") or objCustHdl.childNodes(0).text  = request("hidCust") or objCustHdl.childNodes(0).text  = request("hidCatalog")or objCustHdl.childNodes(0).text  = request("hidSales") then
			'if custfield is changed then update
			if objCustHdl.childNodes(0).text  = request("hidCust") then
				objCustHdl.childNodes(0).text   = request("txtCust")
			elseif objCustHdl.childNodes(0).text  = request("hidSales") then
				objCustHdl.childNodes(0).text   = request("txtStore")
			elseif objCustHdl.childNodes(0).text  = request("hidKeeper") then
				objCustHdl.childNodes(0).text   = request("txtKeeper")
			elseif objCustHdl.childNodes(0).text  = request("hidBuyer") then
				objCustHdl.childNodes(0).text   = request("txtBuyer")
			elseif objCustHdl.childNodes(0).text  = request("hidCatalog") then
				objCustHdl.childNodes(0).text   = request("txtCatalog")
			end if
		 end if
		Next
				
		'Code to update Sales XML file
		For i=0 To noOfSalesHeadlines - 1 ' Loop for all bar nodes 
		 Set objSalesHdl = objSalesLst.item(i)
		'Check if value exist in file
		 if objSalesHdl.childNodes(0).text  = request("hidCust") or objSalesHdl.childNodes(0).text  = request("hidSales")  or objSalesHdl.childNodes(0).text  = request("hidCatalog")or objSalesHdl.childNodes(0).text  = request("hidKeeper") or objSalesHdl.childNodes(0).text  = request("hidBuyer") then
			'if custfield is changed then update
			if objSalesHdl.childNodes(0).text  = request("hidCust") then
				objSalesHdl.childNodes(0).text   = request("txtCust")
			elseif objSalesHdl.childNodes(0).text  = request("hidSales")then
				objSalesHdl.childNodes(0).text   = request("txtStore")
			elseif objSalesHdl.childNodes(0).text  = request("hidKeeper") then
				objSalesHdl.childNodes(0).text   = request("txtKeeper")
			elseif objSalesHdl.childNodes(0).text  = request("hidBuyer") then
				objSalesHdl.childNodes(0).text   = request("txtBuyer")
			elseif objSalesHdl.childNodes(0).text  = request("hidCatalog") then
				objSalesHdl.childNodes(0).text   = request("txtCatalog")
			end if
		 end if
		Next
		objCustXML.save  (Server.MapPath(strCustXMLfile))
		objSalesXML.save  (Server.MapPath(strSalesXMLfile))

		'Release All XML objects
		Set objCustXML = Nothing
		Set objSalesXML = Nothing
		Set objCustLst = Nothing
		Set objCustHdl = Nothing
		Set objSalesLst = Nothing
		Set objSalesHdl = Nothing

	End if
	'Response.Write("<br>|"&strtoSave& "|")
	'Response.End 
	Set objTextFile = objFile.CreateTextFile(strAppPath & strFilePath , True)
	objTextFile.WriteLine(strtoSave)
	objTextFile.Close
	Set objTextFile = Nothing



	'WMH Delete the old js files that have the old menu theme [Start]
		dim fsoMenu,path
		Set fsoMenu = CreateObject("Scripting.FileSystemObject")
		
		'Customer
		path = server.MapPath ("../../menu/HM_Arrays_Cust.js")
		if fsoMenu.FileExists (path) then
			fsoMenu.DeleteFile(path)
		end if

		'Sales Rep
		path = server.MapPath ("../../menu/HM_Arrays_Sales.js")
		if  fsoMenu.FileExists (path) then
			fsoMenu.DeleteFile(path)
			
		end if

		set fsoMenu = nothing
		
		Response.ExpiresAbsolute
		'Session.Abandon 
	'WMH Delete the old js files that have the old menu theme [End]
	
	Session.Abandon 
	Response.Redirect("../default.asp?AdminLogin=true&userid="& session("strAppUserVar") &"")


%>
