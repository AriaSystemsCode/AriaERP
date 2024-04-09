<%@LANGUAGE="VBSCRIPT"%>
<%Response.Buffer = True %>
<%

'WMA #038182 Ability to customize the menu [start]

'Read from XML
if Request("lstMenuType") = "sales" then
	strXMLfile = "../../SalesMENU.xml"		
elseif Request("lstMenuType") = "customer" then
	strXMLfile = "../../CustMENU.xml"		
end if

Response.Buffer = true
 
Set objXML = Server.CreateObject("Msxml2.DOMDocument")
Set objLst = Server.CreateObject("Msxml2.DOMDocument")
Set objHdl = Server.CreateObject("Msxml2.DOMDocument")

'-----------------------------------------------------------------------------
'-----------------------------------------------------------------------------
objXML.async = False
objXML.Load (Server.MapPath(strXMLfile))

If objXML.parseError.errorCode <> 0 Then
 'handle the error
 Response.Write "<b>Error at line: "
 Response.Write objXML.parseError.Line & "</b><br>"
 'Response.End 
End If

Set objLst = objXML.getElementsByTagName("BAR")
noOfHeadlines = objLst.length


'Save Groups and Menu Items.
 For i=0 To noOfHeadlines - 1 ' Loop for all bar nodes.
  Set objHdl = objLst.item(i)
	'Save Groups 
	objHdl.childNodes(2).text = Request.Form(objHdl.childNodes(0).text)
	
	'Save Menu Items	
	intNumberOfPADS = objHdl.getElementsByTagName ("PAD").length  
	  if intNumberOfPADS <> 0 then
		for j=0 to intNumberOfPADS - 1 ' Loop for all pads in each bar node. 
			objHdl.getElementsByTagName("PAD").item(j).childNodes.item(2).text = Request.form(objHdl.getElementsByTagName("PAD").item(j).childNodes.item(0).text)
		next
  end if 
next 

'at last save the xml file
objXML.save Server.MapPath(strXMLfile)



'Delete the old js files that have the old menu
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


'destroy old session	
Session.Abandon 
Response.Redirect("../default.asp?AdminLogin=true&userid="& session("strAppUserVar") &"")


'WMA #038182 Ability to customize the menu [end]	
%>