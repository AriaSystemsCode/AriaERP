<SCRIPT LANGUAGE=VBScript RUNAT=Server>
'============================================
'Reading XML file and write .js file using ADO.Stream
'By: Ahmed M. Elmoghazy
'============================================
Function CretaMenu(strXMLfile,strJSFile) 
 Response.Buffer = true
 
 Set objXML = Server.CreateObject("Msxml2.DOMDocument")
 Set objLst = Server.CreateObject("Msxml2.DOMDocument")
 Set objHdl = Server.CreateObject("Msxml2.DOMDocument")

 '-----------------------------------------------------------------------------
 CurFName = Request.ServerVariables ("SCRIPT_NAME")
 CurFName = mid(CurFName,instrrev(CurFName,"/")+1)

 path = server.MapPath (CurFName)
 path = mid(path,1,instrrev(path,"\"))
 
 set ado_stream = Server.CreateObject("ADODB.Stream")
 ado_stream.Type = 2
 ado_stream.Charset = "ascii"
 ado_stream.LineSeparator = -1
 ado_stream.Open 
 
 
 '-----------------------------------------------------------------------------

 objXML.async = False
 objXML.Load (Server.MapPath(strXMLfile))

 If objXML.parseError.errorCode <> 0 Then
	 'handle the error
	 Response.Write "<b>Error at line: "
	 Response.Write objXML.parseError.Line & "</b><br>"
	 'Response.End 
 End If

'WMH Select Menu Style [Start]
Select case Session("THEME")
case "STANDARD"
 BGColor          = objXML.getElementsByTagName("backgroundcolor").item(0).text
 BGColorHL        = objXML.getElementsByTagName("backgroundhighlcolor").item(0).text 
 fontColor        = objXML.getElementsByTagName("fontcolor").item(0).text 
 fontColorHL      = objXML.getElementsByTagName("fontcolorhighlight").item(0).text 
 outerbordercolor = objXML.getElementsByTagName("outerbordercolor").item(0).text 
 innerbordercolor = objXML.getElementsByTagName("innerbordercolor").item(0).text  
 cellwidth        = objXML.getElementsByTagName("cellwidth").item(0).text  
 
 'Response.Write "STANDARD"

case "Theme1"	

 BGColor          = objXML.getElementsByTagName("backgroundcolorStyle2").item(0).text
 BGColorHL        = objXML.getElementsByTagName("backgroundhighlcolorStyle2").item(0).text 
 fontColor        = objXML.getElementsByTagName("fontcolorStyle2").item(0).text 
 fontColorHL      = objXML.getElementsByTagName("fontcolorhighlightStyle2").item(0).text 
 outerbordercolor = objXML.getElementsByTagName("outerbordercolorStyle2").item(0).text 
 innerbordercolor = objXML.getElementsByTagName("innerbordercolorStyle2").item(0).text  
 cellwidth        = objXML.getElementsByTagName("cellwidthStyle2").item(0).text  
 'Response.Write "SCULPTURESILK"
case "Theme2"	

 BGColor          = objXML.getElementsByTagName("backgroundcolor3").item(0).text
 BGColorHL        = objXML.getElementsByTagName("backgroundhighlcolor3").item(0).text 
 fontColor        = objXML.getElementsByTagName("fontcolor3").item(0).text 
 fontColorHL      = objXML.getElementsByTagName("fontcolorhighlight3").item(0).text 
 outerbordercolor = objXML.getElementsByTagName("outerbordercolor3").item(0).text 
 innerbordercolor = objXML.getElementsByTagName("innerbordercolor3").item(0).text  
 cellwidth        = objXML.getElementsByTagName("BILLBLASSMENU").item(0).text   
end select

' Response.Write BGColor
' Response.Write BGColorHL
' Response.Write fontColor
' Response.Write fontColorHL
' Response.Write outerbordercolor
' Response.Write innerbordercolor
' Response.Write cellwidth
'Response.End 
'WMH Select Menu Style [End]

 
strConfig = "HM_Array1 = ["
strConfig = strConfig  & "[window.screen.availWidth /" & cellwidth & ","
strConfig = strConfig  & "'HM_f_CenterMenu(""HM_Menu1"")',"
strConfig = strConfig  & "0,"
strConfig = strConfig  & "'" & fontColor & "',"
strConfig = strConfig  & "'" & fontColorHL & "',"
strConfig = strConfig  & "'" & BGColor & "',"		' BACKGROUND COLOR
strConfig = strConfig  & "'" & BGColorHL & "',"
strConfig = strConfig  & "'" & outerbordercolor & "',"
strConfig = strConfig  & "'" & innerbordercolor & "',"
strConfig = strConfig  & "1,"
strConfig = strConfig  & "1,"
strConfig = strConfig  & "0,"
strConfig = strConfig  & "1,"
strConfig = strConfig  & "0,"
strConfig = strConfig  & "1,"
strConfig = strConfig  & "'null',"
strConfig = strConfig  & "'null',"
strConfig = strConfig  & ","
'WMA #038182 Ability to customize the menu [start]
'strConfig = strConfig  & "0],"
strConfig = strConfig  & "0]"
'WMA #038182 Ability to customize the menu [end]
'-----------------------------------------------------
ado_stream.WriteText strConfig,1
'-----------------------------------------------------
 
 Set objLst = objXML.getElementsByTagName("BAR")
 noOfHeadlines = objLst.length

intAvoidMenu = 0
redim arrActiveNodes(noOfHeadlines)
redim arrActiveNodesLocation(noOfHeadlines)

'WMA #038182 Ability to customize the menu [start] 
 For i=0 To noOfHeadlines - 1 ' Loop for all bar nodes 
  Set objHdl = objLst.item(i)

  strHeaders = strHeaders & " , " & objHdl.childNodes(0).text & objHdl.childNodes(1).text
	'  if i = noOfHeadlines - 1 then
	'	ado_stream.WriteText "['"&objHdl.childNodes(0).text&"','"&objHdl.childNodes(1).text&"',1,0,1]]",1
	'  else
	'	ado_stream.WriteText "['"&objHdl.childNodes(0).text&"','"&objHdl.childNodes(1).text&"',1,0,1],",1
	'  end if
	if objHdl.childNodes(2).text = "-1" then
		ado_stream.WriteText ",['"&objHdl.childNodes(0).text&"','"&objHdl.childNodes(1).text&"',1,0,1]",1	
	
	arrActiveNodes(i) = true
	arrActiveNodesLocation(i) = i+1
	
	'elseif objHdl.childNodes(1).text = "custprof.asp" or objHdl.childNodes(1).text = "ots.asp" or objHdl.childNodes(1).text = "catalog.asp" or objHdl.childNodes(1).text = "documents.asp"  or objHdl.childNodes(1).text = "helpdesk.asp" or objHdl.childNodes(1).text = "logoff.asp" then 'avoid single items later in the sub nodes
	'elseif objHdl.childNodes(1).text = "custprof.asp" or objHdl.childNodes(1).text = "ots.asp" or objHdl.childNodes(1).text = "catalog.asp" or objHdl.childNodes(1).text = "documents.asp"  or objHdl.childNodes(1).text = "helpdesk.asp" or objHdl.childNodes(1).text = "logoff.asp" then 'avoid single items later in the sub nodes
	'	intAvoidMenu = intAvoidMenu - 1
	else
		arrActiveNodes(i) = false
	end if
 Next
 	ado_stream.WriteText "]",1
'WMA #038182 Ability to customize the menu [end]  
	 
 '-----------------------------------------------------
 
 '-----------------------------------------------------
 For i=0 To noOfHeadlines - 1 ' Loop for all bar nodes.
  Set objHdl = objLst.item(i)
  intNumberOfPADS = objHdl.getElementsByTagName ("PAD").length 
  
  if intNumberOfPADS <> 0 and objHdl.childNodes(2).text = "-1" then

	'ado_stream.WriteText "HM_Array1_"&i+1&" = [",1
	'WMA #038182 Ability to customize the menu [start]	
	intCurr = 0
	intCurrCount = 0
	for intCurr = 0 to arrActiveNodesLocation(i)-1
		if arrActiveNodes(intCurr) = false then
			intCurrCount = intCurrCount + 1
		end if
	next
	ado_stream.WriteText "HM_Array1_"& arrActiveNodesLocation(i) - intCurrCount &" = [",1
	
	

	'ado_stream.WriteText "[],",1
	ado_stream.WriteText "[]",1 
	'WMA #038182 Ability to customize the menu [end]	 

	for j=0 to intNumberOfPADS - 1 ' Loop for all pads in each bar node. 
	'WMA #038182 Ability to customize the menu [start]	
		'if j = intNumberOfPADS - 1 then
		'	ado_stream.WriteText "['"&objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(0).text&"','"&objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(1).text&"',1,0,0]",1
		'else
		'	ado_stream.WriteText "['"&objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(0).text&"','"&objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(1).text&"',1,0,0],",1
		'end if
		if objHdl.getElementsByTagName("PAD").item(j).childNodes.item(2).text = "-1" then
			ado_stream.WriteText ",['"&objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(0).text&"','"&objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(1).text&"',1,0,0]",1
		end if
	'WMA #038182 Ability to customize the menu [end]		
	next
	ado_stream.WriteText "]",1
	
  end if
next 
'Response.Write  strMesages
'Response.End 
 
 '-----------------------------------------------------
 
 '-----------------------------------------------------
 

'-----------------------------------------------------
'Response.Write path&"menu\"&strJSFile
 
ado_stream.SaveToFile path&"menu\"&strJSFile , 2
ado_stream.Close 
set ado_stream = nothing
'-----------------------------------------------------
end function
</Script>



