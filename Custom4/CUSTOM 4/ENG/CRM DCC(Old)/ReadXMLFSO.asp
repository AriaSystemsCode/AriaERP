<SCRIPT LANGUAGE=VBScript RUNAT=Server>
'============================================
'Readin XML file and write .js file using FSO
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
 'Response.Write path 
 Dim fso, MyFile
 Set fso = CreateObject("Scripting.FileSystemObject")
 Set MyFile = fso.CreateTextFile(path&"menu\"&strJSFile, True) ' filename,[overwrite],[unicode]
 '-----------------------------------------------------------------------------

 objXML.async = False
 objXML.Load (Server.MapPath(strXMLfile))

 If objXML.parseError.errorCode <> 0 Then
	 'handle the error
	 Response.Write "<b>Error at line: "
	 Response.Write objXML.parseError.Line & "</b><br>"
 End If

'WMH Select Menu Style [Start]
Select case UCase(Session("THEME"))
case "STANDARD"
 BGColor          = objXML.getElementsByTagName("backgroundcolor").item(0).text
 BGColorHL        = objXML.getElementsByTagName("backgroundhighlcolor").item(0).text 
 fontColor        = objXML.getElementsByTagName("fontcolor").item(0).text 
 fontColorHL      = objXML.getElementsByTagName("fontcolorhighlight").item(0).text 
 outerbordercolor = objXML.getElementsByTagName("outerbordercolor").item(0).text 
 innerbordercolor = objXML.getElementsByTagName("innerbordercolor").item(0).text  
 cellwidth        = objXML.getElementsByTagName("cellwidth").item(0).text  
  Response.Write "STANDARD"

case "SCULPTURESILK"

 BGColor          = objXML.getElementsByTagName("backgroundcolorStyle2").item(0).text
 BGColorHL        = objXML.getElementsByTagName("backgroundhighlcolorStyle2").item(0).text 
 fontColor        = objXML.getElementsByTagName("fontcolorStyle2").item(0).text 
 fontColorHL      = objXML.getElementsByTagName("fontcolorhighlightStyle2").item(0).text 
 outerbordercolor = objXML.getElementsByTagName("outerbordercolorStyle2").item(0).text 
 innerbordercolor = objXML.getElementsByTagName("innerbordercolorStyle2").item(0).text  
 cellwidth        = objXML.getElementsByTagName("cellwidthStyle2").item(0).text  
 Response.Write "SCULPTURESILK"

case "BILLBLASSNY"

 BGColor          = objXML.getElementsByTagName("backgroundcolor3").item(0).text
 BGColorHL        = objXML.getElementsByTagName("backgroundhighlcolor3").item(0).text 
 fontColor        = objXML.getElementsByTagName("fontcolor3").item(0).text 
 fontColorHL      = objXML.getElementsByTagName("fontcolorhighlight3").item(0).text 
 outerbordercolor = objXML.getElementsByTagName("outerbordercolor3").item(0).text 
 innerbordercolor = objXML.getElementsByTagName("innerbordercolor3").item(0).text  
 cellwidth        = objXML.getElementsByTagName("BILLBLASSMENU").item(0).text  
 Response.Write "BILLBLASS"
case else
	Response.Write("No Case:"& Session("THEME"))
end select
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
strConfig = strConfig  & "0],"
'-----------------------------------------------------
MyFile.WriteLine(strConfig)
'-----------------------------------------------------

 Set objLst = objXML.getElementsByTagName("BAR")
 noOfHeadlines = objLst.length
 
 For i=0 To noOfHeadlines - 1 ' Loop for all bar nodes 
  Set objHdl = objLst.item(i)
  'Response.Write "<b>" & objHdl.childNodes(0).text & "</b><br>"
  'Response.Write  "length: "& objHdl.childNodes.length & "<br>"
  strHeaders = strHeaders & " , " & objHdl.childNodes(0).text & objHdl.childNodes(1).text
  if i = noOfHeadlines - 1 then
	MyFile.WriteLine("['"&objHdl.childNodes(0).text&"','"&objHdl.childNodes(1).text&"',1,0,1]]")
  else
	MyFile.WriteLine("['"&objHdl.childNodes(0).text&"','"&objHdl.childNodes(1).text&"',1,0,1],")
  end if
  
 Next
  
 'Response.Write  strHeaders & "<br>"
 '-----------------------------------------------------
 'MyFile.WriteLine(strHeaders)
 '-----------------------------------------------------
 For i=0 To noOfHeadlines - 1 ' Loop for all bar nodes.
  Set objHdl = objLst.item(i)
  intNumberOfPADS = objHdl.getElementsByTagName ("PAD").length 
  
  'Response.Write objHdl.childNodes.item(0).text 
  if intNumberOfPADS <> 0 then
	MyFile.WriteLine("HM_Array1_"&i+1&" = [")  
	MyFile.WriteLine("[],")
  
	for j=0 to intNumberOfPADS - 1 ' Loop for all pads in each bar node. 

		if j = intNumberOfPADS - 1 then
			MyFile.WriteLine("['"&objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(0).text&"','"&objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(1).text&"',1,0,0]")
		else
			MyFile.WriteLine("['"&objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(0).text&"','"&objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(1).text&"',1,0,0],")
		end if
		'Response.Write "<blockquote>" & objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(0).text & "</blockquote>"  
	next
  	MyFile.WriteLine("]")
  end if
  
 next 

 
 '-----------------------------------------------------
 ' MyFile.WriteLine(i+1 & strChild)
 '-----------------------------------------------------
 

'-----------------------------------------------------
MyFile.Close
Set fso = nothing
'-----------------------------------------------------
end function
</Script>





