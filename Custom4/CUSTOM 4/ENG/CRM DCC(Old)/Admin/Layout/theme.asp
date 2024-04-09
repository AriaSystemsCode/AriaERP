<%Response.Buffer = True%>
<html>

<head>
<meta name="GENERATOR" content="Microsoft FrontPage 4.0">
<meta name="ProgId" content="FrontPage.Editor.Document">
<title>New Page 1</title>
</head>

<body>

<%
 Set objXML = Server.CreateObject("Msxml2.DOMDocument")
 Set objLst = Server.CreateObject("Msxml2.DOMDocument")
 Set objHdl = Server.CreateObject("Msxml2.DOMDocument")

 CurFName = Request.ServerVariables ("SCRIPT_NAME")
 CurFName = mid(CurFName,instrrev(CurFName,"/")+1)

 path = server.MapPath (CurFName)
 path = mid(path,1,instrrev(path,"\"))

 set ado_stream = Server.CreateObject("ADODB.Stream")
 ado_stream.Type = 2
 ado_stream.Charset = "ascii"
 ado_stream.LineSeparator = -1
 ado_stream.Open 
 
 objXML.async = False
 objXML.Load (Server.MapPath("theme.xml"))

 If objXML.parseError.errorCode <> 0 Then
	 'handle the error
	 Response.Write "<b>Error at line: "
	 Response.Write objXML.parseError.Line & "</b><br>"
 End If
		
 Set objLst = objXML.getElementsByTagName("NAME")
 noOfHeadlines = objLst.length
 
 For i=0 To noOfHeadlines - 1 ' Loop for all THEMES
  Set objHdl = objLst.item(i)
  IF strHeaders = "" then
		strHeaders = objHdl.childNodes(0).text
  Else
		strHeaders = strHeaders & "," & objHdl.childNodes(0).text 
  End IF
 Next
%>


</body>

</html>
