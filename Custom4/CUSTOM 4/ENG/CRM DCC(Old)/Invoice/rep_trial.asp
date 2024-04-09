<%set conn = server.CreateObject("adodb.connection")
conn.Open Application("SystemConnectionString")
set rs = server.CreateObject("adodb.recordset")
rs.Open "select Cuser_id,Cusr_name from Syuuser" , conn
Response.Write rs.RecordCount 


reportname = "rep_trial.rpt"
If Not IsObject (session("oApp")) Then                              
	Set session("oApp") = Server.CreateObject("Crystal.CRPE.Application")
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

session("oRpt").DiscardSavedData
set Database = session("oRpt").Database
set Tables = Database.Tables
set Table1 = Tables.Item(1)
Table1.SetPrivateData 3, rs
	
	On Error Resume Next                                                  
	session("oRpt").ReadRecords

		If Err.Number <> 0 Then                                               
		  Response.Write "An Error has occured on the server in attempting to access the data source"
		Else

		  If IsObject(session("oPageEngine")) Then                              
		  	set session("oPageEngine") = nothing
		  End If
		set session("oPageEngine") = session("oRpt").PageEngine
		End If                                                                

		'viewer = Request.QueryString ("viewer")
		viewer="ActiveX"
		'This line collects the value passed for the viewer to be used, and stores
		'it in the "viewer" variable.

		If cstr(viewer) = "ActiveX" then
		%>
		<!-- #include file="SmartViewerActiveX.asp" -->
		<%
		ElseIf cstr(viewer) = "Netscape Plug-in" then
		%>
		<!-- #include file="ActiveXPluginViewer.asp" -->
		<%
		ElseIf cstr(viewer) = "JVM" then
		%>
		<!-- #include file="SmartViewerJava.asp" -->
		<%
		ElseIf cstr(viewer) = "Java-Plug-in" then
		%>
		<!-- #include file="JavaPluginViewer.asp" -->
		<%
		ElseIf cstr(viewer) = "HTML Frame" then
			Response.Redirect("htmstart.asp")
		Else
			Response.Redirect("rptserver.asp")
		End If

%>