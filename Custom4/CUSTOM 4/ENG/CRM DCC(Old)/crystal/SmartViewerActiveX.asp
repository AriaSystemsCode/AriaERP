<%
'This file contains the HTML code to instantiate the Smart Viewer ActiveX.      
'                                                                     
'You will notice that the Report Name parameter references the rptserver.asp file.
'This is because the report pages are actually created by rptserver.asp.
'Rptserver.asp accesses session("oApp"), session("oRpt") and session("oPageEngine")
'to create the report pages that will be rendered by the ActiveX Smart Viewer.
'Modified by: Moghazy
VR = mid (Request.ServerVariables("SCRIPT_NAME") , 1 , InstrRev(Request.ServerVariables ("SCRIPT_NAME"),"/"))
'Response.Write(VR)
'Response.End
%>
<HTML>
<HEAD>
<TITLE>Seagate Crystal Smart Viewer for ActiveX</TITLE>
<LINK rel="stylesheet" type="text/css" href="crmMain.css">
</HEAD>
<BODY BGCOLOR=C6C6C6 LANGUAGE=VBScript ONLOAD="Page_Initialize">

<OBJECT ID="CRViewer" CLASSID="CLSID:C4847596-972C-11D0-9567-00A0C9273C2A"	WIDTH=100% HEIGHT=100%  
CODEBASE="../viewers/ActiveXViewer/ActiveXViewer.cab#Version=8,0,0,387">
<PARAM NAME="EnableRefreshButton" VALUE=1>
<PARAM NAME="EnableGroupTree" VALUE=1>
<PARAM NAME="DisplayGroupTree" VALUE=0>
<PARAM NAME="EnablePrintButton" VALUE=1>
<PARAM NAME="EnableExportButton" VALUE=1>
<PARAM NAME="EnableDrillDown" VALUE=1>
<PARAM NAME="EnableSearchControl" VALUE=1>
<PARAM NAME="EnableAnimationControl" VALUE=1>
<PARAM NAME="EnableZoomControl" VALUE=1>
</OBJECT>

<SCRIPT LANGUAGE="VBScript">
<!--
Sub Page_Initialize
	On Error Resume Next
	Dim webBroker
	Set webBroker = CreateObject("WebReportBroker.WebReportBroker")
	if ScriptEngineMajorVersion < 2 then
		window.alert "IE 3.02 users on NT4 need to get the latest version of VBScript or install IE 4.01 SP1. IE 3.02 users on Win95 need DCOM95 and latest version of VBScript, or install IE 4.01 SP1. These files are available at Microsoft's web site."
		'CRViewer.ReportName = Location.Protocol + "//" + Location.Host +"/mogh/Moghazy_Rep_221000/Report/Report_Local/rptserver.asp"
		'CRViewer.ReportName = Location.Protocol + "//" + Location.Host + "<%=VR%>rptserver.asp"
		CRViewer.ReportName = Location.Protocol + "//" + Location.Host + "<%=VR%>rptserver.asp"
		
	else
		Dim webSource
		Set webSource = CreateObject("WebReportSource.WebReportSource")
		webSource.ReportSource = webBroker
		webSource.URL = Location.Protocol + "//" + Location.Host + "<%=VR%>rptserver.asp"
		webSource.PromptOnRefresh = True
		CRViewer.ReportSource = webSource
	end if
	CRViewer.ViewReport
End Sub
-->
</SCRIPT>

</BODY>
</HTML>