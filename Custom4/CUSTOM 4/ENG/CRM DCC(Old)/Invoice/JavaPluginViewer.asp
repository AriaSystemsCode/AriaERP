<html>
<head>
<title>Seagate Java Viewer using Java Plug-in</title>
</head>
<body bgcolor=C6C6C6 onunload="CallDestroy();">

<P align="center">
<object
	classid="clsid:8AD9C840-044E-11D1-B3E9-00805F499D93"
	width=100%
	height=100%
	codebase="/viewer/JavaPlugIn/Win32/j2re1_3_0-win-i.exe#Version=1,3,0,0">
<param name=type value="application/x-java-applet;version=1.3.0">
<param name=code value="com.seagatesoftware.img.ReportViewer.ReportViewer">
<param name=codebase value="/viewer/javaviewer/">
<param name=archive value="ReportViewer.jar">
<param name=Language value="en">
<param name=ReportName value="/scrsamples/rptserver.asp">
<param name=ReportParameter value="">
<param name=HasGroupTree value="true">
<param name=ShowGroupTree value="true">
<param name=HasRefreshButton value="true">
<param name=HasPrintButton value="true">
<param name=HasExportButton value="true">
<param name=HasTextSearchControls value="true">
<param name=CanDrillDown value="true">
<param name=HasZoomControl value="true">
<param name=PromptOnRefresh value="true">
<comment>
<embed
	width=100%
	height=90%
	type="application/x-java-applet;version=1.3.0"
	pluginspage="/viewer/JavaPlugIn/Win32/j2re1_3_0-win-i.exe"
	java_code="com.seagatesoftware.img.ReportViewer.ReportViewer"
	java_codebase="/viewer/javaviewer/"
	java_archive="ReportViewer.jar"
Language="en"
ReportName="/scrsamples/active server pages/rptserver.asp"
ReportParameter=""
HasGroupTree="true"
ShowGroupTree="true"
HasRefreshButton="true"
HasPrintButton="true"
HasExportButton="true"
HasTextSearchControls="true"
CanDrillDown="true"
HasZoomControl="true"
PromptOnRefresh="true"
></embed>
</comment>
</object>
</p>
<script language="javascript">

function CallDestroy()
{
	window.open("Cleanup.asp");
}
</script>
</body>
</html>