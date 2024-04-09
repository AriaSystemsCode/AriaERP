<%'@ Language=VBScript %>
<%
'Dim strConn
'strConn  = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=tamplates;Data Source=WEBSERVER;uid=SA;pwd="
'Session("ConnectionString") = strConn
'Set Conn = Server.CreateObject("ADODB.Connection")
'Conn.Open(Session("ConnectionString"))
'Set session("rsOrdersMatch") = Server.CreateObject("ADODB.RecordSet")

'session("rsOrdersMatch").Open "NewsData",Conn

%>
<%
session("rsOrdersMatch").MoveFirst
' display number of  Records
response.write "<b>" & session("rsOrdersMatch").recordCount & " Records</b>"
response.write "<table border=1 bordercolor=#AAAAC9 cellpadding=2 cellspacing=0 style=""font-size:10pt""><tr>"

If session("rsOrdersMatch").BOF and session("rsOrdersMatch").EOF then
response.write("No Data Found")
else
' Display names of fields
For i =0 to session("rsOrdersMatch").Fields.count-1
	response.write "<th>" & session("rsOrdersMatch").fields(i).name &"</th>"
Next
response.write "</tr>"

' Displaying values
Do While Not session("rsOrdersMatch").EOF 
Response.Write ("<tr>")
for i=0 to session("rsOrdersMatch").Fields.count-1
	response.write "<td>" & session("rsOrdersMatch").fields(i).value & "</td>"
Next
session("rsOrdersMatch").MoveNext 
response.write "</tr>"
Loop
response.write "</TABLE>"
End if

%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
</HEAD>
<BODY>

<P>&nbsp;</P>

</BODY>
</HTML>
