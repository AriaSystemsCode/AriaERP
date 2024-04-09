<%'@ Language=VBScript %>
<%
'Dim strConn
'strConn  = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=tamplates;Data Source=WEBSERVER;uid=SA;pwd="
'Session("ConnectionString") = strConn
'Set Conn = Server.CreateObject("ADODB.Connection")
'Conn.Open(Session("ConnectionString"))
'Set session("rsInvoiceDetail1") = Server.CreateObject("ADODB.RecordSet")

'session("rsInvoiceDetail1").Open "NewsData",Conn

%>
<%
session("rsInvoiceDetail1").MoveFirst
' display number of  Records
response.write "<b>" & session("rsInvoiceDetail1").recordCount & " Records</b>"
response.write "<table border=1 bordercolor=#AAAAC9 cellpadding=2 cellspacing=0 style=""font-size:10pt""><tr>"

If session("rsInvoiceDetail1").BOF and session("rsInvoiceDetail1").EOF then
response.write("No Data Found")
else
' Display names of fields
For i =0 to session("rsInvoiceDetail1").Fields.count-1
	response.write "<th>" & session("rsInvoiceDetail1").fields(i).name &"</th>"
Next
response.write "</tr>"

' Displaying values
Do While Not session("rsInvoiceDetail1").EOF 
Response.Write ("<tr>")
for i=0 to session("rsInvoiceDetail1").Fields.count-1
	response.write "<td>" & session("rsInvoiceDetail1").fields(i).value & "</td>"
Next
session("rsInvoiceDetail1").MoveNext 
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
