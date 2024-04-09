<%'@ Language=VBScript %>
<%
'Dim strConn
'strConn  = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=tamplates;Data Source=WEBSERVER;uid=SA;pwd="
'Session("ConnectionString") = strConn
'Set Conn = Server.CreateObject("ADODB.Connection")
'Conn.Open(Session("ConnectionString"))
'Set Session("CustStatHdr") = Server.CreateObject("ADODB.RecordSet")

'Session("CustStatHdr").Open "NewsData",Conn

%>
<%
Session("CustStatHdr").MoveFirst
' display number of  Records
response.write "<b>" & Session("CustStatHdr").recordCount & " Records</b>"
response.write "<table border=1 bordercolor=#AAAAC9 cellpadding=2 cellspacing=0 style=""font-size:10pt""><tr>"

If Session("CustStatHdr").BOF and Session("CustStatHdr").EOF then
response.write("No Data Found")
else
' Display names of fields
For i =0 to Session("CustStatHdr").Fields.count-1
	response.write "<th>" & Session("CustStatHdr").fields(i).name &"</th>"
Next
response.write "</tr>"

' Displaying values
Do While Not Session("CustStatHdr").EOF 
Response.Write ("<tr>")
for i=0 to Session("CustStatHdr").Fields.count-1
	response.write "<td>" & Session("CustStatHdr").fields(i).value & "</td>"
Next
Session("CustStatHdr").MoveNext 
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
