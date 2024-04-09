<%@ Language=VBScript %>
<%
'Response.Write(request("sty"))
'strTemp = "<br>"
'Response.Write(strTemp)
set conn = server.CreateObject("adodb.connection")
conn.Open Application("DataConnectionString")
Set rsStyCount = server.CreateObject("ADODB.RECORDSET")
'strsqlCount = "select count(*) as COUNT from icistru where citemrecty='U'"
strsqlCount = "select * from icistru where citemrecty='U'"
rsStyCount.Open strsqlCount,Conn,2,4
I = 0 
DO WHILE (NOT rsStyCount.EOF) 
	LSEGENDMAJ	=	rsStyCount.FIELDS("LSEGENDMAJ").Value
	'Response.Write "<FONT SIZE=3>LSEGENDMAJ" & LSEGENDMAJ & "</FONT><BR><BR>"
	I = I + 1
	IF LSEGENDMAJ = TRUE THEN EXIT DO  
rsStyCount.MoveNext 
LOOP
MAJOR = I  
'Response.Write "MAJOR== " & MAJOR

IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
Else
	Session("RSStyStruct").MoveFirst
	Dim intStart, intEnd ' as integer
	intStart = 1 
	Session("getstyle") = Request("sty")
	'Response.Write(Request("desc1"))
	Session("LongDesc") = Request("desc1")
	'Response.Write(Session("LongDesc"))
	DO While Not Session("RSStyStruct").Eof
		intEnd = Session("RSStyStruct").fields("nisegsize")
		
		Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) = Mid(Request("sty"),intStart,intEnd)
		'Response.Write "CISEGSDES = "&Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) & "<br>"
		'NEK[Start] In Case Of More Than One Part in the Major .It doesn't get the start correctly
		
		'intStart = 1 + cdbl(intEnd)+cdbl(intStart)
		if MAJOR >= 2 then 
			intStart = 1 + cdbl(intEnd)+ cdbl(intStart)
		else
			intStart = 2 + cdbl(intEnd)
		end if 
		'NEK[End]
		'Response.Write(Session(Trim(Session("RSStyStruct").fields("cisegsdes"))))
		Session("RSStyStruct").MoveNext
	Loop
End IF
Response.Redirect("ots.asp?Result=T")

%>
