<%@ Language=VBScript %>
<%Response.Buffer=true

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

IF Session("rsRetStyStruct").EOF And Session("rsRetStyStruct").BOF Then
Else
	Session("rsRetStyStruct").MoveFirst
	Dim intStart, intEnd ' as integer
	intStart = 1 
	Session("getstyle") = Request("sty")
	Session("LongDesc") = Request("desc1")
	DO While Not Session("rsRetStyStruct").Eof
		intEnd = Session("rsRetStyStruct").fields("nisegsize")
		
		Session(Trim(Session("rsRetStyStruct").fields("cisegsdes"))) = Mid(Request("sty"),intStart,intEnd)
		if MAJOR >= 2 then 
			intStart = 1 + cdbl(intEnd)+ cdbl(intStart)
		else
			intStart = 2 + cdbl(intEnd)
		end if 
		'intStart = 2 + cdbl(intEnd)
		Session("rsRetStyStruct").MoveNext
	Loop
End IF
Session("FindStyleBtnPressed") = "YES"
Session("text1")="0"
Session("text2")="0"
Session("text3")="0"
Session("text4")="0"
Session("text5")="0"
Session("text6")="0"
Session("text7")="0"
Session("text8")="0"

Response.Redirect("ReturnDetail.asp")

%>
