<%@ Language=VBScript %>

<%
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 

%>
<%
'====
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
	Session("getstyle") = Request("sty") ' style code
	Session("LongDesc") = Request("desc1")
	Session("price") = request("price")
	Session("seasonCode") = request("seasonCode")
	DO While Not Session("RSStyStruct").Eof
		intEnd = Session("RSStyStruct").fields("nisegsize")
		
		Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) = Mid(Request("sty"),intStart,intEnd)
		'NEK[Start] In Case Of More Than One Part in the Major .It doesn't get the start correctly
		
		if MAJOR >= 2 then 
			intStart = 1 + cdbl(intEnd)+ cdbl(intStart)
		else
			intStart = 2 + cdbl(intEnd)
		end if 
		'NEK[End]
		Session("RSStyStruct").MoveNext
	Loop
End IF
Session("text1") = ""
Session("text2") = ""
Session("text3") = ""
Session("text4") = ""
Session("text5") = ""
Session("text6") = ""
Session("text7") = ""
Session("text8") = ""
if Request.QueryString ("Type") = "M" then
	Response.Redirect("modifyorder.asp?come=U")
else
	Response.Redirect("custorder.asp")
end if
'Response.Redirect("custorder.asp")

%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
</HEAD>
<BODY>

<P>&nbsp;</P>

</BODY>
</HTML>
