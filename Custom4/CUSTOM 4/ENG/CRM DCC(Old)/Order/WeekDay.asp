<%@ Language=VBScript %>
<%
Response.CacheControl = "no-cache"
Response.AddHeader "Pragma", "no-cache"
Response.Expires = -1
Response.Buffer=true
'server.ScriptTimeout = 1050
%>

<%
Set conn = server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")
Set RsNormANORMVaction = server.CreateObject("ADODB.Recordset")
'SQLNorANORvact = "Select fishd.Cfisnonwd,fishd.Cfisystat,fishd.Cfisfyear,fshld.Cfisfyear,fshld.Dfshhdate from fishd,fshld where Cfisystat = 'C' and fshld.Cfisfyear = fishd.Cfisfyear"
SQLNorANORvact = "Select * from fishd where Cfisystat = 'C' "
'Response.Write SQLNorANORvact&"<br>"
RsNormANORMVaction.Open SQLNorANORvact,conn

%>
<%
strDate = now()
dDate = cDate(strDate)
Startday = WeekDay(date(),2)
Response.Write Startday&"<br>"
today = Month(now()) & "/" & Day(now()) & "/" & Year(now())
y = year(today)

xbeg = date()
xend = dateadd("d",10,date())

if cdate(RsNormANORMVaction.Fields("cfisfyear"))= cdate(y) then
	for i=1 to len(trim(RsNormANORMVaction.Fields("Cfisnonwd").Value))
		NormalVacation = mid(RsNormANORMVaction.Fields("Cfisnonwd").Value,i,1)
		
		strNwd = Trim(RsNormANORMVaction.Fields("Cfisnonwd").Value)
		strfirts = Mid(strNwd,1,1)
		strSecond = Mid(strNwd,2,1)
	next
else
	Response.Write ("different year "&"<br>")
end if
strNoOfDays = 0
 
do while (xbeg <=xend)
	'Response.Write (xbeg&"<br>")	
	Startday1 = WeekDay(cdate(xbeg),2)	
	'Response.Write (Startday1&"<br>")		
	if Startday1 = cint(strfirts) or Startday1 = cInt(strSecond) then
		strNoOfDays = strNoOfDays+1
	end if
	xbeg = dateadd("d",1,xbeg)'Month(xbeg) & "/" & Day(xbeg)+1 & "/" & Year(xbeg)
	'Response.Write (xbeg&"<br>")	
loop
xDate = dateadd("d",int(strNoOfDays),xend)
Startday1 = WeekDay(cdate(xDate),2)	
if Startday1 = cint(strfirts) or Startday1 = cInt(strSecond) then
	xDate = dateadd("d",1,xDate)
end if
Startday1 = WeekDay(cdate(xDate),2)	
if Startday1 = cint(strfirts) or Startday1 = cInt(strSecond) then
	xDate = dateadd("d",1,xDate)
end if
Response.Write (xDate&"<br>")	
Response.End 
%>

<HTML>
<HEAD>
<META name=VI60_defaultClientScript content=VBScript>

<SCRIPT ID=clientEventHandlersVBS LANGUAGE=vbscript>
<!--

Sub button1_onclick
	strDate = document.FORM1.text1.value
	dDate = cDate(strDate)
	
	MsgBox WeekDay(dDate,2)
End Sub

-->
</SCRIPT>
</HEAD>
<BODY>
<FORM action="" id=FORM1 method=post name=FORM1>
<P>&nbsp;</P>
<P><INPUT id=text1 name=text1></P>
<P><INPUT id=button1 name=button1 type=button value=Button></P></FORM>
<br>
<%response.write now()&"<br>"%>
</BODY>
</HTML>
