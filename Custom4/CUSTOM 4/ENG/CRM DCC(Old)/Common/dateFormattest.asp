<!--#include file="checkDateFormat.asp"-->
<body onLoad="<%=DateFormat()%>">
<%if Session("DateFormat")<>"" then
	Response.Write "Our Date FOrmat "&Session("DateFormat")
else	
	Response.Write "NO  Date Format "
end if %>
</body>