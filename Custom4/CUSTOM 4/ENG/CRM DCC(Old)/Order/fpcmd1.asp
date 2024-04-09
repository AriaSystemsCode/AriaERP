<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<TITLE></TITLE>
</HEAD>
<BODY>
<%
dim o 
set o = server.CreateObject("FPCMD.FPCMD")
Response.Write("|"& o.docmd("return sys(2018)") &"|")

%>
<P>&nbsp;</P>

</BODY>
</HTML>
