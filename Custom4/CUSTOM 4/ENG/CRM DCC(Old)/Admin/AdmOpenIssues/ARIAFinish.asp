<%Response.Buffer = true%>
<%if Trim(Session("AdmID"))="" then
	Response.Redirect "Default.asp"
end if
%>
<HTML>
<TITLE>ARIA Systems</TITLE>
<BODY>
<p>
Your Tracking Entry has been recorded as #<b><%=Request("TrackEntry")%></b>
</p>



<p>
<TABLE border=0 cellPadding=1 cellSpacing=1 width="75%">
  
  <TR>
    <TD>
      <P><A href="Default.asp">Back to issues list</A> </P></TD>
    <TD><A href="Default.asp">Back to support main 
page</A></TD></TR></TABLE>
</p>
<P>&nbsp;</P></BODY>
</HTML>

