<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<HTML><HEAD>
<META http-equiv=Content-Type content="text/html; charset=unicode">
<META content="MSHTML 6.00.2462.0" name=GENERATOR></HEAD>
<BODY leftmargin=0 topmargin=0 bgcolor="#aecae6">
<TABLE cellSpacing=1 cellPadding=1 width="50%" align=center border=0>
  <TR>
    <TD align=middle width=50%><%
    IF Request.QueryString("CurPage") > 1 Then    
			%><a target="_parent" href=gcatpage.asp?CurPage=<%=Request.QueryString("CurPage")-1%>><img src=../images/back.gif alt=Back align=middle border=0></a>
		<%End IF%>	
		</TD>
    <TD align=middle width=50%><%
    IF cint(Request.QueryString("CurPage")) <> Cint(Request.QueryString("UpLimit")) Then
    %><a target="_parent" href=gcatpage.asp?CurPage=<%=Request.QueryString("CurPage")+1%>><img src=../images/next.gif alt=Back align=middle border=0></a>
    <%End IF%>
   </TD>
  </TR>
</TABLE>
<%'response.write(Cint(Request.QueryString("UpLimit")))%>
</BODY>
</HTML>
