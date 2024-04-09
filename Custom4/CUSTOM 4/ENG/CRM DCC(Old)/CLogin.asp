<%@ Language=VBScript %>
<%Response.Buffer = true
 Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>


<html>
<head>
<title>CRM - Main Page</title>
<LINK rel="stylesheet" type="text/css" href="images/<%=Session("THEME")%>/Common.css">
</head>
<body>
<Div align='cecnter'>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
</div>
<p><br><br><br><br><br><br></p>

  <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="23%" rowspan="3" align="middle" class="dark_cell"><b>
      <font face="Arial" size="6">Welcome</font></b></td>
      <td width="52%" class="dark_cell"><FONT 
      face=Arial>&nbsp;</FONT></td>
      <td width="25%" rowspan="3" align="middle" class="dark_cell"><b>
      <font size="4" face=Arial>to
        our CRM site</font></b></td>
    </tr>
     <%if not IsObject(Session("RSCust")) then%>
		<script language="javascript">
		parent.location.href ="default.asp"
		</script>
      <tr>
    <%else%>
    <%'	Response.Write "<font size=2 color=red>"&aPPLICATION("sTORE")%>
     <td width="52%" align="middle" class=light_cell><b><%=Session("Cust_Name") %></b></td>
    </tr>
    <%end if%> 
    <tr>
      <td width="52%" class="dark_cell">&nbsp;</td>
    </tr>
  </table>


</body>
</html>