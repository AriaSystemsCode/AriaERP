<%@ Language=VBScript %>
<% Response.CacheControl = "no-cache" 
 Response.AddHeader "Pragma", "no-cache" 
 Response.Expires = -1 
 Response.Buffer=true


IF Trim(Session("ID")) = "" AND Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 


IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
End IF
%>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=windows-1252">
<meta http-equiv="Content-Language" content="en-us">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<meta name="ProgId" content="FrontPage.Editor.Document">
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/help.css">

<title>CRM - Helpdesk</title>
</Head>
<Body>
<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>

<%Else%>
	<%End IF%>

<div align="center">
  <center>
  <table border="1" cellspacing="1" width="95%" id="AutoNumber1">
    <tr>
      <td width="100%" class="title">Helpdesk</td>
    </tr>
  </table>
  </center>
</div>

<BR>

  <div align="center">
    <center>

  <table border="0" width="95%" cellspacing="0" cellpadding="0" style="border-collapse: collapse" bordercolor="#111111">
    <tr>
      <td width="100%"><Font face="arial" color="#000080" size=2><STRONG>
      Your issue has been recorded as issue #<b> <%Response.Write(Request("strIssueNo"))%> </b>
        </STRONG></font></td>
    </tr>
    <tr>
		<TD><A href=Helpdesk.asp?Criteria=X><Font face="arial"  size=2>back to issues list</font></A></TD>
    </tr>
  </table>
 
    </center>
</div>
 
 <%'Start by AAK
'Added by AAK to save Issue No. in a table to use it in sql trigger that sends email to the NotificationAddress
Set Conn=Server.CreateObject("ADODB.Connection")
Set RSIssue=Server.CreateObject("ADODB.Recordset")
Conn.Open Application("SqlServer")
StrIssue="Update Email Set cissueno='"&Request("strIssueNo")&"'" 
RSIssue.Open StrIssue,conn,3,3
Conn.Close
Set Conn=nothing
'End bY AAK
%>    
</BODY></HTML>