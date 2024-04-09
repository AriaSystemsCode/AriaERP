<%Response.Buffer=true
Session("customerid")=""
Session("rep")=""
Session("ID")=""
session.Abandon ()
'NEK[Start] 1/9/2003
'Response.Redirect "S_Login.asp"
'Response.Redirect "crmfrm.htm"%>
	<script language="javascript">
		parent.location.href ="login.asp"
	</script>	

<%'NEK[End]1/9/2003%>