<%Response.Buffer=true
Session("customerid")=""
Session("rep")=""
Session("ID")=""
'NEK[Start]
session.Abandon ()
'Response.Redirect "../S_Login.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%'NEK [End]%>	