<%Response.Buffer=true
Session("customerid")=""
Session("rep")=""
Session("ID")=""
session.Abandon ()
'NEK[Start]
'Response.Redirect "../S_Login.asp"%>

	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%'NEK [End]%>	