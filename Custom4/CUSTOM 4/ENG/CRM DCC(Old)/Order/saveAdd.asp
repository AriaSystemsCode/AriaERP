<%@ Language=VBScript %>
<%
Response.Buffer=True
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'	Response.redirect "../default.asp"%>
<script language="javascript">
parent.location.href ="../default.asp"
</script>
<%End if



Session("RSStyStruct").MoveFirst
'update ship address if changed[start]
if Request.form ("selShipAdd") = "A" then
	Session("Add1")   = Request.Form ("txtadd1")
		
	Session("Add2") = Request.Form ("txtadd2")
	
	Session("Add3") = Request.Form ("txtadd3")
		
	Session("Add4") = Request.Form ("txtAdd4")
			
	Session("Add5") = Request.Form ("txtadd5")
		
	Session("chgAdd") = "A"
	Session("Type") = "A"
	Session("AddChg") = true
end if
Response.Redirect "custorder.asp?From="&Request.QueryString ("From")&"&selShipAdd=A"
%>
