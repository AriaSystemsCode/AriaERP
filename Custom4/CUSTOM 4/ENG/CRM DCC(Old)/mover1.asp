<%
Select case request("type")
	case "M"
		IF request("lstsource") <> "" Then
			Session("RSSource").Filter = "account='" & request("lstsource") & "'" 
			Session("RSTarget").Filter = "account='" & request("lstsource") & "'" 
			IF (Session("RSTarget").EOF and Session("RSTarget").BOF) Then
				Session("RSTarget").addnew
				for inti=0 to Session("RSSource").Fields.count - 1
					Session("RSTarget").fields(Session("RSSource").Fields(inti).name).value	 = Session("RSSource").Fields(inti).value
				next
				Session("RSTarget").updatebatch
			End IF
			Session("RSTarget").Filter = "" 
			Session("RSSource").filter = "" 
		End IF
		Response.Redirect("mover.asp")
	case "ML"
		Set Session("RSTarget") = Session("RSSource")
		Response.Redirect("mover.asp")
	case "R"
		IF request("lsttarget") <> "" Then
			Session("RSTarget").Filter = "account='" & request("lstTarget") & "'" 
			IF Not(Session("RSTarget").EOF and Session("RSTarget").BOF) Then
				Session("RSTarget").delete
				Session("RSTarget").updatebatch
				Session("RSTarget").movefirst
			End IF
			Session("RSTarget").Filter = "" 
			Session("RSSource").filter = "" 
		End IF
		Response.Redirect("mover.asp")
	Case "RA"
		IF Not(Session("RSTarget").EOF and Session("RSTarget").BOF) Then
			Session("RSTarget").Movefirst
		End IF
		Do while Not Session("RSTarget").Eof
			Session("RSTarget").delete
			Session("RSTarget").movefirst
		Loop
		Response.Redirect("mover.asp")
End Select
%>
