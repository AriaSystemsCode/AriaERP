<%@ Language=VBScript %>

<%

IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
Else
	Session("RSStyStruct").MoveFirst
	Dim intStart, intEnd ' as integer
	intStart = 1 
	Session("getstyle") = Request("Style")
	
	DO While Not Session("RSStyStruct").Eof
		intEnd = Session("RSStyStruct").fields("nisegsize")
		
		Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) = Mid(Request("Style"),intStart,intEnd)
		intStart = 2 + cdbl(intEnd)
		Session("RSStyStruct").MoveNext
	Loop
End IF

	if Session("RSLine").EOF AND Session("RSLine").BOF then 
	else
		Session("RSLine").MoveFirst()
		Do while not Session("RSLine").EOF
		
			IF Trim(Request("LineNo")) = Trim(Session("RSLine").Fields("lineNo")) Then
				Session("LongDesc") = Session("RSLine").Fields("desc1")
				For inti = 1 to 8
					if cdbl(Session("RSLine")("Qty"&inti)) = 0 then
						Session("text"&inti) = ""
					else
						Session("text"&inti) = Session("RSLine")("Qty"&inti)
					end if
				Next 
				'Session("text1") = Session("RSLine").Fields("Qty1")
				'Session("text2") = Session("RSLine").Fields("Qty2")
				'Session("text3") = Session("RSLine").Fields("Qty3") 
				'Session("text4") = Session("RSLine").Fields("Qty4")
				'Session("text5") = Session("RSLine").Fields("Qty5")
				'Session("text6") = Session("RSLine").Fields("Qty6")
				'Session("text7") = Session("RSLine").Fields("Qty7")
				'Session("text8") = Session("RSLine").Fields("Qty8")
				'Session("Disc")  = Session("RSLine").Fields("Disc_pcnt")
				Session("Comm")  = Session("RSLine").Fields("Comm1")
				Session("Grp")   = Session("RSLine").Fields("Group")
				Session("Price") = Session("RSLine").Fields("Gros_price")
				Session("SelLine") = Trim(Session("RSLine").Fields("lineNo"))
				session("Curr_disc") = Session("RSLine").Fields("Disc_pcnt")
				Exit Do
			End IF 
			Session("RSLine").MoveNext()
		Loop
	End IF ' first time add

IF foundFlag="YES" then 
Else
End if

if Request.QueryString ("Type") <> "M" then
	Response.Redirect"custorder.asp?From="&Request.QueryString ("From")
else
	Response.Redirect"modifyorder.asp?Display=T&Come=U&From="&Request.QueryString ("From")
end if
'Response.Redirect"custorder.asp?From="&Request.QueryString ("From")



%>


