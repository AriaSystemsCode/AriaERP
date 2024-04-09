<%@ Language=VBScript %>
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>
<%Response.Buffer = true%>
<%
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 

%>
<%
'Session("ConnectionString") = Application("DataConnectionString")
session("ConnectionString") =  "Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("DataPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"

Set Connect = Server.CreateObject("ADODB.Connection")
Connect.Open(Session("ConnectionString"))

Session("RSStyStruct").MoveFirst

IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
	Response.Write("EOF")
	Response.End 
Else
	Dim strStyle, strcoll, strasd
	strStyle = ""
	if trim(request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value))) <> "" then
		DO While Not Session("RSStyStruct").Eof
			strcoll = request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value))
			Response.Write(strcoll & "-<BR>")
			Session(Trim(Session("RSStyStruct").Fields("cisegsdes").Value)) = strcoll
			intTemp = Session("RSStyStruct").Fields("nisegsize")
			intCount = cdbl(intTemp) - Len(strcoll)
			Do While intCount > 0
				intCount = intCount - 1
				strcoll = strcoll & " "
			Loop
			strStyle = strStyle & strcoll & Trim(Session("RSStyStruct").Fields("cisegsepr").Value)
			Session("RSStyStruct").MoveNext
		Loop
		Response.Write(Session(strcoll))
		'Response.End 
	End if
End IF

Set RSGetSty = Server.CreateObject("ADODB.Recordset")

'To Handel the rep loggin.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF


			strhdr= "Select pack_id from spck_hdr where type='P' and (account='*****' or account='" & CustID & "') and pack_id like 'WEB%'"  
			strlin = "select style,pack_id ,Account  from spck_lin Where type='P' and (account='*****' or account='" & CustID & "')"
		    strSql = "SHAPE {" & strhdr & "}  AS spck_hdr APPEND ({" & strlin & " }  AS packlin RELATE 'pack_id' TO 'pack_id') AS packlin"

'If strFoundIt = "Y" Then
If Trim(strStyle) <> "" Then
	
	'RSGetSty.Close 
	strsql = "select * from style where style ='" & Ucase(strStyle) & "'"
	RSGetSty.open  strSql , connect,2,4
	Response.Write("<BR>H:"&strStyle)
	'Response.End 
	
	If RSGetSty.EOF and RSGetSty.BOF Then
		Session("getstyle") = Ucase(strStyle)
		Response.Redirect "ots.asp?Result=F"
	Else
		Session("getstyle") = Ucase(strStyle)
		Session("ShortDesc") = RSGetSty("desc")
		Session("LongDesc") = RSGetSty("desc1")
		Response.Redirect "ots.asp?Result=T"
	End If

else
	
	IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
	Else
			Session("RSStyStruct").MoveFirst
			Dim intStart, intEnd ' as integer
			intStart = 1 
			Session("getstyle") = Trim(strStyle)

			
			Session("LongDesc") = Request("desc1")
			DO While Not Session("RSStyStruct").Eof
				intEnd = Session("RSStyStruct").fields("nisegsize")
				
				Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) = Mid(strStyle,intStart,intEnd)
				intStart = 2 + cdbl(intEnd)
				Session("RSStyStruct").MoveNext
			Loop
	End IF
	Response.Redirect("../common/findstyle.asp?logintype=T")

End IF



if RSGetSty.eof And RSGetSty.bof then
	'Response.Redirect("findstyle.asp")
else
	'Session("getstyle") = Ucase(strStyle)
	'Session("ShortDesc") = RSGetSty("desc")
	'Session("LongDesc") = RSGetSty("desc1")
 ' Response.Redirect("custorder.asp")
end if


%>