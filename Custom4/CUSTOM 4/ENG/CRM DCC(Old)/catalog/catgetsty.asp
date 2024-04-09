<%@ Language=VBScript %>

<%
Session("ConnectionString") = "Provider=MSDATASHAPE;DSN=CRM;SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=YES;"
Set Connect = Server.CreateObject("ADODB.Connection")
Connect.Open Application("DataConnectionString")

Session("RSStyStruct").MoveFirst

IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
Else
	Dim strStyle, strcoll, strasd
	strStyle = ""
	DO While Not Session("RSStyStruct").Eof
		strcoll = request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value))
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
End IF





Set RSGetSty = Server.CreateObject("ADODB.Recordset")

'To Handel the rep loggin.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF


'The next Select statment to verifay the styles from the style file  - ARD - 
'strSql = "select style, desc, desc1 from style where style='" & Ucase(strStyle) & "'"  'Ucase(Trim(request("txtstyle"))) & "'"

'The next select statment is to verifay the style from pack file - ARD - 
if Session("Season")= "*" then
	strhdr= "Select pack_id from spck_hdr where cdivision='" & Trim(Session("Division"))  & "' And type='P' and (account='*****' or account='" & CustID & "') and pack_id like 'WEB%'"  
	strlin = "select style,pack_id ,Account  from spck_lin Where type='P' and (account='*****' or account='" & CustID & "')"
  strSql = "SHAPE {" & strhdr & "}  AS spck_hdr APPEND ({" & strlin & " }  AS packlin RELATE 'pack_id' TO 'pack_id') AS packlin"

	
else
	strhdr= "Select pack_id from spck_hdr where cdivision='" & Trim(Session("Division"))  & "' And type='P' and (account='*****' or account='" & CustID & "') and season='" & Session("Season") & "' and pack_id like 'WEB%'"  
	strlin = "select style,pack_id ,Account  from spck_lin Where type='P' and (account='*****' or account='" & CustID & "')"
  strSql = "SHAPE {" & strhdr & "}  AS spck_hdr APPEND ({" & strlin & " }  AS packlin RELATE 'pack_id' TO 'pack_id') AS packlin"

end if

RSGetSty.open  strSql , connect,2,4

If RSGetSty.EOF And RSGetSty.Bof then 

Else
	RSGetSty.MoveFirst
	Do while not RSGetSty.EOF
			Set RSTEmp = RSGetSty.Fields("packlin").Value  
			if not (RSTEmp.eof and RSTEmp.bof) then
				RSTEmp.MoveFirst
				Do while not RSTEmp.EOF
					IF Ucase(strStyle) = RSTEmp("style") Then
						strFoundIt = "Y"
						exit do
					Else
						'Response.Redirect()
						strFoundIt = "N"
					End IF
					RSTEmp.MoveNext
				Loop
			end if	
		RSGetSty.MoveNext
	Loop
End IF

'Response.Write(Ucase(strStyle) & "-----" & RSTEmp("style"))

If strFoundIt = "Y" Then
	
	RSGetSty.Close 
	strsql = "select * from style where style ='" & Ucase(strStyle) & "'"
	RSGetSty.open  strSql , connect,2,4

	Session("getstyle") = Ucase(strStyle)
	Session("ShortDesc") = RSGetSty("desc")
	Session("LongDesc") = RSGetSty("desc1")
  Response.Redirect("catcustord.asp")

else
	Response.Redirect("catfindsty.asp")

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
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
</HEAD>
<BODY>
    

<P>&nbsp;</P>

</BODY>
</HTML>
