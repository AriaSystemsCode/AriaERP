<%@ Language=VBScript %>
<%
Response.Buffer=True
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'	Response.redirect "../default.asp"%>
<script language="javascript">
parent.location.href ="../login.asp"
</script>
<%End if

Session("ConnectionString") = Application("DataConnectionString")
Set Connect = Server.CreateObject("ADODB.Connection")
Connect.Open(Session("ConnectionString"))

Session("RSStyStruct").MoveFirst
'update ship address if changed[start]
'if Request.form ("selShipAdd") = "A" then
'	session("RSCust")("stname")    = Request.Form ("txtadd1")
'		
'	session("RSCust")("caddress1") = Request.Form ("txtadd2")
'	
''	session("RSCust")("caddress2") = Request.Form ("txtadd3")
'		
'	Session("Add3") = Request.Form ("txtAdd4")
'			
'	session("RSCust")("caddress6") = Request.Form ("txtadd5")
'		
'	Session("chgAdd") = "A"
'	Session("ShipAdd") = true
'end if
'update ship address if changed[end]
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
	'Multiple Seasons WMA 5/24/2004 START
	'strhdr= "Select pack_id from spck_hdr where cdivision='" & Trim(Session("Division"))  & "' And type='P' and (account='*****' or account='" & CustID & "') and season='" & Session("Season") & "' and pack_id like 'WEB%'"  
	strhdr = "Select pack_id from spck_hdr where cdivision='" & Trim(Session("Division"))  & "' And type='P' and (account='*****' or account='" & CustID & "') "
	strSeasConArr  = split(Session("StyleColor"),",")
	intSeasConCount = int(UBound(strSeasConArr)/24)
	strhdr = strhdr & " AND ("
	for i = 0 to  intSeasConCount 
		for j = i*24 to (i*24+24) -1
			if j = UBound(strSeasConArr) then
				strSQLall = strSQLall + "'" &  strSeasConArr(j) & "'"
			elseif j < UBound(strSeasConArr) then
				if ((j+1) mod 24 = 0 and j<>0) then
					strSQLall = strSQLall + "'" &  strSeasConArr(j) & "'"			
				else
					strSQLall = strSQLall + "'" &  strSeasConArr(j) & "',"			
				end if	
			end if		
		next		
		if i =0 then 'first item
			strhdr = strhdr & " Season in("& strSQLall &") " 
		else
			strhdr = strhdr & " or Season in(" & strSQLall & ") " 
		end if
		strSQLall = ""
	next
	strhdr = strhdr & " )"
	strhdr = strhdr + "and pack_id like 'WEB%'"  
    'Multiple Seasons WMA 5/24/2004 END
	strlin = "select style,pack_id ,Account  from spck_lin Where type='P' and (account='*****' or account='" & CustID & "')"
  strSql = "SHAPE {" & strhdr & "}  AS spck_hdr APPEND ({" & strlin & " }  AS packlin RELATE 'pack_id' TO 'pack_id') AS packlin"

end if
'Response.Write strSql
'Response.End 
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
    if Request.QueryString ("Type") = "M" then
		Response.Redirect("modifyorder.asp")
	else
		Response.Redirect("custorder.asp")
	end if
else
	Response.Redirect("../common/findstyle.asp?logintype=O&Type="&Request.QueryString ("Type"))
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
