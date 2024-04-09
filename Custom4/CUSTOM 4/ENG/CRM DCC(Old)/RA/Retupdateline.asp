<%@ Language=VBScript %>
<%Response.Buffer=true

If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 
if Session("Seq") = "" then
	Session("Seq") = 0
else
end if
IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("repcust.asp")
	END IF
	strFile = "reb"
End IF
%>
<div align="center">
  <center>
  <table border="0" width="100%" cellspacing="0" cellpadding="0">
    <tr>
      <td width="100%" align="center"><p>
<OBJECT align=baseline classid=clsid:D27CDB6E-AE6D-11cf-96B8-444553540000 
            codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" 
height=125 id=ShockwaveFlash1 width=80% border = 0>
    <param name="_cx" value="11748">
    <param name="_cy" value="3307">
    <param name="Movie" value="flash/<%=StrFile%>Nav.swf">
    <param name="Src" value="flash/<%=StrFile%>Nav.swf">
    <param name="WMode" value="Transparent">
    <param name="Play" value="0">
    <param name="Loop" value="0">
    <param name="Quality" value="Medium">
    <param name="SAlign" value>
    <param name="Menu" value="0">
    <param name="Base" value>
    <param name="Scale" value="ExactFit">
    <param name="DeviceFont" value="0">
    <param name="EmbedMovie" value="0">
    <param name="BGColor" value="AECAE6">
    <param name="SWRemote" value><embed src="flash/<%=StrFile%>Nav.swf" align="baseline" border="0" width="100" height="172" loop="false" menu="false" quality="medium" wmode="transparent" bgcolor="#AECAE6" type="application/x-shockwave-flash" pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash" designtimesp="22693" DESIGNTIMESP="23127" DESIGNTIMESP="23294" DESIGNTIMESP="19128">
</OBJECT>
</p>
      </td>
    </tr>
    <tr>
      <td width="100%" align="center"><p><img width=80% border="0" src="images/CheckOTS0001.jpg"></p></td>
    </tr>
  </table>
  </center>
</div>
<%
Set conn=server.CreateObject("ADODB.connection")
conn.open Application("DataConnectionString")
Set rsRetStyStruct=server.CreateObject("ADODB.recordset")
strSql="select * from style where style='" & Ucase(Request("SlctColor")) & "'"
rsRetStyStruct.open strSql,conn 
Session("selectReason2") = Request.Form("selectReason")

' make fields in the table empty 
Session("text1") = ""
Session("text2") = ""
Session("text3") = ""
Session("text4") = ""
Session("text5") = ""
Session("text6") = ""
Session("text7") = ""
Session("text8") = ""
Session("TotalAmount") = 0
missingFlag = "NO"
'check the form for missing values ...
IF  Session("FindStyleBtnPressed") = "NO" and Request("Typeline") <> "U"THEN
	Response.Write ("You Must write a valid style and press the find style button <BR>")
	Response.Write ("<A HREF=""ReturnDetail.asp"">Go Back and do this</A>")
  missingFlag = "YES"
	Session("FindStyleBtnPressed") = "NO"
	'Response.Write "<BR>" & Trim(Request("SlctColor")) & "<BR>" &  Session("FindStyleBtnPressed")
END IF
' check if try to delete when the record set is empty .. 
IF Request("Typeline") = "R" AND (Session("rsReturnLine").EOF AND Session("rsReturnLine").BOF) THEN
	Session("FindStyleBtnPressed") = "NO"
	Session("getstyle") = request("style")
	Response.Redirect "ReturnDetail.asp"
END IF  

'check to see if trying to update adn didn't fill the quantity textboxs ..
IF Request("Typeline") = "U" AND Len(request("txtord1"))=0  AND Len(request("txtord2"))=0 AND Len(request("txtord3"))=0 AND Len(request("txtord4"))=0 AND Len(request("txtord5"))=0 AND Len(request("txtord6"))=0 AND Len(request("txtord7"))=0 AND Len(request("txtord8"))=0 THEN
	Response.Write ("Dont try to update fields without entering the Qty of each style you want<BR>")
	Response.Write ("<A HREF=""ReturnDetail.asp"">Go Back and enter values for your request </A>")
	missingFlag = "YES"
	session("getstyle")=""
	Session("FindStyleBtnPressed") = "NO"
END IF
  			 		
IF missingFlag = "YES" THEN
ELSE
	Select Case Request("Typeline")
	Case "U":
		' see if the Style and  Reason exist in the record set ( means we want to edit this record
		' else we re going to add new record to the record set.
						
						
			foundFlag ="NO"
			' check to see if first time add item .. 
			IF Session("rsReturnLine").EOF AND Session("rsReturnLine").BOF THEN
			ELSE
				Session("rsReturnLine").MoveFirst()
								
				Do While Not Session("rsReturnLine").EOF
					'Response.Write Session("rsReturnLine").Fields("cra_linno") & Request.QueryString ("LineNo")
					'Response.End 
'									IF (Session("selectReason2")= Session("rsReturnLine").Fields("Reason") AND Request("SlctColor") = Session("rsReturnLine").Fields("style") ) THEN
					'wma
					IF Request("SlctColor") = Session("rsReturnLine").Fields("style")  and Session("rsReturnLine").Fields("cra_linno") = Session("LineNo") THEN
					'IF Request("SlctColor") = Session("rsReturnLine").Fields("style")  and Session("rsReturnLine").Fields("cret_linno") = Session("LineNo") THEN
						foundFlag ="YES"
						Exit Do
					END IF
					Session("rsReturnLine").MoveNext()
				Loop
			END IF ' first time add
						
			IF foundFlag="YES" THEN 
				IF Len(trim(session("rep")))>0 THEN			
					Session("rsReturnLine").Fields("account").Value = Session("customerid")
				ELSE
					Session("rsReturnLine").Fields("account").Value = Session("rsReturnLine").Fields("account")
				END IF
				Session("rsReturnLine").Fields("style").value = Request("SlctColor")
				Session("rsReturnLine").Fields("Reason").value =Session("selectReason2")
					  
	  			IF Len(request("txtord1"))=0 Then
					Session("rsReturnLine").fields("qty1") = 0
				Else
					Session("rsReturnLine").fields("qty1") = cdbl(request("txtord1"))
				END IF
						
				IF Len(request("txtord2"))=0 Then
					Session("rsReturnLine").fields("qty2") = 0
				Else
					Session("rsReturnLine").fields("qty2") = cdbl(request("txtord2"))
				END IF

				IF Len(request("txtord3"))=0 Then
					Session("rsReturnLine").fields("qty3") = 0
				Else
					Session("rsReturnLine").fields("qty3") = cdbl(request("txtord3"))
				END IF

				IF Len(request("txtord4"))=0 Then
					Session("rsReturnLine").fields("qty4") = 0
				Else
					Session("rsReturnLine").fields("qty4") = cdbl(request("txtord4"))
				END IF

				IF Len(request("txtord5"))=0 Then
					Session("rsReturnLine").fields("qty5") = 0
				Else
					Session("rsReturnLine").fields("qty5") = cdbl(request("txtord5"))
				END IF

				IF Len(request("txtord6"))=0 Then
					Session("rsReturnLine").fields("qty6") = 0
				Else
					Session("rsReturnLine").fields("qty6") = cdbl(request("txtord6"))
				END IF

				IF Len(request("txtord7"))=0 Then
					Session("rsReturnLine").fields("qty7") = 0
				Else
					Session("rsReturnLine").fields("qty7") = cdbl(request("txtord7"))
				END IF

				IF Len(request("txtord8"))=0 Then
					Session("rsReturnLine").fields("qty8") = 0
				Else
					Session("rsReturnLine").fields("qty8") = cdbl(request("txtord8"))
				END IF
						
				Dim inttemp1 ' as integer

				inttemp1 = cdbl(Session("rsReturnLine").fields("qty1")) + cdbl(Session("rsReturnLine").fields("qty2"))
				inttemp1 = inttemp1 + cdbl(Session("rsReturnLine").fields("qty3")) + cdbl(Session("rsReturnLine").fields("qty4"))
				inttemp1 = inttemp1 + cdbl(Session("rsReturnLine").fields("qty5")) + cdbl(Session("rsReturnLine").fields("qty6"))
				inttemp1 = inttemp1 + cdbl(Session("rsReturnLine").fields("qty7")) + cdbl(Session("rsReturnLine").fields("qty8"))
				Session("rsReturnLine").fields("totqty") = inttemp1
				Session("rsReturnLine").fields("price") = cdbl(rsRetStyStruct("pricea"))
			else ' Not Found .. so we are going to add it as new record
				Session("rsReturnLine").AddNew
				'Session("rsReturnLine").Fields("cordtype").Value = "O"
				'Session("rsReturnLine").Fields("account").Value = Session("RSCust").Fields("account").value
				if len(trim(session("rep")))>0 then
				Session("rsReturnLine").Fields("account").Value =session("customerid")
				else
				Session("rsReturnLine").Fields("account").Value =session("id")
				end if
				Session("rsReturnLine").Fields("style").value = Request("SlctColor")
				'wma
				Session("rsReturnLine").Fields("cra_linno").value = Session("Seq")+1'Session("rsReturnLine").recordcount
				'Session("rsReturnLine").Fields("cret_linno").value = Session("Seq")+1'Session("rsReturnLine").recordcount
				'Session("rsReturnLine").Fields("desc1").value = rsRetStyStruct("desc1")
				'Session("rsReturnLine").Fields("scale").value = rsRetStyStruct("scale")
				'Session("rsReturnLine").Fields("prepak").value = rsRetStyStruct("prepak")
				'Session("rsReturnLine").Fields("nsugretpri").value = 0
				'Session("rsReturnLine").Fields("season").value = Session("Season")
				Session("rsReturnLine").Fields("Reason").value =Session("selectReason2")
					  

				IF Len(request("txtord1"))=0 Then
					Session("rsReturnLine").fields("qty1") = 0
				Else
					Session("rsReturnLine").fields("qty1") = cdbl(request("txtord1"))
				END IF
							
				IF Len(request("txtord2"))=0 Then
					Session("rsReturnLine").fields("qty2") = 0
				Else
					Session("rsReturnLine").fields("qty2") = cdbl(request("txtord2"))
				END IF

				IF Len(request("txtord3"))=0 Then
					Session("rsReturnLine").fields("qty3") = 0
				Else
					Session("rsReturnLine").fields("qty3") = cdbl(request("txtord3"))
				END IF

				IF Len(request("txtord4"))=0 Then
					Session("rsReturnLine").fields("qty4") = 0
				Else
					Session("rsReturnLine").fields("qty4") = cdbl(request("txtord4"))
				END IF
						
				IF Len(request("txtord5"))=0 Then
					Session("rsReturnLine").fields("qty5") = 0
				Else
					Session("rsReturnLine").fields("qty5") = cdbl(request("txtord5"))
				END IF

				IF Len(request("txtord6"))=0 Then
					Session("rsReturnLine").fields("qty6") = 0
				Else
					Session("rsReturnLine").fields("qty6") = cdbl(request("txtord6"))
				END IF

				IF Len(request("txtord7"))=0 Then
					Session("rsReturnLine").fields("qty7") = 0
				Else
					Session("rsReturnLine").fields("qty7") = cdbl(request("txtord7"))
				END IF

				IF Len(request("txtord8"))=0 Then
					Session("rsReturnLine").fields("qty8") = 0
				Else
					Session("rsReturnLine").fields("qty8") = cdbl(request("txtord8"))
				END IF
						
				Dim inttemp ' as integer
						
				inttemp = cdbl(Session("rsReturnLine").fields("qty1")) + cdbl(Session("rsReturnLine").fields("qty2"))
				inttemp = inttemp + cdbl(Session("rsReturnLine").fields("qty3")) + cdbl(Session("rsReturnLine").fields("qty4"))
				inttemp = inttemp + cdbl(Session("rsReturnLine").fields("qty5")) + cdbl(Session("rsReturnLine").fields("qty6"))
				inttemp = inttemp + cdbl(Session("rsReturnLine").fields("qty7")) + cdbl(Session("rsReturnLine").fields("qty8"))
				Session("rsReturnLine").fields("totqty") = inttemp
				Session("rsReturnLine").fields("price") = cdbl(rsRetStyStruct("pricea"))

				'	IF Len(request("txtord"))=0 Then
				'		objrsReturnLine("totqty") = 0
				'	Else
				'		objrsReturnLine("totqty") = request("txtord")
				'	END IF
				Session("Seq") = Session("Seq")+1			
			END IF ' For foundFlag
						
	case "R" :
		' see if the Style and  Reason exist in the record set ( means we want to delete this record
		' else no thing done because the record not exist ..
						
						
			foundFlag ="NO"
			' check to see if first time add item .. 
			IF Session("rsReturnLine").EOF AND Session("rsReturnLine").BOF then 
			Session("getstyle") = "" ' this line has been added by ARD
			ELSE
				Session("rsReturnLine").MoveFirst()
							
				Do while not Session("rsReturnLine").EOF
					if (Session("selectReason2")= Session("rsReturnLine").Fields("Reason") AND Request("SlctColor") = Session("rsReturnLine").Fields("style") ) then
						foundFlag ="YES"
						Exit Do
					END IF
					Session("rsReturnLine").MoveNext()
				Loop
			END IF ' first time add
						
			IF foundFlag="YES" THEN 
				Session("rsReturnLine").Delete()
			END IF
	END SELECT
	
	'DO While Not Session("rsRetStyStruct").Eof
	'	Session(Trim(Session("rsRetStyStruct").fields("cisegsdes"))) = ""
	'	Session("rsRetStyStruct").MoveNext
	'Loop

	Session("getstyle")=""
	Session("OrderFlag") = "X"
	Session("LongDesc") = ""
	Session("ShortDesc") = ""

	'Compute the Total Quantity and the total amount and put them in the session to let 
	'them be displayed in the Return Detail form. 
	'1- Total Account
	tempTotalQty = 0 
	tempTotalAmount = 0

	Session("rsReturnLine").MoveFirst()
	IF Session("rsReturnLine").EOF AND Session("rsReturnLine").BOF then 
		Session("TotalQty")= 0
		Session("TotalAmount") = 0
	ELSE
		DO While Not Session("rsReturnLine").EOF
			tempTotalQty =tempTotalQty + cdbl(Session("rsReturnLine").fields("totqty"))
			intAmount = cdbl(Session("rsReturnLine").fields("totqty")) * cdbl(Session("rsReturnLine").fields("price"))
			tempTotalAmount = tempTotalAmount + intAmount
			Session("rsReturnLine").MoveNext()
		LOOP

		Session("rsReturnLine").MoveFirst()

		Session("TotalQty")= tempTotalQty
		Session("TotalAmount") = tempTotalAmount
	END IF
		'if len(trim(session("rep")))>0 then
		'Response.Redirect("repReturnDetail.asp") ' by ARD
		'else
	session("LineNo") = ""
	Response.Redirect("ReturnDetail.asp") 
		'end if
		'Response.Write(inttemp)
						
END IF ' for missingFlag in the start of the page ...
'Empty line no session after updating it

%>
