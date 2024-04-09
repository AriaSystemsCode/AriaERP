<%@ Language=VBScript %>
<%Response.Buffer=true
IF Trim(Session("ID")) = "" And Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 

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

<body leftmargin="0" topmargin="0" bgColor=#aecae6 background="images/tile1.gif">

	<div align="center">
  <center>
<table border="0" width="100%" cellspacing="0" cellpadding="0">
  <tr>
    <td width="100%" align="center">
<p>
<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"    codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" WIDTH=80% HEIGHT=125 id="ShockwaveFlash1">
 <param name="_cx" value="13653">
 <param name="_cy" value="3307">
 <param name="Movie" value="flash/<%=StrFile%>Nav.swf">
 <param name="Src" value="flash/<%=StrFile%>Nav.swf">
 <param name="WMode" value="Transparent">
 <param name="Play" value="0">
 <param name="Loop" value="0">
 <param name="Quality" value="High">
 <param name="SAlign" value>
 <param name="Menu" value="0">
 <param name="Base" value>
 <param name="Scale" value="ExactFit">
 <param name="DeviceFont" value="0">
 <param name="EmbedMovie" value="0">
 <param name="BGColor" value="AECAE6">
 <param name="SWRemote" value><embed src="flash/<%=StrFile%>Nav.swf" loop="false" menu="false" quality="medium" wmode="transparent" bgcolor="#AECAE6" WIDTH="100%" HEIGHT="172" TYPE="application/x-shockwave-flash" PLUGINSPAGE="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash">
</object>
</p>
    </td>
  </tr>
  <tr>
    <td width="100%" align="center"><p><img width=80% height=50 border="0" src="images/remoteorder0001.jpg"></p></td>
  </tr>
</table>
  </center>
</div>
<%
'Response.Write(request("sty"))
'strTemp = "<br>"
'Response.Write(strTemp)

IF Session("rsRetStyStruct").EOF And Session("rsRetStyStruct").BOF Then
Else
	Session("rsRetStyStruct").MoveFirst
	Dim intStart, intEnd ' as integer
	intStart = 1 
	Session("getstyle") = Request("Style")
	'Response.Write(Request("desc1"))
	
	'Session("LongDesc") = Request("desc1")
	
	'Response.Write(Session("LongDesc"))
	DO While Not Session("rsRetStyStruct").Eof
		intEnd = Session("rsRetStyStruct").fields("nisegsize")
		
		Session(Trim(Session("rsRetStyStruct").fields("cisegsdes"))) = Mid(Request("Style"),intStart,intEnd)
		intStart = 2 + cdbl(intEnd)
		'Response.Write(Session(Trim(Session("rsRetStyStruct").fields("cisegsdes"))))
		Session("rsRetStyStruct").MoveNext
	Loop
End IF

Session("selectReason2") = Request.QueryString("Reason")
Session("LineNo")  = Request.QueryString ("LineNo")

'Put values of each item in the table 
foundFlag ="NO"
' check to see if first time add item .. 
	if Session("rsReturnLine").EOF AND Session("rsReturnLine").BOF then 
	else
		Session("rsReturnLine").MoveFirst()
		Do while not Session("rsReturnLine").EOF
			'if (Request("Reason")= Session("rsReturnLine").Fields("Reason") AND Request("Style") = Session("rsReturnLine").Fields("style") ) then
			if  Request("Style") = Session("rsReturnLine").Fields("style") and Session("rsReturnLine").Fields("cra_linno") = Session("LineNo")then
				foundFlag ="YES"
				Exit Do
			end if 
			Session("rsReturnLine").MoveNext()
		Loop
	end if ' first time add

if foundFlag="YES" then 
	Session("text1") = Cstr(Session("rsReturnLine").Fields("Qty1"))
	Session("text2") = Cstr(Session("rsReturnLine").Fields("Qty2"))
	Session("text3") = Session("rsReturnLine").Fields("Qty3") 
	Session("text4") = Session("rsReturnLine").Fields("Qty4")
	Session("text5") = Session("rsReturnLine").Fields("Qty5")
	Session("text6") = Session("rsReturnLine").Fields("Qty6")
	Session("text7") = Session("rsReturnLine").Fields("Qty7")
	Session("text8") = Session("rsReturnLine").Fields("Qty8")
Else
End if

Response.Redirect("ReturnDetail.asp")
%>


