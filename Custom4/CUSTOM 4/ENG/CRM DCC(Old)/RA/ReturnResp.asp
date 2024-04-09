<%@ Language=VBScript %>
<%
if Trim(Session("ID")) = "" then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 

%>
<HTML>
<HEAD>
<meta NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
</head>
<body bgcolor="#aecae6" topmargin="0" leftmargin="0" background="../images/Tile1.gif">

<div align="center">
  <center>
  <table border="0" width="100%" cellspacing="0" cellpadding="0">
    <tr>
      <td width="100%" align="center"><p>
<OBJECT align=baseline classid=clsid:D27CDB6E-AE6D-11cf-96B8-444553540000 
   codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" 
height=142 id=ShockwaveFlash1 width=800 border = 0>
    <param name="_cx" value="21167">
    <param name="_cy" value="3704">
    <param name="Movie" value="../flash/CustNav.swf">
    <param name="Src" value="../flash/CustNav.swf">
    <param name="WMode" value="Transparent">
    <param name="Play" value="0">
    <param name="Loop" value="0">
    <param name="Quality" value="Medium">
    <param name="SAlign" value>
    <param name="Menu" value="0">
    <param name="Base" value>
    <param name="Scale" value="ShowAll">
    <param name="DeviceFont" value="0">
    <param name="EmbedMovie" value="0">
    <param name="BGColor" value="AECAE6">
    <param name="SWRemote" value><embed src="../flash/CustNav.swf" align="baseline" border="0" width="100" height="172" loop="false" menu="false" quality="medium" wmode="transparent" bgcolor="#AECAE6" type="application/x-shockwave-flash" pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash" designtimesp="22693" DESIGNTIMESP="23127" DESIGNTIMESP="23294" DESIGNTIMESP="19128">
</OBJECT>
</p>
      </td>
    </tr>
    <tr>
      <td width="100%" align="center"><p></p></td>
    </tr>
  </table>
  </center>
</div>
<%

'DB Connection
set conn=server.CreateObject("ADODB.connection")
'Open Connection and execute quiries
conn.Open "dsn=CRM;uid=aria;pwd=aria"

'RecordSets
set rsRANO = server.CreateObject("ADODB.RecordSet") 
rsRANO = conn.Execute ("SELECT Cseq_type, Nseq_no,nfld_wdth FROM Sequence WHERE Cseq_type='RANO'")
currentRANO = rsRANO("Nseq_no")
' Update RANO in Sequence Table
newRANO = cint (rsRANO("Nseq_no")) + 1
sqlUpdateSeq = "UPDATE Sequence SET Nseq_no = " & newRANO & " WHERE Cseq_type='RANO'" 
'Response.Write sqlUpdateSeq & "<BR>"
conn.Execute sqlUpdateSeq

Do while Not Len(currentRano) =Cint(rsRANO("nfld_wdth"))
	currentRano = "0" & currentRano
Loop
currentRano = "'" & currentRano & "'"


entered = "{" & Request.Form("txtEntered") & "}"
VoidDate = "{" & Request.Form("txtVoid") & "}"
Qty = Request.Form("txtQty")
Amount = Request.Form("txtAmount")
Store = "'" & Request.Form("selectStore") & "'"
Location = "'" & Request.Form("selectLocation") & "'"
Reason  = "'" & Request.Form("selectReason") & "'"
Division  = "'" & Request.Form("selectDivision") & "'"
UserID = "'" & Session("ID") & "'"

'AddRetAuth Query
sqlRetAuth = "INSERT INTO RetAuth (Rano, Account, Store,Status, Radate, Void, Reason, Cdivision, Auth, Authamt, Cwarecode) VALUES"
sqlRetAuth = sqlRetAuth & "(" & CurrentRANO & "," & UserID & "," & Store & ",'O'," & entered & "," & VoidDate & "," & Reason & "," & Division & "," & Qty & "," & Amount & "," & Location & ")"

'Response.Write sqlRetAuth
conn.Execute sqlRetAuth

%>


</BODY>
</HTML>
