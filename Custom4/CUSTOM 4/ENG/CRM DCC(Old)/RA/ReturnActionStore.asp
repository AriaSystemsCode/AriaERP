<%@ Language=VBScript %>
<%Response.Buffer=true
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
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
      <td width="100%" align="center"><p><img width=80% border="0" src="images/requestra0001.jpg"></p></td>
    </tr>
  </table>
  </center>
</div>


<HTML>
<HEAD><LINK REL=stylesheet HREF="crmmain.css" TYPE="text/css">
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<Title>CRM - Select Store</Title>
</HEAD>
<%
Session("ConnectionString") = "dsn=CRM;uid=aria;pwd=aria"
Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open(Session("ConnectionString"))
strStore =Ucase( Trim(Request.Form("txtSearchStore"))) & "%"
strCity	 =Ucase( Trim(Request.Form("txtSearchCity"))) & "%"
strState =Ucase( Trim(Request.Form("txtSearchState"))) & "%"
strZipCode =Ucase( Trim(Request.Form("txtSearchZipCode"))) & "%"
Set rsStoreResult = Server.CreateObject("ADODB.RECORDSET")
IF Len(trim(session("rep")))>0 THEN
	strSql = "Select * from Customer where Store like '" & strStore & "' AND Caddress3 like '" & strCity & "' AND Caddress4 like '" & strState & "' AND Caddress5 like '" & strZipCode & "' And Account='" & Session("customerID") & "' Order By Type"
ELSE
	strSql = "Select * from Customer where Store like '" & strStore & "' AND Caddress3 like '" & strCity & "' AND Caddress4 like '" & strState & "' AND Caddress5 like '" & strZipCode & "' And Account='" & Session("ID") & "' Order By Type"
END IF
rsStoreResult.Open strSql,Conn
%>
<BODY leftmargin="0" topmargin="0" bgColor=#aecae6  background="Tile1.gif">

<BR>
<%
IF rsStoreResult.EOF AND rsStoreResult.bof  THEN
	Response.Write("No Records Found.")
ELSE
	rsStoreResult.MoveFirst 
%>

<%'---------------------------------------------------------------------------------
  '								Print Results Part					
  '---------------------------------------------------------------------------------
%>


<table align=center border="1" width="800" bgcolor="#6495D0" bordercolor="#AECAE6">
<TR>
	<TD width=25%><Font size="2" face="Arial" color="#000080"><Strong>Store</Strong></Font>
	</TD>
	<TD Width=50%><Font size="2" face="Arial" color="#000080"><Strong>Store Name</Strong></Font>
	</TD>
	<TD Width=25%><Font size="2" face="Arial" color="#000080"><Strong>City</Strong></Font>
	</TD>
	<TD Width=25%><Font size="2" face="Arial" color="#000080"><Strong>Store</Strong></Font>
	</TD>
	<TD Width=25%><Font size="2" face="Arial" color="#000080"><Strong>Zip Code</Strong></Font>
	</TD>
</TR>
<%
Do While Not rsStoreResult.EOF 
strTemp  = "<TR>"
Response.Write(strTemp)

strTemp  = "<TD width=""25%""><Font face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)

' see if the store is the main or not
IF Trim(rsStoreResult("Type"))= "M" THEN
	strTemp = "<a href=""ReturnResoulveStore.asp?Store=MAIN"" >Main</a>"
ELSE
	strTemp = "<a href=""ReturnResoulveStore.asp?Store=" & rsStoreResult("Store") & """>" & rsStoreResult("Store") & "</a>"
END IF
Response.Write(strTemp)

strTemp  = "<TD width=""25%""><Font size=""2"" face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
Response.Write(rsStoreResult("Stname")) 

strTemp  = "<TD width=""25%""><Font size=""2"" face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
Response.Write(rsStoreResult("Caddress3"))

strTemp  = "<TD width=""25%""><Font size=""2"" face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
Response.Write(rsStoreResult("Caddress4")) 

strTemp  = "<TD width=""25%""><Font size=""2"" face=""Arial"" color=""#ffffff"">"
Response.Write(strTemp)
Response.Write(rsStoreResult("Caddress5"))  

rsStoreResult.MoveNext 
Loop
END IF
%>

</table>


<P>&nbsp;</P>

</BODY>
</HTML>
