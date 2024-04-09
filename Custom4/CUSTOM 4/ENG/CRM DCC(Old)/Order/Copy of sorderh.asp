<%@ Language=VBScript %>
<%
Response.Buffer=True
Response.Expires=-1
%>

<%

If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
	parent.location.href="../login.asp"
	</script>
<%End if
Session("getstyle")=""
Session("LongDesc") = ""
Session("ShortDesc") = ""
Session("Price") = ""
Session("Disc") = ""
Session("Comm") = ""
Session("Grp") = ""
session("Discount") = ""
Session("DefTax")  = ""
session("Tax") = ""
Session("Taxable")   = ""

Session("text1") = ""
Session("text2") = ""
Session("text3") = ""
Session("text4") = ""
Session("text5") = ""
Session("text6") = ""
Session("text7") = ""
Session("text8") = ""
Session("ConnectionString") = Application("DataConnectionString")'"dsn=CRM;uid=aria;pwd=aria"
Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open Application("DataConnectionString")
Set cnnFox = server.CreateObject ("ADODB.connection")
cnnFox.open Application("SystemConnectionString")
function getCodesFromFile(CodeName)
	Set objFileSystem = Nothing
	Dim objFile
	Set objFile = Server.CreateObject("Scripting.FileSystemObject")
	strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))
	If Right(strAppPath,1) = "\" Then
		strFilePath = "admin\crmsetup\setup\setup.txt"
	Else
		strFilePath = "\admin\crmsetup\setup\setup.txt"
										
	End If
	Set objTxtFile = objFile.OpenTextFile(strAppPath & strFilePath,1)
	Dim strLine

	strFile = objTxtFile.ReadAll
	
	Dim strArSetups
	strArSetups = Split(strFile," AND ")

	Dim strKey
	Dim strValue
	For intLoop = 0 To UBound(strArSetups)
	strArKeyValue = Split(strArSetups(intLoop) , "=" )
		if Trim(strArKeyValue(0)) = Trim(CodeName) then
			getCodesFromFile = strArKeyValue(1)
		end if 
	Next

	
	
end function
function getCodes(CodeName,CodeNo) 
	'set Codesconn = server.CreateObject("adodb.connection") 
	'Codesconn.Open Application("DataConnectionString")
	'Response.Write "CodeNo== " & CodeNo
	if Trim(CodeNo) = "" then
		strsql = "select Cdiscrep , Ccode_no from Codes where cdefcode+crltfield+cfld_name = 'NN" & CodeName & "' and Cdiscrep <> ''"
	else
		strsql = "select Cdiscrep , Ccode_no from Codes where cdefcode+ccode_no+crltfield+cfld_name = 'N" & CodeNo & Space(6 - len(CodeNo)) & "N" & CodeName & "' and Cdiscrep <> ''"
	end if 
	set rsCodes = server.CreateObject("adodb.recordset")
'	Response.Write strsql
	rsCodes.Open strsql , Conn,2, 4
	set getCodes = rsCodes
end function 
'WAL_ function to get def code
function getDefCode(CodeName) 
dim rsDef
set rsDef = server.CreateObject ("ADODB.RECORDSET")
rsDef.Open "Select Ccode_no from Codes where cdefcode+crltfield+cfld_name like 'DN" & CodeName & "'", Conn
if not rsDef.EOF then
	getDefCode = trim(rsDef("ccode_no"))
else
	getDefCode = ""
end if
end function


IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
	strSalesRep = Ucase(Session("RSCust").fields("salesrep"))
END IF
	
IF Len(Trim(Session("rep")))>0 Then
 
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
	strSalesRep = Ucase(Trim(Session("rep")))
End IF
if not Session("rsCust").eof  then
	if Session("rsCust").fields("Status")="P" then
		Response.Redirect("../Common/Msgs.asp")
	end if
end if
'WAL_ get different option for ship address[start]
set Session("ShipAdd") = server.CreateObject ("ADODB.Recordset")
set Session("StoreAdd") = server.CreateObject ("ADODB.Recordset")
set Session("SalesAdd") = server.CreateObject ("ADODB.Recordset")
'Response.Write session("store")
'
'1 - get salesrep address
if trim(strSalesRep) <> "" then
	Session("SalesAdd").open "Select * from salesrep where repcode='" & trim(strSalesRep) & "'",Conn,1,3
end if
'2 - get ship store address if exists
'check if there or store or not 
if trim(Session("Store")) <> "" then
	Session("StoreAdd").open "Select * from Customer where type+account+store like 'S" & custid & Session("Store") & "%'" ,Conn,1,3
end if

if trim(Request.QueryString ("val")) <> "" then
	Session("Type") = Request.QueryString ("val")
	'check if the address is not sales rep
	if trim(Request.QueryString ("val")) = "S" or trim(Request.QueryString ("val")) = "A" then
		Session("ShipChg") = true
	end if
end if
'Response.Write "<font size=2>TT"&Session("Type")
'fill display address
if Request("selShipAdd") = "M" or Session("Type") = "M" then
	Session("Add1") = trim(Session("RSCust")("btName"))
	Session("Add2") = trim(Session("RSCust")("caddress12"))
	Session("Add3") = trim(Session("RSCust")("caddress22"))
	Session("Add41") = trim(Session("RSCust")("caddress32"))
	Session("Add42") = trim(Session("RSCust")("caddress42"))
	Session("Add43") = trim(Session("RSCust")("caddress52"))
	Session("Add5") = trim(Session("RSCust")("caddress62"))
	Session("Type") = "M" 
	Session("AddChg") = true
elseif Request("selShipAdd") = "S" or Session("Type") = "S" then'or Request("selShipAdd") = "A"then
	if trim(Session("Store")) = "" then
		Session("Add1") = trim(Session("RSCust")("btName"))
		Session("Add2") = trim(Session("RSCust")("caddress1"))
		Session("Add3") = trim(Session("RSCust")("caddress2"))
		Session("Add41") = trim(Session("RSCust")("caddress3"))
		Session("Add42") = trim(Session("RSCust")("caddress4"))
		Session("Add43") = trim(Session("RSCust")("caddress5"))
		Session("Add5") = trim(Session("RSCust")("caddress6"))
	else
		Session("Add1") = trim(Session("StoreAdd")("stName"))
		Session("Add2") = trim(Session("StoreAdd")("caddress1"))
		Session("Add3") = trim(Session("StoreAdd")("caddress2"))
		Session("Add41") = trim(Session("StoreAdd")("caddress3"))
		Session("Add42") = trim(Session("StoreAdd")("caddress4"))
		Session("Add43") = trim(Session("StoreAdd")("caddress5"))
		Session("Add5") = trim(Session("StoreAdd")("caddress6"))
	end if
	Session("Type") = "S"
elseif Request("selShipAdd") = "A" or Session("Type") = "A"then
	Session("Type") = "A"
elseif Request("selShipAdd") = "R" or (Request("selShipAdd") = "")  or Session("Type") = "R" then
	Session("Add1") = trim(Session("SalesAdd")("Name"))
	Session("Add2") = trim(Session("SalesAdd")("caddress1"))
	Session("Add3") = trim(Session("SalesAdd")("caddress2"))
	Session("Add41") = trim(Session("SalesAdd")("caddress3"))
	Session("Add42") = trim(Session("SalesAdd")("caddress4"))
	Session("Add43") = trim(Session("SalesAdd")("caddress5"))
	Session("Add5") = trim(Session("SalesAdd")("caddress6"))
	Session("Type") = "R"
	Session("AddChg") = true
end if
'WAL_ get different option for ship address[end]
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<Title>CRM - Remote Order</Title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
<script LANGUAGE="javascript" src="../checkForm.js">
</script>
</head>
<body onload="enable('<%=Request("selShipAdd")%>');">
<SCRIPT LANGUAGE=javascript>
<!--
function SubmitNext()
{
	document.FORM2.action = 'orderhsave.asp?From=<%=request.querystring("From")%>';
	document.FORM2.submit()
}
function enable(val)
{
	var val;
	if (val == 'A' || '<%=Session("Type")%>' == 'A')
	{
		document.FORM2.txtAdd1.disabled = false;
		document.FORM2.txtAdd2.disabled = false;
		document.FORM2.txtAdd3.disabled = false;
		document.FORM2.txtAdd41.disabled = false;
		document.FORM2.txtAdd42.disabled = false;
		document.FORM2.txtAdd43.disabled = false;
		document.FORM2.txtAdd5.disabled = false;
	}
	else 
	{
		document.FORM2.txtAdd1.disabled = true;
		document.FORM2.txtAdd2.disabled = true;
		document.FORM2.txtAdd3.disabled = true;
		document.FORM2.txtAdd41.disabled = true;
		document.FORM2.txtAdd42.disabled = true;
		document.FORM2.txtAdd43.disabled = true;
		document.FORM2.txtAdd5.disabled = true;
	}
}
//-->
</SCRIPT>

	<%IF strFile = "cust" Then%>
		<SCRIPT LANGUAGE="JavaScript1.2"
		        SRC="../HM_Loader_Cust.js"
		        TYPE='text/javascript'>
		</SCRIPT>
	<p><BR><BR><br></p>

	<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>

	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=session("rscust")("btname")%></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></a></TD>
	<!-- ARD -->
</TR>
</table>
 <%
End IF
'Response.Write "<font size=5>" & Session("Season") & "<hr>"
%>



<Table width=95% align=center height="50" border="1">
<TR>
<TD class="title">Remote Order</TD>
</TR>
</Table>
	<%
	If Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
	If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"ADDORD") <= 0 Then
	%>
	<Table border=0 width=95% align=center>
		<tr>
		<td class="Title"><font color="Yellow">
			You're not authorized to view this page</font>
		</td>
		</tr>
	</Table>

	<%
		Response.End 
	End If
	%>

	<%IF compWork = "Y" Then%>
    

<script language="JavaScript">
function chgAdd(val)
{
	document.FORM2.action='sorderh.asp?From=<%=request.querystring("From")%>&val='+val;
	document.FORM2.submit ();
}
function Search_Stores(objInput)
{
	objInput.SearchStoresFlag.value = "YES";
}
function openwindow(filename) 
{  
	window.open(filename,'','toolbar=no,status=no,scrollbars=no,location=no,resizable=yes,menubar=no,directories=no,top=' + ((window.screen.height-500)/2) + ',left=' + ((window.screen.width-800)/2) + ',width=800,height=500')
}
</script>


<%
Dim RSSeason,RSDSeason ' as ADODB.RECORDSET
Dim RSSetups 'as ADODB.RECORDSET
Dim RSCustStor ' as ADODB.Recoedset
Dim strTemp,ID ' as string
Dim intCompDate ' as integer


Set RSSeason = Server.CreateObject("ADODB.RecordSet")
RSSeason.Open "SELECT * FROM codes where cdefcode+crltfield+cfld_name='NNSEASON'",Conn

Set RSDivision = Server.CreateObject("ADODB.RecordSet")
RSDivision.Open "SELECT * FROM codes where cdefcode+crltfield+cfld_name='NNCDIVISION'",Conn


Set RSDSeason = Server.CreateObject("ADODB.RecordSet")
'RSSeason.Open "SELECT * FROM codes where cdefcode+crltfield+cfld_name='DNSEASON'",Conn
'nek
RSDSeason.Open "SELECT * FROM codes where cdefcode+crltfield+cfld_name='DNSEASON'",Conn

Set RSCustStor = Server.CreateObject("ADODB.RecordSet")
ID = session("ID")
strTemp = "SELECT * FROM customer where Type+account+store like 'S" & ID & "%'"
RSCustStor.Open strTemp,Conn

Set RSSetups = Server.CreateObject("ADODB.RecordSet")
RSSetups.Open "SELECT Mdata_def FROM setups where Capp_id+Cfld_name = 'SOM_COMPDATE'",Conn

%>

<FORM action="../common/OrdFindStore.asp?logintype=O&From=<%=Request("From")%>" id=FORM2 method=post name=FORM2>
<P>
<div align="center">
  <center>
    <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width="100%"><strong>Enter Sales Order Header:</strong></td>
    </tr>
  </table>
<TABLE  border=1  width="95%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<TR>
		<TD width="30%" class="dark_cell"><strong><%=Session("StoreField")%></strong>
		</TD>
		<TD width="70%" class="light_cell">
        <input type="text" name="slctStore" size="10" maxlength="8" value="<%=Session("StoreValue")%>" readonly>
        
			<INPUT id=submit2 name=submit2 type=Submit value="Get <%=Session("StoreField")%>" onclick="Search_Stores(this.form)">
        	<input type="hidden" name="SearchStoresFlag" value="NO">
		</TD>
	</TR>
	<!--wal_127343 add po if setup=yes and login=customer-->
	<%if session("CustPO") = "T" and strFile = "cust" then%>
		<TR>
		  <TD width="30%" class="dark_cell"><strong>P.O.#</strong></TD>
		  <TD width="70%" class="light_cell">
				<INPUT id=txtpo name=txtpo size="10" maxlength="15">
		  </TD>
		</TR>
	<%end if%>
  <%' Season Start From Here
 		set connSQL= server.CreateObject ("ADODB.Connection")
		'strcon = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source="&Trim(Application("SqlServer"))&";uid="&Trim(Session("SqlUserName"))&";pwd="&Trim(Session("SqlPassWord"))
		strcon = Application("SqlServer")
		connSQL.Open strcon


		If Session("M_STYVIEW") = "P" Then
			'Get packs ID from SQl Server tables
			set rsPacks = Server.CreateObject ("ADODB.Recordset")
			strSQL= "select CustGroup.PackID from CustClassification ,CustGroup  "
			strSQL = strSQL & " where CustClassification.CustGroup=CustGroup.GroupID "
			strSQL = strSQL & " AND CustClassification.CustID='" & Trim(CustID) & "'"
			rsPacks.Open strSQL,connSQL,1,3
			
			'Response.Write "<font size=3>PACK STRSQL ==" & strsql & "</font><br><br>"
			'GET the Pack_id Information.
			IF Not (rsPacks.EOF And rsPacks.BOF) Then
				strsql = "Select PACK_ID, ACCOUNT, SEASON, CDIVISION from spck_hdr where "
				strsql = strsql & " type+account+pack_id = 'P*****" & Trim(rsPacks.Fields("PackID").Value) & "'"
			Else
				strsql = "Select PACK_ID, ACCOUNT, SEASON, CDIVISION from spck_hdr where "
				strsql = strsql & " type+account+pack_id like 'P" & Trim(CustID) & "WEB%" & "'"
			End IF
	'		Response.Write "<font size=3>STRSQL ==" & strsql & "</font>"
			Set RShdr = Server.CreateObject("ADODB.RECORDSET")
			RShdr.Open strSql,Conn,2,4
			'NEK [Start] If there are No Packs For this customer it gets Season and Division
			if RShdr.EOF and RShdr.BOF then
				RShdr.Close 
				strsql = "Select PACK_ID, ACCOUNT, SEASON, CDIVISION from spck_hdr where "
				strsql = strsql & " type+account+pack_id like  'P*****%'" 
				RShdr.Open strSql,Conn,2,4	
			end if 
			'NEK [End]
			IF Not(RShdr.EOF and RShdr.BOF) Then
				'Session("Season") = Trim(rshdr.Fields("season").Value) 'wma multiple seasons
				Session("Division") = Trim(RShdr.Fields("cdivision").Value)
			End If
		Else
		'IF Not(RShdr.EOF and RShdr.BOF) Then
			Seasons = Session("StyleColor")
			SeasonsArr=split(Seasons,",")
			strDefSeason = getDefCode("SEASON")
        If Ubound(SeasonsArr) > 0  then%>
        	<tr>
			<td class="dark_cell"><strong>Season</strong></td>
			<td class="light_cell" >
				<select name="slctSeason">
				<OPTION selected value="ALL">All
			<%for intSeasonLoop = lbound(SeasonsArr) to ubound(SeasonsArr)
				
				set rsSeasons = getCodes("SEASON",SeasonsArr(intSeasonLoop))
				if Trim(SeasonsArr(intSeasonLoop)) = Trim(rsSeasons.Fields("Ccode_no").Value) then %>
					<!--option value="<%=Trim(rsSeasons.Fields("Ccode_no").Value)%>" <%if trim(strDefSeason) = trim(rsSeasons.Fields("Ccode_no").Value) then%>selected<%end if%>><%=Trim(rsSeasons.Fields("Cdiscrep").Value)%></option-->
					<option value="<%=Trim(rsSeasons.Fields("Ccode_no").Value)%>"><%=Trim(rsSeasons.Fields("Cdiscrep").Value)%></option>
				<%end if 
			next%>
				</select>
			   </td>
			</tr>
        <%else 
        	if Trim(UCase(Seasons))= "NONE" or Trim(UCase(Seasons))= ""then%>
        		<tr>
				<td class="dark_cell"><strong>Season</strong></td>
				<td class="light_cell" >
					<select name="slctSeason">
        		<%set rsSeasons = getCodes("SEASON","")
        		do while not rsSeasons.eof %>
        			<option value="<%=Trim(rsSeasons.Fields("Ccode_no").Value)%>" <%if trim(strDefSeason) = trim(rsSeasons.Fields("Ccode_no").Value) then%>selected<%end if%>><%=Trim(rsSeasons.Fields("Cdiscrep").Value)%></option>
        		<%rsSeasons.movenext
        		loop%>
        		</select>
				</td>
				</tr>
        	<%else
        		set rsSeasons = getCodes("SEASON",Trim(UCase(Seasons)))%>
            					
        		<%if not rsSeasons.eof then %>
        			<input type=hidden name="slctSeason" value="<%=Trim(rsSeasons.Fields("Ccode_no").Value)%>">
        			<!--option value="<%=Trim(rsSeasons.Fields("Ccode_no").Value)%>"><%=Trim(rsSeasons.Fields("Cdiscrep").Value)%></option-->
	    		<%end if 
	    	end if 	
	    end if 	%>
        <%Divisions = Session("CatDivision")
        DivisionsArr=split(Divisions,",")
        strDefDiv = getDefCode("CDIVISION")
        %>
            
	    <%if ubound(DivisionsArr)> 0 then%>
			<tr>
			<td class="dark_cell"><strong>Division</strong></td>
			<td class="light_cell" >	
			&nbsp;<select name="slctDivision" colspan=2>
			<!--option value="All">All</option-->
			<%for intDivLoop = lbound(DivisionsArr) to ubound(DivisionsArr)
				set rsDivisions = getCodes("CDIVISION",DivisionsArr(intDivLoop))
			if Trim(DivisionsArr(intDivLoop)) = Trim(rsDivisions.Fields("Ccode_no").Value) then %>
				<option value="<%=Trim(rsDivisions.Fields("Ccode_no").Value)%>" <%if trim(strDefDiv) = trim(rsDivisions.Fields("Ccode_no").Value) then%>selected<%end if%>><%=Trim(rsDivisions.Fields("CDiscrep").Value)%></option>
			<%end if 
		next%>
			</select>
		    </td>
		</tr>
   	<%	else 
           if Trim(UCase(Divisions))= "NONE" then%>
				<tr>
				<td class="dark_cell"><strong>Division</strong></td>
				<td class="light_cell" >	
				<select name="slctDivision" colspan=2>
			
				<%set rsDivisions = getCodes("CDIVISION","")
				do while not rsDivisions.eof %>
					<option value="<%=Trim(rsDivisions.Fields("Ccode_no").Value)%>" <%if trim(strDefDiv) = trim(rsDivisions.Fields("Ccode_no").Value) then%>selected<%end if%>><%=Trim(rsDivisions.Fields("Cdiscrep").Value)%></option>
				<%rsDivisions.movenext
				loop%>
			</select>
		    </td>
		    </tr>
		   <%else	
			  set rsDivisions = getCodes("CDIVISION",Trim(UCase(Divisions)))
			  if not rsDivisions.eof then %>
					<input type=hidden name="slctDivision" value="<%=Trim(rsDivisions.Fields("Ccode_no").Value)%>">
					<!--option value="<%=Trim(rsDivisions.Fields("Ccode_no").Value)%>"><%=Trim(rsDivisions.Fields("Cdiscrep").Value)%></option-->
			<%end if 
		end if 
	end if 	 %>
<%
  End If
  %>
<%if Trim(Request.Form("txtStarted")) = "" then
		strSMon = month(date())
		strSDay = day(date())
		if Len(strSDay) = 1 then
			strSDay = "0" & strSDay
		End if
		if Len(strSMon) = 1 then
			strSMon = "0" & strSMon
		End if
		dFrom = strSMon& "/" & strSDay & "/" & year(date())
  else
		dFrom = Trim(Request.Form("txtStarted"))
  end if
  intCompDate = RSSetups("Mdata_def")
  dTo = date() + intCompDate
  strEMon = month(dTo)
  if Len(strEMon) = 1 then
	strEMon = "0" & strEMon
  End if
  strEDay = day(dTo)
  if Len(strEDay) = 1 then
	strEDay = "0" & strEDay
  End if
  'dTo = formatdatetime(Cdate(dTo & "/1/" & year(date())) - 1, 0)
  if Trim(Request.Form("txtCompleteded")) = "" then					  
		dTo = strEMon & "/" & strEDay & "/" & year(date())
  else
		dTo = Trim(Request.Form("txtCompleteded"))
  end if
%>
   <TR>
    <TD width="30%" class="dark_cell"><strong>Ship via</strong></TD>
    <TD width="70%" class="light_cell">
	<%
	'WAL_add ship via select[start]
	dim rsCode
	set rsCode = server.CreateObject ("ADODB.RECORDSET")
	rsCode.Open "Select Crltd_vlu, Cdiscrep, Ccode_no,CrLtd_nam,crltfield from Codes where cdefcode+crltfield+cfld_name ='NNSHIPVIA' order by cDiscrep", conn

	%>
	<select name="selShip" style="width:255px">
		<%do while not rsCode.EOF%>
			<option value="<%=rsCode("Ccode_no")%>" <%if trim(Session("RSCust")("ShipVia")) = trim(rsCode("Ccode_no")) then%>selected<%end if%>>
			<%=rsCode("cDiscrep")%>
		<%rsCode.MoveNext ()
		  loop
		%>
	</select>
	</TD>
  </TR>
  <TR>
    <TD width="30%" class="dark_cell"><strong>Start Date</strong></TD>
    <TD width="70%" class="light_cell">
    <INPUT id=txtStart name=txtStarted value="<%=dFrom%>" size="10" onFocus="javascript:vDateType='1'" onBlur="DateFormat(this,this.value,event,true,'1')">
      <INPut type=hidden name=txtStart value="<%=dFrom%>">
      </TD>
  </TR>
  <!--TR>
    <TD width="30%" class="dark_cell"><strong>Complete Date</strong></TD>
    <TD width="70%" class="light_cell"-->
  <%'wal_127343 add the 2 notes fileds%>
  <TR>
    <TD width="30%" rowspan=2 valign=top class="dark_cell"><strong>Notes</strong></TD>
    <TD width="70%" class="light_cell">
      <INPUT type=text name=txtNote1 value="<%=session("Note1")%>" size="62" maxlength=30>
   
    </TD>
  </TR>
  <TR>
    <TD width="70%" class="light_cell">
      <INPUT type=text name=txtNote2 value="<%=session("Note2")%>" size="62" maxlength=30>
      
    </TD>
  </TR>
   	<INPut type=hidden name=txtCompleted value="<%=dTo%>">
  
<TR>
	<TD colspan=2  class="dark_cell" valign=center height=25><b><p>Address Information:</p></b></TD>
</TR>
<TR>
<TD colspan=2  class="dark_cell">  
  <%'WAL_add shipping address [start]%>
<table border="1" width="100%" align=center bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
  <TR>
	<TD width=50% class="dark_cell">
		<p><strong>Billing Address</strong></p>
	</TD>
	<TD width=50% colspan=2 class="dark_cell">
		Ship To:
		<select name="selShipAdd" onchange="chgAdd(this.value);">
			<option value="R" <%if Request("selShipAdd") = "R" or Session("Type") = "R"then%>selected<%end if%>>Sales Rep. Address
			<option value="S" <%if Request("selShipAdd") = "S" or Session("Type") = "S"then%>selected<%end if%>>Shipping Address
			<option value="A" <%if Request("selShipAdd") = "A" or Session("Type") = "A"then%>selected<%end if%>>Alternative Shipping Address
			<%if trim(Session("Store")) <> "" then%>
				<option value="M" <%if Request.Form ("selShipAdd") = "M" or Session("Type") = "M"then%>selected<%end if%>>Copy from Main Address
			<%end if%>
		</select>
	</TD>
  </TR>
  <TR>
	<TD class="light_cell">
		<%if trim(Session("Store")) = "" then
			Response.write session("RSCust")("btname")
		  else
			Response.write session("StoreAdd")("stname")
		  end if	
		%>
	</TD>
	<TD  colspan=2 class="light_cell">
		<input type=text name=txtAdd1 size="30"value="<%=trim(Session("Add1"))%>" disabled>
	</TD>
  </TR>
  <TR>
	<TD  class="light_cell">
		<%if trim(Session("Store")) = "" then
			Response.write session("RSCust")("caddress12")
		  else
			Response.write session("StoreAdd")("caddress12")
		  end if	
		%>
	</TD>
	<TD  colspan=2 class="light_cell">
		<input type=text name=txtAdd2 size="30"value="<%=trim(Session("Add2"))%>" disabled>
	
	</TD>
  </TR>
  <TR>
	<TD class="light_cell">
		<%if trim(Session("Store")) = "" then
			Response.write session("RSCust")("caddress22")
		  else
			Response.write session("StoreAdd")("caddress22")
		  end if	
		%>
	</TD>
	<TD colspan=2 class="light_cell">
		<input type=text name=txtAdd3 size="30"value="<%=trim(Session("Add3"))%>" disabled>
		
	</TD>
  </TR>
  <TR>
	<TD class="light_cell">
		<%if trim(Session("Store")) = "" then
			Response.write trim(session("RSCust")("caddress32"))&"," &trim(session("RSCust")("caddress42"))&"," &trim(session("RSCust")("caddress52"))
		  else
			Response.write trim(session("StoreAdd")("caddress32"))&"," &trim(session("StoreAdd")("caddress42"))&"," &trim(session("StoreAdd")("caddress52"))
		  end if	
		%>
		
	</TD>
	<%'get fields length[start]%>
	<%dim rsInt
	  set rsInt = server.CreateObject ("ADODB.Recordset")
	  rsInt.Open "select * from sycint where ccont_code = '" &trim(Session("Add5"))& "'",cnnFox
	  if not rsInt.EOF then
		strLen1 = trim(rsInt("nPart3len"))
		strLen2 = trim(rsInt("nPart4len"))
		strLen3 = trim(rsInt("nPart5len"))
	  end if
	%>
	<%'get fields length[end]%>
	<TD colspan=2 class="light_cell">
		<input type=text name="txtAdd41" size="<%=strLen1%>"maxlength="<%=strLen1%>"value="<%=trim(Session("Add41"))%>" disabled>
		<input type=text name="txtAdd42" size="<%=strLen2%>"maxlength="<%=strLen2%>"value="<%=trim(Session("Add42"))%>" disabled>
		<input type=text name="txtAdd43" size="<%=strLen3%>"maxlength="<%=strLen3%>"value="<%=trim(Session("Add43"))%>" disabled>
	</TD>
  </TR>
   <TR>
	<TD valign=top class="light_cell">
		<%if trim(Session("Store")) = "" then
			Response.write session("RSCust")("caddress62")
		  else
			Response.write session("StoreAdd")("caddress62")
		  end if	
		%>
	</TD>
	<TD valign=top class="light_cell">
	
		<input type=text name=txtAdd5 size="30"value="<%=trim(Session("Add5"))%>" disabled>
	</TD>

  </TR>
</Table>
</td>
</tr>
</table>
<%'WAL_add shipping address [end]%>
  </center>
</div>
  
<Table width=95%>
<tr>
	<td width="50%">
	</td>
	<td width="50%"  align="right">
		<%if Request.QueryString ("From") = "Ord" then%>
			<INPUT name=btnCharge type="button" value="View Order Charges" onclick="javascript:openwindow('ordCharge.asp');">
			<INPUT name=btnback type="button" value="Back to Order Lines" onclick="javascript:window.history.back ();">
		<%else
			Session("ordAmount") = ""
			Session("ordQty")= ""
		end if%>
		<INPUT id=submit1 name=submit1 type=submit <%if Request.QueryString ("From") = "Ord" then%>value="Save" <%else%>value="Next"<%end if%>onclick="return SubmitNext()">
	</TD>
</TR>
</TABLE>

</FORM>
<P></P>
<%
RSCustStor.Close
RSSetups.Close

Conn.Close

Set RSJobsFil = Nothing
Set RSCustStor = Nothing
Set RSSetups = Nothing
%>

<P>&nbsp;</P>
<%End if

Session("Division") = ""
'Response.Write "<font size=5>" & Session("Season") & "<hr>"
'Session("Season") = ""  'wma multiple seasons
%>
</BODY>
</HTML>