<%@ Language=VBScript %>
<%
Response.Buffer=True
Response.Expires=-1
If Trim(Session("ID")) = "" And Session("rep")= "" Then
'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%End if
Session("Type") = "R"
if Request("slctDivision")<> "" then
	Session("Division")=Trim(Request("slctDivision"))
else
	Session("Division")=Trim(Session("CatDivision"))
end if 
if Request("slctSeason")<> "" then
	Session("Season")=Trim(Request("slctSeason"))
else
	Session("Season")=Trim(Session("StyleColor"))
end if
'Response.Write "<font size=3>Season = " & Session("Season") & "   Division = " & Session("Division")&"</font>"
'Response.End 

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF
%>
<HTML>
<Head>
<Title>CRM - Find Store</Title>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
</head>
<body>

<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><BR><BR><BR></p>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><BR></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected <%=session("CustField")%> is<%=Session("customerid")%> - <%=Session("rsCust")("btname").value%></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>
 <%End IF%>
<TAble width=95% align=center border=1 height="50">
<TR>
<TD class="title">Get <%=Session("StoreField")%></TD>
</TR>
</Table>

<%
IF compWork = "Y" Then

	Session("ConnectionString") = Application("DataConnectionString")
	Set Conn = Server.CreateObject("ADODB.Connection")
	Conn.Open(Session("ConnectionString"))
	'wal_127795 check the list of stores and add condition for the user's stores only[start]
	dim strStrs
	strStrs = ""
	if instr(Session("Store"),",") > 0 then
		dim arrStr
		arrStr = split (Session("Store"),",")
		for intCt=0 to Ubound(arrStr)
			strStrs = strStrs & "'" & arrStr(intCt) & "',"
		next
		strStrs = "store, " & left(strStrs,len(strStrs)-1)
	end if
	'wal_127795 check the list of stores and add condition for the user's stores only[end]
	If  trim(request("txtSearchStore"))<>""  Then

			Session("strstore")=Ucase(trim(request("txtSearchStore")))& "%"
		
			Set rsStoreResult = Server.CreateObject("ADODB.RECORDSET")
			'If trim(Session("ID"))<>"" Then
				strSql= "Select * from Customer where "
				strSql = strSql & "type+account+Upper(store) like 'S" & custid & Session("strstore") & "%' "
				if strStrs <> "" then
					strSql = strSql & " and inlist (" & strStrs &")"
				end if
				strSql = strSql & " Order By store"
			'Elseif trim(Session("rep"))<>"" Then
			'	strSql= "Select * from Customer where account='"&Session("customerid")&"' and Store like '" & Session("strstore") & "'"
			'End If

	Elseif trim(request("slctStore"))<>"" Then

			Session("strstore")=Ucase(trim(request("slctStore")))& "%"
			Set rsStoreResult = Server.CreateObject("ADODB.RECORDSET")
		'	If trim(Session("ID"))<>"" Then
				strSql= "Select * from Customer where  type+account+Upper(store) like 'S" & custid & Session("strstore") & "%' or type+account+Upper(store) like 'M" & custid & Session("strstore") & "%'"
				if strStrs <> "" then
					strSql = strSql & " and inlist(" & strStrs &")"
				end if
				strSql = strSql & " Order By store"
			'ElseIf trim(Session("rep"))<>"" Then
		'		strSql= "Select * from Customer where account='"&Session("customerid")&"' and Store like '" & Session("strstore") & "'"
		'	End If

	Else
			'IF Trim(Request.Form("txtSearchStore")) <> "" Then

				Session("strStore") =Ucase( Trim(Request.Form("txtSearchStore"))) & "%"
			'End IF
	
			'IF Trim(Request.Form("txtSearchCity")) <> "" Then
				Session("strCity")	 =Ucase( Trim(Request.Form("txtSearchCity"))) & "%"
			'End IF
	
			'IF Trim(Request.Form("txtSearchState")) <> "" Then
				Session("strState") =Ucase( Trim(Request.Form("txtSearchState"))) & "%"
			'End IF
	
			'IF Trim(Request.Form("txtSearchZipCode")) <> "" Then
				Session("strZipCode") =Ucase( Trim(Request.Form("txtSearchZipCode"))) & "%"
			'End IF

			Set rsStoreResult = Server.CreateObject("ADODB.RECORDSET")
			strSql = "Select * from Customer where "
			strSql = strSql & " (type+account+Upper(Store) like 'S" & custid & Session("strstore") & "%' or type+account+Upper(Store) like 'M" & custid & Session("strstore") & "%')"
			strSql = strSql & " AND Caddress3 like '" & Session("strCity") & "' AND Caddress4 like '" & Session("strState") & "' AND Caddress5 like '" & Session("strZipCode") & "' "
			if strStrs <> "" then
				strSql = strSql & " and inlist(" & strStrs &")"
			end if
			strSql = strSql & " Order By store"
End IF
	'Set records per page
	Const NumPerPage = 25
	
	'Retrive what page we are Currently on
	Dim CurrPage
	IF Request.QueryString("CurrPage") = "" Then
		CurrPage = 1
	Else
		CurrPage = Request.QueryString("CurrPage")
	End IF
	
	'Set Cursor Location
	rsStoreResult.CursorLocation = 2
	
	'Set Cachsize to no. of records/page
	rsStoreResult.CacheSize = NumPerPage
	'Response.Write "<font size=3>" &  trim(request("txtSearchStore")) 
	'Response.End 
	rsStoreResult.Open strSql,Conn
	If not rsStoreResult.eof and not rsStoreResult.bof Then
			rsStoreResult.PageSize = NumPerPage
			TotalPages = rsStoreResult.PageCount 
			rsStoreResult.AbsolutePage = CurrPage
	
%>

<form action="Ordfindstore.asp?From=<%=request("From")%>&logintype=<%=Request("logintype")%>" method="post" id=form1 name=form1>
	<p><strong> </p>
    
	<div align="center">
      <center>
    
	<table bordercolor="#111111" border="1" width="95%" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
		<TR>
			<td Align="left" valign="center" width="20%" class="dark_cell">
				
				<strong><%=Session("StoreField")%></strong>
			</TD>
			
			<td Align="left" valign="center" width="20%" class="dark_cell">
				
				<strong><%=trim(Session("CityField"))%></strong>
			</TD>
			
			<td Align="left" valign="center" width="20%" class="dark_cell">
				
				<strong><%=trim(Session("StateField"))%></strong>
			</TD>
			
			<td Align="left" valign="center" width="20%" class="dark_cell">
				
				<strong><%=trim(Session("ZipField"))%></strong>
			</TD>
			
		</TR>
		<tr>
			<td Align="left" valign="center" class="dark_cell" >
				<INPUT type="text" id=text1 name="txtSearchStore" value="<%=Request.Form("txtSearchStore")%>" size="20">
			</td>
			
			<td Align="left" valign="center" class="dark_cell" >
				<INPUT type="text" id=text2 name="txtSearchCity" value="<%=Request.Form("txtSearchCity")%>" size="20">
			</td>
			
			<td Align="left" valign="center" class="dark_cell" >
				<INPUT type="text" id=text3 name="txtSearchState" value="<%=Request.Form("txtSearchState")%>" size="20">
			</td>
			
			<td Align="left" valign="center" class="dark_cell" >
				<INPUT type="text" id=text4 name="txtSearchZipCode" value="<%=Request.Form("txtSearchZipCode")%>" size="20">
			</td>
			
		</tr>
		<tr>
			<td Align="right" class="dark_cell" colspan=4>
				<input type="submit" name="B2" value="Search">
				<input type="reset" name="B3" value="Reset">
			</td>
     
		</tr>
    </table>
      </center>
    </div>
</form>


		

<%'---------------------------------------------------------------------------------
  '								Print Results Part					
  '---------------------------------------------------------------------------------
%>
<div align="center">
  <center>

<table border="1" width="95%" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<TR>
	<TD class="dark_cell" ><Strong><%=Session("StoreField")%></Strong>
	</TD>
	<TD class="dark_cell" ><strong><%=Session("StoreField")%> Name</strong>
	</TD>
	<TD class="dark_cell" ><Strong><%=trim(Session("CityField"))%></Strong>
	</TD>
	<TD class="dark_cell" ><Strong><%=trim(Session("StateField"))%></Strong>
	</TD>

	<TD class="dark_cell" ><Strong><%=trim(Session("ZipField"))%></Strong>
	</TD>
</TR>
<%Else
response.write("<TR><TD>")
	Session("storevalue")=""
	response.write "<b><Font size=2>No "& Session("StoreField") &"(s) found.</font></b><br>"
	If request("logintype")="R" And trim(Session("ID"))<>"" Then
		response.write "<a href=../RA/returnadd.asp><Font size=2>back</Font></a>"
	End If
	If request("logintype")="R" And trim(Session("rep"))<>"" Then
		response.write "<a href=../RA/reprequestra.asp><Font size=2>back</Font></a>"
	End If
	If request("logintype")="O" Then
		response.write "<a href=../order/sorderh.asp?From="& request("From") &"><Font size=2>back</Font></a>"
	End If
	response.write("&nbsp;</TD></TR>")
End If
%> 
<%
	Dim Count 
	Count =0
	If not rsStoreResult.eof and not rsStoreResult.bof then
				If request("logintype")="R" Then

					Do While Not rsStoreResult.EOF And Count < rsStoreResult.PageSize 
					strTemp  = "<TR>"
					Response.Write(strTemp)

					strTemp  = "<TD class=""light_cell"">"
					Response.Write(strTemp)

					' see if the store is the main or not
					if Trim(rsStoreResult("Type"))= "M" then
						strTemp = "<a href=""../RA/returnResoulveStore.asp?Store=MAIN"" >Main</a>"
					else
						strTemp = "<a href=""../RA/returnResoulveStore.asp?Store=" & rsStoreResult("Store") & """>" & rsStoreResult("Store") & "</a>"
					end if
					Response.Write(strTemp)

					strTemp  = "<TD class=""light_cell"">"
					Response.Write(strTemp)
					Response.Write(rsStoreResult("Stname")) 

					strTemp  = "<TD class=""light_cell"">"
					Response.Write(strTemp)
					Response.Write(rsStoreResult("Caddress3"))

					strTemp  = "<TD class=""light_cell"">"
					Response.Write(strTemp)
					Response.Write(rsStoreResult("Caddress4")) 

					strTemp  = "<TD class=""light_cell"">"
					Response.Write(strTemp)
					Response.Write(rsStoreResult("Caddress5"))  

					Count = Count + 1
					rsStoreResult.MoveNext 
					Loop
					Response.Write("</Table><center><br><font face=Arial size=2>")
					IF  CurrPage > 1 Then
						Response.Write("<A href=""ordfindstore.asp?From="& request("From") &"&logintype=R&CurrPage=" & CurrPage - 1 & """>Prev. </a>" )
					End IF

					IF Cint(CurrPage) <> Cint(TotalPages) Then
						Response.Write("<A href=""ordfindstore.asp?From="& request("From") &"&logintype=R&CurrPage=" & CurrPage + 1 & """>Next</a>")
					End IF
					Response.Write("</center></font>")
				End IF

				IF Request("logintype")="O" Then

					Do While Not rsStoreResult.EOF And Count < rsStoreResult.PageSize 
					strTemp  = "<TR>"
					Response.Write(strTemp)

					strTemp  = "<TD class=""light_cell"">"
					Response.Write(strTemp)

					' see if the store is the main or not
					if Trim(rsStoreResult("Type"))= "M" then
						strTemp = "<a href=""../Order/OrdResoulveStore.asp?From="& request("From") &"&Store=MAIN"" >Main</a>"
					else
						strTemp = "<a href=""../order/OrdResoulveStore.asp?From="& request("From") &"&Store=" & rsStoreResult("Store") & """>" & rsStoreResult("Store") & "</a>"
					end if
					Response.Write(strTemp)

					strTemp  = "<TD class=""light_cell"">"
					Response.Write(strTemp)
					Response.Write(rsStoreResult("Stname")) 

					strTemp  = "<TD class=""light_cell"">"
					Response.Write(strTemp)
					Response.Write(rsStoreResult("Caddress3"))

					strTemp  = "<TD class=""light_cell"">"
					Response.Write(strTemp)
					Response.Write(rsStoreResult("Caddress4")) 

					strTemp  = "<TD class=""light_cell"">"
					Response.Write(strTemp)
					Response.Write(rsStoreResult("Caddress5"))  

					Count = Count + 1
					rsStoreResult.MoveNext 
					Loop
					Response.Write("<td></tr></Table><Center><br><font face=Arial size=2>")
					IF  CurrPage > 1 Then
						Response.Write("<A href=""ordfindstore.asp?From="& request("From") &"&logintype=O&CurrPage=" & CurrPage - 1 & """>Prev. </a>")
					End IF

					IF Cint(CurrPage) <> Cint(TotalPages) Then
						Response.Write("<A href=""ordfindstore.asp?From="& request("From") &"&logintype=O&CurrPage=" & CurrPage + 1 & """>Next</a>")
					End IF
					Response.Write("</center></font>")
				END IF

			End IF


End If

%>

    <td width="181">

<BR>

</table>

    

  </center>
</div>

    

</body>