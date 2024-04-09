<%@ Language=VBScript %>
<%response.expires=-1
'Session("ID") = "JCP10"
'CurComp = "99"
CurComp = session("CompanyID")
if Trim(Session("rep")) = "" and trim(Session("ID"))= "" then
'	Response.redirect "../default.asp"%>
<script language="javascript">
	parent.location.href ="../default.asp"
</script>
<%end if

if trim (Session("rep")) = "" then
	CurCust = Session("ID")
	strFile="cust"
	
else 
	CurCust = Session("CustomerID")
	strFile="rep"
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF	
end if
if not Session("rsCust").eof  then
	if Session("rsCust").fields("Status")="P" then
		Response.Redirect("../Common/Msgs.asp")
	end if
end if

memfilename = Application("DataPath") & "Arstmsg.mem"
memfilename = cstr(memfilename)
'Response.Write "<font size=3><Br><br><br> emfilename = " & memfilename & "</font>"
%>
<html>
<head>
<title>CRM - Customer Statement</title>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/Invoice.css">
</head>
<body>
<SCRIPT LANGUAGE=javascript>

function GoReport()
{
	var flg = navigator.appVersion  ;
	
	if(flg.indexOf("MSIE") != -1)
		{
			//alert("Going to redirect");
			document.location.href = "CustStatement/CustStatRep.asp?viewer=ActiveX";
		}
	else
		{
			document.location.href = "CustStatement/CustStatRep.asp?viewer=Java-Plug-in";
		}
}
//
</SCRIPT>
<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>
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
	<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></P>
	</TD>
	<TD align=right><a href="repcust.asp" style="text-decoration: none">Get <%=session("CustField")%></a></TD>
	<!-- ARD -->
</TR>
</table>
<%End IF%>


<%

Set Conn = Server.CreateObject("ADODB.Connection")

ConnString = Application("DataConnectionString")
Conn.Open ConnString
Set SysConn = Server.CreateObject("ADODB.Connection")
ConnString = Application("SystemConnectionString")
SysConn.Open ConnString

'Response.Write("<font size=3>"&Application("DataPath")& "</font>")
'---------------------------------------------------------------------------------------
' build header Query
'---------------------------------------------------------------------------------------
set session("CustStatHdr") = server.CreateObject("ADODB.RecordSet")
strSQL = "Select Account, Caddress1, Caddress2, Caddress3, Caddress4, Caddress5, Caddress6, Caddress1 AS SoldToAd1, Caddress2 AS SoldToAd2, Caddress3 AS SoldToAd3, Caddress4 AS SoldToAd4, Caddress5 AS SoldToAd5, Caddress6 AS SoldToAd6, SPACE(120) AS Advertise1, SPACE(120) AS Advertise2, Space(120) AS Advertise3, SPACE(120) AS Advertise4, SPACE(120) AS Advertise5, SPACE(120) AS OrderDue1, SPACE(120) AS OrderDue2, SPACE(120) AS OrderDue3, SPACE(20) AS PostDChecks, Cfaccode, Age120, Age90, Age60, Age30 from Customer "
'WMA Delete NtBal Condition [Start]
'strSQL = strSQL & " where type+Account+store like'M" & CurCust & "%'And Netbal>=0"
strSQL = strSQL & " where type+Account+store like'M" & CurCust & "%'"
'WMA Delete NtBal Condition [End]
'Response.Write("<font size=3>"& strSQL&"<BR>")
Session("CustStatHdr").open strSQL, Conn ,2 , 4
' if netbalance <0 then no report ..
if Session("CustStatHdr").BOF and Session("CustStatHdr").EOF then%>
	<b><font size=3>Your net balance < 0, you can't print the report</font></b>
<%else
		'Change the Advertise fields to the required data .. 

		set objMem = Server.CreateObject("DBREadMem.ReadMem")
		
		Ad1 = objMem.LoadMemFile ("MADL1", cstr(memfilename))
		Ad2 = objMem.LoadMemFile ("MADL1", cstr(memfilename))
		Ad3 = objMem.LoadMemFile ("MADL1", cstr(memfilename))
		Ad4 = objMem.LoadMemFile ("MADL1", cstr(memfilename))
		Ad5 = objMem.LoadMemFile ("MADL1", cstr(memfilename))
		' which lines in the warnings to choose .. 
		if cdbl(Session("CustStatHdr").Fields("Age120")) > 0.0 then
			WarningVariable1 = "M120L1"
			WarningVariable2 = "M120L2"
			WarningVariable3 = "M120L3"
		else
			if cdbl(Session("CustStatHdr").Fields("Age90")) > 0 then
				WarningVariable1 = "M90L1"
				WarningVariable2 = "M90L2"
				WarningVariable3 = "M90L3"
			else
				if cdbl(Session("CustStatHdr").Fields("Age60")) > 0 then
					WarningVariable1 = "M60L1"
					WarningVariable2 = "M60L2"
					WarningVariable3 = "M60L3"
				else 
					if cdbl(Session("CustStatHdr").Fields("Age30")) > 0 then
						WarningVariable1 = "M30L1"
						WarningVariable2 = "M30L2"
						WarningVariable3 = "M30L3"
					else
						nowarinings = true
					end if
				end if
			end if
		end if
		
		if nowarinings then
			OD1 = ""
			OD2 = ""
			OD3 = ""
		else
			OD1 = objMem.LoadMemFile (cstr(WarningVariable1), cstr(memfilename))
			OD2 = objMem.LoadMemFile (cstr(WarningVariable2), cstr(memfilename))
			OD3 = objMem.LoadMemFile (cstr(WarningVariable3), cstr(memfilename))
		end if

		Session("CustStatHdr").Fields("Advertise1") = cstr(Ad1) 
		Session("CustStatHdr").Fields("Advertise2") = cstr(Ad2) 
		Session("CustStatHdr").Fields("Advertise3") = Ad3 
		Session("CustStatHdr").Fields("Advertise4") = Ad4 
		Session("CustStatHdr").Fields("Advertise5") = Ad5 
		Session("CustStatHdr").Fields("OrderDue1") = OD1 
		Session("CustStatHdr").Fields("OrderDue2") = OD2 
		Session("CustStatHdr").Fields("OrderDue3") = OD3 

		'Change the PostDChecks fields to get the value from the PostDchq file
		set rsPostDChecks = Server.CreateObject ("ADODB.RecordSet")
		strSQL = "SELECT SUM(Amount) AS PostDChecks FROM Postdchq WHERE Account ='" & CurCust & "'"
		'Response.Write "<font size=3>"& strSQL
		rsPostDChecks.Open strSQL , Conn
		if not rsPostDChecks.BOF and not rsPostDChecks.EOF then
			Session("CustStatHdr").Fields("PostDChecks") = Cstr(rsPostDChecks("PostDChecks"))
		else
			Session("CustStatHdr").Fields("PostDChecks") = "N/A"
		end if

			set rsCompanyAd = Server.CreateObject("ADODB.RecordSet")
			strSQL = "SELECT Caddress1, Caddress2, Caddress3, Caddress4, Caddress5, Caddress6 FROM SycComp Where Ccomp_id='" & CurComp & "'"
			'Response.Write "<font size=3>"& strSQL
			rsCompanyAd.Open strSQL , SysConn
			if not rsCompanyAd.BOF and not rsCompanyAd.EOF then
				Session("CustStatHdr").Fields("Caddress1") = rsCompanyAd.Fields("Caddress1")
				Session("CustStatHdr").Fields("Caddress2") = rsCompanyAd.Fields("Caddress2")
				Session("CustStatHdr").Fields("Caddress3") = rsCompanyAd.Fields("Caddress3")
				Session("CustStatHdr").Fields("Caddress4") = rsCompanyAd.Fields("Caddress4")
				Session("CustStatHdr").Fields("Caddress5") = rsCompanyAd.Fields("Caddress5")
				Session("CustStatHdr").Fields("Caddress6") = rsCompanyAd.Fields("Caddress6")
			else
				Response.Write ("<font size=3>"&"No Company Exist - " &CurComp& "</font>")
			end if 
			set rsCompanyAd = nothing
			Session("CustStatHdr").Fields("Cfaccode")=""
		session("CustStatHdr").Update()	
			

		'Const NumPerPage = 11
		Const NumPerPage = 100

		Dim CurPage
		If Request.QueryString("CurPage") = "" then
		     CurPage = 1 'We're on the first page
		Else
		     CurPage = Request.QueryString("CurPage")
		End If

		set session("CustStatDet") = server.CreateObject("ADODB.RecordSet")
	    'session("CustStatDet").CursorLocation = 2
	    session("CustStatDet").CacheSize = NumPerPage

		'WMA Amount and Due Date Correct [Start]
'		strSQL = "SELECT Debit.Account,Debit.Duedate, Debit.Trandate, Debit.Tran, Debit.Desc, Debit.Reference,Debit.Amount AS Amount , Debit.Amount AS BALANCE, Debit.Amount AS GroupNo  FROM Debit WHERE Debit.Trandate<={" & date() & "} AND Account='" & CurCust & "' UNION ALL SELECT Credit.Account,Credit.Dpostdate, Credit.Trandate,Credit.Tran,Credit.Desc,Credit.Reference,Credit.Amount AS Amount , Credit.Amount AS BALANCE, Credit.Amount AS GroupNo  FROM Credit where Credit.Trandate<={" & date() & "} AND Account='" & CurCust & "' ORDER BY 3"
		strSQL ="SELECT debit.trantype,Debit.Account,Debit.Duedate, Debit.Trandate, Debit.Tran, Debit.Desc, Debit.Reference,Debit.Amount AS Amount , Debit.Amount AS BALANCE, Debit.Amount AS GroupNo ,iif(debit.trantype='1' or debit.trantype='2' or debit.trantype='3','Debit','Credit') as DC "
		strSQL = strSQL + "FROM Debit WHERE Debit.Trandate<={" & date() & "} AND Account='" & CurCust & "' "
		strSQL = strSQL + "UNION ALL SELECT Credit.trantype,Credit.Account,Credit.Dpostdate, Credit.Trandate,Credit.Tran,Credit.Desc,Credit.Reference,Credit.Amount AS Amount , Credit.Amount AS BALANCE, Credit.Amount AS GroupNo,iif(credit.trantype='0' or credit.trantype='4' or credit.trantype='5' or credit.trantype='6','Credit','Debit') as DC "
		strSQL = strSQL + "FROM Credit where Credit.Trandate<={" & date() & "} AND Account='" & CurCust & "'  ORDER BY 3"
		'strSQL = strSQL + "FROM Credit where Credit.Trandate<={" & date() & "} AND Account='" & CurCust & "' "
		'strSQL = strSQL + "UNION ALL SELECT Arhist.trantype,Arhist.Account,Arhist.Dpostdate, Arhist.Trandate,Arhist.Tran,Arhist.Desc,Arhist.Reference,Arhist.Amount AS Amount , Arhist.Amount AS BALANCE, Arhist.Amount AS GroupNo,iif(Arhist.trantype='0' or Arhist.trantype='4' or Arhist.trantype='5' or Arhist.trantype='6','Credit','Debit') as DC "
		'strSQL = strSQL + "FROM Arhist where Arhist.Trandate<={" & date() & "} AND Account='" & CurCust & "' ORDER BY 3"
		
		'strSQL = "SELECT Debit.Account,Debit.Duedate, Debit.Trandate, Debit.Tran, Debit.Desc, Debit.Reference,Debit.Amount AS Amount , Debit.Amount AS BALANCE, Debit.Amount AS GroupNo ,ALIAS("Debit") as DC  FROM Debit WHERE Debit.Trandate<={" & date() & "} AND Account='" & CurCust & "' UNION ALL SELECT Credit.Account,Credit.Dpostdate, Credit.Trandate,Credit.Tran,Credit.Desc,Credit.Reference,Credit.Amount AS Amount , Credit.Amount AS BALANCE, Credit.Amount AS GroupNo , ALIAS("Credit") as DC  FROM Credit where Credit.Trandate<={" & date() & "} AND Account='" & CurCust & "' ORDER BY 3"
		'WMA Amount and Due Date Correct [End]
		
		'Response.Write "<br><Font Size=3>" & (strSQL)
		session("CustStatDet").Open strSQL , conn,2,4
		'session("CustStatDet").ActiveConnection = Nothing
		'Change the Amount field for Debit and Credit. .. 
			IF not(session("CustStatDet").EOF And session("CustStatDet").BOF) Then
				'session("CustStatDet").MoveFirst()
				'Response.Write("<br><Font Size=3>"&session("CustStatDet")("Tran")&"</font><BR>")
				session("CustStatDet").PageSize = NumPerPage
				session("CustStatDet").AbsolutePage = CurPage
			End IF

			Dim TotalPages
			TotalPages = session("CustStatDet").PageCount
			 
			'HDM
			'If not session("CustStatDet").EOF and Count < session("CustStatDet").PageSize then
				'session("CustStatDet").MoveFirst()
			'End If
			'HDM
			'do while not session("CustStatDet").EOF and Count < session("CustStatDet").PageSize
			Function GetAmount(dtDate)
				set rsAmount = Server.CreateObject("ADODB.RecordSet")
				'strSQL = "Select (Amount + Openamt ) AS Amount From Arhist Where Account='" & CurCust & "' AND Tran ='" & session("CustStatDet").fields("Tran") & "'"
				'WMA 0 Amount Correct [Start]
				'strSQL = "Select (Amount + Openamt ) AS Amount From Arhist Where Account='" & CurCust & "' AND Tran ='" & dtDate & "'"
				strSQL = "Select sum((Amount)) AS Amount From Arhist Where Account='" & CurCust & "' AND Tran ='" & dtDate & "'"
				'WMA 0 Amount Correct [End]
				'Response.Write "<font face=3>" & strSQL
				rsAmount.Open strSQL, Conn
				if not rsAmount.BOF and not rsAmount.EOF then
					GetAmount = rsAmount.Fields("Amount")
				end if
				rsAmount.Close
				
				set rsAmount = nothing
			End Function

		if session("CustStatDet").EOF And session("CustStatDet").BOF then
			Response.Write "<br><br><br><Table width=""95%"" align=center ><TR><TD align=center >No Records Exist</td></tr></table>"
			Response.End
		else

		'---------------------------------------------------------------------------------------
		' End of header and detial Query
		'--------------------------------------------------------------------------------------
%>
<%

	If Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
	If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"PRNTSTMNT") <= 0 Then
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

<Table width=95% align=center>
<TR>
	<TD align=center><a href="javascript:GoReport();">Printer Friendly Report</a></td>
</TR>
</Table>

<%
	'session("CustStatHdr").MoveFirst()
	'If CurPage = 2  then
		'session("CustStatDet").MoveFirst()
	'end if
%>				
<table width=95% align=center>
	<tr>
		<td align="right">
			<table border="1" style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
				<tr>
					<td class="dark_cell">Statement Date</td>
					<td class="dark_cell"><%=session("CustField")%> No.</td>
				</tr>
				<tr>
					<td class="light_cell" align=center><%=Date()%>&nbsp;</td>
					<td class="light_cell" ><%=session("CustStatHdr").Fields("Account")%>&nbsp;</td>
				</tr>
			</table>
		</td>
	</tr>
</table>
<BR>
<div align="center">
  <center>
<table border="1"  width=95% style="border-collapse: collapse" bordercolor="#111111" cellpadding="0" cellspacing="0">
	<tr>
		<td class="dark_cell" align=center>Due Date</td>
		<td class="dark_cell" align=center>Transaction Date</td>
		<td class="dark_cell">Tran. No.</td>
		<td class="dark_cell">Description</td>
		<td class="dark_cell">Reference</td>
		<td class="dark_cell" align=right>Amount</td>
		<td class="dark_cell" align=right>Balance</td>
	</tr>
	<%  Dim count
		Count = 0
		i = 0
		j = 1
		TotalBalance =0
		TotalCurrent =0
		TotalDueNow  =0
		TotalCredit  =0
		do while not session("CustStatDet").EOF and Count < session("CustStatDet").PageSize
			'WMA 0 Amount Correct [Start]
			'session("CustStatDet").Fields("Amount") = cdbl(GetAmount(session("CustStatDet").Fields("Tran"))) 			
			session("CustStatDet").Fields("Amount") = cdbl(session("CustStatDet").Fields("Amount")) + cdbl(GetAmount(session("CustStatDet").Fields("Tran"))) 
			'WMA 0 Amount Correct [End]
			x = i mod 11
			if x = 0 then j = j + 1
			GroupNo = j-1 '& "<BR>"
			session("CustStatDet").Fields("GroupNo") = GroupNo
			i = i + 1%>
	<TR>		
		<td class="light_cell" align=center><%if Trim(session("CustStatDet").Fields("Duedate"))<> "12:00:00 AM" then Response.write session("CustStatDet").Fields("Duedate") end if %>&nbsp;</td>
		<td class="light_cell" align=center><%if Trim(session("CustStatDet").Fields("Trandate"))<> "12:00:00 AM" then Response.Write Session("CustStatDet").Fields("Trandate") end if %>&nbsp;</td>
		<td class="light_cell"><%=session("CustStatDet").Fields("Tran")%>&nbsp;</td>
		<td class="light_cell"><%=session("CustStatDet").Fields("Desc")%>&nbsp;</td>
		<td class="light_cell"><%=session("CustStatDet").Fields("Reference")%>&nbsp;</td>
		<td class="light_cell" align=right>
		<%if Session("CurrencyAlign")="LEFT" then%>
			<%=Session("Currency") & FormatNumber(session("CustStatDet").Fields("Amount"))%>
		<%else%>
			<%=FormatNumber(session("CustStatDet").Fields("Amount")) & Session("Currency")%>
		<%end if %>
		</td>
		<td class="light_cell" align=right>
		<%if Session("CurrencyAlign")="LEFT" then%>
			<%=Session("Currency") & FormatNumber(session("CustStatDet").Fields("BALANCE"))%>
		<%else%>
			<%=FormatNumber(session("CustStatDet").Fields("BALANCE")) & Session("Currency")%>
		<%end if %>
		</td>
	</TR>

		<%Count = Count + 1
		TotalBalance = cdbl(TotalBalance) + cdbl(session("CustStatDet").Fields("BALANCE"))
		
		'WMA Total Credit
		if session("CustStatDet").Fields("DC") = "Credi"  then  
			TotalCredit = cdbl(TotalCredit) +  cdbl(session("CustStatDet").Fields("BALANCE"))
		end if
		
		'WMA Correct Total Current Value		
		'if cdbl(session("CustStatDet").Fields("BALANCE")) > 0 and cdate(session("CustStatDet").Fields("Duedate")) + 30 > date() then
		if session("CustStatDet").Fields("DC") = "Debit" and cdate(session("CustStatDet").Fields("Duedate")) >= date() then  
			TotalCurrent = cdbl(TotalCurrent) +  cdbl(session("CustStatDet").Fields("BALANCE"))
		end if

		'WMA Correct Due Now Value		
		'if cdbl(session("CustStatDet").Fields("BALANCE")) > 0 and cdate(session("CustStatDet").Fields("Duedate"))<date() then
		if cdate(session("CustStatDet").Fields("Duedate"))< date() or session("CustStatDet").Fields("DC") = "Credi" then  
			TotalDueNow = TotalDueNow + cdbl(session("CustStatDet").Fields("BALANCE"))
		end if 
		session("CustStatDet").MoveNext()
		loop%>
		
	<tr>
		<td class="dark_cell" align=right>Due Now</td>
		<td class="dark_cell" align=right>Current</td>
		<td colspan=3 class="dark_cell"></td>
		<td class="dark_cell" align=right>Total Credit</td>
		<td class="dark_cell" align=right>Total</td>
	</tr>
	<tr>
		<td class="light_cell" align=right>
		<%if Trim(Session("CurrencyAlign"))="LEFT" then%>
			<%=Session("Currency") & FormatNumber(TotalDueNow)%>
		<%else%>
			<%=FormatNumber(TotalDueNow) & Session("Currency")%>
		<%end if %>
		</td>
		<td class="light_cell" align=right>
		<%if Trim(Session("CurrencyAlign"))="LEFT" then%>
			<%=Session("Currency") & FormatNumber(TotalCurrent)%>
		<%else%>	
			<%=FormatNumber(TotalCurrent) & Session("Currency")%>
		<%end if %>
		</td>
		<td colspan=3 class="dark_cell"></td>
		<!-- WMA Total Credit [Start]-->		
		<td class="light_cell" align=right>
		<%if Trim(Session("CurrencyAlign"))="LEFT" then%>
			<%=Session("Currency") & FormatNumber(TotalCredit)%>
		<%else%>
			<%=FormatNumber(TotalCredit) & Session("Currency")%>
		<%end if %></td>
		<!-- WMA Total Credit [End]-->		
		<td class="light_cell" align=right>
		<%if Trim(Session("CurrencyAlign"))="LEFT" then%>
			<%=Session("Currency") & FormatNumber(TotalBalance)%>
		<%else%>
			<%=FormatNumber(TotalBalance) & Session("Currency")%>
		<%end if %></td>
	</tr>
</table>	
  </center>
</div>
<table width=95% align=center>
	<Tr>
		<td width = 30%><STRONG>Post Dated Checks: </STRONG></td>
		<td><%=session("CustStatHdr").Fields("PostDChecks")%></td>
	</tr>
</table>	
		

<HR width=95% align=center>
<Table width=95% align=center><TR><TD align=center>
		<%
			Response.Write("Page " & CurPage & " of " & TotalPages & "<P>")
			if CurPage > 1 then
			        'We are not at the beginning, show the prev button%>
			       <a href="custstat.asp?curpage=<%= (curpage - 1)%>"><IMG border=0 SRC="../images/<%=Session("Theme")%>/back.gif"></a>
			<%End If

			if CInt(CurPage) <> CInt(TotalPages) then
			        'We are not at the end, show a next button%>
			        <a href="custstat.asp?curpage=<%= (curpage + 1)%>"><IMG border=0 SRC="../images/<%=Session("Theme")%>/next.gif"></a>
			<%End If
		%>
				
<%	End if
end if
%>
</TD></TR></TAble>
</body>
</html>