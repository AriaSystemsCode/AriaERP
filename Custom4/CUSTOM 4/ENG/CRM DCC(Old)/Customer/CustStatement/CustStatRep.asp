<%@ Language=VBScript %>
<%response.expires=-1
'Session("ID") = "JCP10"
'CurComp = "99"
if Trim(Session("rep")) = "" and trim(Session("ID"))= "" then
	Response.redirect "../login.asp"
end if

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

if right(application("datapath"),1) ="\" then
  memfilename = Application("datapath") & "Arstmsg.mem"
else
   memfilename = Application("datapath") & "\Arstmsg.mem"
end if
'memfilename = Application("DataPath") & "Arstmsg.mem"
memfilename = cstr(memfilename)
%>


<html>
<head>
<title>CRM - Customer Statement</title>
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Invoice.css">
</head>
<body>
<br><br><br><br>
<SCRIPT LANGUAGE=javascript>

function GoReport()
{
	var flg = navigator.appVersion  ;
	
	if(flg.indexOf("MSIE") != -1)
		{
			document.location.href = "CustStatement/CustStatRep.asp?viewer=ActiveX";
		}
	else
		{
			document.location.href = "CustStatement/CustStatRep.asp?viewer=Java-Plug-in";
		}
}
//
</SCRIPT>


<%
Set Conn = Server.CreateObject("ADODB.Connection")

ConnString = Application("DataConnectionString")
Conn.Open ConnString
Set SysConn = Server.CreateObject("ADODB.Connection")
ConnString = Application("SystemConnectionString")
SysConn.Open ConnString
'---------------------------------------------------------------------------------------
' build header Query
'---------------------------------------------------------------------------------------
set session("CustStatHdr") = server.CreateObject("ADODB.RecordSet")
strSQL = "Select Account, btname, Caddress1, Caddress2, Caddress3, Caddress4, Caddress5, Caddress6, "
strSQL = strSQL & " btname AS CompName, Caddress1 AS SoldToAd1, Caddress2 AS SoldToAd2, Caddress3 AS SoldToAd3, "
strSQL = strSQL & " Caddress4 AS SoldToAd4, Caddress5 AS SoldToAd5, Caddress6 AS SoldToAd6, "
strSQL = strSQL & " SPACE(120) AS Advertise1, SPACE(120) AS Advertise2, Space(120) AS Advertise3, "
strSQL = strSQL & " SPACE(120) AS Advertise4, SPACE(120) AS Advertise5, SPACE(120) AS OrderDue1, "
strSQL = strSQL & " SPACE(120) AS OrderDue2, SPACE(120) AS OrderDue3, SPACE(20) AS PostDChecks, "
strSQL = strSQL & " Cfaccode, Age120, Age90, Age60, Age30 from Customer where "
strSQL = strSQL & " Type+Account+Store like 'M" & CurCust & "%'And Netbal>=0"

Session("CustStatHdr").open strSQL, Conn ,2 , 4

' if netbalance <0 then no report ..
if Session("CustStatHdr").BOF and Session("CustStatHdr").EOF then%>
	<b><font size=2>Your net balance < 0, you can't print the report</font></b>
<%else
		'Change the Advertise fields to the required data .. 

		set objMem = Server.CreateObject("DBREadMem.ReadMem")
		'
		on error resume next
		Ad1 = objMem.LoadMemFile ("MADL1", cstr(memfilename))
		Ad2 = objMem.LoadMemFile ("MADL2", cstr(memfilename))
		Ad3 = objMem.LoadMemFile ("MADL3", cstr(memfilename))
		Ad4 = objMem.LoadMemFile ("MADL4", cstr(memfilename))
		Ad5 = objMem.LoadMemFile ("MADL5", cstr(memfilename))
		on error goto 0
		
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
			on error resume next
			OD1 = objMem.LoadMemFile (cstr(WarningVariable1), cstr(memfilename))
			OD2 = objMem.LoadMemFile (cstr(WarningVariable2), cstr(memfilename))
			OD3 = objMem.LoadMemFile (cstr(WarningVariable3), cstr(memfilename))
			'on error go to 0
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
		strSQL = "SELECT SUM(Amount) AS PostDChecks FROM Postdchq WHERE account+DTOS(paydate)like '" & CurCust & "%'"
		rsPostDChecks.Open strSQL , Conn
		if not rsPostDChecks.BOF and not rsPostDChecks.EOF then
			Session("CustStatHdr").Fields("PostDChecks") = Cstr(rsPostDChecks("PostDChecks"))
		else
			Session("CustStatHdr").Fields("PostDChecks") = "N/A"
		end if

			set rsCompanyAd = Server.CreateObject("ADODB.RecordSet")
			strSQL = "SELECT cCom_name, Caddress1, Caddress2, Caddress3, Caddress4, Caddress5, Caddress6 FROM SycComp Where Ccomp_id='" & Session("CompanyID") & "'"
			rsCompanyAd.Open strSQL , SysConn
			if not rsCompanyAd.BOF and not rsCompanyAd.EOF then
				Session("CustStatHdr").Fields("btname") = rsCompanyAd.Fields("cCom_name")
				Session("CustStatHdr").Fields("Caddress1") = rsCompanyAd.Fields("Caddress1")
				Session("CustStatHdr").Fields("Caddress2") = rsCompanyAd.Fields("Caddress2")
				Session("CustStatHdr").Fields("Caddress3") = rsCompanyAd.Fields("Caddress3")
				Session("CustStatHdr").Fields("Caddress4") = rsCompanyAd.Fields("Caddress4")
				Session("CustStatHdr").Fields("Caddress5") = rsCompanyAd.Fields("Caddress5")
				Session("CustStatHdr").Fields("Caddress6") = rsCompanyAd.Fields("Caddress6")
			else
				Response.Write "<font size=3>No Company Exist</font>"
			end if 
			set rsCompanyAd = nothing
			Session("CustStatHdr").Fields("Cfaccode")=""
		session("CustStatHdr").Update()	
			

		Const NumPerPage = 11

		Dim CurPage
		If Request.QueryString("CurPage") = "" then
		     CurPage = 1 'We're on the first page
		Else
		     CurPage = Request.QueryString("CurPage")
		End If

		set session("CustStatDet") = server.CreateObject("ADODB.RecordSet")
		
	    session("CustStatDet").CursorLocation = 3
	    session("CustStatDet").CacheSize = NumPerPage

		
'		strSQL = "SELECT Debit.Account,Debit.Duedate, Debit.Trandate, Debit.Tran, Debit.Desc, Debit.Reference,Debit.Amount AS Amount , "
'		strSQL = strSQL & " Debit.Amount AS BALANCE, Debit.Amount AS GroupNo  "
'		strSQL = strSQL & " FROM Debit WHERE "
'		strSQL = strSQL & " account+tran+cinstalno+DTOS(trandate) like '" & CurCust & "%' "
'		strSQL = strSQL & " UNION ALL "
'		strSQL = strSQL & " SELECT Credit.Account,Credit.Dpostdate, Credit.Trandate,Credit.Tran,Credit.Desc,Credit.Reference,"
'		strSQL = strSQL & " Credit.Amount AS Amount , Credit.Amount AS BALANCE, Credit.Amount AS GroupNo  "
'		strSQL = strSQL & " FROM Credit where "
'		strSQL = strSQL & "  account+tran+DTOS(trandate) like '" & CurCust & "%' "
'		strSQL = strSQL & " ORDER BY 3"

		'WMA Amount and Due Date Correct [Start]
'		strSQL = "SELECT Debit.Account,Debit.Duedate, Debit.Trandate, Debit.Tran, Debit.Desc, Debit.Reference,Debit.Amount AS Amount , Debit.Amount AS BALANCE, Debit.Amount AS GroupNo  FROM Debit WHERE Debit.Trandate<={" & date() & "} AND Account='" & CurCust & "' UNION ALL SELECT Credit.Account,Credit.Dpostdate, Credit.Trandate,Credit.Tran,Credit.Desc,Credit.Reference,Credit.Amount AS Amount , Credit.Amount AS BALANCE, Credit.Amount AS GroupNo  FROM Credit where Credit.Trandate<={" & date() & "} AND Account='" & CurCust & "' ORDER BY 3"
		strSQL ="SELECT Debit.Account,Debit.Duedate, Debit.Trandate, Debit.Tran, Debit.Desc, Debit.Reference,Debit.Amount AS Amount , Debit.Amount AS BALANCE, Debit.Amount AS GroupNo ,iif(debit.trantype='1' or debit.trantype='2' or debit.trantype='3','Debit','Credit') as DC, debit.trantype "
		strSQL = strSQL + ", Debit.Amount as TotalDueNow,Debit.Amount as TotalCurrent, Debit.Amount as TotalCredit, Debit.Amount as Total "
		strSQL = strSQL + "FROM Debit WHERE Debit.Trandate<={" & date() & "} AND Account='" & CurCust & "' "
		strSQL = strSQL + "UNION ALL SELECT Credit.Account,Credit.Dpostdate, Credit.Trandate,Credit.Tran,Credit.Desc,Credit.Reference,Credit.Amount AS Amount , Credit.Amount AS BALANCE, Credit.Amount AS GroupNo,iif(credit.trantype='0' or credit.trantype='4' or credit.trantype='5' or credit.trantype='6','Credit','Debit') as DC, Credit.trantype "
		strSQL = strSQL + ", Credit.Amount as TotalDueNow,Credit.Amount as TotalCurrent, Credit.Amount as TotalCredit,Credit.Amount as Total "
		strSQL = strSQL + "FROM Credit where Credit.Trandate<={" & date() & "} AND Account='" & CurCust & "'  ORDER BY 3"
		'strSQL = strSQL + "FROM Credit where Credit.Trandate<={" & date() & "} AND Account='" & CurCust & "' "
		'strSQL = strSQL + "UNION ALL SELECT Arhist.trantype,Arhist.Account,Arhist.Dpostdate, Arhist.Trandate,Arhist.Tran,Arhist.Desc,Arhist.Reference,Arhist.Amount AS Amount , Arhist.Amount AS BALANCE, Arhist.Amount AS GroupNo,iif(Arhist.trantype='0' or Arhist.trantype='4' or Arhist.trantype='5' or Arhist.trantype='6','Credit','Debit') as DC "
		'strSQL = strSQL + "FROM Arhist where Arhist.Trandate<={" & date() & "} AND Account='" & CurCust & "' ORDER BY 3"
		
		'strSQL = "SELECT Debit.Account,Debit.Duedate, Debit.Trandate, Debit.Tran, Debit.Desc, Debit.Reference,Debit.Amount AS Amount , Debit.Amount AS BALANCE, Debit.Amount AS GroupNo ,ALIAS("Debit") as DC  FROM Debit WHERE Debit.Trandate<={" & date() & "} AND Account='" & CurCust & "' UNION ALL SELECT Credit.Account,Credit.Dpostdate, Credit.Trandate,Credit.Tran,Credit.Desc,Credit.Reference,Credit.Amount AS Amount , Credit.Amount AS BALANCE, Credit.Amount AS GroupNo , ALIAS("Credit") as DC  FROM Credit where Credit.Trandate<={" & date() & "} AND Account='" & CurCust & "' ORDER BY 3"
		'WMA Amount and Due Date Correct [End]
		'Response.Write "<font size=3>" & strSQL & "<br>"
		session("CustStatDet").Open strSQL , conn, 2, 4
		
		'Change the Amount field for Debit and Credit. .. 
		IF not(session("CustStatDet").EOF And session("CustStatDet").BOF) Then
			session("CustStatDet").MoveFirst()
			'session("CustStatDet").PageSize = NumPerPage
			'session("CustStatDet").AbsolutePage = CurPage
		End IF
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

			
'			set rsAmount = Server.CreateObject("ADODB.RecordSet")
'			i = 0
'			j = 1 
'
'			'strSQL = "Select (Amount + Openamt ) AS Amount From Arhist Where account='" & CurCust & "' AND Tran ='" & session("CustStatDet").fields("Tran") & "'"
'			strSQL = "Select (Amount + Openamt ) AS Amount,tran From Arhist Where account+history like '" & CurCust & "%'"
'			rsAmount.Open strSQL, Conn

			Count = 0
			do while not session("CustStatDet").EOF 'and Count < session("CustStatDet").PageSize
'				session("CustStatDet").Fields("Amount") = cdbl(session("CustStatDet").Fields("Amount")) + cdbl(GetAmount(session("CustStatDet").Fields("Tran"))) 
'				x = i mod 11
'				if x = 0 then j = j + 1
'				GroupNo = j-1 '& "<BR>"
'				session("CustStatDet").Fields("GroupNo") = GroupNo
'				'Response.Write "<BR>" & GroupNo
'				i = i + 1	
				
				'Totals
				Count = Count + 1	
				session("CustStatDet").Fields("Amount") = cdbl(session("CustStatDet").Fields("Amount")) + cdbl(GetAmount(session("CustStatDet").Fields("Tran"))) 
				x = i mod 11
				if x = 0 then j = j + 1
				GroupNo = j-1 '& "<BR>"
				session("CustStatDet").Fields("GroupNo") = GroupNo
				i = i + 1				
			
				TotalBalance = cdbl(TotalBalance) + cdbl(session("CustStatDet").Fields("BALANCE"))
		
				'Total Credit
				if session("CustStatDet").Fields("DC") = "Credi"  then  
					TotalCredit = cdbl(TotalCredit) +  cdbl(session("CustStatDet").Fields("BALANCE"))
				end if
		
				'Correct Total Current Value		
				'if cdbl(session("CustStatDet").Fields("BALANCE")) > 0 and cdate(session("CustStatDet").Fields("Duedate")) + 30 > date() then
				if session("CustStatDet").Fields("DC") = "Debit" and cdate(session("CustStatDet").Fields("Duedate")) >= date() then  
					TotalCurrent = cdbl(TotalCurrent) +  cdbl(session("CustStatDet").Fields("BALANCE"))
				end if

				'Correct Due Now Value		
				'if cdbl(session("CustStatDet").Fields("BALANCE")) > 0 and cdate(session("CustStatDet").Fields("Duedate"))<date() then
				if cdate(session("CustStatDet").Fields("Duedate"))< date() or session("CustStatDet").Fields("DC") = "Credi" then  
					TotalDueNow = TotalDueNow + cdbl(session("CustStatDet").Fields("BALANCE"))
				end if 
				
				'Fill Totals
				session("CustStatDet").Fields("TotalDueNow") =  cdbl(TotalDueNow)
				session("CustStatDet").Fields("TotalCurrent")= cdbl(TotalCurrent)
				session("CustStatDet").Fields("TotalCredit") = cdbl(TotalCredit)
				session("CustStatDet").Fields("Total") = cdbl(TotalBalance)
					
				session("CustStatDet").MoveNext()
			loop


		if session("CustStatDet").EOF And session("CustStatDet").BOF then
			Response.Write "<font size=3>No Records Exist!</font>"
			
		else

		'---------------------------------------------------------------------------------------
		' End of header and detial Query
		'--------------------------------------------------------------------------------------
%>
<%
reportname = "CustStat40.rpt"
If Not IsObject (session("oApp")) Then                              
	Set session("oApp") = Server.CreateObject("Crystal.CRPE.Application")
End If                                                                
Path = Request.ServerVariables("PATH_TRANSLATED")        

While (Right(Path, 1) <> "\" And Len(Path) <> 0)                      
iLen = Len(Path) - 1                                                  
Path = Left(Path, iLen)                                               
Wend                                                                  

If IsObject(session("oRpt")) then
	Set session("oRpt") = nothing
End if

Set session("oRpt") = session("oApp").OpenReport(path & reportname, 1)


''ARD
'' Send the Line recordset [Start] 
	set session("subRep")= session("oRpt").OpenSubReport("sub")
	set subDatabase = session("subRep").Database
	Set fs = session("subRep").FormulaFields
	'NEK [Start]
	Set f1 = fs.Item(4)
	Set f2 = fs.Item(5)
	f1.Text = "'" & Session("Currency") & "'"
	f2.Text = "'" & Session("CurrencyAlign") & "'"
	'NEK [End]
	set subTables = subDatabase.Tables
	set subTable1 = subTables.Item(1)
	subTable1.SetPrivateData 3, session("CustStatDet")
'' Send the Line recordset [End] 

'' Send the Footer recordset [Start] 
	set session("subRepFooter")= session("oRpt").OpenSubReport("Footer")
	Set fs = session("subRepFooter").FormulaFields
	'NEK [Start]
	Set f1 = fs.Item(1)
	Set f2 = fs.Item(2)
	f1.Text = "'" & Session("Currency") & "'"
	f2.Text = "'" & Session("CurrencyAlign") & "'"
	'NEK [End]
	set subDatabaseFooter = session("subRepFooter").Database
	set subTablesFooter = subDatabaseFooter.Tables
	set subTableFooter1 = subTablesFooter.Item(1)
	subTableFooter1.SetPrivateData 3, session("CustStatDet")
'' Send the Line recordset [End] 


''ARD


'ARD
' Send the header recordset [Start] 
session("oRpt").DiscardSavedData
'WAL_E302083,1 set field values in the report [Start]
	Set fs = Session("oRpt").FormulaFields
	Set f1 = fs.Item(1)
	Set f2 = fs.Item(2)
	Set f3 = fs.Item(3)
f1.Text = "'"& session("CustField") &"'"
f2.Text = "'" & Session("Currency") & "'"
f3.Text = "'" & Session("CurrencyAlign") & "'"
'WAL_E302083,1 set field values in the report [End]
set Database = session("oRpt").Database
set Tables = Database.Tables
set Table1 = Tables.Item(1)
Table1.SetPrivateData 3, Session("CustStatHdr")
'set Table2 = Tables.Item(2)
'Table2.SetPrivateData 3,session("CustStatDet")

' Send the header recordset [End] 
'ARD

On Error Resume Next                                                  
err.Clear 
session("oRpt").ReadRecords

%>

<%
		If Err.Number <> 0 Then                                               
		  Response.Write "<font size=3>An Error has occured on the server in attempting to access the data source"
		Else

		  If IsObject(session("oPageEngine")) Then                              
		  	set session("oPageEngine") = nothing
		  End If
		set session("oPageEngine") = session("oRpt").PageEngine
		End If                                                                

		viewer = Request.QueryString ("viewer")
		viewer="ActiveX"
		'This line collects the value passed for the viewer to be used, and stores
		'it in the "viewer" variable.

		If cstr(viewer) = "ActiveX" then
		%>
		<!-- #include file="../../crystal/SmartViewerActiveX.asp" -->
		<%
		ElseIf cstr(viewer) = "Netscape Plug-in" then
		%>
		<!-- #include file="../../crystal/ActiveXPluginViewer.asp" -->
		<%
		ElseIf cstr(viewer) = "JVM" then
		%>
		<!-- #include file="../../crystal/SmartViewerJava.asp" -->
		<%
		ElseIf cstr(viewer) = "Java-Plug-in" then
		%>
		<!-- #include file="../../crystal/JavaPluginViewer.asp" -->
		<%
		ElseIf cstr(viewer) = "HTML Frame" then
			Response.Redirect("htmstart.asp")
		Else
			Response.Redirect("rptserver.asp")
		End If
		'The above If/Then/Else structure is designed to test the value of the "viewer" varaible
		'and based on that value, send down the appropriate Crystal Smart Viewer.

		''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
		%>
		<%End if ' for the Query doens't contain data.%>
<%end if ' for netbalance < 0%>
<BR><BR>
<CENTER>
<a href="../CustStat.asp"><IMG border=0 SRC="../../Images/<%=session("Theme")%>/back.gif"></a>
</CENTER>
<br>
</BODY>
</HTML>
<%
''''''''''''''''''''''''''''''''''''''''''


%>