<%@ Language=VBScript %>
<%Response.Buffer=true%>

<%IF Len(trim(session("rep")))>0 THEN
			IF Trim(Session("customerid")) = "" THEN
					Response.Redirect("repcust.asp")
			END IF
END IF

IF Trim(Session("ID")) = "" AND Trim(Session("rep"))= "" THEN
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 

Set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
Set session("rsRanoMatch") = server.CreateObject("ADODB.RecordSet")
BeginDate = (Trim(Request.Form("txtBeginDate")))
EndDate  = (Trim(Request.Form("txtEndDate")))
'strSQL = "SELECT Cartons, Reason FROM Retauth WHERE Account='" & Session("ID") & "' AND Rano='" & Request("RanoNo") & "'"
'strSQL = "SELECT Account, Cartons, Auth, Authamt, Rano,Status,Radate,Void, Store, Reason, Cdivision, Nreta_bud, Nreta_rec, Nreta_can, Nreta_opn, Nrtopnamt FROM Retauth WHERE Account='" & Session("ID") & "' AND Rano='" & Request("RanoNo") & "'"
'strSQL = "SELECT Retauth.Account, Retauth.Rano,	Retauth.Status,	Retauth.Store,	Retauth.Radate,		Retauth.Void,CodesReason.Cdiscrep AS ReasonDisc,	CodesDivision.Cdiscrep AS DivisionDisc,	Retauth.Cartons,Retauth.Nreta_bud,Retauth.Auth,	Retauth.Authamt,Retauth.Nreta_rec, Retauth.Nreta_can, Retauth.Nreta_opn, Retauth.Nrtopnamt FROM	Retauth,Codes AS CodesReason,Codes AS CodesDivision WHERE CodesReason.Cdefcode='N' AND 	CodesReason.Crltfield='N' AND CodesReason.Ccode_no = Retauth.Reason AND     CodesReason.Cfld_name = 'REASON' AND CodesDivision.Cdefcode='N' AND	CodesDivision.Crltfield='N' AND    CodesDivision.Cfld_name='CDIVISION' AND CodesDivision.Ccode_no = Retauth.Cdivision AND Retauth.Account='" & Session("ID") & "' AND Retauth.Rano = '" & Request("RanoNo") & "'"
'strSQL = "SELECT Retauth.Radate,Retauth.Void,Retauth.order,Retauth.invoice,Retauth.Auth,Retauth.Account,Retauth.Store,Retauth.Custpo,Retauth.Cartons,CodesReason.Cdiscrep AS ReasonDisc,Customer.Stname,Customer.Caddress1,Customer.Caddress2,Customer.Caddress3,Customer.Caddress4,Customer.Caddress5,Customer.Caddress6,Retauth.Rano FROM Retauth,Codes AS CodesReason,Codes AS CodesDivision,customer WHERE CodesReason.Cdefcode='N' AND 	CodesReason.Crltfield='N' AND CodesReason.Ccode_no = Retauth.Reason AND     CodesReason.Cfld_name = 'REASON' AND CodesDivision.Cdefcode='N' AND	CodesDivision.Crltfield='N' AND    CodesDivision.Cfld_name='CDIVISION' AND CodesDivision.Ccode_no = Retauth.Cdivision AND Retauth.Account='" & Session("ID") & "' AND Retauth.Rano = '" & Request("RanoNo") & "' AND Retauth.store = Customer.store AND Customer.account = '"& Session("ID") & "'" 
IF Len(trim(session("rep")))>0 THEN
	strSQL = "SELECT Retauth.Rano,Retauth.Radate,Retauth.Void,Retauth.order,Retauth.invoice,Retauth.Auth,Retauth.Account,Retauth.Store,Retauth.Custpo,Retauth.Cartons,CodesReason.Cdiscrep AS easonDisc, Customer.Stname,Customer.Caddress1,Customer.Caddress2,Customer.Caddress3,Customer.Caddress4,Customer.Caddress5,Customer.Caddress6, Customer.phone1,Warehous.cwarecode,Warehous.cdesc,Warehous.Caddress1,Warehous.Caddress2,Warehous.Caddress3,Warehous.Caddress4,Warehous.Caddress5,Warehous.Caddress6,Warehous.Cphone,Retauth.cretnote1,Retauth.cretnote2,Retauth.cretnote3,Retauth.cretnote4,Retauth.Rano FROM Retauth,Codes AS CodesReason,Codes AS CodesDivision,customer,warehous WHERE CodesReason.Cdefcode='N' AND 	CodesReason.Crltfield='N' AND CodesReason.Ccode_no = Retauth.Reason AND     CodesReason.Cfld_name = 'REASON' AND CodesDivision.Cdefcode='N' AND	CodesDivision.Crltfield='N' AND    CodesDivision.Cfld_name='CDIVISION' AND CodesDivision.Ccode_no = Retauth.Cdivision AND Retauth.Account='" & Session("customerID") & "' AND Retauth.Rano = '" & Request("RanoNo") & "' AND Retauth.store = Customer.store AND Customer.account = '"& Session("customerID") & "' AND Retauth.Cwarecode = Warehous.Cwarecode"
ELSE
	strSQL = "SELECT Retauth.Rano,Retauth.Radate,Retauth.Void,Retauth.order,Retauth.invoice,Retauth.Auth,Retauth.Account,Retauth.Store,Retauth.Custpo,Retauth.Cartons,CodesReason.Cdiscrep AS easonDisc, Customer.Stname,Customer.Caddress1,Customer.Caddress2,Customer.Caddress3,Customer.Caddress4,Customer.Caddress5,Customer.Caddress6, Customer.phone1,Warehous.cwarecode,Warehous.cdesc,Warehous.Caddress1,Warehous.Caddress2,Warehous.Caddress3,Warehous.Caddress4,Warehous.Caddress5,Warehous.Caddress6,Warehous.Cphone,Retauth.cretnote1,Retauth.cretnote2,Retauth.cretnote3,Retauth.cretnote4,Retauth.Rano FROM Retauth,Codes AS CodesReason,Codes AS CodesDivision,customer,warehous WHERE CodesReason.Cdefcode='N' AND 	CodesReason.Crltfield='N' AND CodesReason.Ccode_no = Retauth.Reason AND     CodesReason.Cfld_name = 'REASON' AND CodesDivision.Cdefcode='N' AND	CodesDivision.Crltfield='N' AND    CodesDivision.Cfld_name='CDIVISION' AND CodesDivision.Ccode_no = Retauth.Cdivision AND Retauth.Account='" & Session("ID") & "' AND Retauth.Rano = '" & Request("RanoNo") & "' AND Retauth.store = Customer.store AND Customer.account = '"& Session("ID") & "' AND Retauth.Cwarecode = Warehous.Cwarecode"
END IF
Session("rsRanoMatch").Open strSQL, conn
%>

<HTML>
<HEAD><LINK REL=stylesheet HREF="crmmain.css" TYPE="text/css">
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<Title>CRM - Reterun Autorizatio Request - R.A. # <%=Request("RanoNo")%></Title>
</HEAD>
<body bgcolor="#aecae6" topmargin="0" leftmargin="0"  background="../images/Tile1.gif">
<br>
<CENTER>
<a href = "ReturnStatusDetail.asp?RanoNo=<%=Request("RanoNo")%>"><Img src="../Images/back.gif"></a>
</CENTER>
<br>
<%
IF session("rsRanoMatch").EOF AND session("rsRanoMatch").BOF THEN %>
	<BR>There is no records matching your entered selection criteria<BR>
	<%'if len(trim(session("rep")))>0 then%>
	<A HREF="ReturnStatus.asp">back</A>
<%	
ELSE
	session("rsRanoMatch").MoveFirst()
	Set session("rsRanoDetail") = Server.CreateObject("ADODB.RecordSet")
	IF Len(trim(session("rep")))>0 THEN
		strSQL2 = "SELECT Raline.Reason, Raline.Style, Style.Desc1, Raline.Price, Raline.Qty1, Raline.Qty2, Raline.Qty3, Raline.Qty4, Raline.Qty5, Raline.Qty6, Raline.Qty7, Raline.Qty8, Raline.Totqty, (Raline.Totqty * Raline.Price) AS Amount, Raline.Nopnqty1,Raline.Nopnqty2,Raline.Nopnqty3,Raline.Nopnqty4,Raline.Nopnqty5,Raline.Nopnqty6, Raline.Nopnqty7,Raline.Nopnqty8, Raline.Ntotopnqty, (Raline.Ntotopnqty * Raline.Price) As OpnAmount ,  Scale.Sz1, Scale.Sz2,  Scale.Sz3,  Scale.Sz4, Scale.Sz5, Scale.Sz6, Scale.Sz7, Scale.Sz8 FROM Raline,Style,Scale WHERE Style.Style=Raline.Style AND Scale.Scale=Style.Scale AND Scale.Type='S' AND Raline.Rano='" & session("rsRanoMatch")("Rano") & "' AND Raline.Account='" & Session("customerID") & "' Order By Raline.Cra_linno"
	ELSE
		strSQL2 = "SELECT Raline.Reason, Raline.Style, Style.Desc1, Raline.Price, Raline.Qty1, Raline.Qty2, Raline.Qty3, Raline.Qty4, Raline.Qty5, Raline.Qty6, Raline.Qty7, Raline.Qty8, Raline.Totqty, (Raline.Totqty * Raline.Price) AS Amount, Raline.Nopnqty1,Raline.Nopnqty2,Raline.Nopnqty3,Raline.Nopnqty4,Raline.Nopnqty5,Raline.Nopnqty6, Raline.Nopnqty7,Raline.Nopnqty8, Raline.Ntotopnqty, (Raline.Ntotopnqty * Raline.Price) As OpnAmount ,  Scale.Sz1, Scale.Sz2,  Scale.Sz3,  Scale.Sz4, Scale.Sz5, Scale.Sz6, Scale.Sz7, Scale.Sz8 FROM Raline,Style,Scale WHERE Style.Style=Raline.Style AND Scale.Scale=Style.Scale AND Scale.Type='S' AND Raline.Rano='" & session("rsRanoMatch")("Rano") & "' AND Raline.Account='" & Session("ID") & "' Order By Raline.Cra_linno"
	END IF
	Session("rsRanoDetail").Open strSQL2, conn

'check to see if there is detail for theis Order
	IF session("rsRanoDetail").EOF AND session("rsRanoDetail").BOF THEN %>
	<BR>No Detail Exist for this Return Authorization number<BR>
	<A HREF="ReturnStatus.asp">back</A>
	<%
	ELSE
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
		reportname = "RturnAuth.rpt"
		IF Not IsObject (session("oApp")) THEN                              
		Set session("oApp") = Server.CreateObject("Crystal.CRPE.Application")
		'NOTE:Prev. line For old Version
		'Set session("oApp") = Server.CreateObject("CrystalRuntime.Application")
		END IF
		Path = Request.ServerVariables("PATH_TRANSLATED")                     
		While (Right(Path, 1) <> "\" And Len(Path) <> 0)                      
			iLen = Len(Path) - 1                                                  
			Path = Left(Path, iLen)                                               
		Wend                                                                  
		IF IsObject(session("oRpt")) THEN
			Set session("oRpt") = Nothing
		End if
		Set session("oRpt") = session("oApp").OpenReport(path & reportname, 1)
		'session("oRpt").MorePrintEngineErrorMessages = False
		'session("oRpt").EnableParameterPrompting = False

		Session("oRpt").DiscardSavedData
		Set Database = session("oRpt").Database
		Set Tables = Database.Tables
		Set Table1 = Tables.Item(1)
		Table1.SetPrivateData 3,session("rsRanoMatch")
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
		Set session("subRep") = session("oRpt").OpenSubReport("subRturnAuth.rpt")
		Set subDatabase = session("subRep").Database
		Set subTables = subDatabase.Tables
		Set subTable1 = subTables.Item(1)
		subTable1.SetPrivateData 3, session("rsRanoDetail")


		On Error Resume Next                                                  
		session("oRpt").ReadRecords                                           
		IF Err.Number <> 0 THEN                                             
			Response.Write "An Error has occured on the server in attempting to access the data source"
		ELSE

			IF IsObject(session("oPageEngine")) THEN                              
				Set session("oPageEngine") = Nothing
			END IF
		Set session("oPageEngine") = session("oRpt").PageEngine
END IF                                                                

viewer = Request.QueryString ("Viewer")

'This line collects the value passed for the viewer to be used, and stores
'it in the "viewer" variable.

IF cstr(viewer) = "ActiveX" THEN
%>
<!-- #include file="../crystal/SmartViewerActiveX.asp" -->
<%
ElseIf cstr(viewer) = "Netscape Plug-in" then
%>
<!-- #include file="../crystal/ActiveXPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "JVM" then
%>
<!-- #include file="../crystal/SmartViewerJava.asp" -->
<%
ElseIf cstr(viewer) = "Java-Plug-in" then
%>
<!-- #include file="../crystal/JavaPluginViewer.asp" -->
<%
ElseIf cstr(viewer) = "HTML Frame" then
	Response.Redirect("htmstart.asp")
Else
	Response.Redirect("rptserver.asp")
End If
'The above If/Then/Else structure is designed to test the value of the "viewer" varaible
'and based on that value, send down the appropriate Crystal Smart Viewer.

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
END IF' for if the No is valid ...
END IF
%>
<BR>
<br>
<CENTER>
<a href = "ReturnStatusDetail.asp?RanoNo=<%=Request("RanoNo")%>"><Img src="../Images/back.gif"></a>
</CENTER>
<br>
</BODY>
</HTML>