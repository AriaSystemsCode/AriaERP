<%@ Language=VBScript %>
<%Response.Buffer = true%>
<HTML>
<HEAD>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<title>CRM - Check O.T.S.</title>
</HEAD>
<body>

<script language="javascript">
function openwindow(filename) 
{  
	window.open(filename,'','toolbar=no,status=no,scrollbars=no,location=no,menubar=no,directories=no,top=' + ((window.screen.height-500)/2) + ',left=' + ((window.screen.width-800)/2) + ',width=800,height=500')
}
function GoReport()
{
	var flg = navigator.appVersion  ;
	
	if(flg.indexOf("MSIE") != -1)
		{
			
			document.location.href = "otsReport.asp?viewer=ActiveX";
		}
	else
		{
			
			document.location.href = "otsReport.asp?viewer=Java-Plug-in";
		}
}
</script>

<%
IF Trim(Session("ID")) = "" And Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
	parent.location.href="../login.asp"
	</script>
	
<%END IF

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	strFile = "reb"
End IF

%>


<% 
'RecordSet that will send to the Report
if request("Select") = "A" then
	Response.Redirect "otsReport.asp?Select=A" 
else
	if Request.QueryString ("firsttime") <> "" then
		
		'get selected records
		if isobject(Session("rsSelect")) then'recrdset not there then create it
		else
			set Session("rsSelect") = server.CreateObject("ADODB.RecordSet")
			call Session("rsSelect").fields.append("style",129,20)
			call Session("rsSelect").fields.append("No",14)
			Session("rsSelect").open
		end if
		'Response.Write Session("rsSelect1").recordcount&"<br>"
		'adjust session recordset
		myPage  = Request.QueryString ("curpage")
		Session("rsSelect").filter = "NO = '"& myPage &"'"
		if not Session("rsSelect").eof then
			Session("rsSelect").movefirst
			do while not Session("rsSelect").eof
				Session("rsSelect").Delete
			Session("rsSelect").movenext
			loop
		end if
		Session("rsSelect").filter = ""
		'get loop count
		intLoop = Request.QueryString ("PageCount")
		for ctr=0 to intLoop - 1
			if trim(Request.Form ("ID"&ctr)) <> "" then
				Session("rsSelect").filter = "style = '"& Request.Form ("ID"&ctr) &"'"
				if Session("rsSelect").eof then
					Session("rsSelect").addnew
						Session("rsSelect")("style") = Request.Form ("ID"&ctr)
						Session("rsSelect")("NO") = myPage
					Session("rsSelect").update
				end if
				Session("rsSelect").filter = ""
			end if
		next
	End if
	if Session("rsSelect").recordcount = 0 then
		Response.Write "<center><font class=MessageFont>"
		Response.Write "<font size=2>No records selected!<br>"
		Response.Write ("<a href=""javascript:window.history.back()"" class=link>Back</a></font>")
		Response.End
	else
		Session("rsSelect").movefirst()
				
		do while not Session("rsSelect").eof 
			strStyle="'"& Session("rsSelect")("style") &"'"&","&strStyle
		Session("rsSelect").movenext
		loop 
	end if
	'Response.Write "<font size=2>"&Session("rsSelect").recordcount&"<br>"
	if len(strStyle) > 0 then
		strStyle = mid(strStyle,1,len(strstyle)-1)
		strStyle = "style,"& strStyle
	end if
	'build condition on selected styles[start]
	Dim strNames, arrNamesList, intListNO
	if Session("rsSelect").RecordCount > 0 then
		Session("rsSelect").MoveFirst 
		intListNO = Int(Session("rsSelect").RecordCount / 24) + 1
		Redim arrNamesList(intListNO - 1)
		for i = 0 to UBound(arrNamesList)
			strNames= ""
			for j = (i*24)+1 to (i+1)*24
				if j <= Session("rsSelect").RecordCount then
					strNames="'"& Session("rsSelect")("style") &"'"&","&strNames
					Session("rsSelect").MoveNext
				Else
					exit for
				End if
			Next
			strNames = mid(strNames,1,len(strNames)-1)
			strNames = "style,"& strNames
			'Response.Write strnames&"------<br>"
			arrNamesList(i) = strNames
		Next
		Session("rsSelect").MoveFirst 
	End if

	'Response.End 
	'build condition on selected styles[end]
	Set conn=server.CreateObject("ADODB.connection")
	conn.Open Application("DataConnectionString")

	If Len(Request("CurNo")) = 0 Then

	  Set Session("RSStyle") = Server.CreateObject("ADODB.RECORDSET")
	 
		
	    strSQL2="SELECT * FROM Style WHERE status+cstygroup like 'A%' And("'and inlist (" & strStyle & ") ORDER BY Style"
	    for i = 0 to UBound(arrNamesList)
			If Len(Trim(arrNamesList(i))) > 0 Then
				if i = UBound(arrNamesList) then
					strSQL2 = strSQL2 & "INLIST (" & arrNamesList(i) & ")"
				else
					strSQL2 = strSQL2 & "INLIST (" & arrNamesList(i) & ") or "
				end if
			End If
		Next
		strSQL2 = strSQL2 & ") order by Style"	
		'Response.Write "<font size=2>"&strSQL2&"<br>"
		'Response.End 
	    Session("RSStyle").Open(strSQL2),conn,2,4
	Else
	  Session("FrstRec") = "NO"
	  Set Session("RSStyle") = Session("RecStyle")
	End If
	Response.Redirect "otsReport.asp"  
End if
%>

</BODY>
</HTML>