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
			
			document.location.href = "otsReport.asp?viewer=ActiveX&Select=<%=Request("Select")%>";
		}
	else
		{
			
			document.location.href = "otsReport.asp?viewer=Java-Plug-in&Select=<%=Request("Select")%>";
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
<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><BR></p>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><BR></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>

</table>
<%End IF%>

<Table width=95% align=center height=50 border=1>
	<TR>
		<TD class=Title>Check OTS</TD>
	</TR>
</Table>

<% 
'RecordSet that will send to the Report


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


Dim lcStr1
lcStr1 = "" 

Const NumPerPage  = 25

Dim Page
If Request.QueryString("Page") = "" then
    Page = 1 'We're on the first page
Else
    Page = Request.QueryString("Page")
End If
If Not UCASE(Session("Season")) = "ALL" Then 
  If Not Len(lcStr1) = 0 Then 
    lcStr1 = lcStr1 + "AND Style.Season='" & UCASE(Session("Season")) & "'" 
  Else 
    lcStr1 = "Style.Season='" & UCASE(Session("Season")) & "'" 
  End If 
End If 

If Not UCASE(Session("Group")) = "ALL" Then 
  If Not Len(lcStr1) = 0 Then 
    lcStr1 = lcStr1 + "AND Style.cStyGroup='" & UCASE(Session("Group")) & "'" 
  Else 
    lcStr1 = "Style.cStyGroup='" & UCASE(Session("Group")) & "'" 
  End If 
End If 

Dim Count
Count = 0
If Len(Request("CurNo")) = 0 Then
  Session("FrstRec") = "YES"
  Set Session("RSStyle") = Server.CreateObject("ADODB.RECORDSET")
  Session("RSStyle").CursorLocation = 2
  Session("RSStyle").CacheSize = NumPerPage
  Session("RSStyle").CursorType = 3
  if request("Select") = "A" then
	set Session("RSStyle") = Session("RSSTyResult")
	Session("RSStyle").PageSize = NumPerPage
	TotalPages = Session("RSStyle").PageCount 
	Session("RSStyle").AbsolutePage = Page
  else
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
   
    Session("RSStyle").PageSize = NumPerPage
	TotalPages = Session("RSStyle").PageCount 
	Session("RSStyle").AbsolutePage = Page 
	'Response.Write "<font size=2>"&Session("RSStyle").recordcount&"<br>"
  end if
Else
  Session("FrstRec") = "NO"
  Set Session("RSStyle") = Session("RecStyle")
End If
  
%>
<br>
<Table width=95% border=0 align=center> 
	<tr>
		<TD align=center>
			<input type=button onclick="javascript:GoReport();" value="Print Friendly Report">
		</TD>
	</tr>
</Table>

<BR>
<table border="0" width=95% cellspacing="0" cellpadding="0" align=center>
  <tr>
    <td width="100%" ><strong>Style Information :</strong></td>
  </tr>
</table>
<form name=frm method=post>
<%'Response.Write "<font size=2>"&Session("RSStyle").recordcount

If Not Session("RSStyle").Eof Then 
	strDesc = ""
	Do While Not Session("RSStyle").Eof And Count < NumPerPage
	
%>
	<table  bordercolor="#aecae6" width=95% cellspacing="0" cellpadding="0" border="0" align=center>
	
	<tr>
	<td>
		<%if strDesc <> Session("RSStyle")("cstymajor") then%>
			<table border="1" bordercolor="#111111" width="100%" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
			<% Set RSScale = Conn.Execute("SELECT * FROM Scale WHERE Type+scale+prepak = 'S"&Session("RSStyle")("Scale")&"'") 
			   SzCount = RSScale("cnt") 
			%> 
			<tr>
				<td Align="left" width="15%" class="light_cell"><b>Style/     Size</b></td>
				<%For inti = 1 to 8'Cdbl(SzCount)
					'wal_skip the size"ASST"
					if trim(RSScale("Sz"&inti)) = "ASST" then
						strTemp = "<TD Align=right width=10% class=light_cell>&nbsp;</TD>"
						Response.Write(strTemp)
					else
						strTemp = "<TD Align=right width=10% class=light_cell><b>" &RSScale("Sz"&inti)& "</B>&nbsp;</TD>"
						Response.Write(strTemp)
					end if
				  Next 
				%>
			 </tr>
			 <tr><td  class="dark_cell" colspan=9><%=Session("RSStyle")("Desc")%></td></tr>
			 
			 </table>
		<%end if%>
		<!--table border="1" bordercolor="#111111" width="100%" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	    <tr>
			<td Align="left" width="15%" class="dark_cell">Style</td>
	        <td colspan="6" Align="left" class="dark_cell">Description</td>
		</tr>
	    <tr>
			<td Align="left" width="21%" class="light_cell"><% Response.Write(Session("RSStyle")("Style")) %></a>&nbsp;</td>
	        <td colspan="6" Align="left" class=light_cell><% Response.Write(Session("RSStyle")("Desc1")) %>&nbsp;</td>
		</tr>
		</table-->
	   <table border="1" bordercolor="#111111" width="100%" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
		<%
		'WAL_calc of ots display[start]
	    Dim rsOtsChk
	    set rsOtsChk = server.CreateObject ("ADODB.Recordset")
		strSQl = "Select (style.stk1 - sum(ordline.qty1)) as sum1, (style.stk2 - sum(ordline.qty2)) as sum2, "& _
				 "(style.stk3 - sum(ordline.qty3)) as sum3, (style.stk4 - sum(ordline.qty4)) as sum4, "& _ 
				 "(style.stk5 - sum(ordline.qty5)) as sum5, (style.stk6 - sum(ordline.qty6)) as sum6, "& _
				 "(style.stk7 - sum(ordline.qty7)) as sum7, (style.stk8 - sum(ordline.qty8)) as sum8, "& _
				 "(style.stk1 + style.wip1 - sum(ordline.qty1)) as sum11, (style.stk2 + style.wip2 - sum(ordline.qty2)) as sum22, "& _
				 "(style.stk3 + style.wip3 - sum(ordline.qty3)) as sum33, (style.stk4 + style.wip4 - sum(ordline.qty4)) as sum44, "& _ 
				 "(style.stk5 + style.wip5 - sum(ordline.qty5)) as sum55, (style.stk6 + style.wip6 - sum(ordline.qty6)) as sum66, "& _
				 "(style.stk7 + style.wip7 - sum(ordline.qty7)) as sum77, (style.stk8 + style.wip8 - sum(ordline.qty8)) as sum88, "& _
				 "style.make from ordline, ordHdr, style where ordline.style+DTOS(ordline.complete)+ordline.cordtype+ordline.order+ordline.store+STR(ordline.lineno,6) like '" &Session("RSStyle")("Style")& "%' "& _
				 "and ordHdr.status <> 'C' and ordHdr.Status <> 'X' and ordline.order = ordHdr.order and ordline.style = style.style"
		rsOtsChk.Open strSQL, conn

		if rsOtsChk.EOF then'no orders for this style then get value in stock
			rsOtsChk.Close ()
			strSQl = "Select style.stk1 as sum1, style.stk2 as sum2, "& _
					 "style.stk3 as sum3, style.stk4 as sum4, "& _ 
					 "style.stk5 as sum5, style.stk6 as sum6, "& _
					 "style.stk7 as sum7, style.stk8 as sum8, "& _
					 "sum(style.stk1 + style.wip1) as sum11, sum(style.stk2 + style.wip2) as sum22, "& _
					 "sum(style.stk3 + style.wip3) as sum33, sum(style.stk4 + style.wip4) as sum44, "& _ 
					 "sum(style.stk5 + style.wip5) as sum55, sum(style.stk6 + style.wip6) as sum66, "& _
					 "sum(style.stk7 + style.wip7) as sum77, sum(style.stk8 + style.wip8) as sum88, "& _
					 "style.make from style where style like '" &Session("RSStyle")("Style")& "' "
			
			rsOtsChk.Open strSQL, conn
		end if
		if not rsOtsChk.eof then
			Session("Make") = rsOtsChk("make")
		%>
		<tr>
			<td Align="left" width="15%" class="dark_cell"><% Response.Write(Session("RSStyle")("Style")) %></td>
			<%For i = 1 to 8'Cdbl(SzCount)
			  if trim(RSScale("Sz"&i)) = "" or trim(RSScale("Sz"&i)) = "ASST" then%>
				<TD width=10%>
				 	&nbsp
			<%else%>
				<%if cdbl(rsOtsChk("sum"&i)) > 0 then%>
					<TD width=10% class="light_cell" style="background-color: green">
					<input type=hidden name="txtType<%=i%>" value="green">
				<%elseif cdbl(rsOtsChk("sum"&i&i)) > 0 then%>
					<TD width=10% class="light_cell" style="background-color: orange">
					<input type=hidden name="txtType<%=i%>" value="orange">
				<%else%>
				 	<TD width=10% class="light_cell" align=center style="background-color: red">
				 	&nbsp
				<%end if
			   end if
			next
			%>
			
		</tr>	
		<%end if%> 
	    </table>
	  </td>
	</tr>
	</table>
	<% 
	strDesc = Session("RSStyle")("cstymajor")
	Count = Count + 1
    Session("RSStyle").MoveNext 
	Loop 
    %> 
<Table border=0 width=95% align=center>
<TR>
<TD>
<%

	IF count > 0 Then
		Response.Write("<Center><font face=Arial size=2>Page " & Page & " of " & TotalPages & "</font></center>")
		Response.Write "<table align=center border=0><tr><td>"
		if Page > 1 then
			'We are not at the beginning, show the back button%>
				<a href="javascript:document.frm.action='showAva.asp?Page=<%=Page - 1%>&Select=<%=Request("Select")%>&PageCount=<%=Count%>&CurPage=<%=myPage%>&Paging=P';document.frm.submit()"><img border=0 src="../images/<%=Session("theme")%>/back.gif"></a>
		<%End If

		if CInt(Page) <> CInt(TotalPages) then
		    'We are not at the end, show a next button%>
				<a href="javascript:document.frm.action='showAva.asp?Page=<%=Page+ 1%>&Select=<%=Request("Select")%>&PageCount=<%=Count%>&CurPage=<%=myPage%>&Paging=N';document.frm.submit()"><img border=0 src="../images/<%=Session("theme")%>/next.gif"></a>
		<%End If
	End IF
		%>
</TD>
</TR>
</Table>
</form>
	<br>
	<table  bordercolor="#aecae6" width=95% cellspacing="0" cellpadding="0" border="0" align=center>
	  <TR >
	    <TD width="20%" style="WIDTH: 20%">
			<!--WMA Lenged Table-->
			<table  bordercolor="#aecae6" width=100% cellspacing="0" cellpadding="0" border="0" align=center>
				<TR >
					<TD >
						<table border=1 bordercolor="#000000" align=left Width=270  cellpadding=0 cellspacing=0 style="border-collapse: collapse"> 
							<tr>
								<TD width="90"  style="background-color: green" align=center><font color=white>Available</font></TD>
								<TD width="90" style="background-color: orange" align=center><font color=white>In Progress</font></TD>
								<TD width="90" style="background-color: red" align=center><font color=white>Waiting List</font></TD>
							</tr>	
						</table>		
					</TD>
				</TR>
			</Table>
		</TD>
		<TD width="25%" colSpan=2 style="WIDTH: 25%"></TD>
	    <TD width="15%"></TD>
	    <TD width="20%" colSpan=2>
	      <P align=right>
	      <INPUT id=button3 name=button3 style="HEIGHT: 24px; WIDTH: 96px" 
					type="button" value="Back" onclick="javascript:window.history.back ();">&nbsp;
		</TD>
	   </TR>
	</Table>
	<br>
	<p><br>
	<br>
	</p>
	<p></p>
	<!-- premier bloc-->

	<% 
	  If Not (Session("Direct") = "N") AND Not (Session("Direct") = "P") Then
	    RSScale.Close 
	    llOpenScal = 1 
	  End If 
Else
%>
<font color="navy" face=Arial size=2><b><p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;There are no records matching the selected criteria... </p><b>

<% 
End If 
If Not (Session("Direct") = "N") AND Not (Session("Direct") = "P") Then
  If llOpenScal = 1 Then 
    Set RSScale  = Nothing 
  End If
End If
%>
</BODY>
</HTML>