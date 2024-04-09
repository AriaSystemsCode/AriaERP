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
<TR>
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=Session("rsCust")("btname")%></P>
	</TD>
	<TD align=right><a href="repcust.asp">Get <%=session("CustField")%></a></TD>
	<!-- ARD -->
</TR>
</table>
<%End IF%>

<Table width=95% align=center height=50 border=1>
<TR>
<TD class=Title>Check OTS</TD>
</TR></Table>

<%  
'Session("ConnectionString") = Application("DataConnectionString")
session("ConnectionString") =  "Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("DataPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"

Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open(Session("ConnectionString"))


If Len(Request("CurNo")) = 0 Then
  Session("Ware")     = Request.Form("Ware")
  Session("Option")   = Request.Form("Option")
  Session("Style")    = Request.Form("Style")
  Session("Color")    = Request.Form("Color")
  Session("Season")   = Request.Form("Season")
  Session("Group")    = Request.Form("Group")
  Session("CustName") = UCASE(Request.Form("CustName"))
End If


' get the style and color in order to display them in the form
IF session("RSStyStruct").EOF And session("RSStyStruct").BOF Then
Else							
	session("RSStyStruct").MoveFirst
	stylevalue=""
	strTemp = ""
	strTemp = Request.Form( Trim(session("RSStyStruct").fields("cisegsdes")) ) 
	if (trim(strTemp))="" then 
	stylevalue="%"
	else
	stylevalue=styvalue & trim(strTemp) & "%" 
	end if
	DO While Not session("RSStyStruct").Eof
	
	stylevalue=stylevalue & trim(session("RSStyStruct").fields("cisegsepr"))
	session("RSStyStruct").MoveNext
	if session("RSStyStruct").eof then
	else
		strTemp = Request.Form( Trim(session("RSStyStruct").fields("cisegsdes")) ) 
		if (trim(strTemp))="" then
		' modifie
		stylevalue = stylevalue & "%"
		' end modifie
		else
		stylevalue = stylevalue & trim(strTemp) & "%"
		end if
	end if
	Loop
End IF

strSQL = "Select * From StyDye Where Style like '" & stylevalue  & "' "
'Set RSStyDye = Conn.Execute(strSQL)

If Not Len(Request("CurNo")) = 0 Then
  Session("Direct") = Mid(Request("CurNo"),1,1)
Else
  Session("Direct") = "N"
End If

Dim lcStr1
lcStr1 = "" 
If Not Len(Session("Style")) = 0 Then 
  lcStr1 = "Style.cStyMajor='" & UCASE(Session("Style")) & "'" 
End If 

If Not Len(Session("Color")) = 0 Then 
  If Not Len(lcStr1) = 0 Then 
    lcStr1 = lcStr1 + "AND Style LIKE '%" & UCASE(Session("Color")) & "%'" 
  Else 
    lcStr1 = "Style LIKE '%" & UCASE(Session("Color")) & "%'" 
  End If 
End If 

if trim(stylevalue)=""then
else
lcStr1 = "Style.Style LIKE '" & Ucase(stylevalue) & "'" 
end if

If Not UCASE(Session("Season")) = "ALL" and Session("Season")<> "*" and Session("Season")<> "" Then 
  If Not Len(lcStr1) = 0 Then 
    lcStr1 = lcStr1 + " AND Style.Season='" & UCASE(Session("Season")) & "'" 
  Else 
    lcStr1 = "Style.Season='" & UCASE(Session("Season")) & "'" 
  End If 
End If 
'Response.Write "<font size=2>SS="& lcStr1
'Response.End 
  If Not UCASE(Session("Group")) = "ALL" Then 
    If Not Len(lcStr1) = 0 Then 
	  lcStr1 = lcStr1 + "AND Style.cStyGroup='" & UCASE(Session("Group")) & "'" 
	Else 
	  lcStr1 = "Style.cStyGroup='" & UCASE(Session("Group")) & "'" 
	End If 
  End If 
  
If Len(Request("CurNo")) = 0 Then
  Session("FrstRec") = "YES"
  If Not Len(lcStr1) = 0 Then 
    Set Session("RecStyle") = CreateObject("ADODB.RECORDSET")
    Set RSStyle = Session("RecStyle")
    RSStyle.ActiveConnection= Conn
    RSStyle.CursorType = 3
   strSQL2="SELECT * FROM Style WHERE " & lcStr1 & " and status+cstygroup like 'A%' ORDER BY Style"
 
   RSStyle.Open(strSQL2)
  Else 
    Set Session("RecStyle") = CreateObject("ADODB.RECORDSET")
	  
    Set RSStyle = Session("RecStyle")
    RSStyle.ActiveConnection = Conn
    RSStyle.CursorType = 3
    RSStyle.Open("SELECT * FROM Style where status+cstygroup like 'A%' ORDER BY Style")
  End If 
  'response.write("<font size=3>" & "SELECT * FROM Style WHERE " & lcStr1 & " ORDER BY Style" & "<br>")
  Session("RecCnt") = RSStyle.RecordCount
Else
  Session("FrstRec") = "NO"
  Set RSStyle = Session("RecStyle")
End If
'Response.Write "<font size=2>"&strsql2
'Response.End   
  %>
<BR>
<table border="0" width=95% cellspacing="0" cellpadding="0" align=center>
  <tr>
    <td width="100%" ><strong>Style Information :</strong></td>
  </tr>
</table>

<% 
  If Session("Direct") = "P" Then 
	Session("FrstRec") = "NO"
		If RSStyle.Eof Then 
		  If Session("RecCnt") <= 21 Then 
		      RSStyle.MoveFirst
		    Session("FrstRec") = "YES"
		  Else
		    RSStyle.Move(-20)
		  End If
		Else
		  If RSStyle.Bookmark <= 21 Then
		      RSStyle.MoveFirst
		    Session("FrstRec") = "YES"
		  Else
		    RSStyle.Move(-20)
		  End If
		End If
  End If

  Dim llOpenScal , J
  llOpenScal = 0 
  If Not RSStyle.Eof Then 
	J = 1
    Do While Not RSStyle.Eof
	  If J = 11 Then
			Exit Do
	  End If
%>

<table  bordercolor="#aecae6" width=95% cellspacing="0" cellpadding="0" border="0" align=center>
<tr>
	
	<td>
		<table border="1" bordercolor="#111111" width="100%" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
        <tr>
			<td Align="left" width="21%" class="dark_cell">Style</td>
            <td Align="left" width="13%" class="dark_cell">Season</td>
            <td Align="left" width="13%" class="dark_cell">Group</td>
            <td Align="left" width="6%" class="dark_cell" align=right>Price</td>
            <td colspan="6" Align="left" class="dark_cell">Description</td>
		</tr>
        <tr>
			<td Align="left" width="21%" class="light_cell"><% Response.Write(RSStyle("Style")) %></a>&nbsp;</td>
            <td Align="left" width="13%" class=light_cell>
				<%
                    'Write  Season
				     set RSgetseason = server.CreateObject("ADODB.RECORDSET")
				     strsqly = "select cdiscrep from Codes Where cdefcode+crltfield+cfld_name = 'NNSEASON' And ccode_no = '"& RSStyle("Season") &"'"
				    RSgetseason.open strsqly,conn
				    IF Not(RSgetseason.EOF And RSgetseason.BOF )Then
							Response.Write(RSgetseason("cdiscrep"))
				    End IF
				    RSgetseason.close
				    %>
                 
			&nbsp;</td>
            <td Align="left" width="13%" class=light_cell>
                <%' Write Group
                   set RSgetgroup = server.CreateObject("ADODB.RECORDSET")
                    strsqly = "select cdiscrep from Codes Where cdefcode+crltfield+cfld_name = 'NNCSTYGROUP' And ccode_no = '"& RSStyle("cStyGroup") &"'"
                    RSgetgroup.open strsqly,conn
                    IF Not(RSgetgroup.EOF And RSgetgroup.BOF ) Then
						Response.Write(RSgetgroup("cdiscrep"))
                    End IF
                    RSgetgroup.close
                 %>
                
			&nbsp;</td>
            <td Align="right" width="6%" class=light_cell><%if Session("CurrencyAlign")="LEFT" then Response.Write(Session("Currency") & FormatNumber(RSStyle("PriceA"))) else Response.Write(FormatNumber(RSStyle("PriceA")) & Session("Currency")) end if %></td>	
            <!--<td Align="left" valign="middle" width="20%" bgcolor="Ivory"><% Response.Write(RSStyle("Desc")) %></td>-->
            <td colspan="6" Align="left" class=light_cell><% Response.Write(RSStyle("Desc1")) %>&nbsp;</td>
		</tr>
		</table>
         <table border="1" bordercolor="#111111" width="100%" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
        <% Set RSScale = Conn.Execute("SELECT * FROM Scale WHERE Type+scale+prepak = 'S"&RSStyle("Scale")&"'") %> 
        <tr>
			<td Align="left" width="21%" class="dark_cell">Size</td>
            <td Align="right" width="9%" class="dark_cell"><% Response.Write(RSScale("Sz1")) %>&nbsp;</td>
            <td Align="right" width="9%" class="dark_cell"><% Response.Write(RSScale("Sz2")) %>&nbsp;</td>
            <td Align="right" width="9%" class="dark_cell"><% Response.Write(RSScale("Sz3")) %>&nbsp;</td>
            <td Align="right" width="9%" class="dark_cell"><% Response.Write(RSScale("Sz4")) %>&nbsp;</td>
            <td Align="right" width="9%" class="dark_cell"><% Response.Write(RSScale("Sz5")) %>&nbsp;</td>
            <td Align="right" width="9%" class="dark_cell"><% Response.Write(RSScale("Sz6")) %>&nbsp;</td>
            <td Align="right" width="9%" class="dark_cell"><% Response.Write(RSScale("Sz7")) %>&nbsp;</td>
            <td Align="right" width="8%" class="dark_cell"><% Response.Write(RSScale("Sz8")) %>&nbsp;</td>
            <td Align="right" width="8%" class="dark_cell">Total</td>
		</tr>
		  <% 
			If Not UCASE(Session("Ware")) = "ALL" And Not Len(Trim(Session("Ware"))) = 0 Then 
			  Set RSStyDye = Conn.Execute("Select * From StyDye Where style+cwarecode+dyelot LIKE '" & RSStyle("Style") & "' And cWareCode= '" & UCASE(Session("Ware")) & "%'")
			  If Not RSStyDye.Eof Then
	      %>
        <tr>
	       <td Align="left" width="19%" class="dark_cell">Immediate OTS</td>
           <td Align="right" width="9%" class="light_cell">
			<%if (INT(CLng(RSStyDye("Stk1")) - CLng(RSStyDye("Ord1")))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyDye("Stk1")) - CLng(RSStyDye("Ord1")))) %>
			<%end if%>
           &nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
            <%if (INT(CLng(RSStyDye("Stk2")) - CLng(RSStyDye("Ord2")))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyDye("Stk2")) - CLng(RSStyDye("Ord2")))) %>
			<%end if%>
           &nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
            <%if (INT(CLng(RSStyDye("Stk3")) - CLng(RSStyDye("Ord3")))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyDye("Stk3")) - CLng(RSStyDye("Ord3")))) %>
			<%end if%>
            &nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
            <%if (INT(CLng(RSStyDye("Stk4")) - CLng(RSStyDye("Ord4")))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyDye("Stk4")) - CLng(RSStyDye("Ord4")))) %>
			<%end if%>
			&nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
            <%if (INT(CLng(RSStyDye("Stk5")) - CLng(RSStyDye("Ord5")))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyDye("Stk5")) - CLng(RSStyDye("Ord5")))) %>
			<%end if%>
           &nbsp;</td>
		   <td Align="right" width="9%" class="light_cell">
		    <%if (INT(CLng(RSStyDye("Stk6")) - CLng(RSStyDye("Ord6")))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyDye("Stk6")) - CLng(RSStyDye("Ord6")))) %>
			<%end if%>
		    &nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
            <%if (INT(CLng(RSStyDye("Stk7")) - CLng(RSStyDye("Ord7")))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyDye("Stk7")) - CLng(RSStyDye("Ord7")))) %>
			<%end if%>
            &nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
            <%if (INT(CLng(RSStyDye("Stk8")) - CLng(RSStyDye("Ord8")))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyDye("Stk8")) - CLng(RSStyDye("Ord8")))) %>
			<%end if%>
            &nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
           <% Response.Write(INT(CLng(RSStyDye("TotStk")) - CLng(RSStyDye("TotOrd")))) %>&nbsp;</td>
		</tr>
        <tr>
                
            <td Align="left" width="19%" class="dark_cell">
            <% If CLng(RSStyDye("Stk1"))=0 And CLng(RSStyDye("Stk2"))=0 And CLng(RSStyDye("Stk3"))=0 And CLng(RSStyDye("Stk4"))=0 And CLng(RSStyDye("Stk5"))=0 And CLng(RSStyDye("Stk6"))=0 And CLng(RSStyDye("Stk7"))=0 And CLng(RSStyDye("Stk8"))=0 And CLng(RSStyDye("Wip1"))=0 And CLng(RSStyDye("Wip2"))=0 And CLng(RSStyDye("Wip3"))=0 And CLng(RSStyDye("Wip4"))=0 And CLng(RSStyDye("Wip5"))=0 And CLng(RSStyDye("Wip6"))=0 And CLng(RSStyDye("Wip7"))=0 And CLng(RSStyDye("Wip8"))=0 And CLng(RSStyDye("Ord1"))=0 And CLng(RSStyDye("Ord2"))=0 And CLng(RSStyDye("Ord3"))=0 And CLng(RSStyDye("Ord4"))=0 And CLng(RSStyDye("Ord5"))=0 And CLng(RSStyDye("Ord6"))=0 And CLng(RSStyDye("Ord7"))=0 And CLng(RSStyDye("Ord8"))=0 Then %> O.T.S. <% Else %> 
										
				<a href="javascript:openwindow('styOTS.asp?StyNo=<%=RSStyle("Style")%>')">O.T.S.</a>
					
				<% End If %> 
            </td>
            <td Align="right" width="9%" class="light_cell">
				<%if (INT(CLng(RSStyDye("Stk1")) + CLng(RSStyDye("Wip1")) - CLng(RSStyDye("Ord1")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyDye("Stk1")) + CLng(RSStyDye("Wip1")) - CLng(RSStyDye("Ord1")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
               <%if (INT(CLng(RSStyDye("Stk2")) + CLng(RSStyDye("Wip2")) - CLng(RSStyDye("Ord2")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyDye("Stk2")) + CLng(RSStyDye("Wip2")) - CLng(RSStyDye("Ord2")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
				<%if (INT(CLng(RSStyDye("Stk3")) + CLng(RSStyDye("Wip3")) - CLng(RSStyDye("Ord3")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyDye("Stk3")) + CLng(RSStyDye("Wip3")) - CLng(RSStyDye("Ord3")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
               <%if (INT(CLng(RSStyDye("Stk4")) + CLng(RSStyDye("Wip4")) - CLng(RSStyDye("Ord4")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyDye("Stk4")) + CLng(RSStyDye("Wip4")) - CLng(RSStyDye("Ord4")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
               <%if (INT(CLng(RSStyDye("Stk5")) + CLng(RSStyDye("Wip5")) - CLng(RSStyDye("Ord5")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyDye("Stk5")) + CLng(RSStyDye("Wip5")) - CLng(RSStyDye("Ord5")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
               <%if (INT(CLng(RSStyDye("Stk6")) + CLng(RSStyDye("Wip6")) - CLng(RSStyDye("Ord6")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyDye("Stk6")) + CLng(RSStyDye("Wip6")) - CLng(RSStyDye("Ord6")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
               <%if (INT(CLng(RSStyDye("Stk7")) + CLng(RSStyDye("Wip7")) - CLng(RSStyDye("Ord7")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyDye("Stk7")) + CLng(RSStyDye("Wip7")) - CLng(RSStyDye("Ord7")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
               <%if (INT(CLng(RSStyDye("Stk8")) + CLng(RSStyDye("Wip8")) - CLng(RSStyDye("Ord8")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyDye("Stk8")) + CLng(RSStyDye("Wip8")) - CLng(RSStyDye("Ord8")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyDye("TotStk")) + CLng(RSStyDye("TotWip")) - CLng(RSStyDye("TotOrd")))) %>&nbsp;</td>
		</tr>
            <% Else %>
        <tr>
                <td Align="left" width="19%" class="dark_cell">Immediate OTS</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
            </tr>
            <tr>
                <td Align="left" width="19%" class="dark_cell">O.T.S.</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
                <td Align="right" width="9%" class="light_cell">0</td>
		</tr>
            <% End If %><% Else %>
        <tr>
            <td Align="left" width="19%" class="dark_cell"><strong>Immediate OTS</strong></td>
            <td Align="right" width="9%" class="light_cell">
			<%if INT(CLng(RSStyle("Stk1")) - CLng(RSStyle("Ord1"))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyle("Stk1")) - CLng(RSStyle("Ord1")))) %>
			<%end if%>
           &nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
            <%if INT(CLng(RSStyle("Stk2")) - CLng(RSStyle("Ord2"))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyle("Stk2")) - CLng(RSStyle("Ord2")))) %>
			<%end if%>
           &nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
            <%if INT(CLng(RSStyle("Stk3")) - CLng(RSStyle("Ord3"))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyle("Stk3")) - CLng(RSStyle("Ord3")))) %>
			<%end if%>
            &nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
            <%if INT(CLng(RSStyle("Stk4")) - CLng(RSStyle("Ord4"))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyle("Stk4")) - CLng(RSStyle("Ord4")))) %>
			<%end if%>
			&nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
            <%if INT(CLng(RSStyle("Stk5")) - CLng(RSStyle("Ord5"))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyle("Stk5")) - CLng(RSStyle("Ord5")))) %>
			<%end if%>
           &nbsp;</td>
		   <td Align="right" width="9%" class="light_cell">
		    <%if INT(CLng(RSStyle("Stk6")) - CLng(RSStyle("Ord6"))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyle("Stk6")) - CLng(RSStyle("Ord6")))) %>
			<%end if%>
		    &nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
            <%if INT(CLng(RSStyle("Stk7")) - CLng(RSStyle("Ord7"))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyle("Stk7")) - CLng(RSStyle("Ord7")))) %>
			<%end if%>
            &nbsp;</td>
           <td Align="right" width="9%" class="light_cell">
            <%if INT(CLng(RSStyle("Stk8")) - CLng(RSStyle("Ord8"))) = 0 then%>
				&nbsp
			<%else%>
				<% Response.Write(INT(CLng(RSStyle("Stk8")) - CLng(RSStyle("Ord8")))) %>
			<%end if%>
            &nbsp;</td>
                <!--td Align="right" width="9%" class="light_cell">
                <% Response.Write(INT(CLng(RSStyle("Stk1")) - CLng(RSStyle("Ord1")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk2")) - CLng(RSStyle("Ord2")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk3")) - CLng(RSStyle("Ord3")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk4")) - CLng(RSStyle("Ord4")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk5")) - CLng(RSStyle("Ord5")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk6")) - CLng(RSStyle("Ord6")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk7")) - CLng(RSStyle("Ord7")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk8")) - CLng(RSStyle("Ord8")))) %>&nbsp;</td-->
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("TotStk")) - CLng(RSStyle("TotOrd")))) %>&nbsp;</td>
        </tr>
   
        <tr>
                
            <td Align="left" width="19%" class="dark_cell">
            <% If CLng(RSStyle("Stk1"))=0 And CLng(RSStyle("Stk2"))=0 And CLng(RSStyle("Stk3"))=0 And CLng(RSStyle("Stk4"))=0 And CLng(RSStyle("Stk5"))=0 And CLng(RSStyle("Stk6"))=0 And CLng(RSStyle("Stk7"))=0 And CLng(RSStyle("Stk8"))=0 And CLng(RSStyle("Wip1"))=0 And CLng(RSStyle("Wip2"))=0 And CLng(RSStyle("Wip3"))=0 And CLng(RSStyle("Wip4"))=0 And CLng(RSStyle("Wip5"))=0 And CLng(RSStyle("Wip6"))=0 And CLng(RSStyle("Wip7"))=0 And CLng(RSStyle("Wip8"))=0 And CLng(RSStyle("Ord1"))=0 And CLng(RSStyle("Ord2"))=0 And CLng(RSStyle("Ord3"))=0 And CLng(RSStyle("Ord4"))=0 And CLng(RSStyle("Ord5"))=0 And CLng(RSStyle("Ord6"))=0 And CLng(RSStyle("Ord7"))=0 And CLng(RSStyle("Ord8"))=0 Then %> O.T.S. <% Else %> 
										
				<a href="javascript:openwindow('styOTS.asp?StyNo=<%=RSStyle("Style")%>')">O.T.S.</a>
					
				<% End If %> 
            </td>
            <td Align="right" width="9%" class="light_cell">
				<%if (INT(CLng(RSStyle("Stk1")) + CLng(RSStyle("Wip1")) - CLng(RSStyle("Ord1")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyle("Stk1")) + CLng(RSStyle("Wip1")) - CLng(RSStyle("Ord1")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
               <%if (INT(CLng(RSStyle("Stk2")) + CLng(RSStyle("Wip2")) - CLng(RSStyle("Ord2")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyle("Stk2")) + CLng(RSStyle("Wip2")) - CLng(RSStyle("Ord2")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
				<%if (INT(CLng(RSStyle("Stk3")) + CLng(RSStyle("Wip3")) - CLng(RSStyle("Ord3")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyle("Stk3")) + CLng(RSStyle("Wip3")) - CLng(RSStyle("Ord3")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
               <%if (INT(CLng(RSStyle("Stk4")) + CLng(RSStyle("Wip4")) - CLng(RSStyle("Ord4")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyle("Stk4")) + CLng(RSStyle("Wip4")) - CLng(RSStyle("Ord4")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
               <%if (INT(CLng(RSStyle("Stk5")) + CLng(RSStyle("Wip5")) - CLng(RSStyle("Ord5")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyle("Stk5")) + CLng(RSStyle("Wip5")) - CLng(RSStyle("Ord5")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
               <%if (INT(CLng(RSStyle("Stk6")) + CLng(RSStyle("Wip6")) - CLng(RSStyle("Ord6")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyle("Stk6")) + CLng(RSStyle("Wip6")) - CLng(RSStyle("Ord6")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
               <%if (INT(CLng(RSStyle("Stk7")) + CLng(RSStyle("Wip7")) - CLng(RSStyle("Ord7")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyle("Stk7")) + CLng(RSStyle("Wip7")) - CLng(RSStyle("Ord7")))) %>
				<%end if%>
            &nbsp;</td>
            <td Align="right" width="9%" class="light_cell">
               <%if (INT(CLng(RSStyle("Stk8")) + CLng(RSStyle("Wip8")) - CLng(RSStyle("Ord8")))) = 0 then%>
					&nbsp
				<%else%>
					<% Response.Write(INT(CLng(RSStyle("Stk8")) + CLng(RSStyle("Wip8")) - CLng(RSStyle("Ord8")))) %>
				<%end if%>
            &nbsp;</td>
                <!--td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk1")) + CLng(RSStyle("Wip1")) - CLng(RSStyle("Ord1")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk2")) + CLng(RSStyle("Wip2")) - CLng(RSStyle("Ord2")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk3")) + CLng(RSStyle("Wip3")) - CLng(RSStyle("Ord3")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk4")) + CLng(RSStyle("Wip4")) - CLng(RSStyle("Ord4")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk5")) + CLng(RSStyle("Wip5")) - CLng(RSStyle("Ord5")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk6")) + CLng(RSStyle("Wip6")) - CLng(RSStyle("Ord6")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk7")) + CLng(RSStyle("Wip7")) - CLng(RSStyle("Ord7")))) %>&nbsp;</td>
                <td Align="right" width="9%" class="light_cell"><% Response.Write(INT(CLng(RSStyle("Stk8")) + CLng(RSStyle("Wip8")) - CLng(RSStyle("Ord8")))) %>&nbsp;</td-->
                <td Align="right" width="9%" class="light_cell">
                <% Response.Write(INT(CLng(RSStyle("TotStk")) + CLng(RSStyle("TotWip")) - CLng(RSStyle("TotOrd")))) %>&nbsp;</td>
         </tr>
            <% End If %>
        </table>
      </td>
    </tr>
    
</table>
<p><br>
<br>
<% 
    RSStyle.MoveNext 
	J = J + 1
  Loop 
%> </p>
<p></p>
<!-- premier bloc-->
<blockquote><center><font size="2" face="Arial">
    <% If Session("FrstRec") = "NO" Then %>
		<a href="ActionOTS.asp?CurNo=<% = "P" %>"><p><img src="../images/<%=Session("Theme")%>/back.gif" border=0></a>
    <% End If %> 
    <% If Not RSStyle.Eof Then %>
		 <a href="ActionOTS.asp?CurNo=<% = "N" %>">
		 <img src="../images/<%=Session("Theme")%>/next.gif" border=0><a>
    <% End If %>  </p>
</center></blockquote>
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
    RSStyle.Close 
    RSStyDye.Close
    Conn.Close 
    Set RSStyle  = Nothing 
    Set RSStyDye = Nothing 
    If llOpenScal = 1 Then 
      Set RSScale  = Nothing 
    End If
  End If
%>
</BODY>
</HTML>