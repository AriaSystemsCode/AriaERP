<%@ LANGUAGE="VBSCRIPT" %>
<% Response.CacheControl = "no-cache" 
 Response.AddHeader "Pragma", "no-cache" 
 Response.Expires = -1 
 Response.Buffer=true
'WAL_fill array wz selected groups only
IF arrFound = "" Then  arrFound = split(session("showcatalogval"),",") 
IF Trim(Session("ID")) = "" AND Trim(Session("rep"))= "" Then
%>
<script language="javascript">
	parent.location.href ="../login.asp"
</script>
<%END IF
'Session("ShowOTS") = "F"
IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	'IF Trim(Session("customerid")) = "" Then
		'Response.Redirect("../repcust.asp")
	'END IF
	strFile = "reb"
End IF
'if not Session("rsCust").eof  then
'	if Session("rsCust").fields("Status")="P" then
'		Response.Redirect("../Common/Msgs.asp")
'	end if
'end if

Set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

Set session("RSStyStruct") = nothing
Set session("RSStyStruct")=server.CreateObject("ADODB.recordset")

strSql="select * from icistru where citemrecty='U' order by cisegno"
session("RSStyStruct").open strSql,conn 

'wal_131300 check if there a style profile saved for the logged user
if trim(session("styProfile"))<> "" then
	'get the info for thAT profile
	dim cnnSQL, rsStyGroup
	set cnnSQL  = server.CreateObject ("Adodb.connection")
	cnnSQL.Open Application("SqlServer") 
	
	'set rsStyProfileHdr = server.CreateObject ("Adodb.recordset")
	'set rsStyProfileDt = server.CreateObject ("Adodb.recordset")
	set rsStyGroup = server.CreateObject ("Adodb.recordset")
	
	'rsStyProfileHdr.Open "select * styleprofileheader where cstyleprofilecode='"&trim(session("styProfile"))&"'",cnnSQL,1,3
	'rsStyProfileDt.Open "select * styleprofiledetail where cstyleprofilecode='"&trim(session("styProfile"))&"'",cnnSQL,1,3
	'rsStyGroup.Open "select distinct cstyleGroup from styleprofiledetail where cstyleprofilecode='"&trim(session("styProfile"))&"'",cnnSQL,1,3
	set rsStyGroup = server.CreateObject ("Adodb.recordset")
	arrStyle = split(trim(application("strStyles")),",")
	'if ubound(arrStyle) > 0 then
		strsql = "select distinct FOXStyle.cstyGroup from styleprofiledetail , OPENROWSET('MSDASQL', '"
		strSQL = strSQL + Application("DataConnectionString")		
		strSQL = strSQL + "', 'Select cstyGroup ,style From  style') FOXStyle where cStyleProfileCode='"&trim(session("styProfile"))&"' and FOXStyle.style = styleprofiledetail.cStyle COLLATE Latin1_General_CI_AS"
		'for i=0 to ubound(arrStyle)
			'strSql = strSql & "style =" & arrStyle(i)
			'if i <> UBound(arrStyle) then
				'strSql = strSql & " OR "
			'End if
					
		'next
		'strsql = strsql & " order by style"
		'Response.Write "<font size=2>"&strsql
		'Response.End 
		rsStyGroup.Open strsql,cnnSQL,1,3
	'rsStyGroup.Open "select distinct cstyGroup from style where style in (" &trim(application("strStyles"))& ")",conn,1,3
	if rsStyGroup.RecordCount = 1 then
		Group =  trim(rsStyGroup("cstyGroup"))
		'check to apply search 
		if Search= "" then
			Search = "Y"
		end if
	else
		Group = Request("Group")	
	end if
else
	'if Group = "" then
	Group = Request("Group")
end if

%>
<html>
<head><link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/order.css" TYPE="text/css">
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<title>CRM - Check O.T.S.</title>
</head>
<body >

<SCRIPT LANGUAGE=javascript>
<!--
function FindStySubmit(ots)
{
	var ots;
	if (ots == 'F')
	{
		document.form1.action = 'findstyle.asp?LoginType=T&firsttime=F';
		document.form1.submit();
	}
	else
	{
		document.form1.action = 'OTSredirect.asp?CALLER=OTS';
		document.form1.submit();

	}
}
function chkOts()
{
	var strEmpty;

	<%
		session("RSStyStruct").movefirst
		do While Not Session("RSStyStruct").Eof
			if Session("RSStyStruct")("lsegendmaj") = "True" then%>
				
				obj = eval(document.form1.<%=Trim(session("RSStyStruct").fields("cisegsdes"))%>);
				
				if (obj.value == '')
					strEmpty = true;
				else
					strEmpty = false;
				
	<%			exit do
			end if
		Session("RSStyStruct").movenext
		loop
	%>

	/*wma avoid have to select one style	
	if (strEmpty == true)
	{
		alert("Please select Style.");
		return false;
	}
	*/
}
//-->
</SCRIPT>
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
<!--TR>
	<TD colspan=13>
	
	</TD>
	<TD align=right><a href="repcust.asp" style="text-decoration: none">Get <%=session("CustField")%></a></TD>
	
</TR-->
</table>
<%End IF%>
<Table width=95% align=center height=50 border=1>
	<TR>
	<TD class=Title>Check OTS</TD>
	</TR>
</Table>

<%
'wal_130674 correct check on privilage case multi user
	if Application("CustomerLoginUsing")= "User"  then
		strAppUserVar = session("userID")
	ElseIf Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"CHECKOTS") <= 0 Then
%>
<Table border=0 width=95% align=center>
	<tr>
	<td class="Title"><font color="Yellow">
		You're not authorized to view this page</font>
	</td>
	</tr>
</Table>
<%Response.End 
End If
%>
<%
'ARD - Show O.T.S. [Start]
IF Session("M_OTS") = "N" Then
'ARD - Show O.T.S.
%>
	<CENTER>
	<table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
	    <tr>
			<td>
				<font face="Arial" size="2" color="#000080">
				<center><b><BR>No Open To Sell Available.</b></center>
				</font>
			</td>
		</tr>
	</table>
	</CENTER>
<%else%>
<%	Session("ConnectionString") = Application("DataConnectionString")
	Set Conn = Server.CreateObject("ADODB.Connection")
	Conn.Open(Session("ConnectionString"))
	Set RSSeason = Conn.Execute("Select * From Codes Where cDefCode+crltfield+cFld_Name = 'NNSEASON' Order By cCode_No")
	Set RSGroup  = Conn.Execute("Select * From Codes Where cDefCode+crltfield+cFld_Name = 'NNCSTYGROUP' and cdiscrep <> '' Order By cCode_No")
	Set RSMltWhr = Conn.Execute("Select Setups.mData_Def From Setups Where cFld_Name='M_WAREHOUS'")
	Set RSWare   = Conn.Execute("Select cwarecode,cdesc From WareHous Order By 1") 
	Dim lcMltWhr , lcSeason , lcCurWare
	lcMltWhr = UCASE(TRIM(RSMltWhr("mData_Def")))
%>
	<form <%if Session("ShowOTS") = "F" then%>action="showAva.asp?CurNo="<%else%>action="ActionOTS.asp?CurNo="<%end if%> method="POST" id=form1 name=form1>
	<%If lcMltWhr = "Y" And Not RSWare.Eof Then %>
		<p><font size="2" face="Aria"><strong> </p>
		<div align="center">
		  <center>
		  <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
		      <tr>
		  		<td width="100%" ><strong>Enter your searching criteria to select the Style(s) you want to check the open to sell quantities:</strong></td>
		  	</tr>
		  </table>
		  <table bordercolor="#111111" border="1" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
			<tr>	
		    <%Session("RSStyStruct").MoveFirst()
			  IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
			  Else
				strTemp = "<TD class=""dark_cell"" valign=bottom><strong>"
				Response.Write(strTemp)
				Session("RSStyStruct").MoveFirst
				DO While Not Session("RSStyStruct").Eof
				IF Len(Session("getstyle"))=0 Then
						strValue = ""
					Else
						strValue = Session(Trim(Session("RSStyStruct").fields("cisegsdes")))
					End IF
					strTemp = "" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "" & "&nbsp;&nbsp;&nbsp;<INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" '& strValue & ">"
					IF Trim(Request("Result")) = "" Then
						strTemp = strTemp & ">"
					ELSE
						strTemp =  strTemp & strValue & ">"
					END IF
					strTemp = strTemp & " " & Trim(Session("RSStyStruct").fields("cisegsepr")) & " "
					Response.Write(strTemp)
					Session("RSStyStruct").MoveNext
				Loop
				End IF
				Session("getStyle")=""%>
			    <INPUT type="submit" value="Get Style" id=submit1 name=submit1 onclick="return FindStySubmit('<%=Session("ShowOTS")%>')">
				</td>
		        
		            <tr>
		            <td Align="left" class="dark_cell">Group
			            <%'wal_131300 check if i've a tyle profile then display the list of grpoups per user
						if trim(session("styProfile"))<> "" then%>
								<SELECT name=Group size=1> 
									  <%if rsStyGroup.RecordCount  > 1 then%>
											<OPTION  value="ALL">All
									  <%end if%>
								
								<%IF Not RSGroup.EOF And Not RSGroup.BOF Then%>
									
									<%RSGroup.MoveFirst 
										do while not rsStyGroup.EOF 
											RSGroup.Filter = "ccode_no='"& trim(rsStyGroup("cstyGroup")) & space(6 - len(trim(rsStyGroup("cstyGroup")))) & "'"
														
											if not RSGroup.EOF then
												if Group = trim(rsStyGroup("cStyGroup")) Then
													Response.Write("<option value=""" & Trim(RSGroup.Fields("ccode_no").Value )& """ selected>" & RSGroup.Fields("cdiscrep").Value )
												Else
													Response.Write("<option value=""" & Trim(RSGroup.Fields("ccode_no").Value )& """>" & RSGroup.Fields("cdiscrep").Value )
												End IF
											end if
											RSGroup.Filter = ""
										rsStyGroup.MoveNext 
										loop
								END IF%>
								</SELECT>
							</td>	
						<%else%>
				            <select name="Group" size="1">
				            
			                <% lcGroup = " " 
								'wal_130731 check on style group per user
								if trim(session("StyGroup")) = "" then
									arrFound= split(session("showcatalogval"),",")
								else
									arrFound = split(trim(UCASE(session("StyGroup"))),",")
								end if
			                %>
				                <%if Not (RSGroup.Eof and RSGroup.Bof) then%>
									<%if ubound(arrFound) > 0 then%>
											<OPTION selected value="ALL">All
									<%end if%>
									<%
									
									'arrFound= split(session("showcatalogval"),",")
									for intcount =0 to ubound(arrFound)
									RSGroup.Filter = "ccode_no='" & UCase(arrFound(intcount)) & space(6 - len(arrFound(intcount))) & "'"
		'								Response.Write "<option>" & RSGroup.recordcount	& "</option>"	
									if Not rsGroup.eof then
										if Group =  arrFound(intcount) Then
											Response.Write("<option value=""" & Trim(RSGroup.Fields("ccode_no").Value )& """ selected>" &RSGroup("cCode_No") & " - "& RSGroup.Fields("cdiscrep").Value )
										Else
											Response.Write("<option value=""" & Trim(RSGroup.Fields("ccode_no").Value )& """>" & RSGroup.Fields("cdiscrep").Value )
										End IF
									end if 
									RSGroup.Filter = ""
									next
								else
									Response.Write "<option>END OF FILE</option>"	
								End if
								%>
				            </select>
			            <%end if%>
		            </td>
		            <td Align="left" class="dark_cell" >Locations</font>
						<select name="Ware" size="1">
						    <option selected value="ALL">All</option>
						    <% lcCurWare = " " %>
						    <% Do While Not RSWare.Eof %>
								<%lcCurWare = RSWare("cwarecode") & " - " & RSWare("cDesc") %>
								<option value="<% = RSWare("cwarecode") %>" <%if Ucase(application("WareCode")) = Ucase(RSWare("cwarecode")) then%> selected <%end if%>> <% = lcCurWare %> </option>
						    <% RSWare.MoveNext %>
						    <% Loop %>
						</select>
		            </td>
		       </tr>  
		       <%if Session("ShowOTS") <> "F" then%>
					<!-- wma unused feature -->	
					<tr><td Align="right"  class="dark_cell" colspan="2"><input type="submit" name="B2" value="Submit" onclick="return chkOts();"><input type="reset" name="B3" value="Reset"></td></tr>
			   <%end if%>
		    </table>
		    </center>
            </div>
		    </center>
		    <center>
		    </strong></font><p>&nbsp;</p>
	  <%Else%>
		    <p><font color="navy" size="1" face="Arial"><strong> </p>
		    <div align="center">
            <center>
		    <table bordercolor="#111111" border="1" width="95%" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
		        <tr>
		        <td Align="left" width="10%" class="dark_cell">Style Inquire</td>
				<%Session("RSStyStruct").MoveFirst()
				  IF Session("RSStyStruct").EOF And Session("RSStyStruct").BOF Then
				  Else
				  	strTemp = "<TD class=""dark_cell"" valign=bottom><strong>"
				  	Response.Write(strTemp)
				  	Session("RSStyStruct").MoveFirst
				  	DO While Not Session("RSStyStruct").Eof
				  		IF Len(Session("getstyle"))=0 Then
				  			strValue = ""
				  		Else
				  			strValue = Session(Trim(Session("RSStyStruct").fields("cisegsdes")))
				  		End IF
				  		strTemp = "" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "" & "&nbsp;&nbsp;&nbsp;<INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" '& strValue & ">"
				  		IF Trim(Request("Result")) = "" Then
				  			strTemp = strTemp & ">"
				  		ELSE
				  			strTemp =  strTemp & strValue & ">"
				  		END IF
				  		strTemp = strTemp & " " & Trim(Session("RSStyStruct").fields("cisegsepr")) & " "
				  		Response.Write(strTemp)
				  		Session("RSStyStruct").MoveNext
				  	Loop
				  End IF
				  Session("getStyle")= ""
				  %>
				  
		         
		            <td Align="left" width="25%" class="dark_cell">Group
						<select name="Group" size="1">
						    <option selected value="ALL">All</option>
						    <% lcGroup = " " %>
						    <% Do While Not RSGroup.Eof %>
								<%lcGroup = RSGroup("cCode_No") & " - " & RSGroup("cDiscrep") %>
								<option value="<% = RSGroup("cCode_No") %>" <%if Ucase(session("StyGroup")) = Ucase(RSGroup("cCode_No")) then%> selected <%end if%>> <% = lcGroup %> </option>
						    <% RSGroup.MoveNext %>
						    <% Loop %>
						</select>
		            </td>
		            <td Align="right" width="25%" class="dark_cell">
						
						<INPUT type="submit" value="Get Style(s)" id=submit1 name=submit1 onclick="return FindStySubmit('<%=Session("ShowOTS")%>')">
		            </td>
		        </tr>
		       
		       <%if Session("ShowOTS") <> "F" then%>
					<tr><td Align="center" class="dark_cell" colspan="4"><input type="submit" name="B2" value="Submit" onclick="return chkOts();"><input type="reset" name="B3" value="Reset"></td></tr>
			   <%end if%>
		    </table>
		      </center>
            </div>
		    </center>
		    </strong></font>
	 <%END IF%>
</form>
</body>
</html>
<% RSSeason.Close %>
<% RSGroup.Close %>
<% RSMltWhr.Close %>
<% RSWare.Close %>
<% 'Conn.Close %>
<% Set RSSeason = Nothing %>
<% Set RSGroup  = Nothing %>
<% Set RSMltWhr = Nothing %>
<% Set RSWare   = Nothing %>
<%end if%>