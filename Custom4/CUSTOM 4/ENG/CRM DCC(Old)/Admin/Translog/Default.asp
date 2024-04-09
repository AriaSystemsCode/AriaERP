<%@ Language=VBScript%>
<%
'WAL_05/17/2004 add code tp read values in setup file[start]
'get values seved in the file 
Dim strAppPath
Dim strFilePath

strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))

If Right(strAppPath,1) = "\" Then
	strFilePath = "admin\crmsetup\setup\setup.txt"
Else
	strFilePath = "\admin\crmsetup\setup\setup.txt"
End If

'Response.Write(strAppPath & strFilePath)
Dim objFile
Set objFile = Server.CreateObject("Scripting.FileSystemObject")
on error resume next
Set objTxtFile = objFile.OpenTextFile(strAppPath & strFilePath,1)
	
If err.number<>0 then
		response.write "You must setup CRM first."
		response.write "<br><a href=""../CrmSetup\crmSetup.asp"">Setup CRM</a>"
		Response.End
else
	strFile = objTxtFile.ReadAll
	Dim strArSetups
		strArSetups = Split(strFile," AND ", -1 , 1)

		'Declare Vartiables To Hold the temporary key and values
		Dim strKey
		Dim strValue
		For intLoop = 0 To UBound(strArSetups)
			strArKeyValue = Split(strArSetups(intLoop) , "=" , -1 , 1)
			Application(strArKeyValue(0)) = strArKeyValue(1)
			Session(strArKeyValue(0)) = strArKeyValue(1)
		Next
		Application("DataConnectionString") = "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("DataPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"
		Application("SystemConnectionString") = "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("SystemPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"
	objTxtFile.Close
	Set objTxtFile = Nothing
End If
On Error GoTo 0
'WAL_05/17/2004 add code tp read values in setup file[end]
ACCOUNT = Request.Form ("ACC")
TRANSTYPE = Request.Form ("TransType")
ORDER = Request.Form ("Order")
ORDTYPE = Request.Form ("OrdType")

strSQL = ""

IF trim(ACCOUNT) <> "" AND trim(TRANSTYPE) <> "" AND trim(ORDER) <> "" AND trim(ORDTYPE) <> "" THEN
	IF ACCOUNT = "ALL" THEN
		strSQL = ""
	ELSE
		strSQL = "WHERE ACCOUNT='" & ACCOUNT & "' "
	END IF
	
	IF TRANSTYPE = "ALL" THEN
		strSQL = strSQL
	ELSE
		IF strSQL = "" THEN
			strSQL = "WHERE TransType LIKE '" & TRANSTYPE & "%' "
		ELSE
			strSQL = strSQL & "AND TransType LIKE '" & TRANSTYPE & "%' "
		END IF
	END IF
	
	SELECT CASE ORDER
		CASE "strDate"
			strSQL = strSQL & " ORDER BY CADD_DATE "
		CASE "TransID"	
			strSQL = strSQL & " ORDER BY Account "
	END SELECT

	strSQL = strSQL & ORDTYPE
END IF
''''''''''''''''''''''''''''''''
'''aRIA-DEV:Constr = "DRIVER={SQL Server};SERVER=ARIA-DEV;DATABASE=TransLog;UID=sa"
'Constr = "DRIVER={SQL Server};SERVER=" & Session("SQLServer") & ";DATABASE=TransLog;UID=sa"
'constr = Application("SqlServer")
'constr = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source=" & Trim(Session("SQLServer")) & ";UID="& Trim(Session("SqlUserName"))& ";PWD="&Trim(Session("SqlPassWord"))
'Response.Write Application("SqlServer")
'Response.End 
if trim(Application("SqlServer")) = "" then
	Response.Write "Invalid Database connection!Please close your explorer and open it again"
	Response.End 
end if
''''''''''''''''''''''''''''''''
set ADOCon = server.CreateObject ("ADODB.connection")
If Session("DBType") = "ORACLE" then
	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=OraOLEDB.Oracle;Data Source="&Trim(session("SQLServer"))&";user id=" & Trim(Session("SqlUserName")) & ";password=" & Trim(Session("SqlPassWord")) & ""	
Else
	Application("SqlServer") = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog="&Trim(session("DBName"))&";Data Source="&Trim(session("SQLServer"))&";uid=" & Trim(Session("SqlUserName")) & ";pwd=" & Trim(Session("SqlPassWord")) & ""
End If
ADOCon.Open Application("SqlServer")
constr = Application("SqlServer")
'ADOCon.Open Constr


if len(err.Description) > 0 then
	Response.Write err.Description 
	Response.Write ("<br><a href=javascript:window.history.back();>Back</a>")
	Response.End
end if
on error goto 0
set ADORs = server.CreateObject ("ADODB.recordset")
ADORs.Open "SELECT DISTINCT Account FROM Trans ORDER BY Account",ADOCon,3,1
InitRecCount = ADORs.RecordCount 

IF InitRecCount > 0 THEN
	DIM I
	REDIM ArrAcc(InitRecCount)
	FOR I = 1 TO InitRecCount
		ArrAcc(I-1) = ADORs.Fields(0).Value 
		ADORs.MoveNext  
	NEXT
ELSE 
	NoRecs = True
END IF

ADORs.Close 
ADOCon.Close 
SET ADORs = nothing
SET ADOCon = nothing
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
%>
<HTML>
<HEAD>
<TITLE>CRM - Transaction Log Admin</TITLE>
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
<SCRIPT LANGUAGE=javascript>
<!--
function SubmitNavigation(formaction)
{
	document.TransLog.action = formaction;
	document.TransLog.submit();
}
//-->
</SCRIPT>
</HEAD>


<BODY BGCOLOR="#AECAE6" TOPMARGIN="0" LEFTMARGIN="0">

<BODY bgcolor="#AECAE6" topmargin="0" leftmargin="0" background="../images/tile1.gif">
<!--TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
  <TR> 
	<td>
		<div border=1 id="Layer8" style="position:absolute; left:0; top:0; width:700; height:69; z-index:35; background-image: url(image/heder.JPG); layer-background-image: url(image/heder.JPG); border: 1px none #000000">
			<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"      codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" width="700" height="69" id=ShockwaveFlash1>
    <param name=movie value="../../banner.swf">
    <param name=quality value=high>
    <embed src="../../banner.swf" quality=high pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash" type="application/x-shockwave-flash" width="750" height="69">
    </embed> 
  </object>
		</div>
	</td>
  </TR>
</TABLE><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR-->
<TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
  <TR> 
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=8 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=39 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=33 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=60 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=10 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=47 HEIGHT=1></TD>
    <TD width="100%"> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=6 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=125 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=10 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=13 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=361 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=30 HEIGHT=1></TD>
    <TD> <IMG SRC="../../images/<%=session("theme")%>/spacer.gif" WIDTH=8 HEIGHT=1></TD>
  </TR>
  <TR> 
    <TD COLSPAN=6> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_01.jpg" WIDTH=197 HEIGHT=8></TD>
    <TD background="../../images/<%=Session("Theme")%>/Login1_02.jpg"> <IMG SRC="images/<%=Session("Theme")%>/Login1_02.jpg" WIDTH=6 HEIGHT=8></TD>
    <TD COLSPAN=6> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_03.jpg" WIDTH=547 HEIGHT=8></TD>
  </TR>
  <TR> 
    <TD COLSPAN=3 ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_04.jpg" WIDTH=80 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_05.jpg" WIDTH=60 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_06.jpg" WIDTH=10 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_07.jpg" WIDTH=47 HEIGHT=59></TD>
    <TD ROWSPAN=2 background="../../images/<%=Session("Theme")%>/Login1_08.jpg"> <IMG SRC="images/<%=Session("Theme")%>/Login1_08.jpg" WIDTH=6 HEIGHT=59></TD>
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_09.jpg" WIDTH=125 HEIGHT=59></TD>
    <TD COLSPAN=5> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_10.jpg" WIDTH=422 HEIGHT=30></TD>
  </TR>
  <TR> 
    <TD COLSPAN=5> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_11.jpg" WIDTH=422 HEIGHT=29></TD>
  </TR>
  <TR> 
    <TD COLSPAN=6> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_12.jpg" WIDTH=197 HEIGHT=8></TD>
    <TD background="../../images/<%=Session("Theme")%>/Login1_13.jpg"> <IMG SRC="images/<%=Session("Theme")%>/Login1_13.jpg" WIDTH=6 HEIGHT=8></TD>
    <TD COLSPAN=6> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_14.jpg" WIDTH=547 HEIGHT=8></TD>
  </TR>
</TABLE>


<BR>
<table bordercolor="#111111" border="0" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<TR>
		<TD align=right><a href=../default.asp><font size=2 face=arial>Back to Admin Page</font></a>
		</TD>
	</TR>
</Table>

<%
strAppUserVar = session("strAppUserVar")
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"ALOG") <= 0 Then
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

<BR>
	<%if NoRecs <> true then%>
	<table bordercolor="#111111" border="1" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
		<FORM NAME="TransLog" ID="TransLog" METHOD="POST" ACTION="Default.asp">
		<TR>
			<TD colspan=2>
				<B>View Transaction Log:</B></FONT>
			</td>
		</tr>
		<tr>	
			<td>
				&nbsp;Account:</FONT>
				<SELECT ID="ACC" NAME="ACC">
					<OPTION VALUE="ALL">ALL</OPTION>
					<%FOR I=0 TO UBOUND(ArrAcc)-1%>
						<OPTION <%If ACCOUNT = ArrAcc(I) Then %>Selected <%end if%> VALUE="<%=ArrAcc(I)%>"><%=ArrAcc(I)%></OPTION>
					<%NEXT%>
				</SELECT>
				&nbsp;Trans. Type:</FONT>
				<SELECT ID="TransType" NAME="TransType">
					<OPTION VALUE="ALL">ALL</OPTION>
					<OPTION VALUE="Add" <%If TRANSTYPE="Add" Then%>Selected<%end if%>>Add</OPTION>
					<OPTION VALUE="Edit" <%If TRANSTYPE="Edit" Then%>Selected<%end if%>>Edit</OPTION>
					<OPTION VALUE="Del" <%If TRANSTYPE="Del" Then%>Selected<%end if%>>Del</OPTION>
				</SELECT>
			<!--/TD>
			<TD-->
				Sort By:</FONT>
			<!--/TD>
			<TD-->
				<SELECT ID="Order" NAME="Order"><OPTION VALUE="strDate" <%If ORDER="strDate" Then%>Selected<%end if%>>Date</OPTION><OPTION VALUE="TransID" <%If ORDER="TransID" Then%>Selected<%end if%>>Account</OPTION></SELECT>
			<!--/TD>
			<TD-->
				<SELECT ID="OrdType" NAME="OrdType">
				<OPTION VALUE="DESC" <%If ORDTYPE="DESC" Then%>Selected<%end if%>>Descending</OPTION>
				<OPTION VALUE="ASC" <%If ORDTYPE="ASC" Then%>Selected<%end if%>>Ascending</OPTION>
				</SELECT>
			</TD>
			<TD>
				<INPUT TYPE="SUBMIT" VALUE="Search">
			</TD>
		</TR>
	</TABLE>
</form>
	<%
	IF strSQL <> "" THEN
		set ui = server.CreateObject ("TransLogUI.TransUI")
		ui.ConParameter = Constr
		'ui.Load(strSQL)
	%>
	<BR>
	<table bordercolor="#111111" border="1" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
		<%
		if ui.Load(strSQL) then%>
			<TR class=light_cell>
				<!--TD><FONT COLOR="NAVY" SIZE="-1"><B>Contact</B></FONT></TD-->
				<TD class=light_cell><B>Account</B></FONT></TD>
				<TD class=light_cell><B>Time</B></FONT></TD>
				<TD class=light_cell><B>Date</B></FONT></TD>
				<TD class=light_cell><B>Trans. ID</B></FONT></TD>
				<TD class=light_cell><B>Trans. Type</B></FONT></TD>
				<TD class=light_cell><B>Details</B></FONT></TD>
			</TR>
			<%
			
			NumPerPages = 10
			TotalPages =int (ui.RecordsCount / NumPerPages) + 1
			'Response.Write ui.RecordsCount
			IF Request.QueryString ("CurPage") = "" then
				CurPage = 1
			else
				CurPage = Request.QueryString ("CurPage")
			end if
			
			for i = 1 to ui.RecordsCount
				if i >=(CurPage * NumPerPages - NumPerPages) + 1 and i <=CurPage * NumPerPages then 
					'Response.Write i & "<BR>"%>
				
					<TR>
						<!--TD bgcolor="white"><FONT COLOR="NAVY" SIZE="-1"><%'=ui.Contact%>&nbsp;</FONT></TD-->
						<TD ><%=ui.Account%>&nbsp;</FONT></TD>
						<TD ><%=ui.CADD_TIME%>&nbsp;</FONT></TD>
						<TD ><%=ui.CADD_DATE%>&nbsp;</FONT></TD>
						<TD ><%=ui.TransNumber%>&nbsp;</FONT></TD>
						<TD ><%=ui.TransType%>&nbsp;</FONT></TD>
						<form action="Details.asp" method="post">
						<TD >
							<input type="hidden" name="Memo"  value="<%=cstr(ui.Memo)%>"><input type="submit" value="...">&nbsp;
							</FONT>
						</TD>
					</form>
					</TR>
	
				<%end if
			ui.MoveNext 
			next%>
			</table>
			<table align=center>
			<TR>
			<td colspan="2" align=center>
			<%Response.Write("<FONT face=arial SIZE=2>" & "Page " & CurPage & " of " & TotalPages & "</font></td></tr>")
			if CurPage > 1 then
				'We are not at the beginning, show the prev button%>
				<tr><td><a href="javascript:SubmitNavigation('Default.asp?CurPage=<%=curpage-1%>')"><IMG src="../../images/<%=session("theme")%>/back.gif" border=0></a></td>
			<%End If

			if CInt(CurPage) <> CInt(TotalPages) then
				'We are not at the end, show a next button%>
				<td><a href="javascript:SubmitNavigation('Default.asp?CurPage=<%=curpage+1%>')" ><IMG src="../../images//<%=session("theme")%>/next.gif" border=0></a></td></tr>
			<%End If%>
			</table>
			
					<%else
			Response.Write "No records found"
		end if
		SET ui = nothing
		%>

		<%
		END IF
	Else
	%><P><font color=navy size=2 face=Arial>There is no records to view.</font></P><%
	END IF
		%>
	<BR>
</BODY>

</HTML>
<%
''''''''''''''''''''
''Moghazy 1/1/2001''
''''''''''''''''''''
%>