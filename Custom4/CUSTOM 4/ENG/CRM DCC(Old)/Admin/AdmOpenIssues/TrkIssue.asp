<%Response.Buffer = true%>
<%if Trim(Session("AdmID"))="" then
	'Response.Redirect "Default.asp"
end if
%>
<html>

<head>
<META name=VI60_defaultClientScript content=JavaScript>
<meta http-equiv="Content-Type" content="text/html; charset=windows-1252">
<meta name="GENERATOR" content="Microsoft FrontPage 4.0">
<meta name="ProgId" content="FrontPage.Editor.Document">
<title>CRM - Administrator - Open Issues - Issue# <%=Request("IssueNo")%></title>
<SCRIPT ID=clientEventHandlersJS LANGUAGE=javascript>
<!--
function ZoomWindow(strIssue,intLineNO)
{
	window.open("zoom.asp?strIssue=" + strIssue + "&intLine=" + intLineNO ,"Zoom","scrollbars=yes,menubar=no,status=no,toolbar=no,resizable=no,height=200,width=300")
}

function ZoomDetail()
{
	var windx = window.open("zoom.htm","Zoom","scrollbars=yes,menubar=no,status=no,toolbar=no,resizable=no,height=200,width=300");
	windx.document.write(document.Frm1.txtDetailDesc.value);
}

function D1_onchange(src,srcfrm) {
	
	
	if (src.value == 2)
	{
		document.Frm1.txtResponse.value='';
		document.Frm1.txtResponse.disabled = false;
	}
	else
	{
		document.Frm1.flagSave.value='N';
		document.Frm1.submit();
	}

}

function SetSaveFlag(IssueNo,App,Mod,Type)
{
	var actionfile= 'AriaActionSave.asp?IssueNo=' + IssueNo + '&App=' + App + '&Mod=' + Mod + '&Type=' + Type ;
	document.Frm1.action = actionfile;
	
}

// WMH [Start]
// Validate Function
function ValidateForm(form){
		if (document.Frm1.txtResponse.value == "")
		{
			alert("Please enter your response!");
			document.Frm1.txtResponse.focus();
			return false;
		}
return true;
// WMH [End]
}


//-->
</SCRIPT>
</head>

<BODY bgcolor="#aecae6" leftmargin="0" topmargin="0" background="Tile1.gif">
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


<br>


<%
'WMH - XXXXXX,X to change from Dll to normal code [Start]

	'Dim objOpenIssue
	'Set objOpenIssue = Server.CreateObject("UIOpnIss.UIOpenIssue")

	''HDM [Start] use the Application variable that holds the connection string instead of this rubbish
	''objOpenIssue.ConParameter = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source=" & Trim(Session("SQLServer")) & ";UID="& Trim(Session("SqlUserName"))& ";PWD="&Trim(Session("SqlPassWord"))
	'objOpenIssue.ConParameter = Application("SqlServer")
	''HDM [End]
	
	'objOpenIssue.Load("WHERE CIssueNo='" & Request("IssueNo") & "'")
	'If objOpenIssue Is Nothing Then
	'	'Response.Write("Empty")
	'End If



	Dim cnnSql, strSqlConnString
	Set cnnSql = Server.CreateObject("ADODB.Connection")
	strSqlConnString = Application("SqlServer")
	cnnSql.Open strSqlConnString



	Dim rsOpenIssueHdr,rsOpenIssueDt,strMySQL

	'Alternate OojectOpenIssue
	strMySQL="select * from SuIssHdr WHERE CIssueNo='" & Request("IssueNo") & "'" 	
	set rsOpenIssueHdr = cnnSql.Execute(strMySQL)
	
	'Alternate Child
	strMySQL="select * from SuIssDt WHERE CIssueNo='" & Request("IssueNo") & "'" 		
	set rsOpenIssueDt = cnnSql.Execute(strMySQL)	


'WMH - XXXXXX,X to change from Dll to normal code [End]	
%>

<%
	'Provider=MSDATASHAPE;DSN=DS99;UID=;SourceType=DBF;Exclusive=No;
	'BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;
	Dim RSCust
	'Session("CurCust") = "ACC20"
	'Response.Write(Session("CurCust"))
	Dim Conn
	Session("ConnectionString") = Application("DataConnectionString")'"dsn=crm;uid=aria;pwd=aria;Deleted=Yes"
	Set Conn = Server.CreateObject("ADODB.Connection")
	Conn.Open(Session("ConnectionString"))
	Set RSCust = Server.CreateObject("ADODB.RecordSet")
'WMH - XXXXXX,X to change from Dll to normal code [Start]		
	RSCust.Open "SELECT * FROM Customer WHERE Type = 'M' And Account = '" & rsOpenIssueHdr("CCust_Id") & "'",Conn
'WMH - XXXXXX,X to change from Dll to normal code [End]	
	Dim strCustCode 'As String
	Dim strCustName 'As String
	Dim strStatus 'As String
	Dim strSupStatus 'As String

	If Not RSCust.eof Then
		strCustCode = RSCust("Account")
		strCustName = RSCust("btName")
		strLocation = "Main"
		Select Case RSCust("Status")
			Case "A"
				strStatus = "Active"
				
			Case "P"
				strStatus = "Potential"
			Case "H"
				strStatus = "Hold"
			Case "X"
				strStatus = "Canceled"
		End Select

	Else
		'Response.Write("Empty")
	End If
	RSCust.Close
	Set RSCust = Nothing
'Get issue status used by Get Track Status function
Function GetIssueStatus(strStatus)
		Select Case strStatus
			Case "W" 
				GetIssueStatus = "In Work"
				
			Case "T"
				GetIssueStatus = "In Testing"
				
			Case "R"
				GetIssueStatus = "Waiting for Reply"
			
			Case "C"
				GetIssueStatus = "Completed"
			Case "O"
				GetIssueStatus = "Open"

			Case Else
				GetIssueStatus = "N/A"
		End Select
	End Function
'Get Tracking reference status function	
	Function GetTrackStatus(strEntry)
		Dim strType,strSeq
		strType = Mid(Trim(strEntry),1,1)
		strSeq	= Mid(Trim(strEntry),2,Len(strEntry)-1)
		
		Dim cnnFoxConn
		Set cnnFoxConn = Server.CreateObject("ADODB.Connection")
		Dim strFoxConn
		strFoxConn = Application("Sqlserver")'"dsn=WebTrack1;uid=aria;pwd=aria;Deleted=Yes"
		cnnFoxConn.Open strFoxConn
		
		Dim rsState
		Set rsState = Server.CreateObject("ADODB.Recordset")
		
		Select Case strType
			Case "L"

				'Call Back
				rsState.Open "Select ccalstat From suCallHD Where cIncdntID ='"& strSeq &"'",cnnFoxConn
				If Not rsState.EOF Then
					Select Case Trim(rsState("ccalstat"))
						Case "O"
							GetTrackStatus = "Open"
						Case "K"
							GetTrackStatus = "In Work"
						Case "E"
							GetTrackStatus = "Closed"
						Case "C"
							GetTrackStatus = "Canceled"
					End Select

				End If

			Case "B"
				'Bug
				rsState.Open "Select cBug_Stat From Bug Where cBug_ID ='"& strSeq &"'",cnnFoxConn
				If Not rsState.EOF Then
					GetTrackStatus = GetIssueStatus(Trim(rsState("cBug_Stat")))
				End If

			Case "E"

				'Enhancement
				rsState.Open "Select cEnh_Stat From Enhance Where cEnh_ID ='"& strSeq &"'" , cnnFoxConn
				If Not rsState.EOF Then
					GetTrackStatus = GetIssueStatus(Trim(rsState("cEnh_Stat")))
				End If

			Case "C"

				'Custom Program
				rsState.Open "Select cCPrgStat From CustProg Where ccprgid ='"& strSeq &"'" , cnnFoxConn
				If Not rsState.EOF Then
					GetTrackStatus = GetIssueStatus(Trim(rsState("cCPrgStat")))
				End If
		End Select

		Set rsState = Nothing
				
		cnnFoxConn.Close()
		Set cnnFoxConn = Nothing

	End Function

%>


<Form Action="TrkIssue.asp?IssueNo=<%=Request("IssueNo")%>&Type=<%=Request("Type")%>" Name="Frm1" Method="post" onsubmit="return ValidateForm(this)">

<table width="95%" border=0 align=center> 
	<tr>
		<td><FONT color=#000080 face=Arial size=2><b>Customer Helpdesk</b></FONT>
		</TD>
		<TD align=right><a href=default.asp>Back to the Helpdesk administration page</a>
		</td>
	</tr>
</table>
<br>
<table width="95%" border=1 bgcolor="#6495d0" bordercolor="#aecae6" align=center>
        <tr >
                <td><b><font color="navy" size="2" face="arial">Issue # <%=Request("IssueNo")%></font></b></td>
                <td align="right" colspan=4><b><font color="navy" size="2" face="arial">Status:<%
                'WMH [Start]
				'=GetIssueStatus(objOpenIssue.Status)
                '= GetIssueStatus(rsOpenIssueHdr("CissStat"))
		         Response.write GetIssueStatus(rsOpenIssueHdr("CissStat"))								
				'WMH [End]
                %></font></b></td>
                    </tr>
       
        <tr>
               <td  colspan="5"><strong><font color="navy" size="2" face="arial">Customer
                  information</font></strong>
                </td>
              </tr>
              <tr>
                <td ><STRONG><font color="navy" size="2" face="arial">Customer ID</font></STRONG></td>
                <td colspan=4><STRONG><font color="navy" size="2" face="arial">Customer Name</font></STRONG></td>
                
              </tr>
              <tr>
                <td ><font face="Arial" size=2><input type="text" name="txtCustID" size="10" value="<%
               	'WMH [Start]
				'=objOpenIssue("CustomerID")
                Response.write rsOpenIssueHdr("CCust_Id")
				'WMH [End]
                %>" Disabled></font></td>
                <td  colspan=4><font face="Arial" size=2><input type="text" name="T1" size="70" value="<%=strCustName%>" Disabled></font></td>
              </tr>

        <tr>
		<td  colspan="5" height="19"><STRONG><font color="navy" size="2" face="arial">Issue Details</font></strong></td>
              </tr>
              <tr>
                <td ><STRONG><font color="navy" size="2" face="arial">Type</font></STRONG></td>
                <td ><STRONG><font color="navy" size="2" face="arial">%</font></STRONG></td>
                <td><STRONG><font color="navy" size="2" face="arial">Priority</font></STRONG></td>
                <td><STRONG><font color="navy" size="2" face="arial">Submitted</font></STRONG></td>
              </tr>
              <tr>
                <td >
					
					<select id="lstIssType" name="lstIssType" size="1" disabled>
						<%set ConnFox = Server.CreateObject("ADODB.Connection")
						ConnFox.Open Application("DataConnectionString")'"dsn=crm;deleted=Yes"
						set rsIssueTypes = Server.CreateObject("ADODB.RecordSet")
						strSQL = "SELECT Cdiscrep, Ccode_no,Cdefcode FROM Codes WHERE Cfld_name='CISSTYPE' AND Cdefcode ='N' AND Crltfield='N' ORDER BY Ccode_no ASC,Cdefcode ASC"
						rsIssueTypes.Open strSQL, ConnFox
						
						if 	rsIssueTypes.BOF and rsIssueTypes.EOF then%>
							<option  value="N/A">N/A
						<%else
							do while not rsIssueTypes.EOF
							%>
								<option <%if Trim(rsIssueTypes("Ccode_no"))=Trim(Request("Type")) then Response.Write "SELECTED" end if%> value="<%=Trim(rsIssueTypes("Ccode_no"))%>"><%=rsIssueTypes("Cdiscrep")%>
							<%rsIssueTypes.MoveNext()
							Loop
						end if%>
                    </select></td>
                <td><font face="Arial" size=2><input type="text" name="txtpercent" size=2 maxlength="3" <%If rsOpenIssueHdr("CissStat") = "C" Then Response.Write("disabled")%> value="<%
               	'WMH [Start]
				'=objOpenIssue("CompletePercent")
                Response.write rsOpenIssueHdr("nIssPercnt")
				'WMH [End]                
                %>" ></font></td>
                <td><font face="Arial" size=2><input type="text" name="T1" size="10" value="<%
               	'WMH [Start]
				'select case objOpenIssue("Priority")
                select case rsOpenIssueHdr("CIssPrior")
				'WMH [End]                
				 case "V"
					Response.Write "Very Urgent"
				 case "U"
					Response.Write "Urgent"
				 case "N"
					Response.Write "Normal"
				 case "L"	
					Response.Write "Low"
				 end select%>" Disabled></font></td>
                <td><font face="Arial" size=2><input type="text" name="T1" size="14" value="<%
               	'WMH [Start]
				'=objOpenIssue("StartDate")
                Response.write rsOpenIssueHdr("DIssStart")
				'WMH [End]                
                %>" Disabled></font></td>

              </tr>
              <tr>
                <td ><STRONG><font color="navy" size="2" face="arial">Reported by</font></STRONG></td>
                <td height="19"><STRONG><font color="navy" size="2" face="arial">Phone</font></STRONG></td>
                <td height="19"><STRONG><font color="navy" size="2" face="arial">Fax</font></STRONG></td>
                <td height="19" colspan=2><STRONG><font color="navy" size="2" face="arial">Email</font></STRONG></td>
              </tr>
              <tr>
                <td ><font face="Arial" size=2><input type="text" name="T1" size="32" value="<%
               	'WMH [Start]
				'=objOpenIssue("Contact")
                Response.write rsOpenIssueHdr("Contact")
				'WMH [End]                
                %>" Disabled></font></td>
                <td  height="16"><font face="Arial" size=2><input type="text" name="T1" size="17" value="<%
               	'WMH [Start]
				'=objOpenIssue("Phone")
                Response.write rsOpenIssueHdr("Phone")
				'WMH [End]                
                %>" Disabled></font></td>
                <td  height="16"><font face="Arial" size=2><input type="text" name="T1" size="18" value="<%
               	'WMH [Start]
				'=objOpenIssue("Fax")
                Response.Write rsOpenIssueHdr("Fax")
				'WMH [End]                
                %>" Disabled></font></td>
                <td  colspan="2" height="16"><font face="Arial" size=2><input type="text" name="T1" size="33" value="<%
               	'WMH [Start]
				'=objOpenIssue("eMail")
                Response.Write rsOpenIssueHdr("Cwe_mail")
				'WMH [End]                
                %>" Disabled></font></td>

              </tr>
          		<tr>
                <td  height="16" colspan="5"><STRONG><font color="navy" size="2" face="arial">Subject</font></STRONG></td>
              </tr>
              <tr>
                <td  height="16" colspan="5"><font face="Arial" size=2><input type="text" name="txtSubject" size="82" value="<%
               	'WMH [Start]
				'=objOpenIssue("Subject")
                Response.Write rsOpenIssueHdr("CissSubjct")
				'WMH [End]                
                %>" Disabled></font></td>
              </tr>
              <tr>
                <td  height="16" colspan="5"><STRONG><font color="navy" size="2" face="arial">Detail Description</font></STRONG></td>
              </tr>
              <tr>
                <td colspan="5" valign="top" width="512"><font face="Arial" size=2>
                <textarea rows="3" name="txtDetailDesc" cols="47" Disabled><%
               	'WMH [Start]
				'=objOpenIssue("Details")
                Response.Write rsOpenIssueHdr("mIssDetail")
				'WMH [End]                
                %></textarea>
                <input type="button" value="..." name="cmdZoom" onClick="return ZoomDetail()"></font></td>
              </tr>
</table>



<table width="95%" border=1 bgcolor="#6495d0" bordercolor="#aecae6" align=center>
                <tr>
                <td  height="16" colspan="2"><STRONG><font color="navy" size="2" face="arial">Response</font></STRONG></td>
                </tr>
                <tr>
                  <td width="31%" valign="top"><font face="Arial" size=2>
					<textarea rows="3" name="txtResponse" id="txtResponse" cols="47" <%If rsOpenIssueHdr("CissStat") = "C" Then Response.Write("disabled")%>></textarea></font>
                  </td>
                  <td width="0" valign="bottom"><font face="Arial" size=2><input type="submit" value="Save Action" <%If rsOpenIssueHdr("CissStat") = "C" Then Response.Write("disabled")%> name="B1" onclick="return SetSaveFlag('<%=Request("IssueNo")%>','<%=Request("App")%>','<%=Request("Mod")%>','<%=Request("Type")%>')">
                    </font>
                  </td>
                </tr>
                <tr>
                  <td colspan="2">
                    <p align="right">
                  </td>
                </tr>
</table>

<table width="95%" border=1 bgcolor="#6495d0" bordercolor="#aecae6" Align=center>
		  <TR>
			<td colSpan=5 align='left'><STRONG><FONT color=#4b0082 face=Arial size=2>Issue History</FONT></STRONG></td>
			<TR bgColor=#4b0082>
				<TD><FONT color=white face=Arial size=2>Date</FONT></TD>
				<TD><FONT color=white face=Arial size=2 >Time</FONT></TD>
				<TD><FONT color=white face=Arial size=2 >Contact</FONT></TD>
				<TD colspan=2><FONT color=white face=Arial size=2 >Action</FONT></TD>
    
			</TR>
<%
'WMH [Start]
'If Not objOpenIssue.ChildEOF(1) Then
'		Dim objChild
'		Do While Not objOpenIssue.ChildEOF(1)
'			Set objChild = objOpenIssue.ChildGet(1)		
if Not rsOpenIssueDt.EOF then
	Do While Not rsOpenIssueDt.EOF
'WMH [End]									     									
						
%>
			<TR bgColor="<%
          	'WMH [Start]
			'If objChild.ResponseType="Y" Then Response.Write("#fffaf0") Else Response.Write("#ffffe0")
			If rsOpenIssueDt("cRespType")="Y" Then Response.Write("#fffaf0") Else Response.Write("#ffffe0")
			'WMH [End]                
			%>">
				<TD><FONT color=#4b0082 face=Arial size=2><%
               	'WMH [Start]
				'=objChild("ResponseDate")
                Response.Write rsOpenIssueDt("DRespDate")
				'WMH [End]                
				%></FONT></TD>
				<TD><FONT color=#4b0082 face=Arial size=2><%
               	'WMH [Start]
				'=objChild("ResponseTime")
                Response.Write rsOpenIssueDt("tRespTime")
				'WMH [End]                
				%></FONT> </TD>
				<TD><FONT color=#4b0082 face=Arial size=2><%
               	'WMH [Start]
				'=objChild("ResponseBy")
                Response.Write rsOpenIssueDt("cRespBy")
				'WMH [End]                
				%></FONT></TD>
				<TD><FONT color=#4b0082 face=Arial size=2><%
               	'WMH [Start]
				'=Mid(objChild("ResponseAction"),1,40)
				Response.Write Mid(rsOpenIssueDt("mRespAct"),1,40)
				'WMH [End]                
				%></FONT>    </TD>
				<TD><FONT color=#4b0082 face=Arial size=2><INPUT id=button1 name=button1 type=button value="..." onClick="ZoomWindow('<%
               	'WMH [Start]
				'Response.Write(objOpenIssue("IssueNo"))
				Response.Write(rsOpenIssueHdr("CIssueNo"))				
				'WMH [End]                				
				%>' , <%
               	'WMH [Start]
				'Response.Write(objChild("LineNo"))
				Response.Write(rsOpenIssueDt("intLineNo"))				
				'WMH [End]                				
				%>)"></FONT></TD>
			</TR>
<%
'WMH [Start]
							'				Set objChild = Nothing
				'objOpenIssue.ChildMoveNext(1)
				'Loop
			'End If
				
				rsOpenIssueDt.MoveNext()
	Loop
'WMH [End]				
%>
  
</table>


<P>

<table width="95%" border=1 bgcolor="#6495d0" bordercolor="#aecae6" align=center>
  <TR>
    <TD bgColor=#fffaf0  height=30></TD>
    <TD><STRONG><font color="navy" size="2" face="arial">Indicates your response</font></STRONG></TD></TR>
  <TR>
    <TD bgColor=#ffffe0 height=30></TD>
    <TD><STRONG><font color="navy" size="2" face="arial">Indicates Customer response</font></STRONG></TD></TR>
</TABLE>

<%End If%>
<%
'WMH [Start]
set rsOpenIssueDt=nothing		
Set rsOpenIssueHdr = Nothing
Set cnnSql = Nothing
'WMH [End]				
%>
</form>
<br><br>
</body>
</html>
