<%@ Language=VBScript %>
<% Response.CacheControl = "no-cache" 
 Response.AddHeader "Pragma", "no-cache" 
 Response.Expires = -1 
 Response.Buffer=true
 
IF Trim(Session("ID")) = "" AND Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
End IF
%>

<HTML>
<HEAD>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("Theme")%>/help.css">
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<title>CRM - Helpdesk - Open Issues - Issue# <%=Request("IssNo")%></title>
</HEAD>
<body topmargin="0" leftmargin="0" marginheight="0" marginwidth="0" bgcolor="#aecae6">

<SCRIPT LANGUAGE=javascript>
<!--

function ZoomWindow(strIssue,intLineNO)
{
	window.open("zoom.asp?strIssue=" + strIssue + "&intLine=" + intLineNO ,"Zoom","scrollbars=yes,menubar=no,status=no,toolbar=no,resizable=no,height=200,width=300")
}

function ZoomDetail()
{
	var windx = window.open("zoom.htm","Zoom","scrollbars=yes,menubar=no,status=no,toolbar=no,resizable=no,height=200,width=300");
	windx.document.write(document.Form1.txtDetail.value);
}

function DispMod(objMe,strvalue,objform)
{

	intListLen = document.Form1.lstAppl.options.length;
	for (intLoop = 0 ; intLoop < intListLen; intLoop++)
	{
		strobj = document.Form1.lstAppl.options(intLoop).value;
		obj = eval('objform.'+strobj);
		obj.style.display='none' ;
	}
	objToDisp = eval('objform.' + strvalue);
	objToDisp.style.display='block';

	/*obj = eval("objfrom." + strtarget);
	objfrom .txtcnt.value = obj.value*/

}



function lfComplete(src)
{
	

	if(src.checked)
	{
		document.Form1.txtRespDetail.value = "Mark this issue as complete"
		document.Form1.nxtstatue.value = "C"
		//src.value = "C"
	}
	else
	{
		document.Form1.txtRespDetail.value = "Mark this issue as open"
		document.Form1.nxtstatue.value = "O"
		//alert(src.value)
	}
	//return false

}


// WMH [Start]
// Validate Function
/*function CheckRespForm(objForm)
{
			if (objForm.txtRespDetail.value == "")
			{
				alert(objForm.txtRespDetail.getAttribute("ERRORTXT",false))
				return false
			}
	
	return true
}*/

function ValidateForm(form){
		//Email
		if (document.Form1.txteMail.value == "")
		{
			alert("Please enter your e-mail!");
			document.Form1.txteMail.focus();
		return false;
		}
		
		// Response
		if (document.Form1.txtRespDetail.value == "")
		{
			alert("Please enter your response!");
			document.Form1.txtRespDetail.focus();
			return false;
		}
return true;
}
// WMH [End]

//-->
</SCRIPT>

<%
	'This function used to convert min. to hours and minuets
	' returns a string formatted hh:mm
	Function GetTime(dblTime)
	
		dblHours = dblTime / 60
		intHours = Int(dblHours)
		dblMin = (dblHours - intHours) *60
		strHours = cStr(intHours)
		strMin = cStr(dblMin)
		GetTime = strHours & ":" & strMin

	End Function

	Function GetIssueType(strType)
	
	
		Dim rsIssueType
		Set rsIssueType = Server.CreateObject("ADODB.Recordset")
		Dim strCodesSQL

		strCodesSql = "Select cCode_no, cDiscrep From Codes Where cFld_Name='CISSUTYPE  ' and cDefCode='N' AND cCode_no='" & strType & "'"

		rsIssueType.Open strCodesSql,Conn
		if rsIssueType.BOF and rsIssueType.EOF then
			Response.Write "N/A"
		else
			GetIssueType = rsIssueType("cDiscrep")
		
			rsIssueType.Close()
		end if
		set rsIssueType = Nothing
	End Function
	
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
				
			Case "X"
				GetIssueStatus = "Canceled"

			Case Else
				GetIssueStatus = "N/A"
		End Select
	End Function

	Function GetPriority(strPri)
		Select Case strPri
			Case "V"
				GetPriority = "Very Urgent"
			Case "U"
				GetPriority = "Urgent"
			Case "N"
				GetPriority = "Normal"
			Case "L"
				GetPriority = "Low"
		End Select
	End Function

	Function GetApplicationName(strAppKey)
		Dim rsApp
		Set rsApp = Server.CreateObject("ADODB.RecordSet")
		rsApp.Open "Select cApp_Name From ARIA_App Where cBug_App='" & strAppKey & "'",Conn
		If Not rsApp.EOF Then
			GetApplicationName = Trim(rsApp("cApp_Name"))
		Else
			GetApplicationName = "NA"
		End IF
		
		rsApp.Close()
		Set rsApp = Nothing
	End Function
	
	Function GetModuleName(strAppKey , strModuleID)
		Dim rsModule
		Set rsModule = Server.CreateObject("ADODB.RecordSet")
		rsModule.Open "Select cMod_Desc From suAppMod Where cBug_App='" & strAppKey & "' AND cMod_ID='" & strModuleID & "'",Conn
		If Not rsModule.EOF Then
			GetModuleName = Trim(rsModule("cMod_Desc"))
		Else
			GetModuleName = "N/A"
		End IF
		
		rsModule.Close()
		Set rsModule = Nothing
		
	End Function

	Function GetTrackStatus(strEntry)
		Dim strType,strSeq
		strType = Mid(Trim(strEntry),1,1)
		strSeq	= Mid(Trim(strEntry),2,Len(strEntry)-1)
		
		Dim cnnFoxConn
		Set cnnFoxConn = Server.CreateObject("ADODB.Connection")
		Dim strFoxConn
		strFoxConn = Application("DataConnectionString")'"dsn=crm;uid=aria;pwd=aria;Deleted=Yes"
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

		'rsState.Close()
		Set rsState = Nothing
				
		cnnFoxConn.Close()
		Set cnnFoxConn = Nothing

	End Function

	Dim Conn
	Session("ConnectionString") = Application("DataConnectionString")'"dsn=crm;uid=aria;pwd=aria"
	Set Conn = Server.CreateObject("ADODB.Connection")
	Conn.Open(Session("ConnectionString"))

	Dim strCustCode 'As String
	Dim strCustName 'As String
	Dim strStatus 'As String
	Dim strSupStatus 'As String

		strCustCode = Session("RSCust").Fields("account")
		strCustName = Session("RSCust").Fields("btName")
		strLocation = "Main"
		Select Case Session("RSCust").Fields("Status")
			Case "A"
				strStatus = "Active"
				
			Case "P"
				strStatus = "Potential"
			Case "H"
				strStatus = "Hold"
			Case "X"
				strStatus = "Canceled"
		End Select

		
		Dim strAlTime , strUsedTime , strRemTime
		
		'strAlTime = rsCustProf("ctot_tm_al")
		'strUsedTime = rsCustProf("ctot_tm_us")
		'strRemTime = DateDiff("n" , strUsedTime ,strAlTime)
		'strRemTime = GetTime(cDbl(strRemTime))

	
	
	'Collect All issues that maches the filter criteria
	Dim cnnSql
	Set cnnSql = Server.CreateObject("ADODB.Connection")
	
	Dim strSqlConnString
	
	strSqlConnString = Application("SqlServer")
	
	cnnSql.Open strSqlConnString
	
	Dim rsIssues
	Set rsIssues = Server.CreateObject("ADODB.RecordSet")

	'If we are coming from the support main page
	'(Load open issues for default filter)
	'Else
	'Use the applied filter in the form

	Dim strSqlWhere

	If Request.QueryString("Criteria") = "X" Then
		strSqlWhere = "WHERE cCust_Id='"& Session("ID") &"' AND cissStat='O'"
	Else
	
		'strSqlWhere = "WHERE cCust_Id='"& Session("CurCust") &"' AND CissStat='" & Request("lstIssStatus") & "'"
		If Request("lstIssType") = "A" Then
			strIssTypeCr = ""
		Else
			strIssTypeCr =  " AND cIssType='" & Request("lstIssType") & "'"
		End IF
		
		If Request("lstIssPri") = "A" Then
			strIssPriCr = ""
		Else
			strIssPriCr = " AND cIssPrior='" & Request("lstIssPri") & "'"
		End IF
		strSqlWhere = "WHERE cCust_Id='"& Session("ID") & "' AND cIssStat='" & Request("lstIssStatus") & "'" & strIssPriCr & strIssTypeCr
		
	End If
	
	rsIssues.Open "Select * From SUIssHdr " & strSqlWhere , cnnSql

'WMH - XXXXXX,X to change from Dll to normal code [Start]
'	Dim objOpenIssue
'	Set objOpenIssue = Server.CreateObject("UIOpnIss.UIOpenIssue")
'	objOpenIssue.ConParameter = Application("SqlServer")
'	objOpenIssue.Load("WHERE cCust_Id='"& Session("RSCust").Fields("account") &"'AND CIssueNo='" & Request("IssNo") & "'")
'	If objOpenIssue Is Nothing Then
'		Response.Write("Empty")
'	End If
'	'Set objOpenIssue = Nothing

	Dim rsOpenIssueHdr,rsOpenIssueDt,strSQLSt
	
	strSQLSt="select * from SuIssHdr WHERE cCust_Id='"& Session("RSCust").Fields("account") &"'AND CIssueNo='" & Request("IssNo") & "'" 	
	
	'Alternate OojectOpenIssue
	Set rsOpenIssueHdr = cnnSql.Execute(strSQLSt)
	
	'Alternate Child
	strSQLSt="Select * from SuIssDt WHERE cIssueNo='" & Trim(Request("IssNo")) & "'" 		
	set rsOpenIssueDt = cnnSql.Execute(strSQLSt)	
'WMH - XXXXXX,X to change from Dll to normal code [End]	



				
	Session("ConnectionString") = Application("DataConnectionString")'"dsn=crm;uid=aria;pwd=aria;Deleted=Yes"
	Set Conn = Server.CreateObject("ADODB.Connection")
	Conn.Open(Session("ConnectionString"))

	
	'Get Customer Contacts
	Set RSContact = createobject("ADODB.RecordSet")
	'Force rushmore
	'cconttype+ccont_id+store+contact
	'RSContact.Open "SELECT * FROM Contact Where cCont_Id = '" & UCase(Session("RSCust").Fields("account")) & "' and Cconttype='C'",Conn
	RSContact.Open "SELECT * FROM Contact Where cconttype+ccont_id+store+contact Like 'C" & UCase(Session("RSCust").Fields("account")) & "%'",Conn

%>

	<%IF strFile = "cust" Then%>
	<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<%Else%>

	<%End IF%>


<p><br><br><br></p>
<FORM Name="Form1" id= "Form1" method="post" action=ServerScripts/SaveResponse.asp onsubmit="return ValidateForm(this)">

<div align="center">
  <center>

<table width=95% border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<TR>
		<TD width="33%" class="dark_cell">
            <%=session("CustField")%> ID </TD>
		<TD width="33%" class="dark_cell">
			<%=session("CustField")%> Name </TD>
        <TD width="34%" class="dark_cell">Status</TD>
	</TR>
	<TR>
		<TD class="light_cell">
            <input id="txtCustID" size="5" name="txtCustID" disabled Value="<% =strCustCode %>">
            <INPUT type="hidden" id="hdnCustID" name="hdnCustID" Value=<%Response.Write(strCustCode)%>>
		</TD>
        <TD class="light_cell">
            <input id="txtCustName" name="txtCustName" disabled Value="<%Response.Write(strCustName)%>" size="20">
		</TD>
        <TD class="light_cell">
            <input id="txtStatus" size="6" name="txtStatus" disabled Value="<%Response.Write(strStatus)%>">
		</TD>
	</TR>
</TABLE>

  </center>
</div>

<div align="center">
  <center>

<table width=95% border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
			
			<tr>
				<td align='left' width="100%" colspan=4 class="dark_cell">Details of issue# <%
				'WMH [Start]
				'Response.Write(objOpenIssue.IssueNo)
				Response.Write(rsOpenIssueHdr("CIssueNo"))
				'WMH [End]
				%><INPUT Type = "hidden" name="hdnIssueNo" value="<%
				'WMH [Start]
				'Response.Write(objOpenIssue.IssueNo)
				Response.Write(rsOpenIssueHdr("CIssueNo"))
				'WMH [End]
				%>">
            
				</td>
				<TD align="Left" colSpan=3 style="WIDTH: 50%" width="50%" class="dark_cell">
				Status: <%
				'WMH [Start]
				'Response.Write(GetIssueStatus(objOpenIssue.Status))
				Response.Write(GetIssueStatus(rsOpenIssueHdr("CissStat")))
				'WMH [End]
				%></TD>
			</tr>
		<TR>
			<TD class="dark_cell" >Type</TD>
			<TD class="dark_cell">%Complete</TD>
			<TD class="dark_cell" >Submission Date</TD>
			<TD class="dark_cell" >Completion Date</TD>
			<TD colspan=3 class="dark_cell">Priority</TD>
		</TR>
  <TR>
    <TD align=left class="light_cell" >
				<SELECT size=1 name="lstIsseType" Disabled>
				
				<OPTION Value="<%
				'WMH [Start]
				'Response.Write(Trim(objOpenIssue.IssueType))
				Response.Write(Trim(rsOpenIssueHdr("CIssType")))
				'WMH [End]
				%>" selected><%
				'WMH [Start]
				'Response.Write(GetIssueType(Trim(objOpenIssue.IssueType)))
				Response.Write(GetIssueType(Trim(rsOpenIssueHdr("CIssType"))))
				'WMH [End]				

				%>
				</SELECT>
		</TD>
    
    
    <TD class="light_cell"><INPUT disabled name=txtPercent value="<%
	'WMH [Start]
	'Response.Write(objOpenIssue.CompletePercent)
    Response.Write(rsOpenIssueHdr("nIssPercnt"))
	'WMH [End]				

    %>" Disabled size=10></TD>
    
    <TD class="light_cell"><INPUT disabled name=txtDate size=10 Value="<%
			'WMH [Start]
			'Response.Write(Trim(objOpenIssue.StartDate))
			Response.Write(Trim(rsOpenIssueHdr("DIssStart")))
			'WMH [End]				
			%>" Disabled></TD>
    <TD class="light_cell">
					<INPUT size=10 name="txtCompDate" disabled value="<%
					'WMH [Start]
					'If objOpenIssue.DueDate = "12:00:00 AM" Then Response.Write("N/A") Else Response.Write(objOpenIssue.DueDate)
					If rsOpenIssueHdr("DIssComp") = "12:00:00 AM" Then Response.Write("N/A") Else Response.Write(rsOpenIssueHdr("DIssComp"))
					'WMH [End]				
					%>"></TD>
    <TD class="light_cell"><Input type="text" SIZE = "12" Value="<%
					'WMH [Start]
					'Response.Write(GetPriority(Trim(objOpenIssue.Priority)))
					Response.Write(GetPriority(Trim(rsOpenIssueHdr("CIssPrior"))))
					'WMH [End]				
					%>" Disabled></TD></TR>
  <TR>
    <TD align=left colSpan=1 class="dark_cell">Reported By </TD>
    <TD align=left colSpan=2 class="dark_cell">Email</TD>
    <TD align=left class="dark_cell">Phone</TD>
    <TD colSpan=2 class="dark_cell">Fax</TD></TR>
  <TR>
    <TD align=left colSpan=1 class="dark_cell">
				<SELECT size=1 name="lstReportedBy" Disabled style="width:200px">
					<OPTION value=<%
					'WMH [Start]
					'Response.Write(Trim(objOpenIssue.Contact))
					Response.Write(Trim(rsOpenIssueHdr("Contact")))
					'WMH [End]				
					%>>
					<%
					'WMH [Start]
					'Response.Write(Trim(objOpenIssue.Contact))
					Response.Write(Trim(rsOpenIssueHdr("Contact")))
					'WMH [End]									
					%>
				</SELECT>
        <input type="hidden" name="hdnReportedBy" Value="<%
		'WMH [Start]
		'Response.Write(Trim(objOpenIssue.Contact))
        Response.Write(Trim(rsOpenIssueHdr("Contact")))
		'WMH [End]									        
        %>">
        </TD>
    <TD align=left colSpan=2 class="dark_cell">
            <INPUT size=32 MaxLength=30 name=txteMail <%If rsOpenIssueHdr("CissStat") = "C" Then Response.Write("disabled")%> Value="<%
       		'WMH [Start]
			'Response.Write(Trim(objOpenIssue.eMail))
            Response.Write(Trim(rsOpenIssueHdr("Cwe_mail")))
			'WMH [End]									     
            %>" >
            
            </TD>
    <TD align=left class="dark_cell">
			<INPUT size=14 MaxLength=16 name=txtPhone  <%If rsOpenIssueHdr("CissStat") = "C" Then Response.Write("disabled")%> Value="<%
       		'WMH [Start]
			'Response.Write(Trim(objOpenIssue.Phone))
			Response.Write(Trim(rsOpenIssueHdr("Phone")))
			'WMH [End]									     			
			%>" >
			</TD>
    <TD colSpan=2 class="dark_cell">
    <INPUT size=13 name=txtFax MaxLength=16 <%If rsOpenIssueHdr("CissStat") = "C" Then Response.Write("disabled")%> Value="<%
	'WMH [Start]
	'Response.Write(Trim(objOpenIssue.fax))
    Response.Write(Trim(rsOpenIssueHdr("Fax")))
	'WMH [End]									     			
    %>" >
    </TD></TR>
  <TR>
    <TD align=left colSpan=7 class="dark_cell">
      Subject</TD></TR>
  <TR>
    
    <TD align=left colSpan=7 class="light_cell">
     <INPUT name=txtSubject size="66" ERRORTXT="Please enter the subject!" Req="Y" Value="<%
	 	'WMH [Start]
		'Response.Write(Trim(objOpenIssue.Subject))
		Response.Write(Trim(rsOpenIssueHdr("CissSubjct")))
		'WMH [End]									     			
      %>" Disabled></TD></TR>
  <TR>
    
    <TD colSpan=7 class="dark_cell">Detail Description</TD></TR>
  <TR>
    
    <TD colSpan=7 class="light_cell">
      <P>
      <TEXTAREA name=txtDetail align="left" rows=8 cols=50 disabled><%
	 	'WMH [Start]
		'Response.Write(Trim(objOpenIssue.Details))
	    Response.Write(Trim(rsOpenIssueHdr("mIssDetail")))
		'WMH [End]									     			
      %></TEXTAREA>
      <INPUT type="button" value="..." id=button2 name=button2 onClick="return ZoomDetail()">
      
      </P></TD></TR>
      <%
 	 	'WMH [Start]
		'If Trim(objOpenIssue.IssueType) = "L" Then
		If Trim(rsOpenIssueHdr("CIssType")) = "L" Then
		'WMH [End]									     			
      %>
      <tr>
				<td colspan=7 align=Right class="dark_cell">
					<INPUT type="submit" value="Submit" id=submit2 name=submit2>
				</td>
			</tr>
      <%End If%>
</table>


  </center>
</div>


<%
'WMH [Start]
'If Trim(objOpenIssue.IssueType) <> "L" Then
If Trim(rsOpenIssueHdr("CIssType")) <> "L" Then
'WMH [End]									     			
%>
<div align="center">
  <center>
<table width=95% border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
  
        <TR>
          <TD colSpan=5 class="dark_cell">Response</TD></TR>
        <TR>
          <TD class="dark_cell">Response By</TD>
          <TD colSpan=2 class="dark_cell">Response</TD>
          </TR>
        <TR>
          <TD vAlign=top width="35%" class="light_cell">
<!------------------ WMH when complated it will be disabled [Start] ------------------------>
				<!--<SELECT size=1 name="lstRespBy" >-->
				<SELECT size=1 name="lstRespBy" <%If rsOpenIssueHdr("CissStat") = "C" Then Response.Write("disabled")%>>
<!------------------ WMH when complated it will be disabled [End] ------------------------>				
					<%Do While Not RSContact.EOF%>
					<OPTION value="<%Response.Write(Trim(RSContact("Contact")))%>"><%Response.Write(Trim(RSContact("Contact")))%>
					<%
					RSContact.MoveNext()
					Loop%>
				</SELECT>

        </TD>
          <TD colspan=2 align='left' class="light_cell">
<!------------------ WMH when complated it will be disabled [Start] ------------------------>          
          <!--<TEXTAREA name="txtRespDetail" rows=6 cols=60 ERRORTXT="Please enter your response!" Req="Y" ></TEXTAREA>-->
          <TEXTAREA name="txtRespDetail" rows=6 cols=60   <%If rsOpenIssueHdr("CissStat") = "C" Then Response.Write("disabled")%>></TEXTAREA>
<!------------------ WMH when complated it will be disabled [End] ------------------------>          
          </td>
          <tr>
          <td class="dark_cell">
					<INPUT type="CheckBox"  id="Complete" name="Complete"
					<%
					'WMH [Start]
					'If objOpenIssue.Status = "C" Then Response.Write("Checked ")
					If rsOpenIssueHdr("CissStat") = "C" Then Response.Write("Checked ")
					'WMH [End]									     			
					%>
					onClick="return lfComplete(this)" value="ON"><STRONG>Completed<input type='hidden' name = "nxtstatue" value= <%
					'WMH [Start]
					'Response.Write(objOpenIssue.Status)
					Response.Write(rsOpenIssueHdr("CissStat"))
					'WMH [End]									     			
					%>>
				</td>
          <td align='right' class="dark_cell">
						<INPUT type="submit" value="Submit" id=submit1 name=submit1>
          </TD>

				</tr>

</TABLE>

  </center>
</div>
<div align="center">
  <center>

<table width=95% border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	  <TR>
			<td colSpan=5 align='left' class="dark_cell">Issue History</td>
			<TR >
				<TD class=dark_cell>Date</TD>
				<TD class=dark_cell>Time</TD>
				<TD class=dark_cell>Contact</TD>
				<TD colspan=2 class=dark_cell>Action</TD>
			</TR>
			<%
			'WMH [Start]
			'If Not objOpenIssue.ChildEOF(1) Then
			'		Dim objChild
			'		
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
			
			<TD class=light_cell>
					<%
					'WMH [Start]
					'Response.Write(objChild.ResponseDate)
					Response.Write(rsOpenIssueDt("DRespDate"))
					'WMH [End]									     												
					%>
			&nbsp;</TD>
			<TD class=light_cell> 
					<%
					'WMH [Start]
					'Response.Write(objChild.ResponseTime)
					Response.Write(rsOpenIssueDt("tRespTime"))
					'WMH [End]									     																	
					%>
			&nbsp;</TD>
			<TD class=light_Cell>
					<%
					'WMH [Start]
					'Response.Write(objChild.ResponseBy)
					Response.Write(rsOpenIssueDt("cRespBy"))
					'WMH [End]									     																	
					%>
			&nbsp;</TD>
			<TD class=light_cell>
			<%
			'WMH [Start]
			'Response.Write(Mid(objChild.ResponseAction,1,40))
			Response.Write(Mid(rsOpenIssueDt("mRespAct"),1,40))
			'WMH [End]									     																	
			%>
			&nbsp;</td>
			<td class=light_cell>
			<INPUT type="button" value="..." id=button1 name=button1 onClick="ZoomWindow('<%
			'WMH [Start]
			'Response.Write(objOpenIssue.IssueNo)
			Response.Write(rsOpenIssueHdr("CIssueNo"))
			'WMH [End]									     																	
			%>' , <%
			'WMH [Start]
			'Response.Write(objChild.LineNo)
			Response.Write(rsOpenIssueDt("intLineNo"))
			'WMH [End]									     																	
			%>)">
			</TD>
			</TR>
			<%
			'WMH [Start]
							'				Set objChild = Nothing
				'objOpenIssue.ChildMoveNext(1)
				'Loop
			'End If
				
				rsOpenIssueDt.MoveNext()
				Loop
			End If
			set rsOpenIssueDt=nothing
			'WMH [End]									     																		
			%>
		</TABLE>
  </center>
</div>
</FORM>

<P>
<div align="center">
  <center>
<table width=95% border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
  
  <TR>
    <TD bgColor=#fffaf0  height=30>&nbsp;&nbsp; </TD>
    <TD class="dark_cell">&nbsp;Indicates our response</TD></TR>
  <TR>
    <TD bgColor=#ffffe0 height=30>&nbsp; </TD>
    <TD class="dark_cell">&nbsp;Indicates your response</TD></TR>
</TABLE>
    </center>
</div>
    <%End If%>
<%
'WMH [Start]
'Set objOpenIssue = Nothing
Set rsOpenIssueHdr = Nothing
Set cnnSql = Nothing
'WMH [End]									     																	
%>
<p>&nbsp;</p>
<p>&nbsp;</p>

</BODY></HTML>