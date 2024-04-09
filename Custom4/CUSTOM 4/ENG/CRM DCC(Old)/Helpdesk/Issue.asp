<%@ LANGUAGE="VBSCRIPT" %>
<%
 Response.CacheControl = "no-cache" 
 Response.AddHeader "Pragma", "no-cache" 
 Response.Expires = -1 
 Response.Buffer=true
 
' ARD - 604499 change the field cwe_Mail with this one cEmail_Add

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
'count is a variable:when its value is "0",it indicates that this is first time a customer visits this page
	' to get the contact data of the first reported by person
	'And when its value is "1" ,it indicates that this is not the first time

%>

<HTML>
<HEAD>

<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/help.css">
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<title>CRM - Helpdesk - Open Issues - Add New Issue</title>
<script LANGUAGE="javascript">
<!--
function ChangeContact(strVal,objfrom)
{
	strPhone = strVal.options(strVal.selectedIndex).getAttribute("ContPhone",false);
	strFax = strVal.options(strVal.selectedIndex).getAttribute("ContFax",false);
	steEMail = strVal.options(strVal.selectedIndex).getAttribute("ContEMail",false);
	
	objfrom.txtPhone.value  = strPhone;
	objfrom.txtFax.value	= strFax;
	objfrom.txtEMail.value  = steEMail;

}



function formvalidator(frm)
{
	if(frm.txtSubject.value == "")
	{
		alert("Please enter the subject")
		frm.txtSubject.focus()
		return false;
	}
	
	if(frm.txtDetail.value == "")
	{
		alert("Please enter description")
		frm.txtDetail.focus()
		return false;
	}

	return true;
}

//-->
</script>
<script LANGUAGE="javascript" src="ClientScript.js">
</script>
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
		Select Case strType
			Case "L"
				GetIssueType = "Call Back"
			Case "B"
				GetIssueType = "Bug"
			Case "E"
				GetIssueType = "Enhancement"
			Case "C"
				GetIssueType = "Custom Program"
			Case "I"
				GetIssueType = "Open Issue"
			Case Else
				GetIssueType = "N/A"
		End Select
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
			Case Else
				GetIssueStatus = "N/A"
		End Select
	End Function

	Dim Conn
	'Session("ConnectionString") = Application("DataConnectionString")
	Session("ConnectionString") =  Application("SQLServer")'"dsn=webtrack1;uid=aria;pwd=aria"
	Set Conn = Server.CreateObject("ADODB.Connection")
	Conn.Open(Session("ConnectionString"))
	

	Dim strCustCode 'As String
	Dim strCustName 'As String
	Dim strStatus 'As String
	Dim strSupStatus 'As String

	If true then 'Not RSCust.eof Then
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
		
		
	Else
		Response.Write("Empty")
	End If
	
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
		strSqlWhere = "WHERE cCust_Id='"& Session("RSCust").Fields("account") &"' AND cissStat='O'"
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
		strSqlWhere = "WHERE cCust_Id='"& Session("RSCust").Fields("account") & "' AND cIssStat='" & Request("lstIssStatus") & "'" & strIssPriCr & strIssTypeCr
		
	End If
	
	rsIssues.Open "Select * From SUIssHdr " & strSqlWhere , cnnSql
	
	Session("ConnectionString") = Application("DataConnectionString")
	Set Conn = Server.CreateObject("ADODB.Connection")
	Conn.Open(Session("ConnectionString"))
	'count is a variable:when its value is "0",it indicates that this is first time a customer visits this page
	' to get the contact data of the first reported by person
	'And when its value is "1" ,it indicates that this is not the first time

	IF trim(request("count"))="" Then
		count="0"
	Else
		count=1
	End IF
	'Get Customer Contacts
	Dim RSContact
	Set RSContact = createobject("ADODB.RecordSet")
	RSContact.Open "SELECT * FROM Contact Where cContType='C' AND cCont_Id = '" & Trim(UCase(strCustCode))& "'",Conn
'RSTemp is a temporary recordset to get the first contact data(email,phone,fax) when the page loads for the first time
	Dim RSTemp
	Set RSTemp = createobject("ADODB.RecordSet")
	IF not RSContact.EOF And not RSContact.BOF Then
		RSContact.movefirst
'	End IF
	sqls="SELECT * From Contact where  cContType='C' AND cCont_Id = '" & Trim(UCase(strCustCode)) & "' and contact='"&trim(RSContact("contact"))&"'"
	RSTemp.Open sqls ,conn,1,3
	End IF
%>
<body topmargin="0" leftmargin="0" marginheight="0" marginwidth="0" bgcolor="#aecae6">

<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>

<%Else%>
<%End IF%>
<div align="center">
  <center>
  <table border="1" cellspacing="1" width="95%" id="AutoNumber1">
    <tr>
      <td width="100%" class="title">Helpdesk</td>
    </tr>
  </table>
  </center>
</div>

<form Name="Form1" id="Form1" method="post" action="ServerScripts/ADDIssue.asp?Criteria=O" onsubmit="return formvalidator(this)">

<div align="center">
  <center>

<table width=95% border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<TR>
		<TD width="33%" class="dark_cell"><%=session("CustField")%> ID</TD>
        <TD width="33%" class="dark_cell"><%=session("CustField")%> Name</TD>
        <TD width="34%" class="dark_cell">Status</TD>
	</TR>
    <TR>
		<TD class="light_cell">&nbsp;<input id="txtCustID" size="5" name="txtCustID" disabled Value="<%=strCustCode%>"></TD>
        <TD class="light_cell">&nbsp;<input id="txtCustName" name="txtCustName" disabled Value="<%Response.Write(strCustName)%>" size="20"></TD>
        <TD class="light_cell">&nbsp;<input id="txtStatus" size="6" name="txtStatus" disabled Value="<%Response.Write(strStatus)%>"></TD>
	</TR>
</TABLE>


  </center>
</div>
<div align="center">
  <center>


<table width=95% border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<tr>
		<td align='left' colspan=4 class="dark_cell">Issue Details</td>
	</tr>
	<TR>
		<TD class="dark_cell">Type</TD>
		<TD class="dark_cell" >Submission Date</TD>
		<TD colspan=2 class="dark_cell">Priority</TD>
	</TR>
	<TR>
		<TD align=left class="light_cell">
	          &nbsp;<SELECT style="WIDTH: 145px" id=lstIsseType name=lstIsseType >
          
						<%
							Dim rsIssueType
							Set rsIssueType = Server.CreateObject("ADODB.Recordset")
							Dim strCodesSQL
							Dim strDefCodeSQL
							strDefCodeSQL = "Select cCode_no, cDiscrep From Codes Where cFld_Name='CISSUTYPE  ' and cDefCode='D'"
							rsIssueType.Open strDefCodeSQL,Conn
							Dim strDefCode
							If Not rsIssueType.EOF Then
								strDefCode = Trim(rsIssueType("cCode_no"))
							End IF
							rsIssueType.Close()
							strCodesSql = "Select cCode_no, cDiscrep From Codes Where cFld_Name='CISSUTYPE  ' and cDefCode='N'"
							rsIssueType.Open strCodesSql,Conn
						%>
<!-- WMH -->
							<!--<OPTION value='A'>All-->
<!-- WMH -->							
						<% Do While Not rsIssueType.EOF %>
						
									<OPTION 

										<%if Trim(request("lstIsseType")) = "" then 
											if Trim(rsIssueType("cCode_no")) = Trim(strDefCode) then
												Response.Write "Selected"
											end if
										else
											If Trim(rsIssueType("cCode_no")) = Trim(request("lstIsseType"))  Then
										'If Trim(request("lstIsseType")) = Trim(strDefCode) Then
												Response.Write("Selected")
											End If
										end if
										%>
												value="<%Response.Write(trim(rsIssueType("cCode_no")))%>">
										<%Response.Write(Trim(rsIssueType("cDiscrep")))
											%>
										
						<%
									rsIssueType.MoveNext()
							Loop
						%>
          </SELECT>

		</TD>
		<TD class="light_cell">&nbsp;<INPUT disabled name=txtDate size=10 value=<%Response.Write(Date())%>></TD>
		<TD colspan=2 class="light_cell">
			&nbsp;<select size="1" name="lstPri" style="width:215px">
				<option value="V">Very Urgent
				<option value="U">Urgent
				<option Selected value="N">Normal
				<option value="L">Low
			</select></TD>
	</TR>
		<%IF not RSContact.EOF And not RSContact.BOF Then%>
	<TR>
		<TD align=left class="dark_cell">Reported By</TD>
		<TD align=left class="dark_cell">Email:</TD>
		<TD align=left class="dark_cell">Phone:</TD>
		<TD class="dark_cell">Fax:</TD>
	</TR>

	<TR>
		<TD align=left class="light_cell">
				
				&nbsp;<select size="1" name="lstReportedBy" onchange="ChangeContact(this,this.form)">
				<%if not RSContact.BOF and not RSContact.EOF then%>
					<%
					Do While Not RSContact.EOF%>
					<option value="<%Response.Write(Trim(RSContact("Contact")))%>" <%If trim(Request.Form("lstReportedby"))=Trim(RSContact("Contact")) Then Response.Write "Selected"   end if%> ContPhone="<%Response.Write(Trim(RSContact("Phone")))%>" ContFax="<%Response.Write Trim(RSContact("FAX"))%>" ContEMail="<%
					' ARD - 604499 change the field cwe_Mail with this one cEmail_Add [Start]
					'Response.Write trim(RSContact("cwe_mail"))
					Response.Write trim(RSContact("cEmail_Add"))
					' ARD - 604499 change the field cwe_Mail with this one cEmail_Add [End]
					%>">
					<%Response.Write (Trim(RSContact("Contact")))%>
					<%
					RSContact.MoveNext()
					Loop%>
				<%else%>
				<option value="" ContPhone="" ContFax="" ContEMail="">
				<%end if%>	
				</select>
				
		</TD>
		<TD align=left class="light_cell">
			&nbsp;<INPUT name=txtEMail size=40 maxlength="60" value=<%IF trim(Request.form("txtEmail"))="" and count="0"  Then 
			' ARD - 604499 change the field cwe_Mail with this one cEmail_Add [Start]
			'Response.write Trim(RSTemp("cwe_mail"))
			Response.write Trim(RSTemp("cEmail_Add"))
			' ARD - 604499 change the field cwe_Mail with this one cEmail_Add
			 ELSE Response.write trim(Request.form("txtEmail")) End If%>>
		</TD>
		<TD align=left class="light_cell">&nbsp;<INPUT name=txtPhone size=14 MaxLength=16 value=<%IF trim(Request.form("txtphone"))="" and count="0" Then Response.write Trim(RSTemp("Phone"))  ELSE Response.write trim(Request.form("txtphone")) End If%>></TD>
		<TD class="light_cell">&nbsp;<INPUT name=txtFax size=13 MaxLength=16 value=<%IF trim(Request.form("txtfax"))=""  and count="0" Then Response.write  Trim(RSTemp("FAX")) ElSE Response.write trim(Request.form("txtFax")) End If%> ></TD></TR>
	<TR>
	<%End If%>
		<TD align=left colSpan=4 class="dark_cell">Subject</TD>
	</TR>
	<TR>
		<TD align=center colSpan=4 class="light_cell">&nbsp;<INPUT name=txtSubject size="66" ERRORTXT="Please enter the subject!" Req="Y" MaxLength=60></TD>
	</TR>
	<TR>
		<TD colSpan=4 class="dark_cell">Detail Description</TD>
	</TR>
	<TR>
		<TD align=center colspan=4 vAlign=top class="light_cell">
			<TEXTAREA cols="50" name="txtDetail" rows="10" Req="Y" ERRORTXT="Please enter the details!" MaxLength="7000"></TEXTAREA>
		</TD>
	</TR>
	<tr>
		<td Colspan=4 align=Right class="dark_cell">
			<FONT face=Arial size=2><INPUT name=cmdOk type=submit value=Submit name="cmdOk" >
		</td>
	</tr>
</TABLE>
  </center>
</div>
<br>
</form>

</body>
</html>