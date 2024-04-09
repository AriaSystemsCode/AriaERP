<%@ Language=VBScript %>
<%Response.Buffer = true
if Trim(Session("ID")) = "" and trim(Session("customerid"))="" then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
	parent.location.href="../login.asp"
	</script>
<%end if

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	else
		custid = Ucase(Trim(Session("customerid")))
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF


if trim (Session("rep")) = "" then
	CurCust = Session("ID")
else 
	CurCust = Session("CustomerID")
end if


Dim conFox
Set conFox = server.CreateObject("adodb.connection")
conFox.Open Application("DataConnectionString")

Dim rsCodes
Set rsCodes = server.CreateObject("ADODB.Recordset")
strsql = "select * from codes where cdefcode+crltfield+cfld_name = 'NNCPRO_CODE' "
rsCodes.Open strsql,conFox,1,3

Dim rsProfdesc
Set rsProfdesc = server.CreateObject("adodb.recordset")
if len(Request("lstProf")) > 0 or len(Request("rad1")) > 0 then
	if len(Request("rad1")) > 0 then
		prof = mid(Request("rad1"),1,6)
	else
		prof = Request("lstProf")
	end if
	strsql = "select * from arPrFCod where cpro_code+cpro_value like '" & prof & "%'"
else
	strsql = "select * from arPrFCod where cpro_code+cpro_value = ''"
end if
rsProfdesc.Open strsql,conFox,1,3
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 5.0">
<title>CRM - Customer Profile</title>
<meta http-equiv="Content-Type" content="text/html;">
<script LANGUAGE="javascript" src="checkForm.js"></script>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/customer.css">
<SCRIPT LANGUAGE=javascript>
<!--
function do_submtProf()
{
	document.frmMain.action='Custaddprof.asp'
	document.frmMain.submit()
}

function do_Ref()
{
	if(document.frmMain.lstRefe.value == 'NEW')
	{
		document.frmMain.txtref.disabled = false;
		document.frmMain.txtref.focus();
	}
}

function anyCheckDelete()
{
	var total = 0;
	if (eval("document.frmPro.rad1"))
	{
		if (isNaN(document.frmPro.rad1.length))
		{
			if(document.frmPro.rad1.checked)total=1;		
		}	
		else
		{
			var max = document.frmPro.rad1.length;
			for (var idx = 0; idx < max; idx++)
			 {
				if(eval("document.frmPro.rad1[" + idx + "].checked") == true)
				
					total += 1;
			 }
		}
	}

	if (total==0 )
	{
		alert("Please,select one of the profiles to Delete!");	
		return false;
	}
	document.frmPro.action = 'Custaddprof.asp?Type=2'
	return true;	

}

function anyCheckEdit()
{
	var total = 0;
	if (eval("document.frmPro.rad1"))
	{
		if (isNaN(document.frmPro.rad1.length))
		{
			if(document.frmPro.rad1.checked)total=1;		
		}	
		else
		{
			var max = document.frmPro.rad1.length;
			for (var idx = 0; idx < max; idx++)
			 {
				if(eval("document.frmPro.rad1[" + idx + "].checked") == true)
				
					total += 1;
			 }
		}
	}

	if (total==0 )
	{
		alert("Please,select one of the profiles to Edit!");	
		return false;
	}
	document.frmPro.action = 'Custaddprof.asp?Type=1'
	return true;	
}



//-->
</SCRIPT>


</HEAD>
<BODY>

<P><br>
<br>
&nbsp;</P>
<form method="POST" action="custsaveprof.asp?Val=<%=Request("rad1")%>" name="frmMain">
  <div align="center">
    <center>
    <table border="1" cellpadding="0" cellspacing="0" style="border-collapse: collapse" bordercolor="#111111" width="60%" id="AutoNumber1">
      <tr>
        <td width="30%" class="Dark_Cell"><b>&nbsp;Profile</b></td>
        <td width="70%" class="light_cell">
        <%
        if len(Request("rad1")) > 0 and Request("type") = 1 then
        %>
        <select size="1" name="lstProf" onchange="do_submtProf()" disabled>
        <Option value="">Select a profile
        <%
        Do while not rsCodes.EOF
			if mid(Request("rad1"),1,6) = trim(rsCodes.Fields("ccode_no").Value) then
				Response.Write("<option value=""" & Trim(rsCodes.Fields("ccode_no").Value)  & """ selected>" & rsCodes.Fields("cdiscrep").Value )
			else
				Response.Write("<option value=""" & Trim(rsCodes.Fields("ccode_no").Value)  & """>" & rsCodes.Fields("cdiscrep").Value )
			end if
			rsCodes.MoveNext 
        loop
        %>
        </select>
        <%
        else
        %>
        <select size="1" name="lstProf" onchange="do_submtProf()">
        <Option value="">Select a profile
        <%
        Do while not rsCodes.EOF
			if Request("lstProf") = trim(rsCodes.Fields("ccode_no").Value) then
				Response.Write("<option value=""" & Trim(rsCodes.Fields("ccode_no").Value)  & """ selected>" & rsCodes.Fields("cdiscrep").Value )
			else
				Response.Write("<option value=""" & Trim(rsCodes.Fields("ccode_no").Value)  & """>" & rsCodes.Fields("cdiscrep").Value )
			end if
			rsCodes.MoveNext 
        loop
        %>
        </select>
        <%
        End IF
        %>
        </td>
      </tr>
      <tr>
        <td width="30%" class="Dark_Cell"><b>&nbsp;Reference</b></td>
        <td width="70%" class="light_cell">
        <%
        IF len(Request("rad1")) > 0 and Request("type") = 1 then
        %>
        <select size="1" name="lstRefe" onchange="do_Ref()"  disabled>
        <option value="" selected>Select a reference
        <%
        Do while not rsProfdesc.EOF
			if mid(Request("rad1"),7) = trim(rsProfdesc.Fields("cpro_value").Value) then
				Response.Write("<option value=""" & trim(rsProfdesc.Fields("cpro_value").Value)  & """ selected>" & rsProfdesc.Fields("cpro_value").Value )
			else
				Response.Write("<option value=""" & trim(rsProfdesc.Fields("cpro_value").Value)  & """>" & rsProfdesc.Fields("cpro_value").Value )
			end if
			rsProfdesc.MoveNext 
        loop
        %>
        <option value="NEW">Other ...
        </select>
        <%Else%>
        <select size="1" name="lstRefe" onchange="do_Ref()">
        <option value="" selected>Select a reference
        <%
        Do while not rsProfdesc.EOF
			Response.Write("<option value=""" & trim(rsProfdesc.Fields("cpro_value").Value)  & """>" & rsProfdesc.Fields("cpro_value").Value )
			rsProfdesc.MoveNext 
        loop
        %>
        <option value="NEW">Other ...
        </select>
        
        <%End IF%>
        &nbsp;<input type="text" name="txtref" disabled></td>
      </tr>
      <tr>
        <td width="30%" class="Dark_Cell"><b>&nbsp;Profile Date</b></td>
        <td width="70%" class="light_cell">
        <input type="text" name="txtDate" size="10" maxlength=10 value="<%=date()%>" onFocus="javascript:vDateType='1'" onKeyUp="DateFormat(this,this.value,event,false,'1')" onBlur="DateFormat(this,this.value,event,true,'1')"></td>
      </tr>
      <tr>
        <td  align="right" colspan=2 >
        <input type="submit" value="Submit" name="B1">
        <input type="reset" value="Reset" name="B2"></td>
      </tr>
    </table>
    </center>
  </div>
</form>
<p><br>
&nbsp;</p>
<%
Dim rsProfiles
Set rsProfiles = server.CreateObject("ADODb.recordset")


if Request("type") = 2 then
	rsProfiles.Open "select * from profile where cconttype+ccont_id+store+cpro_code+cpro_value like 'C" & CurCust & "%' and cpro_code+cpro_value = '"&Request("rad1")&"'",conFox,1,3

	if not(rsProfiles.EOF and rsProfiles.BOF) then
		rsProfiles.Delete 
		rsProfiles.Update 
	end if
	rsProfiles.Close 
end if


strsql = "select profile.*,codes.* from profile inner join codes ON profile.Cpro_code = codes.ccode_no "
strsql = strsql & " where profile.cconttype+profile.ccont_id+profile.store+profile.cpro_code+profile.cpro_value like 'C" & CurCust & "%' "
strsql = strsql & " and cdefcode+crltfield+cfld_name = 'NNCPRO_CODE'"
'Response.Write("<br><font size=3>" & strsql & "<br></font>")
rsProfiles.Open strsql,conFox,1,3
%>
<div align="center">
  <center>
  <form name=frmPro action="custaddprof.asp" method=post>
  <table border="1" cellpadding="0" cellspacing="0" style="border-collapse: collapse" bordercolor="#111111" width="80%" id="AutoNumber2">
    <tr>
      <td class="Dark_Cell"><b>&nbsp;</b></td>
      <td width="33%" class="Dark_Cell"><b>&nbsp;Profile</b></td>
      <td width="33%" class="Dark_Cell"><b>&nbsp;Reference</b></td>
      <td width="34%" class="Dark_Cell"><b>&nbsp;Profile Date</b></td>
    </tr>
    <%
    IF rsProfiles.EOF and rsProfiles.BOF then
    %>
    <tr>
      <td width="33%" class="light_cell" colspan=4 align=center>&nbsp; No records found.</td>
    </tr>
    <%
    else
		Do while not rsProfiles.EOF 
    %>
			<tr>
			  <td width=1% class="light_Cell">
			  <input type="radio" value="<%Response.Write(trim(rsProfiles("cpro_code")) & trim(rsProfiles("cpro_value")))%>" name="rad1" id="rad1"></td>
			  <td width=33% class="light_cell">&nbsp;<%=trim(rsProfiles.Fields("cdiscrep").Value)%></td>
			  <td width=33% class="light_cell">&nbsp;<%=trim(rsProfiles.Fields("cpro_value").Value)%></td>
			  <td width=33% class="light_cell">&nbsp;<%=trim(rsProfiles.Fields("Dpro_date").Value)%></td>
			</tr>
    <%
			rsProfiles.MoveNext 
		Loop
	%>
    <TR>
		<td colspan=4 class="Dark_Cell" align=center>
		<input type="submit" value="  Edit  " id="Profile" name="Profile" onclick="return anyCheckEdit()">&nbsp;
		<input type="submit" value=" Delete " id="Profile" name="Profile" onclick="return anyCheckDelete()">
		</td>
    </TR>
	
	<%
    end if
    
    %>
  </table>
  </form>
  </center>
</div>

</BODY>
</HTML>