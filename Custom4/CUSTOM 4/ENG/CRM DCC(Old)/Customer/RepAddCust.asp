<%@ Language=VBScript %>
<% 'Response.CacheControl = "no-cache" %>
<%' Response.AddHeader "Pragma", "no-cache" %>
<%' Response.Expires = -1 
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%End if
'ARD Home
'account=Session("ID")
'Ard
Addcustomer = True'trim(Request.Form ("editcust"))
'''''''''''''''''''Create UI object'''''''''''''''''''''''''''''
set oui = server.CreateObject ("CustomerUI.CustomUI")
oui.ConParameter = Application("DataConnectionString")
'if oui.Load (account) then
Set Connt = server.CreateObject("ADODB.Connection")
Connt.Open Application("DataConnectionString")
'ARD - Add the phone/Fax Format [Start]
Set Conn = server.CreateObject("ADODB.Connection")
Conn.Open Application("SystemConnectionString")
Set RSComp = server.CreateObject("ADODB.Recordset")
Set RSFormat = server.CreateObject("ADODB.Recordset")
		    
strsql = "select * from syccomp where ccomp_id='" & Session("CompanyID") & "'"
RSComp.Open strsql,conn
IF Not(RSComp.EOF And RSComp.BOF) Then
	strCountryCode = Trim(RSComp.Fields("ccont_code").Value )
End IF
RSComp.Close 
Set RSComp = Nothing
strsql = "select * from sycint where ccont_code='" & strCountryCode & "'"
		    
RSFormat.Open strsql,conn
IF Not(RSFormat.EOF And RSFormat.BOF) Then
	strFormat = RSFormat.Fields("CPHONETEMP").Value 
End IF

'ARD - Add the phone/Fax Format [End]
'WAL - check if there is a prospect customer selected
'then get his info[start]

if request("CustID") <> "" then
	MM_cnFox_STRING = Application("DataConnectionString")
	Dim rsCustomer
	set rsCustomer = server.CreateObject ("ADODB.Recordset")
	rsCustomer.Source = "Select * from Suwbfed where cwcompany ='"&trim(request("CustID"))&"' "
	rsCustomer.ActiveConnection = MM_cnFox_STRING
	rsCustomer.CursorType = 0
	rsCustomer.CursorLocation = 2
	rsCustomer.LockType = 1
	rsCustomer.Open()

	if not rsCustomer.EOF then
		strPick = "true"
	end if
end if

'WAL - check if there is a prospect customer selected
'then get his info[end]
'WAL_add ship via select[start]
dim rsCode
set rsCode = server.CreateObject ("ADODB.RECORDSET")
rsCode.Open "Select Crltd_vlu, Cdiscrep, Ccode_no,CrLtd_nam,crltfield from Codes where cdefcode+crltfield+cfld_name ='NNSHIPVIA' order by cDiscrep", connt
dim rsDef
set rsDef = server.CreateObject ("ADODB.RECORDSET")
rsDef.Open "Select Crltd_vlu, Cdiscrep, Ccode_no,CrLtd_nam,crltfield from Codes where cdefcode+crltfield+cfld_name ='DNSHIPVIA'", connt	
if not rsDef.EOF then
	strDefCode = trim(rsDef("ccode_no"))
else
	strDefCode = ""
end if
%>	

<html>
<head>
<title>CRM - Customer Profile</title>
<meta http-equiv="Content-Type" content="text/html;">
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/order.css">
<!-- #INCLUDE FILE="../common/format.asp" -->

<script language="JavaScript">

function PickCustomer()
{
	window.open("PickCustomer.asp","PickCustomer")
}

function anyCheckEdit(form) {
/*var total = 0;
var max = form.cont.length;
for (var idx = 0; idx < max; idx++) {
if (eval("document.ContEdit.cont[" + idx + "].checked") == true ) {
    total += 1;
  
   }
}
if (total==0 )
{
alert("Please,select one of the contacts to edit!");
}*/
	var total = 0;
var max = form.cont.length;
for (var idx = 0; idx < max; idx++)
 {
		if (eval("document.ContEdit.cont[" + idx + "].checked") == true ) 
			total += 1;
 }

if (total==0 )
	alert("Please,select one of the contacts to Edit!");	

}
////////////////////////////////////////////////////////////////////////////////////////
function anyCheckDelete(form) 
{
/*
		if(form.cont.value == "")
		{
			alert("Please,select one of the contacts to delete!");
			return false;
		}
	*/
	var total = 0;
var max = form.cont.length;
for (var idx = 0; idx < max; idx++)
 {
		if (eval("document.ContEdit.cont[" + idx + "].checked") == true ) 
			total += 1;
 }

if (total==0 )
	alert("Please,select one of the contacts to Delete!");	
}

//-->
</script>
</SCRIPT>
</head>
<body >
<SCRIPT LANGUAGE=javascript>
<!--

function do_validate()
{

	//alert(document.frmadd.Phone11.value)

	/*if (document.frmadd.txtAccount.value == "")
	{
		alert('Please enter the <%=Session("CustField")%> field.')
		document.frmadd.txtAccount.focus() 
		return false;
	}
	var strTxtAccount = document.frmadd.txtAccount.value
	if (strTxtAccount.length <5 )
	{
		alert('Customer Code Must be 5 Chrecters')
		document.frmadd.txtAccount.focus() 
		return false;
	}*/	
	if (document.frmadd.txtName.value == "")
	{
		alert('Please enter the name field.')
		document.frmadd.txtName.focus() 
		return false;
	}
	//password
	if (document.frmadd.txtPWD.value == '')
	{
		alert ("Please enter password!");
		document.frmadd.txtPWD.focus();
		return false;
	}
	//confirm password
	if (document.frmadd.txtCnfPWD.value == '')
	{
		alert ("Please enter password confirmation!");
		document.frmadd.txtCnfPWD.focus();
		return false;
	}
	//password & confirm password must be the same
	if (document.frmadd.txtPWD.value != document.frmadd.txtCnfPWD.value)
	{
		alert ("Password and Confirm password must be the same!");
		document.frmadd.txtCnfPWD.focus();
		return false;
	}	
/*	if (document.frmadd.txtaddr1.value == "")
	{
		alert('Please enter the Address  field.')
		document.frmadd.txtaddr1.focus() 
		return false;
	}
	if (document.frmadd.txtAddr3.value == "")
	{
		alert('Please enter the City field.')
		document.frmadd.txtAddr3.focus() 
		return false;
	}		
	if (document.frmadd.txtAddr4.value == "")
	{
		alert('Please enter the State field.')
		document.frmadd.txtAddr4.focus() 
		return false;
	}		

	if (document.frmadd.txtAddr6.value == "")
	{
		alert('Please enter the Country field.')
		document.frmadd.txtAddr6.focus() 
		return false;
	}*/		
	/*if (document.frmadd.Phone1.value == "")
	{
		alert('Please enter the phone field.')
		document.frmadd.Phone1.focus() 
		return false;
	}	
	
*/
	return true;
}


//-->
</SCRIPT>

<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><BR><BR><br></p>

<table border="1" width="95%" height=50 cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width=100% height=50 class="Title"><p><%=session("CustField")%> Profile</p></td>
    </tr>
	<%
	If Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
	If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"ADDCUST") <= 0 Then
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
    
	<tr><td width=100%>
	<div align="center">
      <center>
	<table width=100%  border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	    <form name="frmadd" id=frmadd action="AddPotCust.asp" method=post onSubmit="return do_validate();">
		
		<tr>
			<td Align="left" width="8%" class="dark_cell"><strong><%=session("CustField")%> ID</strong></td>
		    <td Align="left" width="16%" class="light_cell">
            <input name="txtAccount" size="30" <%if session("CustomerCodeType") = "Sequential" then Response.Write "readonly"%> maxlength="5" tabindex="1">
            <!--<INPUT type="button" value="Pick Prospect" id=button1 name="PickProspect" onClick="PickCustomer()">-->
            </td>
		    <td Align="left" valign="center" width="8%" class="dark_cell"><strong>Status</strong></td>
			<td Align="left" width="17%" class="light_cell">
		    	<!--WMA Replace only potential by ordinary 4 status-->
			   <!--<input name="txtStatus" size="30" value="Potential" maxlength="30" tabindex="1" readonly-->
   				<select name=lstStatus  tabindex="1">
   					<option value="A" selected>Active</option>
   					<option value="H">Hold</option>		
   					<option value="C">Cancel</option>		
					<option value="P">Potential</option>
			   </select>  
                </td>
		</tr>
		<tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>Name</strong></td>
		    <td Align="left" width="16%" class="light_cell">
            <input name="txtName" size="30" <%if strPick = "true" then%>value="<%=rsCustomer("cWCompany")%>"<%end if%>  maxlength="30" tabindex="1"></td>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=session("DBAField")%></strong></td>
		    <td Align="left" width="16%" class="light_cell">
            <input name="txtDBA" size="30" value="" maxlength="30" tabindex="9"></td>
	   </tr>	
	   <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>Address</strong></td>
		    <td Align="left" width="16%" class="light_cell">
            <input name="txtaddr1" size="30"  maxlength="30"<%if strPick = "true" then%>value="<%=rsCustomer("cAddress1")%>"<%end if%> tabindex="1"></td>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=session("PhoneField")%></strong></td>
		    <td Align="left" width="16%" class="light_cell">
		    <%
		    'ARD,301633 - Add phone/Fax Format [End]
						'WAL - check if there is a customer selected[start]
 						if strPick = "true" then
 							strSource = replace(rsCustomer("cWPhone"),"-","")
 							strSource = replace(strSource,"/","")
 							strSource = replace(strSource,".","")
 							strSource = replace(strSource,"(","")
 							strSource = replace(strSource,")","")
 						end if
 						
 						'WAL - check if there is a customer selected[end]
						intTotLegth = Len(strFormat)
						intOldPos = 1
						intPos1 = 1
						strOutPut = ""
						intpos = 1
						intcount = 1
						intTab = 10
						Do while Not intPos1 = 0
							intpos1 = instr(intOldPos, strFormat, "-",1)
							intpos2 = instr(intOldPos, strFormat, "/",1)
							IF intpos1 > intpos2 OR intpos1 = 0 Then
								intpos1 = intpos2
							End IF
							
							IF intpos1 = 0 Then
								if trim(strSource) <> "" then
									Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(strSource), intpos, intTotLegth)&""" name=""Phone1" & intcount &""" tabindex=""" & intTab & """>")
								else
									Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(oui.cust_phone1), intpos, intTotLegth)&""" name=""Phone1" & intcount &""" tabindex=""" & intTab & """>")
								end if
								
							Else
								if trim(strSource) <> "" then
									Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(strSource), intpos, intpos1 - intOldPos)&""" name=""Phone1" & intcount &""" tabindex=""" & intTab & """>")
								else
									Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(oui.cust_phone1), intpos, intpos1 - intOldPos)&""" name=""Phone1" & intcount &""" tabindex=""" & intTab & """>")
								end if
								
								Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
							End if
							intpos = intpos + intpos1 - intOldPos
							intOldPos = intpos1 + 1
							intcount = intcount + 1
							intTab = intTab + 1
						Loop
		    
		    
		    %>
		    
		    
		    
		    <%
		    ' ARD,301633 - Add phone/Fax Format [End]
		    
		    %> &nbsp;</td>
	   </tr>
	   <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>&nbsp;</strong></td>
		    <td Align="left" width="16%" class="light_cell">
            <input name="txtaddr2" size="30" value="" maxlength="30" tabindex="2"></td>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=session("Phone2Field")%></strong></td>
		    <td Align="left" width="16%" class="light_cell">
		    <%'ARD,301633 - Add phone/Fax Format [End]
						
 						intTotLegth = Len(strFormat)
						intOldPos = 1
						intPos1 = 1
						strOutPut = ""
						intpos = 1
						intcount = 1
						intTab = 14
						Do while Not intPos1 = 0
							intpos1 = instr(intOldPos, strFormat, "-",1)
							intpos2 = instr(intOldPos, strFormat, "/",1)
							IF intpos1 > intpos2 OR intpos1 = 0 Then
								intpos1 = intpos2
							End IF
							IF intpos1 = 0 Then
							
								Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(oui.cust_phone2), intpos, intTotLegth)&""" name=""Phone2" & intcount &""" tabindex=""" & intTab & """>")
							Else
								
								Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(oui.cust_phone2), intpos, intpos1 - intOldPos)&""" name=""Phone2" & intcount &""" tabindex=""" & intTab & """>")
								Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
							End if
							intpos = intpos + intpos1 - intOldPos
							intOldPos = intpos1 + 1
							intcount = intcount + 1
							intTab = intTab + 1
						Loop

		    
		    
		    %>
		    
		    
		    <%'ARD,301633 - Add phone/Fax Format [End]%> &nbsp;</td>
	   </tr>
	   <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>City</strong></td>
		    <td Align="left" width="16%" class="light_cell">
            <input name="txtAddr3" size="30"  <%if strPick = "true" then%>value="<%=rsCustomer("City")%>"<%end if%>maxlength="30" tabindex="3"></td>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=session("faxField")%></strong></td>
		    <td Align="left" width="16%" class="light_cell">
		    <%'ARD,301633 - Add phone/Fax Format [Start]
 						'WAL - check if there is a customer selected[start]
 						if strPick = "true" then
 							strSource = replace(rsCustomer("cWfax"),"-","")
 							strSource = replace(strSource,"/","")
 							strSource = replace(strSource,".","")
 							strSource = replace(strSource,"(","")
 							strSource = replace(strSource,")","")
 						end if
 						'WAL - check if there is a customer selected[end]
 						intTotLegth = Len(strFormat)
						intOldPos = 1
						intPos1 = 1
						strOutPut = ""
						intpos = 1
						intcount = 1
						intTab = 18
						Do while Not intPos1 = 0
							intpos1 = instr(intOldPos, strFormat, "-",1)
							intpos2 = instr(intOldPos, strFormat, "/",1)
							IF intpos1 > intpos2 OR intpos1 = 0 Then
								intpos1 = intpos2
							End IF
							IF intpos1 = 0 Then
								if trim(strSource) <> "" then
									Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(strSource, intpos, intTotLegth)&""" name=""fax" & intcount &""" tabindex=""" & intTab & """>")
								else
									Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(oui.cust_fax), intpos, intTotLegth)&""" name=""fax" & intcount &""" tabindex=""" & intTab & """>")
								end if
							Else
								if trim(strSource) <> "" then
									Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(strSource), intpos, intpos1 - intOldPos)&""" name=""fax" & intcount &""" tabindex=""" & intTab & """>")
								else
									Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(oui.cust_fax), intpos, intpos1 - intOldPos)&""" name=""fax" & intcount &""" tabindex=""" & intTab & """>")
								end if
								Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
							End if
							intpos = intpos + intpos1 - intOldPos
							intOldPos = intpos1 + 1
							intcount = intcount + 1
							intTab = intTab + 1
						Loop
		    
		    
		    
		    %>
		    
		    <%'ARD,301633 - Add phone/Fax Format [End]%> &nbsp;</td>
	   </tr>
	   <tr>
				<td Align="left" valign="center" width="8%" class="dark_cell"><strong>State</strong></td>
		    <td Align="left" width="16%" class="light_cell">
            <input name="txtAddr4" size="30"  maxlength="30" <%if strPick = "true" then%>value="<%=rsCustomer("State")%>"<%end if%>tabindex="4"></td>
		
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=Session("BuyerField")%></strong></td>
		    <td Align="left" width="16%" class="light_cell"><input name="txtBuyer" size="30" value="" maxlength="30" tabindex="22"></td>
	   </tr>		   
	   <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>Zip Code</strong></td>
		    <td Align="left" width="16%" class="light_cell">
            <input name="txtAddr5" size="30" <%if strPick = "true" then%>value="<%=rsCustomer("Zip")%>"<%end if%>maxlength="30" tabindex="5"></td>
		
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=session("keeperField")%></strong></td>
		    <td Align="left" width="16%" class="light_cell"><input name="txtKeeper" size="30" value="" maxlength="20" tabindex="23"></td>
	   </tr>
	   <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>Country</strong></td>
		    <td Align="left" width="16%" class="light_cell">
            <input name="txtAddr6" size="30" maxlength="20"<%if strPick = "true" then%>value="<%=rsCustomer("Country")%>"<%end if%> tabindex="6"></td>
            <td Align="left" valign="center" width="8%" class="dark_cell"><strong>Ship Via</strong></td>
		    <td Align="left" width="16%" class="light_cell">
				<select name=selShip  tabindex="24">
				<%do while not rsCode.EOF%>
					<option value="<%=rsCode("Ccode_no")%>" <%if oui.cust_shipvia = rsCode("Ccode_no") or trim(strDefCode) = trim(rsCode("Ccode_no")) then%>selected<%end if%>><%=rsCode("cDiscrep")%>
				<%rsCode.MoveNext ()
				  loop
				%>
			</select>
			</td>
	 
	   </tr>
	    <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>Password</strong></td>
		    <td Align="left" width="16%" class="light_cell">
				<input type="password" name="txtPWD" size="30" maxlength="8" tabindex="7">
            </td>
            <td Align="left" valign="center" width="8%" class="dark_cell"></td>
            <td Align="left" width="16%" class="light_cell"></td>
	   </tr>
	   <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>Confirm Password</strong></td>
		    <td Align="left" width="16%" class="light_cell">
				<input type="password" name="txtCnfPWD" size="30" maxlength="8" tabindex="8">
            </td>
            <td Align="left" valign="center" width="8%" class="dark_cell"></td>
            <td Align="left" width="16%" class="light_cell"></td>
	   </tr>
	   <tr>
		<!--WMA Move Save buttons under customer not contacts -->
		<td colspan=4 class="dark_cell" align=right>		
			<input type="submit" value=" Save " id="savecust" name="savecust" tabindex="25">
			<input type="button" value=" Cancel " id="Cancelcust" name="Cancelcust" onclick="javascript:location.href='repcust.asp';" tabindex="26">      
		</td>
	   <tr>
			<td colspan=4 height=50 width=100% class="Title">
			Contacts
			</td>
	   </tr>
				
	 <tr>
	    <td Align="left" valign="center" width="5%" class="dark_cell"><strong>Contact</strong></td>
	    <td Align="left" colspan=3 class="light_cell">
	   		<input type=text name="txtContact" size=30 value="<%=Request.Form ("txtContact")%>"  tabindex="27">
		</td>
	    <!--td Align="left" valign="center" width="8%" class="dark_cell"></td>
	    <td Align="left" width="16%" class="light_cell"></td-->
	</tr>
					<tr>
						 <td Align="left" valign="center" width="8%" class="dark_cell"><strong>Title</strong></td>
					     <td Align="left" colspan=3 class="light_cell">
							<input type="text" size="30" maxlength="30" value="" name="Title" tabindex="28">
					     </td>
					     <!--td Align="left" valign="center" width="8%" class="dark_cell"></td>
					     <td Align="left" width="16%" class="light_cell"></td-->
					</tr>
					<tr>
					    <td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=session("PhoneField")%></strong></td>
					    <td Align="left" colspan=3 class="light_cell">
					   	<%
						'WAL - check if there is a customer selected[start]
 						if strPick = "true" then
 							strSource = replace(trim(rsCustomer("cWPhone")),"-","")
 							strSource = replace(strSource,"/","")
 							strSource = replace(strSource,".","")
 							strSource = replace(strSource,"(","")
 							strSource = replace(strSource,")","")
 						end if
 						
 						'WAL - check if there is a customer selected[end]
						intTotLegth = Len(strFormat)
						intOldPos = 1
						intPos1 = 1
						strOutPut = ""
						intpos = 1
						intcount = 1
						intTab = 29
						Do while Not intPos1 = 0
							intpos1 = instr(intOldPos, strFormat, "-",1)
							intpos2 = instr(intOldPos, strFormat, "/",1)
							IF intpos1 > intpos2 OR intpos1 = 0 Then
								intpos1 = intpos2
							End IF
							
							IF intpos1 = 0 Then
								if trim(strSource) <> "" then
									Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(strSource), intpos, intTotLegth)&""" name=""ContPhone" & intcount &""" tabindex=""" & intTab & """>")
								else
									Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(oui.cust_phone1), intpos, intTotLegth)&""" name=""ContPhone" & intcount &""" tabindex=""" & intTab & """>")
								end if
								
							Else
								if trim(strSource) <> "" then
									Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(strSource), intpos, intpos1 - intOldPos)&""" name=""ContPhone" & intcount &""" tabindex=""" & intTab & """>")
								else
									Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(oui.cust_phone1), intpos, intpos1 - intOldPos)&""" name=""ContPhone" & intcount &""" tabindex=""" & intTab & """>")
								end if
								
								Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
							End if
							intpos = intpos + intpos1 - intOldPos
							intOldPos = intpos1 + 1
							intcount = intcount + 1
							intTab = intTab + 1
						Loop
						%>
						</td>
						<!--td Align="left" valign="center" width="8%" class="dark_cell"></td>
					    <td Align="left" width="16%" class="light_cell"></td-->
					    
					</tr>
					<tr>
					 <td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=session("faxField")%></strong></td>
					     <td Align="left" colspan=3 class="light_cell">
						<%
 							'WAL - check if there is a customer selected[start]
 							if strPick = "true" then
 								strSource = replace(rsCustomer("cWfax"),"-","")
 								strSource = replace(strSource,"/","")
 								strSource = replace(strSource,".","")
 								strSource = replace(strSource,"(","")
 								strSource = replace(strSource,")","")
 							end if
 							'WAL - check if there is a customer selected[end]
 							intTotLegth = Len(strFormat)
							intOldPos = 1
							intPos1 = 1
							strOutPut = ""
							intpos = 1
							intcount = 1
							intTab = 34
							Do while Not intPos1 = 0
								intpos1 = instr(intOldPos, strFormat, "-",1)
								intpos2 = instr(intOldPos, strFormat, "/",1)
								IF intpos1 > intpos2 OR intpos1 = 0 Then
									intpos1 = intpos2
								End IF
								IF intpos1 = 0 Then
									if trim(strSource) <> "" then
										Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(strSource, intpos, intTotLegth)&""" name=""Contfax" & intcount &""" tabindex=""" & intTab & """>")
									else
										Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(oui.cust_fax), intpos, intTotLegth)&""" name=""Contfax" & intcount &""" tabindex=""" & intTab & """>")
									end if
								Else
									if trim(strSource) <> "" then
										Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(strSource), intpos, intpos1 - intOldPos)&""" name=""Contfax" & intcount &""" tabindex=""" & intTab & """>")
									else
										Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(oui.cust_fax), intpos, intpos1 - intOldPos)&""" name=""Contfax" & intcount &""" tabindex=""" & intTab & """>")
									end if
									Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
								End if
								intpos = intpos + intpos1 - intOldPos
								intOldPos = intpos1 + 1
								intcount = intcount + 1
								intTab = intTab + 1
							Loop
						%>
					     </td>
					     <!--td Align="left" valign="center" width="8%" class="dark_cell"></td>
					     <td Align="left" width="16%" class="light_cell"></td-->
				    <tr>
						<td Align="left" valign="center" width="8%" class="dark_cell"><strong>E-mail</strong></td>
						<td Align="left" colspan=3 class="light_cell">
				   			<input type=text name="txtMail" size=30 value="<%=Request.Form ("txtMail")%>" tabindex="38">
						</td>
						<!--td Align="left" valign="center" width="8%" class="dark_cell"></td>
						<td Align="left" width="16%" class="light_cell"></td-->
					</tr>
			
		
	   <tr>
			<td colspan=4 class="dark_cell" align=right>
					<!--WMA Move Save buttons under customer not contacts -->
					<input type="submit" value=" Save " id="savecust2" name="savecust2"  tabindex="39">
					<input type="button" value=" Cancel " id="Cancelcust2" name="Cancelcust2" onclick="javascript:location.href='repcust.asp';"  tabindex="40">					
			</td>
		</tr>
	   </FORM>
    </table></center>
    </div>
      </td></tr></table>
<p>      
</body>