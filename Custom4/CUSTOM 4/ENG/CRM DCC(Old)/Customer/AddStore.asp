<%@ Language=VBScript %>
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>
<%

strAccount = Session("CustomerID")

Addcustomer = True'trim(Request.Form ("editcust"))
'''''''''''''''''''Create UI object'''''''''''''''''''''''''''''
set oui = server.CreateObject ("CustomerUI.CustomUI")
oui.ConParameter = Application("DataConnectionString")


'Add the phone/Fax Format [Start]
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
'Add the phone/Fax Format [End]
'WAL_add ship via select[start]
Set Connt = server.CreateObject("ADODB.Connection")
Connt.Open Application("DataConnectionString")
dim rsCode
set rsCode = server.CreateObject ("ADODB.RECORDSET")
rsCode.Open "Select Crltd_vlu, Cdiscrep, Ccode_no,CrLtd_nam,crltfield from Codes where cdefcode+crltfield+cfld_name ='NNSHIPVIA' order by cDiscrep", connt



%>	

<html>
<head>
<title>CRM - Customer Profile</title>
<meta http-equiv="Content-Type" content="text/html;">
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/customer.css">
<!-- #INCLUDE FILE="../common/format.asp" -->

<script language="JavaScript">

function anyCheckEdit(form) 
{
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

function ChangeToUCase(objText)
{
	//alert(objText.value)
	var strVal=objText.value 
	objText.value = strVal.toUpperCase()
	//document.frmadd.txtaddr1.
}

function ApplyPhoneFormat(objText)
{

// Get the curent object name
var objName = objText.name

// The next object name is the current object name -1
var NextObjName = objName.substring(0,objName.length-1)

// The rest of the object name is the last charecter
var ObjRestName = objName.substring(objName.length-1,objName.length)

// Then the rest object name is the last of the current +1
ObjRestName = eval(ObjRestName)+1

// Here we have a full object name to be evaluated
var fullObjName = NextObjName+ObjRestName

// Evaluate the name that we got
var NextObj = eval('objText.form.'+fullObjName)

//Encode the name
var ObjString = escape(NextObj);

// If the object word found then this is an object you should proceed
if (ObjString.search('object')>0)
	{
		// If the current length is the maximum length then skip to the next object in this series
		if (objText.value.length==objText.maxLength)
		{
			NextObj.focus() 
		}
	}
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

	if (document.frmadd.txtAccount.value == "")
	{
		alert('Please enter the <%=Session("CustField")%> field.')
		document.frmadd.txtAccount.focus() 
		return false;
	}

	if (document.frmadd.txtStore.value == "")
	{
		alert('Please enter the <%=Session("StoreField")%> field.')
		document.frmadd.txtStore.focus() 
		return false;
	}

	if (document.frmadd.txtStatus.value == "*")
	{
		alert('Please select status.')
		document.frmadd.txtStatus.focus() 
		return false;
	}

	return true;
}


//-->
</SCRIPT>
<%If Trim(Session("rep"))="" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>

<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<%End If%>
<p><BR><BR><br></p>

<table border="1" width="95%" height=50 cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width=100% height=50 class="Title"><p><%=session("CustField")%> Profile</p></td>
    </tr>
    
    <table width=100% align=center>
	<tr><td width=100%>
	<div align="center">
      <center>
	<table width=95%  border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	  <form name="frmadd" id=frmadd action="SaveStore.asp" method=post onSubmit="return do_validate();">
		<tr>
			<td Align="left" width="8%" class="dark_cell">
				<strong><%=session("CustField")%></strong>
			</td>
		  <td Align="left" width="16%" class="light_cell">
          <input name="txtAccount" size="10" maxlength="5" tabindex="1" Value="<%=strAccount%>" ReadOnly>
      </td>
			<td Align="left" width="8%" class="dark_cell">
				<strong><%=session("StoreField")%></strong>
			</td>
		  <td Align="left" width="16%" class="light_cell">
          <input name="txtStore" size="10" maxlength="8" tabindex="1" onChange="return ChangeToUCase(this)">
      </td>

		</tr>
		<tr>
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong>Name</strong>
			</td>
		  <td Align="left" width="16%" class="light_cell">
				<input name="txtName" size="30" value="" maxlength="30" tabindex="1">
			</td>
			<td Align="left" valign="center" width="16%" class="dark_cell"><strong>Ship Via</strong></td>
			<td Align="left" valign="center" class="light_cell">
			<select name=selShip>
				<%do while not rsCode.EOF%>
					<option value="<%=rsCode("Ccode_no")%>" ><%=rsCode("cDiscrep")%>
				<%rsCode.MoveNext ()
				  loop
				%>
			</select>
			</TD>
		  
	  </tr>	
	   
	  <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong>Address</strong>
			</td>
		  <td Align="left" width="16%" class="light_cell">
				<input name="txtaddr1" size="30" value="" maxlength="30" tabindex="1">
			</td>
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong>Status</strong>
			</td>
			<td Align="left" width="17%" class="light_cell">
			<%'Response.Write "SS"&Trim(Session("M_SRADDSTR"))%>
				<Select Name="txtStatus">
					<%
						'If customer Mode then this customer should be Active and will add stores of staus Active
						If Trim(Session("rep"))="" Then 'Customer Setup
							%>
								<Option Value="A"> Active </Option>
							<%
						Else 'SalesRep Setup
							'Status Field Depends on the available rule
							Select Case Trim(Session("M_SRADDSTR"))
								Case "B"
									%>
										<Option Value="*">-- Select --</Option>
										<Option Value="P">Potential</Option>
										<Option Value="A">Active</Option>
									<%
								Case "A"
									%>
										<Option Value="A">Active</Option>
									<%
								Case "P"
									%>
										<Option Value="P">Potential</Option>
									<%
								case else
									%>
										<Option Value="P">Potential</Option>
									<%
							End Select
						End If
					%>
				</Select>
      </td>
			

	  </tr>
	  <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong>&nbsp;</strong>
			</td>
		  <td Align="left" width="16%" class="light_cell">
				<input name="txtaddr2" size="30" value="" maxlength="30" tabindex="2">
			</td>
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong><%=session("DBAField")%></strong>
			</td>
		  <td Align="left" width="16%" class="light_cell">
				<input name="txtDBA" size="30" value="" maxlength="30" tabindex="7">
			</td>
			
	  </tr>
	  <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong>City</strong>
			</td>
		  <td Align="left" width="16%" class="light_cell">
            <input name="txtAddr3" size="30" value="" maxlength="30" tabindex="3">
      </td>
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong><%=session("PhoneField")%></strong>
			</td>
		    <td Align="left" width="16%" class="light_cell">
					<%
						intTotLegth = Len(strFormat)
						intOldPos = 1
						intPos1 = 1
						strOutPut = ""
						intpos = 1
						intcount = 1
						intTab = 8
						Do while Not intPos1 = 0
							intpos1 = instr(intOldPos, strFormat, "-",1)
							intpos2 = instr(intOldPos, strFormat, "/",1)
							IF intpos1 > intpos2 OR intpos1 = 0 Then
								intpos1 = intpos2
							End IF
								
							IF intpos1 = 0 Then
								Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(oui.cust_phone1), intpos, intTotLegth)&""" name=""Phone1" & intcount &""" tabindex=""" & intTab & """ onKeyPress='ApplyPhoneFormat(this)'>")
							Else
								Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(oui.cust_phone1), intpos, intpos1 - intOldPos)&""" name=""Phone1" & intcount &""" tabindex=""" & intTab & """ onKeyPress='ApplyPhoneFormat(this)'>")
								Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
							End if
							intpos = intpos + intpos1 - intOldPos
							intOldPos = intpos1 + 1
							intcount = intcount + 1
							intTab = intTab + 1
						Loop
					%>&nbsp;
			</td>
			
	  </tr>
	  <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong>State</strong>
			</td>
			<td Align="left" width="16%" class="light_cell">
				<input name="txtAddr4" size="30" value="" maxlength="30" tabindex="4">
			</td>
		  <td Align="left" valign="center" width="8%" class="dark_cell">
				<strong><%=session("Phone2Field")%></strong>
			</td>
		  <td Align="left" width="16%" class="light_cell">
		    <%
 					intTotLegth = Len(strFormat)
					intOldPos = 1
					intPos1 = 1
					strOutPut = ""
					intpos = 1
					intcount = 1
					intTab = 12
					Do while Not intPos1 = 0
						intpos1 = instr(intOldPos, strFormat, "-",1)
						intpos2 = instr(intOldPos, strFormat, "/",1)
						IF intpos1 > intpos2 OR intpos1 = 0 Then
							intpos1 = intpos2
						End IF
						IF intpos1 = 0 Then
							'strOutPut = strOutPut & mid(strSource, intpos, intTotLegth)
							Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(oui.cust_phone2), intpos, intTotLegth)&""" name=""Phone2" & intcount &""" tabindex=""" & intTab & """ onKeyPress='ApplyPhoneFormat(this)'>")
						Else
							'strOutPut = strOutPut & mid(strSource, intpos, intpos1 - intOldPos)
							Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(oui.cust_phone2), intpos, intpos1 - intOldPos)&""" name=""Phone2" & intcount &""" tabindex=""" & intTab & """ onKeyPress='ApplyPhoneFormat(this)'>")
							Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
						End if
						intpos = intpos + intpos1 - intOldPos
						intOldPos = intpos1 + 1
						intcount = intcount + 1
						intTab = intTab + 1
					Loop
		    %>&nbsp;
		  </td>
			
		</tr>		   
	  <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong>Zip Code</strong>
			</td>
			<td Align="left" width="16%" class="light_cell">
				<input name="txtAddr5" size="30" value="" maxlength="30" tabindex="5">
			</td>
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong><%=session("FaxField")%></strong>
			</td>
		  <td Align="left" width="16%" class="light_cell">
		    <%
 					intTotLegth = Len(strFormat)
					intOldPos = 1
					intPos1 = 1
					strOutPut = ""
					intpos = 1
					intcount = 1
					intTab = 16
					Do while Not intPos1 = 0
						intpos1 = instr(intOldPos, strFormat, "-",1)
						intpos2 = instr(intOldPos, strFormat, "/",1)
						IF intpos1 > intpos2 OR intpos1 = 0 Then
							intpos1 = intpos2
						End IF
						IF intpos1 = 0 Then
							'strOutPut = strOutPut & mid(strSource, intpos, intTotLegth)
							Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(oui.cust_fax), intpos, intTotLegth)&""" name=""fax" & intcount &""" tabindex=""" & intTab & """ onKeyPress='ApplyPhoneFormat(this)'>")
						Else
							'strOutPut = strOutPut & mid(strSource, intpos, intpos1 - intOldPos)
							Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(oui.cust_fax), intpos, intpos1 - intOldPos)&""" name=""fax" & intcount &""" tabindex=""" & intTab & """ onKeyPress='ApplyPhoneFormat(this)'>")
							Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
						End if
						intpos = intpos + intpos1 - intOldPos
						intOldPos = intpos1 + 1
						intcount = intcount + 1
						intTab = intTab + 1
					Loop
		    %>&nbsp;
		  </td>
			
		</tr>
		<tr>
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong>Country</strong>
			</td>
		  <td Align="left" width="16%" class="light_cell">
				<input name="txtAddr6" size="30" value="" maxlength="20" tabindex="6">
			</td>
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong><%=session("BuyerField")%></strong>
			</td>
		  <td Align="left" width="16%" class="light_cell">
				<input name="txtBuyer" size="30" value="" maxlength="30" tabindex="20">
			</td>
			
	  </tr>
	  <tr>
			<td colspan=2 class="dark_cell">
				&nbsp;
			</td>
			
			<td Align="left" valign="center" width="8%" class="dark_cell">
				<strong><%=session("keeperField")%></strong>
			</td>
			<td Align="left" width="16%" class="light_cell">
				<input name="txtKeeper" size="30" value="" maxlength="20" tabindex="21">
			</td>

		</tr>
	  
	  <tr>
			
			<td colspan=4 class="dark_cell" align=right>
				<input type="submit" value=" Save " id="savecust" name="savecust" tabindex="22">
				<input type="button" value=" Cancel " id="Cancelcust" name="Cancelcust" onclick="javascript:location.href='custprof.asp';" tabindex="23"> 
			</td>
		</tr>
	</FORM>
</table>
</center>
</div>
</td></tr>
<br>
<BR>
</body>