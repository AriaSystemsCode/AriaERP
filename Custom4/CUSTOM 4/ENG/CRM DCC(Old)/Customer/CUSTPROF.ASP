<%@ Language=VBScript %>
<%Response.Buffer = true
 Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>
<%
Dim Conn
if session("ID") = "" then
	Session("ID") = Application("ID")
	
	Set Conn = server.CreateObject("ADODB.Connection")
	Conn.Open Application("DataConnectionString")

	Set Session("RSCust") = Server.CreateObject("ADODB.RecordSet")
	strsql = "Select * from customer where type+account+store like'M" & Ucase(trim(Session("ID"))) & "%'"

	'WMA change Session("RSCust") to be updatable [Start]
	'Session("RSCust").open strsql,Conn
	Session("RSCust").open  strSql,conn , 2, 4
	'Session("ID")  = trim(Session("RSCust")("account"))
	'Response.Write "<font size=2>" & Session("ID")

end if
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
'account=Session("ID")
editcustomer = trim(Request.Form ("editcust"))


'''''''''''''''''''Create UI object'''''''''''''''''''''''''''''

set ouiCustomer = server.CreateObject ("CustomerUI.CustomUI")
ouiCustomer.ConParameter = Application("DataConnectionString")
Set Connt = server.CreateObject("ADODB.Connection")
Connt.Open Application("DataConnectionString")

if ouiCustomer.Load (trim(CurCust)) then
'Am I going to load the main account
'Response.Write(Trim(Request("Store")))
'Response.End 
If Trim(Request("Store")) = "" Then
	Set oui =  ouiCustomer
Else
	ouiCustomer.ChildFind 2,"Store='" & Trim(Request("Store")) & "'"
	If Not(ouiCustomer.ChildEOF(2)) Then
		Set oui = ouiCustomer.ChildGet(2)
	Else
		'Set oui =  ouiCustomer
		Response.Write("Can't Load Store Data")
		Response.End 
	End If
End If

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

'WAL_add ship via select[start]
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

<SCRIPT LANGUAGE=javascript>
<!--
function TextChange(objText)
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
</SCRIPT>

<SCRIPT LANGUAGE=javascript>
<!--
function do_Profile()
{
	window.open ('Custaddprof.asp','')
	//window.open(,'',',);
	return true
}
function PickStore(ThisForm)
{
	if (ThisForm.Store.value=="-ADDNEW-")
	{
		ThisForm.action = "AddStore.asp"
	}
	ThisForm.submit();
	return true;
}
function GoAddNewStore(ThisForm)
{
	ThisForm.action = "AddStore.asp"
	ThisForm.submit();
	return true;

}
//-->
</SCRIPT>


<script language="JavaScript">


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
//nek
var total = 0;
if (eval("ContEdit.cont"))
{
	if (isNaN(form.cont.length))
	{
		if(ContEdit.cont.checked)total=1;		
	}	
	else
	{
		var max = form.cont.length;
		for (var idx = 0; idx < max; idx++)
		 {
			if(eval("document.ContEdit.cont[" + idx + "].checked") == true)
			
				total += 1;
		 }
	}
	
}

if (total==0 )
{
	alert("Please,select one of the contacts to Edit!");	
	return false;
}
return true;	
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
/*	var total = 0;
var max = form.cont.length;
for (var idx = 0; idx < max; idx++)
 {
		if (eval("document.ContEdit.cont[" + idx + "].checked") == true ) 
			total += 1;
 }
*/
//nek
var total = 0;
if (eval("ContEdit.cont"))
{
	if (isNaN(form.cont.length))
	{
		if(ContEdit.cont.checked)total=1;		
	}	
	else
	{
		var max = form.cont.length;
		for (var idx = 0; idx < max; idx++)
		 {
			if(eval("document.ContEdit.cont[" + idx + "].checked") == true)
			
				total += 1;
		 }
	}
}
if (total==0 )
{
	alert("Please,select one of the contacts to Edit!");	
	return false;
}
return true;	
}

function openwindow(filename) 
{  
	window.open(filename,'','toolbar=no,status=no,scrollbars=no,location=no,resizable=yes,menubar=no,directories=no,top=' + ((window.screen.height-500)/2) + ',left=' + ((window.screen.width-800)/2) + ',width=800,height=500')
}

//-->
</script>
</SCRIPT>
</head>
<body >

<%IF strFile = "cust" Then%>
	<SCRIPT LANGUAGE="JavaScript1.2"
	        SRC="../HM_Loader_Cust.js"
	        TYPE='text/javascript'>
	</SCRIPT>
	<P><br><BR><BR></p>

<%Else%>
	<SCRIPT LANGUAGE="JavaScript1.2"
	        SRC="../HM_Loader_Sales.js"
	        TYPE='text/javascript'>
	</SCRIPT>
	<p><br><br><br></p>

	<%
	Response.Write(Application(Session("ID")))
	%>
	<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
	<TR>
		<!-- ARD -->
		<TD colspan=13>
		<P>Your currently selected <%=Session("CustField")%> is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%></P>
		</TD>
		<TD align=right><!--a href="repcust.asp" style="text-decoration: none">Get <%=Session("CustField")%></a-->
			<input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button2 name=button2>
		</TD>
		<!-- ARD -->
	</TR>
	</table>
<%End IF%>

<table border="1" width="95%" height=50 cellspacing="0" cellpadding="0" align=center>
    <tr>
      <td width=100% height=50 class="Title"><p><%=Session("CustField")%>  Profile</p></td>
    </tr>
    <tr><td><%=Error%>&nbsp;</td></tr>
<%
If editcustomer <> "Edit" then%>
	<tr><td width=100%>
	<%
	'wal_130674 correct check on privilage case multi user
	if Application("CustomerLoginUsing")= "User"  then
		strAppUserVar = Application("userID")
	ElseIf Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
	'If Trim(Session("Rep")) = "" Then
		'strAppUserVar = Session("ID")
	'Else
		'strAppUserVar = Session("Rep")
	'End If
	

	If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"VIEWCUST") <= 0 Then
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

	<div align="center">
      <center>
      <form name="editcustomer" action="custprof.asp" method="post">
	<table width=100% border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
		<tr>
			<td Align="left" width="16%" class="Dark_Cell"><strong><%=Session("CustField")%> ID</strong></td>
		    <td Align="left" width="33%" class="light_cell"><%=oui.cust_account %>&nbsp;</td>

			<td Align="left" width="16%" class="Dark_Cell"><strong><%=Session("StoreField")%></strong></td>
		    <td Align="left" width="33%" class="light_cell">
					<Select Name="Store" onChange="PickStore(this.form)">
					<option Value=""> Main</option>
					<%
					If not ouiCustomer.ChildEOF(2) Then
						ouiCustomer.ChildMoveFirst(2)
						Do While not ouiCustomer.ChildEOF(2)
							Set objStores = ouiCustomer.ChildGet(2)
							%>
							<Option Value='<%=objStores.cust_store%>' <%If Trim(Request("Store"))=Trim(objStores.cust_store) then Response.Write("Selected")%>><%Response.Write(objStores.cust_store&" - "&objStores.cust_stname)%> </Option>
							<%
							Set objStores = Nothing
							ouiCustomer.ChildMoveNext(2)
						Loop
					End If
					%>
					</select>
					<% 
						Dim bolCanAddStore 'As Boolean to determine if this user can add stores
						bolCanAddStore= False
						If Session("Rep") = "" Then 'If the logged in user is a customer
							If UCase(Session("M_CSADDSTR")) = "Y" Then 'check if generally customers are able to add stores
								bolCanAddStore = (ouiCustomer.cust_status ="A") 'True 'Yes
							End If
						Else 'Logged in user is Sales Rep.
							If UCase(Trim(Session("M_SRADDSTR"))) = "N" Then 'if the sales rep setting is none then can't add stores
							Else
								bolCanAddStore = True 'Can Add stores
							End If
						End If
					%>
					<%If bolCanAddStore Then%>
						<%
							If Trim(Session("Rep")) = "" Then
								strAppUserVar = Session("ID")
							Else
								strAppUserVar = Session("Rep")
							End If

							If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"ADDSTORE") <= 0 Then
							%>
							<%
							Else%>
								<Input type="button" Name="AddStore" Value="New <%=Session("StoreField")%>" onclick="GoAddNewStore(this.form)">
							<%
							End If%>
					<%End If%>
		    </td>
		</tr>
		<tr>
			
			<td Align="left" class="Dark_Cell" ><strong>Name</strong></td>
		    <td Align="left" class="light_cell"><%=oui.cust_stname %>&nbsp;</td>
			<td Align="left" valign="center" width="16%" class="dark_cell"><strong>Ship Via</strong></td>
			<td Align="left" valign="center" class="light_cell">
			<select name=selShip disabled>
				<%do while not rsCode.EOF%>
					<option value="<%=rsCode("Ccode_no")%>" <%if ouiCustomer.cust_shipvia = rsCode("Ccode_no") or oui.cust_shipvia = rsCode("Ccode_no") then%>selected<%end if%>><%=rsCode("cDiscrep")%>
				<%rsCode.MoveNext ()
				  loop
				%>
			</select>
			</TD>
		</tr>
		<tr>
			<td Align="left" class="Dark_Cell" ><strong>Address</strong></td>
		    <td Align="left" class="light_cell"><%=oui.cust_caddress1  %>&nbsp;</td>

		    <td Align="left" valign="center" width="16%" class="dark_cell"><strong>Status</strong></td>
  			<td Align="left" valign="center" class="light_cell">
  				<%
  				'WMA Replace only potential by ordinary 4 status
  				'select case ucase(oui.cust_status)
				'	case "A" :
				'		Response.Write "Active"
				'	case "H" :
				'		Response.Write "Hold"
				'   case "C" :
				'		Response.Write "Cancel"
				'   case "P" :
				'		Response.Write "Potential"
				'	end select
				%>				
    			<select name=lstStatus  tabindex="1" disabled>
   					<option value="A" <% if ucase(oui.cust_status)="A" then%>selected<%end if%>>Active</option>
   					<option value="H" <% if ucase(oui.cust_status)="H" then%>selected<%end if%>>Hold</option>		
   					<option value="C" <% if ucase(oui.cust_status)="C" then%>selected<%end if%>>Cancel</option>		
					<option value="P" <% if ucase(oui.cust_status)="P" then%>selected<%end if%>>Potential</option>
			   </select>  
			 &nbsp;</td>
			 
	   </tr>	
	   <tr>
			<td Align="left" class="Dark_Cell" ><strong>&nbsp;</strong></td>
		    <td Align="left" class="light_cell"><%=oui.cust_caddress2  %>&nbsp;</td>

			<td Align="left" valign="center" class="dark_cell" ><strong><%=session("DBAField")%></strong></td>
		    <td Align="left" class="light_cell"><%=oui.cust_dba%>&nbsp;</td>


	   </tr>
	   <tr>
			<td Align="left" class="Dark_Cell" ><strong>City</strong></td>
		    <td Align="left" class="light_cell"><%=oui.cust_caddress3  %>&nbsp;</td>

			<td Align="left" valign="center" class="dark_cell" ><strong><%=session("PhoneField")%></strong></td>
		    <td Align="left" class="light_cell"><%
		    Dim RSComp
		    Dim RSFormat
		    
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
		    
 		    IF len(strFormat) <> 0 Then 
					Response.Write(GetFormat(oui.cust_phone1,strFormat))
		    Else
					Response.Write(oui.cust_phone1)
		    End IF
		    %>&nbsp;</td>

	   </tr>
	   <tr>
			<td Align="left" class="Dark_Cell" ><strong>State</strong></td>
		    <td Align="left" class="light_cell"><%=oui.cust_caddress4%>&nbsp;</td>

			<td Align="left" valign="center" class="dark_cell" ><strong><%=session("Phone2Field")%></strong></td>
		    <td Align="left" class="light_cell"><%
		    IF len(strFormat) <> 0 Then 
					Response.Write(GetFormat(oui.cust_phone2,strFormat))
		    Else
					Response.Write(oui.cust_phone2)
		    End IF
		     %>&nbsp;</td>


	   </tr>
	   <tr>
			<td Align="left" class="Dark_Cell" ><strong>Zip Code</strong></td>
		    <td Align="left" class="light_cell"><%=oui.cust_caddress5%>&nbsp;</td>
			
			<td Align="left" valign="center" class="dark_cell" ><strong><%=session("FaxField")%></strong></td>
		    <td Align="left" class="light_cell"><%
			IF len(strFormat) <> 0 Then 
					Response.Write(GetFormat(oui.cust_fax,strFormat))
		    Else
					Response.Write(oui.cust_fax)
		    End IF
		    %>&nbsp;</td>


	   </tr>		   
	   <tr>
			<td Align="left" class="Dark_Cell" ><strong>Country</strong></td>
		    <td Align="left" class="light_cell"><%=oui.cust_caddress6%>&nbsp;</td>

			<td Align="left" valign="center" class="dark_cell" ><strong><%=Session("BuyerField")%></strong></td>
		    <td Align="left" class="light_cell"><%=oui.buyer%>&nbsp;</td>


	   </tr>
	   <tr>
			<td Align="left" colspan=2 class="Dark_Cell" ><strong>&nbsp</strong></td>
			<td Align="left" valign="center" class="dark_cell" ><strong><%=Session("KeeperField")%></strong></td>
		    <td Align="left" class="light_cell"><%=oui.keeper %>&nbsp;</td>

	   </tr>
			<%
			'ARD - Edit Customer Profile [Start]
			
			IF Session("M_EDITPROF") = "Y" Then
					'IF Session("UserCanEdit") = "Y" Then
					'ARD - Edit Customer Profile [End]
					%>
					<tr>					
					<td colspan=4 class="dark_cell" align=right>
					<input type="hidden" name="Editline" value="">
					<%If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"EDITCUST") <= 0 Then%>
					<%else%>
						<input type="submit" value=" Edit " id="editcust" name="editcust">&nbsp;			
					<%end if%>

					<%
					If Trim(Session("Rep")) = "" Then
						strAppUserVar = Session("ID")
					Else
						strAppUserVar = Session("Rep")
					End If
		
					If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"MODPROFILE") <= 0 Then
					%>
					<%Else%>
					<!--input type="button" value=" Profile " id="Profile" name="Profile" onclick="return do_Profile()"-->
					<%End If%>
					</td>
				</tr>
			</form>
			<%End IF%>
        </table></center>
    </div>
      </td></tr></table>
      
      
<%else  '= "Edit" then %>
	    <table width=100% align=center>
		<%
		If Trim(Request("Store")) = "" Then
			strToken = "EDITCUST"
		Else
			strToken = "EDITSTORE"
		End If

		If Trim(Session("Rep")) = "" Then
			strAppUserVar = Session("ID")
		Else
			strAppUserVar = Session("Rep")
		End If
		If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),strToken) <= 0 Then
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
      <form name="Savecustomer" action="custsave.asp" method="post">	
	<table width=95%  border=1 bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
		<tr>
			<td Align="left" width="8%" class="dark_cell"><strong><%=Session("CustField")%></strong></td>
		    <td Align="left" width="16%" class="light_cell"><%=oui.cust_account %>&nbsp;</td>

			<td Align="left" width="8%" class="dark_cell"><strong><%=Session("StoreField")%></strong></td>
		    <td Align="left" width="16%" class="light_cell"><%Response.Write(oui.cust_Store & " - " & oui.cust_StName) %>&nbsp;<input type="Hidden" name="Store" value="<%=oui.cust_Store%>"></td>


		</tr>
		<tr>
			<td Align="left" colspan=2 class="Dark_Cell" ><strong>&nbsp</strong></td>
			
			<td Align="left" class="dark_cell"><strong>Ship Via</strong></td>
			<td Align="left" class="light_cell">
			<select name=selShip >
				<%rsCode.MoveFirst ()
				do while not rsCode.EOF%>
					<option value="<%=rsCode("Ccode_no")%>" <%if Session("RSCust")("ShipVia") = rsCode("Ccode_no") then%>selected<%end if%>><%=rsCode("cDiscrep")%>
				<%rsCode.MoveNext ()
				  loop
				%>
			</select>
			</TD>
		</tr>
		<tr>
		
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>Name</strong></td>
			<td Align="left" width="16%" class="light_cell"><%=trim(oui.cust_stname)%>&nbsp;</td>
		    <td Align="left" valign="center" width="8%" class="dark_cell"><strong>Status</strong></td>
			<td Align="left" valign="center" width="17%" bgcolor="#FFFFF0" class="light_cell">
				  				<%
  				'WMA Replace only potential by ordinary 4 status
  				'select case ucase(oui.cust_status)
				'	case "A" :
				'		Response.Write "Active"
				'	case "H" :
				'		Response.Write "Hold"
				'   case "C" :
				'		Response.Write "Cancel"
				'   case "P" :
				'		Response.Write "Potential"
				'	end select
				%>				
    			<select name=lstStatus  tabindex="1">
   					<option value="A" <% if ucase(oui.cust_status)="A" then%>selected<%end if%>>Active</option>
   					<option value="H" <% if ucase(oui.cust_status)="H" then%>selected<%end if%>>Hold</option>		
   					<option value="C" <% if ucase(oui.cust_status)="C" then%>selected<%end if%>>Cancel</option>		
					<option value="P" <% if ucase(oui.cust_status)="P" then%>selected<%end if%>>Potential</option>
			   </select>  
			    &nbsp;</td>
	   </tr>	
	   <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>Address</strong></td>
		    <td Align="left" width="16%" class="light_cell"><input name="addr1" size="30" value="<%=trim(oui.cust_caddress1)%>" maxlength="30" tabindex="1"></td>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=session("DBAField")%></strong></td>
		    <td Align="left" width="16%" class="light_cell"><input name="DBA" size="30" value="<%=trim(oui.cust_dba)%>" maxlength="30" tabindex="7"></td>

	   </tr>
	   <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>&nbsp;</strong></td>
		    <td Align="left" width="16%" class="light_cell"><input name="addr2" size="30" value="<%=trim(oui.cust_caddress2)%>" maxlength="30" tabindex="2"></td>

			<td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=session("PhoneField")%></strong></td>
		    <td Align="left" width="16%" class="light_cell">
		    <%
		    'ARD,301633 - Add phone/Fax Format [End]
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
								Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(oui.cust_phone1), intpos, intTotLegth)&""" name=""Phone1" & intcount &""" tabindex=""" & intTab & """ onKeyPress='TextChange(this)'>")
							Else
								Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(oui.cust_phone1), intpos, intpos1 - intOldPos)&""" name=""Phone1" & intcount &""" tabindex=""" & intTab & """ onKeyPress='TextChange(this)'>")
								Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
							End if
							intpos = intpos + intpos1 - intOldPos
							intOldPos = intpos1 + 1
							intcount = intcount + 1
							intTab = intTab + 1
						Loop
		    
		    
		    %>
		    
		    
		    <!--<input name="Phone1" size="30" value="<%'=trim(oui.cust_phone1)%>" maxlength="16">-->
		    <%
		    ' ARD,301633 - Add phone/Fax Format [End]
		    
		    %> &nbsp;</td>
	   </tr>
	   <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>City</strong></td>
		    <td Align="left" width="16%" class="light_cell"><input name="Addr3" size="30" value="<%=trim(oui.cust_caddress3)%>" maxlength="30" tabindex="3"></td>

			<td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=session("Phone2Field")%></strong></td>
		    <td Align="left" width="16%" class="light_cell">
		    <%'ARD,301633 - Add phone/Fax Format [End]
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
								Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(oui.cust_phone2), intpos, intTotLegth)&""" name=""Phone2" & intcount &""" tabindex=""" & intTab & """ onKeyPress='TextChange(this)'>")
							Else
								'strOutPut = strOutPut & mid(strSource, intpos, intpos1 - intOldPos)
								Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(oui.cust_phone2), intpos, intpos1 - intOldPos)&""" name=""Phone2" & intcount &""" tabindex=""" & intTab & """ onKeyPress='TextChange(this)'>")
								Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
							End if
							intpos = intpos + intpos1 - intOldPos
							intOldPos = intpos1 + 1
							intcount = intcount + 1
							intTab = intTab + 1
						Loop

		    
		    
		    %>
		    <!--<input name="Phone2" size="30" value="<%'=trim(oui.cust_phone2)%>" maxlength="16">-->
		    
		    <%'ARD,301633 - Add phone/Fax Format [End]%> &nbsp;</td>


	   </tr>
	   <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>Zip Code</strong></td>
		    <td Align="left" width="16%" class="light_cell"><input name="Addr5" size="30" value="<%=trim(oui.cust_caddress5)%>" maxlength="30" tabindex="4"></td>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=session("FaxField")%></strong></td>
		    <td Align="left" width="16%" class="light_cell">
		    <%'ARD,301633 - Add phone/Fax Format [Start]
 						intTotLegth = Len(strFormat)
						intOldPos = 1
						intPos1 = 1
						strOutPut = ""
						intpos = 1
						intcount = 1
						intTab = 16
						Do While Not intPos1 = 0
							intpos1 = instr(intOldPos, strFormat, "-",1)
							intpos2 = instr(intOldPos, strFormat, "/",1)
							If intpos1 > intpos2 OR intpos1 = 0 Then
								intpos1 = intpos2
							End IF
							If intpos1 = 0 Then
								'strOutPut = strOutPut & mid(strSource, intpos, intTotLegth)
								Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(oui.cust_fax), intpos, intTotLegth)&""" name=""fax" & intcount &""" tabindex=""" & intTab & """onKeyPress='TextChange(this)'>")
							Else
								'strOutPut = strOutPut & mid(strSource, intpos, intpos1 - intOldPos)
								Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(oui.cust_fax), intpos, intpos1 - intOldPos)&""" name=""fax" & intcount &""" tabindex=""" & intTab & """onKeyPress='TextChange(this)'>")
								Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
							End if
							intpos = intpos + intpos1 - intOldPos
							intOldPos = intpos1 + 1
							intcount = intcount + 1
							intTab = intTab + 1
						Loop
		    
		    
		    
		    %>
		    <!--<input name="fax" size="30" value="<%'=trim(oui.cust_fax)%>" maxlength="16">-->
		    <%'ARD,301633 - Add phone/Fax Format [End]%> &nbsp;</td>
	   </tr>		   
	   <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>State</strong></td>
		    <td Align="left" width="16%" class="light_cell"><input name="Addr4" size="30" value="<%=trim(oui.cust_caddress4)%>" maxlength="30" tabindex="5"></td>


			<td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=Session("BuyerField")%></strong></td>
		    <td Align="left" width="16%" class="light_cell"><input name="txtBuyer" size="30" value="<%=trim(oui.buyer)%>" maxlength="30" tabindex="20"></td>

	   </tr>
	   <tr>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong>Country</strong></td>
		    <td Align="left" width="16%" class="light_cell"><input name="Addr6" size="30" value="<%=trim(oui.cust_caddress6)%>" maxlength="20" tabindex="6"></td>
			<td Align="left" valign="center" width="8%" class="dark_cell"><strong><%=Session("keeperField")%></strong></td>
		    <td Align="left" width="16%" class="light_cell"><input name="txtKeeper" size="30" value="<%=trim(oui.keeper)%>" maxlength="20" tabindex="21"></td>

	   </tr>
	 
	   <tr>
			<td class="dark_cell">
				<input type=button value="Change Password" onclick="javascipt:openwindow('password.asp')" id=button1 name=button1>
			</td>
			<td colspan=3 class="dark_cell" align=right>
			
					<input type="submit" value=" Save " id="savecust" name="savecust" tabindex="22">
					<input type="button" value=" Cancel " id="Cancelcust" name="Cancelcust" onclick="javascript:location.href='custprof.asp';" tabindex="23"> 
      
			</td>
		</tr>
	   </FORM>
    </table></center>
    </div>
      </td></tr>
      
<%end if '<> "Edit"%>

<br>
<table border="1" width="95%" height=50 cellspacing="0" cellpadding="0" align=center>
	<tr>
		<td height=50 width=100% class="Title">
			Contacts
		</td>
	</tr>
	<tr>
		<td>
<%
if trim(Request("errmsg")) <> "" then
	Error = "<font color=red face=arial><b>" & trim(Request.QueryString ("errmsg")) & "</b></font>"
	Response.Write(Error)
end if	
%> &nbsp;</td>
	</tr>
	<tr>
		<td width=100%>
<div align="center">
  <center>
	<table  border="1" bordercolor="#111111" width=100% style="border-collapse: collapse" cellpadding="0" cellspacing="0" bgcolor="#4265B5">
	<tr>
		<td class="dark_cell">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
		<td align="left" width="19%" class="dark_cell">
			<b>Contact</b>
		</td>
		<td align="left" width="19%" class="dark_cell">
			<b>Title</b>
		</td>
		<td align="left" width="19%" class="dark_cell">
			<b><%=session("PhoneField")%></b>
		</td>
		<td align="left" width="19%" class="dark_cell">
			<b><%=session("faxField")%></b>
		</td>
		<td align="left" width="19%" class="dark_cell">
            <b>E-mail</b>
		</td>		
	</tr>
	<%
	if not (ouicustomer.ChildEOF (1) and ouicustomer.ChildBOF (1)) then
		ouicustomer.ChildMoveFirst (1)
		'Response.Write("noERROR11")
		'Response.End 

	Else
		'Response.Write("ERROR11")
		'Response.End 
	End If
	
	
	
	
	if not (ouicustomer.ChildEOF (1) and ouicustomer.ChildBOF (1)) then
				ouicustomer.ChildMoveFirst (1)
				%>
				<form name="ContEdit" method="post" action="Contsave.asp?store=<%Response.Write(oui.cust_Store)%>">
				<%
				ouicustomer.ChildFilter 1,"ccont_id='"& ouicustomer.cust_account &"' and Store='"& Trim(Request("Store")) &"'"
				do while not ouicustomer.ChildEOF (1)
					set objCont = ouicustomer.ChildGet (1)
					%>
					<tr>
						<td align="center" width="5%" class="light_cell"><input type="radio" value="<%=trim(cstr(objCont.contact))%>" name="cont" id="cont" tabindex="24"></td>
						<td align="left" width="19%" class="light_cell"><%=objCont.contact%>&nbsp;</td>
						<td align="left" width="19%" class="light_cell"><%=objCont.ccontttl%>&nbsp;</td>
						<td align="left" width="19%" class="light_cell"><%
						IF len(strFormat) <> 0 Then 
							Response.Write(GetFormat(objCont.phone,strFormat))
						Else
							Response.Write(objCont.phone)
						End IF
						%>&nbsp;</td>
							<td align="left" width="19%" class="light_cell"><%
						 IF len(strFormat) <> 0 Then 
							Response.Write(GetFormat(objCont.fax,strFormat))
						 Else
							Response.Write(objCont.fax)
						 End IF
						%>&nbsp;</td>
						<td align="left" width="19%" class="light_cell"><%=objCont.cemail_add%>&nbsp;</td>    
					</tr>
					
					<%
					ouicustomer.ChildMoveNext (1)
				loop
				%>
				<tr>
				<td colspan=6 align=right class="dark_cell">
				<%
				If Trim(Session("Rep")) = "" Then
					strAppUserVar = Session("ID")
				Else
					strAppUserVar = Session("Rep")
				End If
				If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"ADDCONT") <= 0 Then
				%>
				<%Else%>
						<input type="submit" name="ContADD"  value="  Add  " tabindex="25">
				<%End If%>
				<%
				If Trim(Session("Rep")) = "" Then
					strAppUserVar = Session("ID")
				Else
					strAppUserVar = Session("Rep")
				End If
				If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"EDITCONT") <= 0 Then
				%>
				<%Else%>
						<input type="submit" name="ContEDIT"  value="  Edit  " onClick=" return anyCheckEdit(this.form)" tabindex="26">
				<%End If%>
				<%
				If Trim(Session("Rep")) = "" Then
					strAppUserVar = Session("ID")
				Else
					strAppUserVar = Session("Rep")
				End If
				If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"DELCONT") <= 0 Then
				%>
				<%Else%>
						<input type="submit" name="ContDEL" value="Delete" onClick="return anyCheckDelete(this.form)" tabindex="27">
				<%End If%>
				</td>
			</tr>
			</form>
			<%
			
	
		
	else%>
	
		<form name="ContEdit" method="post" action="Contsave.asp?store=<%Response.Write(oui.cust_Store)%>">
		<TR>
			<td colspan=6 align=center class="dark_cell">There are no contacts to display<TD>
		</TR>
		<tr>
				<td colspan=6 align=right class="dark_cell">
				<%
				If Trim(Session("Rep")) = "" Then
					strAppUserVar = Session("ID")
				Else
					strAppUserVar = Session("Rep")
				End If
				If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"ADDCONT") <= 0 Then
				%>
				<%Else%>
						<input type="submit" name="ContADD"  value="  Add  " tabindex="28">
				<%End If%>
				</td>
					</form>
			</tr>
	<%end if %>
	
<%
else
'	Response.Write "Error33"
end if

set oui = nothing

%>
</TABLE></center>
</div>
        </td></tr></table> <BR><BR>
</body>