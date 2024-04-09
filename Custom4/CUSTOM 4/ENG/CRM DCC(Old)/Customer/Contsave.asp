<%@ Language=VBScript %>
<!--#include file="../common/Add2Log.asp"-->
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>
<%Response.Buffer=true
' ARD - 604499 change the field cwe_Mail with this one cEmail_Add
IF Trim(Session("ID")) = "" And Trim(Session("rep"))= "" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%END IF 

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
End IF
if trim (Session("rep")) = "" then
	account = Session("ID")
else 
	account = Session("CustomerID")
end if

Response.Buffer = true
''''''''''''''''''''''
'account = Session("ID")


'Ard,301633 Add phone/fax format [Start]

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
'Ard,301633 Add phone/fax format [End]


''''''''''''''''''''''
If trim(cstr(Request.Form	("ContEdit"))) <>	"" Then
	Mode = "Edit"
ElseIf trim(cstr(Request("ContAdd")))	<> ""	Then
	Mode = "Add"
ElseIf trim(cstr(Request.Form	("ContDel")))	<> ""	Then
	Mode = "Del"
End	If

If trim(cstr(Request.Form ("ContSave"))) <> "" Then
	SaveMode = "SaveEditCont"
End If

If Mode <> "Add"  Then
	  If trim(cstr(Request.Form ("Cont"))) = "" Then Response.Redirect "custprof.asp?store="&Request("Store")  ''''Response.Write "WILL REDIRECT"  
	  Key = trim(cstr(Request.Form ("Cont")))
 End If
%>
<html>
<head>
	<Title>CRM - Contact</Title>
	<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("THEME")%>/Customer.css">

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

</head>
<BODY>
<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<%End IF%>
<p><BR><BR><BR></p>
<Table width=95% align=center border="1" height="50">
<TR>
<TD class="title">Contacts</TD>
</TR>
</Table>
<%
'Response.Write("Moded=" & Mode)
'Response.End 
Select Case Mode
	Case "Add"
	
		response.write "<Table width=95% align=center><TR><TD><br><b>Fill in the fields below to add a new contact:</b></TD></TR></Table>"
		If Len(request("errmsg")) > 0  Then
			Response.Write ("<Table width=95% align=center><TR><TD><br><b><Font color=red>" & request("errmsg") & "</font></b></TD></TR></Table>")
		End If
	
		If SaveMode = "SaveEditCont" Then
			If trim(Request.Form("contact")) = "" Then
				Response.Redirect "custprof.asp?errmsg=Please enter valid Contact."	
			Else
				Set editoui = server.CreateObject ("CustomerUI.CustomUI")
				editoui.ConParameter = Application("DataConnectionString")
				If editoui.Load (account) then
					IF Not(editoui.ChildEof (1) and editoui.ChildBof (1)) Then
						editoui.ChildMoveFirst (1)
					End IF
					Key2 = trim(Request.Form("contact"))
					'editoui.ChildFind 1,"contact = '" & Key2 &"'"
					strAcc = Trim(account) + Space(8-Len(Trim(account)))
					If Trim(Request("Store")) = "" Then
						strStore = Space(8)
					Else
						strStore = Trim(Request("Store")) & Space(8-Len(Trim(Request("Store"))))
					End If
					editoui.ChildFilter 1, "cconttype='C' And ccont_id='" & strAcc &"' and store='" & strStore & "' and contact='" & RTrim(Key2) & Space(30-Len(RTrim(Key2)))&"'"
					'== True if contact does not exist so [object].EOF is TRUE | False if contact exist so [object].EOF is FALSE
					If editoui.childeof(1) Then
						set editchild = editoui.ChildAddNew (1) '===ADDING
						editchild.Ccont_id = account
						editchild.contact = trim(Request.Form("contact"))
						editchild.ccontttl = trim(Request.Form("Title"))
						
						editchild.store = Request("Store")

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

						intTotLegth = Len(strFormat)
						intOldPos = 1
						intPos1 = 1
						strOutPut = ""
						intpos = 1
						intcount = 1
						Do while Not intPos1 = 0
							intpos1 = instr(intOldPos, strFormat, "-",1)
							intpos2 = instr(intOldPos, strFormat, "/",1)
							IF intpos1 > intpos2 OR intpos1 = 0 Then
								intpos1 = intpos2
							End IF
							intpos = intpos + intpos1 - intOldPos
							intOldPos = intpos1 + 1
							intcount = intcount + 1
						Loop

						strtempfax = ""
						strphone2 = ""
						for inttemp=1 to  intcount-1
							strtemp = strtemp & request("phone" & inttemp)
							strtempfax = strtempfax & request("Fax" & inttemp)
						next
							
						editchild.phone = strtemp
						editchild.fax = strtempfax

						editchild.cemail_add = trim(Request.Form("email"))
						editchild.cconttype="C"
						editoui.childset 1,editchild
						editoui.save
						set RSEmail=Server.CreateObject("ADODB.Recordset")
						SQLEmail="Select * from contact where contact='"&trim(Request.Form("contact"))&"'"
						RSEmail.open SQLEmail,editoui.ConParameter,1,3
						RSEmail("cwe_mail")=trim(Request.Form("email"))
						RSEmail.Update 
						strAddMemo = "Add!#!Contact!#!" & trim(editchild.contact) & "!#!" & trim(editchild.ccontttl)& "!#!" &trim(editchild.phone)& "!#!" &trim(editchild.fax)& "!#!" &trim(editchild.cemail_add)
						If Trim(session("ID")) = "" Then
							Add2Log "", Session("CustomerID"),"Adding contact",trim(cstr(editchild.contact)),strAddMemo
						Else
							Add2Log "", session("ID"),"Adding contact",trim(cstr(editchild.contact)),strAddMemo
						End If
						Set editchild = Nothing
					Else
						Response.Redirect ("Contsave.asp?errmsg=Contact already exists.&ContADD=Add")
					End If
				End If
			End If
			Set editoui = Nothing
			Response.Redirect "custprof.asp?store="&Request("Store")
		else
		%> 
		

<!--webbot BOT="GeneratedScript" PREVIEW=" " startspan --><script Language="JavaScript" Type="text/javascript"><!--
function FrontPage_Form1_Validator(theForm)
{

  if (theForm.contact.value == "")
  {
    alert("Please enter a value for the \"contact\" field.");
    theForm.contact.focus();
    return (false);
  }

  if (theForm.contact.value.length > 30)
  {
    alert("Please enter at most 30 characters in the \"contact\" field.");
    theForm.contact.focus();
    return (false);
  }

  if (theForm.Title.value == "")
  {
    alert("Please enter a value for the \"Title\" field.");
    theForm.Title.focus();
    return (false);
  }

  if (theForm.Title.value.length > 30)
  {
    alert("Please enter at most 30 characters in the \"Title\" field.");
    theForm.Title.focus();
    return (false);
  }

  if (theForm.email.value == "")
  {
    alert("Please enter a value for the \"email\" field.");
    theForm.email.focus();
    return (false);
  }

  if (theForm.email.value.length > 30)
  {
    alert("Please enter at most 30 characters in the \"email\" field.");
    theForm.email.focus();
    return (false);
  }
  return (true);
}
//--></script><!--webbot BOT="GeneratedScript" endspan --><form name="FrontPage_Form1" method="post" action="contsave.asp?store=<%Response.Write(Request("Store"))%>" onsubmit="return FrontPage_Form1_Validator(this)" language="JavaScript">
<input type="Hidden" name="store" value="<%Response.Write(Request("Store"))%>">
<div align="center">
<center>
<table width="80%" border="1" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<tr>
		<td class="Dark_cell">
			<b>
			Contact
			</b>
		</td>
		<td class="light_cell">
	  		<b>
	  			<!--webbot bot="Validation" s-data-type="String" b-value-required="TRUE" i-maximum-length="30" -->
	  			<input type="text" size="30" maxlength="30" value="" name="contact">
	  		</b>
	  	</td>
	 </tr>
	 <tr>
		<td class="Dark_cell">
			<b>
			Title
			</b>
		</td>
		<td class="light_cell">
	  		<b>
	  			<!--webbot bot="Validation" s-data-type="String" b-value-required="TRUE" i-maximum-length="30" -->
	  			<input type="text" size="30" maxlength="30" value="" name="Title">
	  		</b>
	  	</td>
	 </tr>
	 <tr>
		<td class="Dark_cell">
			<b>
			Phone
			</b>
		</td>
		<td class="light_cell">
	  		<b><%
	  		'Ard,301633 add phone/Fax Format[Start]
	  			Dim intpos1
	  			Dim intPos2
	  			Dim intTotLegth
	  			Dim intOldPos
	  			Dim strOutPut
								
								
	  			intTotLegth = Len(strFormat)
	  			intOldPos = 1
	  			intPos1 = 1
	  			strOutPut = ""
	  			intpos = 1
	  			intcount = 1
	  			Do while Not intPos1 = 0
	  				intpos1 = instr(intOldPos, strFormat, "-",1)
	  				intpos2 = instr(intOldPos, strFormat, "/",1)
	  				IF intpos1 > intpos2 OR intpos1 = 0 Then
	  					intpos1 = intpos2
	  				End IF
	  				IF intpos1 = 0 Then
	  					Response.Write("<input type=""text"" size="""&intTotLegth-intpos-1&""" maxlength="""&intTotLegth-intpos-1&""" value="""" name=""Phone" & intcount &"""onKeyPress='TextChange(this)'>")
	  				Else
	  					Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""" name=""Phone" & intcount &"""onKeyPress='TextChange(this)'>")
	  					Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
	  				End if
	  				intpos = intpos + intpos1 - intOldPos
	  				intOldPos = intpos1 + 1
	  				intcount = intcount + 1
	  				Loop
							
	  		%>						
							
	  		</b>
	  	</td>
	 </tr>
	 <tr>
		<td class="Dark_cell">
			<b>
			Fax
			</b>
		</td>
		<td class="light_cell">
	  		<b>
	  			<!--<input type="text" size="16" maxlength="16" value="" name="Fax">-->
							
	  		<%
	  			intTotLegth = Len(strFormat)
	  			intOldPos = 1
	  			intPos1 = 1
	  			strOutPut = ""
	  			intpos = 1
	  			intcount = 1
	  			Do while Not intPos1 = 0
	  				intpos1 = instr(intOldPos, strFormat, "-",1)
	  				intpos2 = instr(intOldPos, strFormat, "/",1)
	  				IF intpos1 > intpos2 OR intpos1 = 0 Then
	  					intpos1 = intpos2
	  				End IF
	  				IF intpos1 = 0 Then
	  					Response.Write("<input type=""text"" size="""&intTotLegth-intpos-1&""" maxlength="""&intTotLegth-intpos-1&""" value="""" name=""Fax" & intcount &"""onKeyPress='TextChange(this)'>")
	  				Else
	  					Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""" name=""Fax" & intcount &"""onKeyPress='TextChange(this)'>")
	  					Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
	  				End if
	  				intpos = intpos + intpos1 - intOldPos
	  				intOldPos = intpos1 + 1
	  				intcount = intcount + 1
	  				Loop						
							
	  		'Ard,301633 add phone/Fax Format[End]
							
	  		%>
	  		</b>
	  	</td>
	 </tr>
	 <tr>
		<td class="Dark_cell">
			<b>
			E-mail
			</b>
		</td>
	  	
	  	<td class="light_cell">
	  		<b>
	            <!--webbot bot="Validation" s-data-type="String" b-value-required="TRUE" i-maximum-length="30" -->
	            <input type="text" size="30" maxlength="30" value="" name="email">
	  		</b>
	  	</td>
	  </tr>
	  <tr>
	  	<td colspan=2" class="dark_cell" align=right>
	  		<input type="hidden" name="ContAdd" value="  Add  ">
	  		<input type="submit" value="Save" name="ContSave">
	  		<input type="Button" value="Cancel" name="Cancel" onclick="javascript:location.href='custprof.asp'">
	  	</td>
	  </tr>
</table>
</center>
</div>
</form>
<%
	end if
	case "Edit"
	strTemp = "<table width=95%" & " align=center><tr><td>"
	strTemp = strTemp & "<br><b>Modify the fields below to change contact information:</b>"
	strTemp = strTemp & "</TD></TR></Table>"
	response.write strTemp
		if SaveMode = "SaveEditCont" then
			set editoui = server.CreateObject ("CustomerUI.CustomUI")
			editoui.ConParameter = Application("DataConnectionString")
			if editoui.Load (account) then
				'if not (editoui.ChildBOF (1) AND editoui.ChildEOF (1)) then
					'======================= Check if contact exist=================
						'editoui.ChildMoveFirst (1)
						'Key2 = trim(Request.Form("contact"))
						'editoui.ChildFind 1,"contact = '" & Key2 &"'"
						
					'==================
					'if editoui.childeof(1) then 'True if contact does not exist so [object].EOF is TRUE | False if contact exist so [object].EOF is FALSE
							editoui.ChildMoveFirst (1)
							'editoui.ChildFind 1,"contact = '" & Key &"'"
							strAcc = Trim(account) + Space(8-Len(Trim(account)))
							If Trim(Request("Store")) = "" Then
								strStore = Space(8)
							Else
								strStore = Trim(Request("Store")) & Space(8-Len(Trim(Request("Store"))))
							End If
							editoui.ChildFilter 1, "cconttype='C' And ccont_id='" & strAcc &"' and store='" & strStore & "' and contact='" & RTrim(Key) & Space(30-Len(RTrim(Key)))&"'"
	
							set editchild = editoui.ChildGet (1)
							'''''''''''''''''''''''''''''''''''''''''
							''Save old Inf. before update them'''''''
							strEditMemo = "Edit!#!Contact!#!Old!#!" & trim(editchild.contact) & "!#!" & trim(editchild.ccontttl)& "!#!" &trim(editchild.phone)& "!#!" &trim(editchild.fax)& "!#!" &trim(editchild.cemail_add)
							'''''''''''''''''''''''''''''''''''''''''
							editchild.contact = trim(Request.Form("contact"))
							editchild.ccontttl = trim(Request.Form("Title"))
							'Ard,301633 Add Phone/Fax format [Start]


							strtemp = ""
							intTotLegth = Len(strFormat)
							intOldPos = 1
							intPos1 = 1
							strOutPut = ""
							intpos = 1
							intcount = 1
							Do while Not intPos1 = 0
								intpos1 = instr(intOldPos, strFormat, "-",1)
								intpos2 = instr(intOldPos, strFormat, "/",1)
								IF intpos1 > intpos2 OR intpos1 = 0 Then
									intpos1 = intpos2
								End IF
								intpos = intpos + intpos1 - intOldPos
								intOldPos = intpos1 + 1
								intcount = intcount + 1
							Loop

							strtempfax = ""
							for inttemp=1 to  intcount-1
								strtemp = strtemp & request("phone" & inttemp)
								strtempfax = strtempfax & request("Fax" & inttemp)
							next
							Response.Write(strtempfax)
							editchild.phone = strtemp
							editchild.fax = strtempfax

							'editchild.phone = trim(Request.Form("Phone"))
							'editchild.fax = trim(Request.Form("Fax"))
							'Ard,301633 Add Phone/Fax format [End]

							editchild.cemail_add = trim(Request.Form("email"))
							editoui.childset 1,editchild
							if editoui.save then
								''''''''''''''''''''''''''''''''
								'''For adding to log DB'''''''''
								strEditMemo = strEditMemo & "!#!New!#!" & trim(editchild.contact) & "!#!" & trim(editchild.ccontttl)& "!#!" &trim(editchild.phone)& "!#!" &trim(editchild.fax)& "!#!" &trim(editchild.cemail_add)
								If Trim(Session("CustomerID")) = "" Then
									Add2Log "", session("ID"),"Editing contact",Key,strEditMemo
								Else
									Add2Log "", Session("CustomerID"),"Editing contact",Key,strEditMemo
								End If
								
								''''''''''''''''''''''''''''''''
							end if
							set editchild = nothing
					else '=== NOT ADDING - REDIRECT WITH ERROR MESSAGE
						
							Response.Redirect "custprof.asp?errmsg=Contact already exists."	
						
					'end if
					'=======================END Check if contact exist=================
					
				'end if
			end if
			set editoui = nothing
			Response.Redirect "custprof.asp?store="&Request("Store")
		Else
			Set editoui = server.CreateObject ("CustomerUI.CustomUI")
			editoui.ConParameter = Application("DataConnectionString")
			If editoui.Load (account) Then
				If Not (editoui.ChildBOF (1) And editoui.ChildEOF (1)) Then
					editoui.ChildMoveFirst (1)
					strAcc = Trim(account) + Space(8-Len(Trim(account)))
					If Trim(Request("Store")) = "" Then
						strStore = Space(8)
					Else
						strStore = Trim(Request("Store")) & Space(8-Len(Trim(Request("Store"))))
					End If
					editoui.ChildFilter 1, "cconttype='C' And ccont_id='" & strAcc &"' and store='" & strStore & "' and contact='" & RTrim(Key) & Space(30-Len(RTrim(Key)))&"'"
				
					'editoui.ChildFind 1,"contact = '" & Key &"'"
					If editoui.ChildEof(1) Then
						Response.Write("<font size=3>No record found</font>")
						Response.End 
					Else
						set editchild = editoui.ChildGet (1)
					End If
		%> 
<!--webbot BOT="GeneratedScript" PREVIEW=" " startspan --><script Language="JavaScript" Type="text/javascript"><!--
function FrontPage_Form2_Validator(theForm)
{

  if (theForm.contact.value == "")
  {
    alert("Please enter a value for the \"contact\" field.");
    theForm.contact.focus();
    return (false);
  }

  if (theForm.contact.value.length > 30)
  {
    alert("Please enter at most 30 characters in the \"contact\" field.");
    theForm.contact.focus();
    return (false);
  }

  if (theForm.Title.value == "")
  {
    alert("Please enter a value for the \"Title\" field.");
    theForm.Title.focus();
    return (false);
  }

  if (theForm.Title.value.length > 30)
  {
    alert("Please enter at most 30 characters in the \"Title\" field.");
    theForm.Title.focus();
    return (false);
  }

  if (theForm.email.value == "")
  {
    alert("Please enter a value for the \"email\" field.");
    theForm.email.focus();
    return (false);
  }

  if (theForm.email.value.length > 30)
  {
    alert("Please enter at most 30 characters in the \"email\" field.");
    theForm.email.focus();
    return (false);
  }
  return (true);
}
//--></script><!--webbot BOT="GeneratedScript" endspan --><form name="FrontPage_Form2" method="post" action="contsave.asp?Store=<%Response.Write(Request("Store"))%>" onsubmit="return FrontPage_Form2_Validator(this)" language="JavaScript">
<div align="center">
<center>
<table width="80%" border="1" bordercolor="#111111" style="border-collapse: collapse" cellpadding="0" cellspacing="0">
	<tr>
		<td class="Dark_cell">
			<b>
			Contact
			</b>
		</td>
		<td class="light_cell">
	  		<b>
				<!--webbot bot="Validation" s-data-type="String" b-value-required="TRUE" i-maximum-length="30" -->
				<input type="text" size="30" maxlength="30" value="<%=trim(editchild.contact)%>" name="contact">
			</b>
	  	</td>
	 </tr>
	 <tr>
		<td class="Dark_cell">
			<b>
			Title
			</b>
		</td>
		<td class="light_cell">
	  		<b>
              <!--webbot bot="Validation" s-data-type="String" b-value-required="TRUE" i-maximum-length="30" -->
              <input type="text" size="30" maxlength="30" value="<%=trim(editchild.ccontttl)%>" name="Title">
			</b>
	  	</td>
	 </tr>
	 <tr>
		<td class="Dark_cell">
			<b>
			Phone
			</b>
		</td>
		<td class="light_cell">
	  		<b>
			<%
			'ARD,301633 Add Phone/Fax Format [Start]

			intTotLegth = Len(strFormat)
			intOldPos = 1
			intPos1 = 1
			strOutPut = ""
			intpos = 1
			intcount = 1
			Do while Not intPos1 = 0
				intpos1 = instr(intOldPos, strFormat, "-",1)
				intpos2 = instr(intOldPos, strFormat, "/",1)
				IF intpos1 > intpos2 OR intpos1 = 0 Then
					intpos1 = intpos2
				End IF
				IF intpos1 = 0 Then
					Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos -1& """ value=""" & mid(trim(editchild.phone), intpos, intTotLegth)&""" name=""Phone" & intcount &""" onKeyPress='TextChange(this)'>")
				Else
					Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(editchild.phone), intpos, intpos1 - intOldPos)&""" name=""Phone" & intcount &""" onKeyPress='TextChange(this)'>")
					Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
				End if
				intpos = intpos + intpos1 - intOldPos
				intOldPos = intpos1 + 1
				intcount = intcount + 1
			Loop
						
						
			%>
			</b>
		</td>
	 </tr>
	 <tr>
		<td class="Dark_cell">
			<b>
			Fax
			</b>
		</td>
		<td class="light_cell">
	  		<b>
	  		<%
			intTotLegth = Len(strFormat)
			intOldPos = 1
			intPos1 = 1
			strOutPut = ""
			intpos = 1
			intcount = 1
			Do while Not intPos1 = 0
				intpos1 = instr(intOldPos, strFormat, "-",1)
				intpos2 = instr(intOldPos, strFormat, "/",1)
				IF intpos1 > intpos2 OR intpos1 = 0 Then
					intpos1 = intpos2
				End IF
				IF intpos1 = 0 Then
					Response.Write("<input type=""text"" size=""" & intTotLegth-intpos-1 & """ maxlength=""" & intTotLegth-intpos-1 & """ value=""" & mid(trim(editchild.fax), intpos, intTotLegth)&""" name=""Fax" & intcount &""" onKeyPress='TextChange(this)'>")
				Else
					Response.Write("<input type=""text"" size="""&intpos1 - intOldPos&""" maxlength="""&intpos1 - intOldPos&""" value="""&mid(trim(editchild.fax), intpos, intpos1 - intOldPos)&""" name=""Fax" & intcount &""" onKeyPress='TextChange(this)'>")
					Response.Write(Mid(strFormat, intOldPos+ (intpos1 - intOldPos),1))
				End if
				intpos = intpos + intpos1 - intOldPos
				intOldPos = intpos1 + 1
				intcount = intcount + 1
			Loop%>
			</b>
		&nbsp;</td>
	 </tr>
	 <tr>
		<td class="Dark_cell">
			<b>
			E-mail
			</b>
		</td>
	  	
	  	<td class="light_cell">
			<b>
                <!--webbot bot="Validation" s-data-type="String" b-value-required="TRUE" i-maximum-length="30" -->
                <input type="text" size="30" maxlength="30" value="<%=trim(editchild.cemail_add)%>" name="email">
			</b>
	  	</td>
	  </tr>
	  <tr>
		<td colspan="2" class="dark_cell" align=right>
			<input type="hidden" name="Cont" value="<%=trim(editchild.contact)%>">
			<input type="hidden" name="ContEdit" value="  Edit  ">
			<input type="submit" value="Save" name="ContSave">
			<input type="Button" value="Cancel" name="Cancel" onclick="javascript:location.href='custprof.asp'">
		</td>
	  </tr>
			</table>
		      </center>
            </div>
		</form>
		<%
			end if
		end if	
		set editchild = nothing
		set editoui = nothing
		end if
	case "Del"
		set oui = server.CreateObject ("CustomerUI.CustomUI")
		oui.ConParameter = Application("DataConnectionString")
		if oui.Load (account) then
			If Not (oui.ChildEOF(1) And oui.ChildBOF(1)) Then
				oui.ChildMoveFirst (1)

				strAcc = Trim(account) + Space(8-Len(Trim(account)))
				If Trim(Request("Store")) = "" Then
					strStore = Space(8)
				Else
					strStore = Trim(Request("Store")) & Space(8-Len(Trim(Request("Store"))))
				End If
				oui.ChildFilter 1, "cconttype='C' And ccont_id='" & strAcc &"' and store='" & strStore & "' and contact='" & RTrim(Key) & Space(30-Len(RTrim(Key)))&"'"

				'oui.ChildFind 1,"contact = '" & Key &"'"
				
				''''''''''''''''''''''''''''''''''''''''
				'Save old Inf. before update them'''''''

				set delchild = oui.ChildGet (1)
				strDelMemo = "Delete!#!Contact!#!" & trim(delchild.contact) & "!#!" & trim(delchild.ccontttl)& "!#!" &trim(delchild.phone)& "!#!" &trim(delchild.fax)& "!#!" &trim(delchild.cemail_add)
				set delchild = nothing
				''''''''''''''''''''''''''''''''''''''''
				
				oui.ChildDelete (1)
				if oui.Save then 
					''''''''''''''''''''''''''''''''
					'''For adding to log DB'''''''''
					'strDelMemo = "Delete!#!Contact!#!" & oldContact & "!#!" & oldTitle & "!#!" & oldPhone & "!#!" & oldFax & "!#!" & oldEmail
					Add2Log "", session("ID"),"Deleting contact",Key,strDelMemo
					'Add2Log "", Session("CustomerID"),"Deleting contact",Key,strDelMemo
					
					''''''''''''''''''''''''''''''''
					
					Response.Redirect "custprof.asp?store="&Request("store")
				end if
			else
				Response.Write "ERROR"
			end if
		End if
		set oui = nothing	
end select

%>
</body>