<%'Start by AAK
'Added by AAK to save NotificationAddress in a table to use it in sql trigger that sends email to the NotificationAddress
'ARD - 06/24/01
'Set Conn=Server.CreateObject("ADODB.Connection")
'Set RSEmail=Server.CreateObject("ADODB.Recordset")
'Conn.Open Application("SqlServer")
'StrEmail="SELECT * FROM Email" 
'RSEmail.Open StrEmail,conn,3,3
'IF not RSEmail.EOF AND not RSEmail.BOF Then
'	RSEmail.MoveLast 
'End IF
'RSEmail("email")=session("NotificationAddress")
'on error resume next
'RSEmail.Update 
'Conn.Close
'Set Conn=nothing
'End bY AAK
'ARD - 06/24/01

'******************** Menu********************************
	dim fso,path
	Set fso = CreateObject("Scripting.FileSystemObject")
	
	'''''''''''''Customer'''''''''''''''''''''''''
	path = server.MapPath ("menu/HM_Arrays_Cust.js")
	if fso.FileExists (path) then
	else
		CretaMenu "CustMENU.xml","HM_Arrays_Cust.js" 
	end if
	'''''''''''''Sales Rep'''''''''''''''''''''''''
	path = server.MapPath ("menu/HM_Arrays_Sales.js")
	if fso.FileExists (path) then
	else
		CretaMenu "SalesMENU.xml","HM_Arrays_Sales.js" 
	end if
	set fso = nothing
'********************Menu******************************


%>
<!--#include file="ReadXMLADO.asp"-->
<html>
<head>
<title>CRM - Login</title>
<SCRIPT LANGUAGE=javascript>
<!--
function foucus_id()
{
	document.form1.txtuser.focus();
}

//-->
</SCRIPT>
    <SCRIPT LANGUAGE=javascript>
		<!--
		function formvalid(frm)
		{
			if(frm.txtuser.value == "")
			{
				alert("Please Enter the UserName!");
				return false;
			}
			
			if(frm.txtpw.value == "")
			{
				alert("Please Enter the Password!");
				return false;
			}

			return true;
		}
		//-->
		</SCRIPT>
<LINK rel="stylesheet" type="text/css" href="images/<%=Session("THEME")%>/Common.css">
</head>
<body topmargin="0" leftmargin="0" marginheight="0" marginwidth="0" onload="foucus_id()">
<TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
  <TR> 
    <TD> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=8 HEIGHT=1></TD>
    <TD> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=39 HEIGHT=1></TD>
    <TD> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=33 HEIGHT=1></TD>
    <TD> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=60 HEIGHT=1></TD>
    <TD> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=10 HEIGHT=1></TD>
    <TD> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=47 HEIGHT=1></TD>
    <TD width="100%"> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=6 HEIGHT=1></TD>
    <TD> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=125 HEIGHT=1></TD>
    <TD> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=10 HEIGHT=1></TD>
    <TD> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=13 HEIGHT=1></TD>
    <TD> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=361 HEIGHT=1></TD>
    <TD> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=30 HEIGHT=1></TD>
    <TD> <IMG SRC="images/<%=session("theme")%>/spacer.gif" WIDTH=8 HEIGHT=1></TD>
  </TR>
  <TR> 
    <TD ROWSPAN=4> <IMG SRC="images/<%=session("theme")%>/Login1_15.jpg" WIDTH=8 HEIGHT=274></TD>
    <TD COLSPAN=5> <IMG SRC="images/<%=session("theme")%>/Login1_16.jpg" WIDTH=189 HEIGHT=25></TD>
    <TD background="images/<%=session("theme")%>/Login1_17.jpg"> <IMG SRC="images/<%=session("theme")%>/Login1_17.jpg" WIDTH=6 HEIGHT=25></TD>
    <TD COLSPAN=2> <IMG SRC="images/<%=session("theme")%>/Login1_18.jpg" WIDTH=135 HEIGHT=25></TD>
    <TD ROWSPAN=4> <IMG SRC="images/<%=session("theme")%>/Login1_19.jpg" WIDTH=13 HEIGHT=274></TD>
    <TD WIDTH=361 HEIGHT=274 ROWSPAN=4 background="images/<%=session("theme")%>/Login1_02.jpg"><font face="Arial" size="1" color="#000000">In 
      this time of revolutionary change in the business landscape, having a customer 
      centric strategy is key. That is why the integration of the Internet into 
      every customer interaction before, during and after the sale has become 
      a vital component to an organization's overall business strategy.Aria's 
      new Web Based CRM module allows apparel companies to integrate their back-end 
      ERP systems with the Internet to offer 24 hours on line customer service 
      to their retail customer stores. The system offers Retail stores and Sales-reps 
      features such as on line product Catalog, order entry/inquiry, Stock Availability, 
      Return Authorization, Customer statements and much more. The system also 
      integrates with Workflow to automatically manage customer on-line interactions. 
      Research indicates that customers today expect you to have an interactive 
      web-site to allow them to get online service.Aria's initial release of the 
      CRM module has built-in interfaces to Aria's ERP system but the system can 
      be easily integrated with any other back-end system.</font></TD>
    <TD ROWSPAN=4> <IMG SRC="images/<%=session("theme")%>/Login1_21.jpg" WIDTH=30 HEIGHT=274></TD>
    <TD ROWSPAN=4> <IMG SRC="images/<%=session("theme")%>/Login1_22.jpg" WIDTH=8 HEIGHT=274></TD>
  </TR>
  <TR> 
    <TD ROWSPAN=2> <IMG SRC="images/<%=session("theme")%>/Login1_23.jpg" WIDTH=39 HEIGHT=229></TD>
    <TD COLSPAN=4> <IMG SRC="images/<%=session("theme")%>/Login1_24.jpg" WIDTH=150 HEIGHT=137></TD>
    <TD ROWSPAN=2 background="images/<%=session("theme")%>/Login1_25.jpg"> <IMG SRC="images/<%=session("theme")%>/Login1_25.jpg" WIDTH=6 HEIGHT=229></TD>
    <TD COLSPAN=2 ROWSPAN=2 background="images/<%=session("theme")%>/Login1_26.jpg" valign="top" align="center"> 
    <form method="POST" action="validate.asp" onsubmit="return formvalid(this)" name=form1 target="_parent">
      <p>
      <input type="text" name="txtuser" size="10" style="border-style: solid; border-width: 2; padding-top: 0" maxlength=5></p>
      <p>
      <input type="password" name="txtpw" size="10" style="border-style: solid; border-width: 2"></p>
      <p><select size="1" name="lstType"><%
      
        
        select case Trim(Session("M_LOGIN"))
					Case "C"
						Response.Write("<Option value=""C"">"&session("CustField"))
					Case "S"
						Response.Write("<Option value=""R"">Sales Rep.")
					Case "B"
						Response.Write("<Option value=""C"">"&session("CustField"))
						Response.Write("<Option value=""R"">Sales Rep.")
        End select
      
      %></select></p>
      <p><input type="submit" value="Submit" name="B1"><input type="reset" value="Reset" name="B2"></p>
    </form>
&nbsp;</TD>
  </TR>
  <TR> 
    <TD COLSPAN=4> <IMG SRC="images/<%=session("theme")%>/Login1_27.jpg" WIDTH=150 HEIGHT=92></TD>
  </TR>
  <TR> 
    <TD COLSPAN=5> <IMG SRC="images/<%=session("theme")%>/Login1_28.jpg" WIDTH=189 HEIGHT=20></TD>
    <TD background="images/<%=session("theme")%>/Login1_29.jpg"> <IMG SRC="images/<%=session("theme")%>/Login1_29.jpg" WIDTH=6 HEIGHT=20></TD>
    <TD COLSPAN=2> <IMG SRC="images/<%=session("theme")%>/Login1_30.jpg" WIDTH=135 HEIGHT=20></TD>
  </TR>
  <TR> 
    <TD COLSPAN=6> <IMG SRC="images/<%=session("theme")%>/Login1_31.jpg" WIDTH=197 HEIGHT=38></TD>
    <TD background="images/<%=session("theme")%>/Login1_32.jpg"> <IMG SRC="images/<%=session("theme")%>/Login1_32.jpg" WIDTH=6 HEIGHT=38></TD>
    <TD COLSPAN=6> <IMG SRC="images/<%=session("theme")%>/Login1_33.jpg" WIDTH=547 HEIGHT=38></TD>
  </TR>
  <tr> 
    <td height="1"><img height="1" width="8" src="images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="39" src="images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="33" src="images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="60" src="Test/images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="10" src="Test/images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="47" src="images/<%=session("theme")%>/spacer.gif"></td>
    <td></td>
    <td><img height="1" width="125" src="images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="10" src="Test/images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="13" src="images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="361" src="images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="30" src="images/<%=session("theme")%>/spacer.gif"></td>
    <td><img height="1" width="8" src="images/<%=session("theme")%>/spacer.gif"></td>
  </tr>
</TABLE>
  
<!-- End ImageReady Slices -->
</body>
</html>