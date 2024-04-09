<%@LANGUAGE="VBSCRIPT"%>
<HTML>
<Head>
<!--#include file="../USERSEC/Connections/cnConn.asp" -->
<Title>CRM Menu Items</Title>
<%
'WMA #038182 Ability to customize the menu [start]
	Function WriteULTag(strID,strAttribute)
		WriteULTag = "<UL id='" & strID & "' Name='" & strID & "' onClick='doCheck(this)' style='display:''' " & strAttribute &">"
	End Function

	Function WriteLITag(strID,strClass,strAttribute)
		WriteLITag = "<Li id='" & strID & "' Name='" & strID & "' style='display:' Class='" & strClass & "' " & strAttribute & ">"
	End Function
	

	Function WriteCheckBox(strID,strAttribute,strName)
		WriteCheckBox = "<input type='CheckBox' id='" & strID & "' name='" & strName & "' value='"& "-1" &"' onClick='doCheckClick(this)' " & strAttribute &">"
	End Function	
%>

<style type="text/css">
<!--
.Expanded {
	list-style-image: url(Images/min.GIF);
}
.Collapsed {
	list-style-image: url(Images/plus.GIF);
}
.PlainLI{
list-style-type: none;
list-style-image: none;
}
TD.Title
{
    FONT-SIZE: 19pt;
    COLOR: lightskyblue;
    FONT-FAMILY: Impact, Arial;
    BACKGROUND-COLOR: #4166b7;
    TEXT-ALIGN: center
}
TD.Light_cell
{
    FONT-SIZE: 10pt;
    COLOR: navy;
    FONT-FAMILY: Arial;
    BACKGROUND-COLOR: #fffff0
}

-->
</style>

<SCRIPT LANGUAGE=javascript>
<!--

function doCheck(objSrc)
{
	window.event.cancelBubble = true;
	 
	var intLoop = 0;

	for (intLoop=0; intLoop < objSrc.all.length; intLoop++)
	{
		if (objSrc.all.item(intLoop).id==objSrc.id)
		{
		}
		else
		{
			if (objSrc.all.item(intLoop).style.display=='none')
			{
				objSrc.className = 'Expanded'
				objSrc.all.item(intLoop).style.display=''
			}
			else
			{
				objSrc.className = 'Collapsed'
				objSrc.all.item(intLoop).style.display='none'
			}
		}
	}
}

function CancelTokenBubble()
{
	window.event.cancelBubble = true;
}

function doCheckClick(objSrc)
{
	window.event.cancelBubble = true;
	var intLoop = 0;
	for (intLoop=0; intLoop < objSrc.parentElement.all.length; intLoop++)
	{
		objSrc.parentElement.all.item(intLoop).checked = objSrc.checked
	}
}

function GetUser(srcForm)
{
	srcForm.action = 'default.asp'
	srcForm.submit()
}

function CheckSave(srcForm)
{
//alert('Check')
	if (srcForm.lstMenuType.value=='')
	{
		alert('Please Select Menu Type');
		return false;
	}
	return true;
}
//-->
</SCRIPT>
<script language="JavaScript" type="text/JavaScript">
<!--


function MM_reloadPage(init) {  //reloads the window if Nav4 resized
  if (init==true) with (navigator) {if ((appName=="Netscape")&&(parseInt(appVersion)==4)) {
    document.MM_pgW=innerWidth; document.MM_pgH=innerHeight; onresize=MM_reloadPage; }}
  else if (innerWidth!=document.MM_pgW || innerHeight!=document.MM_pgH) location.reload();
}
MM_reloadPage(true);
//-->
</script>
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
</HEAD>
<BODY BGCOLOR="#aecae6" TOPMARGIN="0" LEFTMARGIN="0">
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
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/DCC logo.jpg" WIDTH=60 HEIGHT=59></TD>
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

<form Name="FORM1" method=post action='save.asp' onSubmit="return CheckSave(this)">


<table bordercolor="#111111" border="0" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<TR>
    <TD align=right><a href="../Default.asp"><font size=2 face=arial>Back to 
      Admin Page</font></a></TD>
</TR>
</Table>


<%
strAppUserVar = session("strAppUserVar")
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"AMENU") <= 0 Then
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


<table bordercolor="#111111" border="1" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
  <tr>
    <td width=100% colspan=2 valign="top" height=5% class='Title'> 
			Menu Items
		</td>
  </tr>
  <tr>
    <td width="12%" valign="top" height=5%><font color="#FFFFF0" size="2" face="Verdana, Arial, Helvetica, sans-serif">
    Select Menu </font><br><br>
    </td>
    <td width="88%" valign="top" height=5%> <font color="#FFFFF0" size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
      <select name="lstMenuType" id="lstMenuType" onChange="GetUser(this.form)">
			<option value="sales"  <%If Trim(Request("lstMenuType"))= "sales" Then Response.Write("SELECTED")%>>Sales Rep.</option>
			<option value="customer" <%If Trim(Request("lstMenuType"))= "customer" Then Response.Write("SELECTED")%>>Customers.</option>
      </select>
      <br>
      </font>
    </td>
  </tr>
	<tr> 
	<td colspan="2" valign="top">
<%
'Read from XML
'Function CretaMenu(strXMLfile,strJSFile) 
If Request("lstMenuType")="" Then
	strtUserID = "*"
	strXMLfile = "../../SalesMENU.xml"	
Else
	strtUserID = Request("lstMenuType")
	if Request("lstMenuType") = "sales" then
		strXMLfile = "../../SalesMENU.xml"		
	elseif Request("lstMenuType") = "customer" then
		strXMLfile = "../../CustMENU.xml"		
	end if
End If

Response.Buffer = true
 
Set objXML = Server.CreateObject("Msxml2.DOMDocument")
Set objLst = Server.CreateObject("Msxml2.DOMDocument")
Set objHdl = Server.CreateObject("Msxml2.DOMDocument")

'-----------------------------------------------------------------------------
'-----------------------------------------------------------------------------
objXML.async = False
objXML.Load (Server.MapPath(strXMLfile))

If objXML.parseError.errorCode <> 0 Then
 'handle the error
 Response.Write "<b>Error at line: "
 Response.Write objXML.parseError.Line & "</b><br>"
 'Response.End 
End If

Set objLst = objXML.getElementsByTagName("BAR")
noOfHeadlines = objLst.length

'-----------------------------------------------------
 For i=0 To noOfHeadlines - 1 ' Loop for all bar nodes.
  Set objHdl = objLst.item(i)

  	'Read Groups 
	if objHdl.childNodes(2).text = "-1" Then
		strChecked = "Checked"
	Else
		strChecked = ""
	End If  
	Response.Write(WriteULTag(objHdl.childNodes(0).text,"Class='Expanded'") & WriteLITag(objHdl.childNodes(0).text,"","")& WriteCheckBox(objHdl.childNodes(0).text,strChecked ,objHdl.childNodes(0).text) & objHdl.childNodes(0).text & Chr(10))
	
   	'Read Menu Items
	parentNode = objHdl.childNodes(0).text 
	intNumberOfPADS = objHdl.getElementsByTagName ("PAD").length  
	  if intNumberOfPADS <> 0 then
		for j=0 to intNumberOfPADS - 1 ' Loop for all pads in each bar node. 
			if objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(2).text = "-1" Then
				strChecked = "Checked"
			Else
				strChecked = ""
			End If
		    Response.Write("<UL>")  
		    currNode = objHdl.getElementsByTagName ("PAD").item(j).childNodes.item(0).text
			strChecked = strChecked & " Group='" & Trim(parentNode) & "'"
			Response.Write(WriteLITag(currNode,"PlainLI","onClick='CancelTokenBubble()'")& WriteCheckBox(currNode,strChecked,currNode) & currNode & Chr(10))
			Response.Write("</UL>")  
		next
  end if 
  Response.Write("</ul>")
  Response.Write("<hr>")
next 


'WMA #038182 Ability to customize the menu [end]	
%>
</td></tr>
<tr>
<td align='right' colspan=2 valign="top" height=5%>
<INPUT type="submit" value="Submit" id=button1 name=button1 ><INPUT type="reset" value="Reset" id=button2 name=button2>
</td>
</tr>
</table></form>
<p>