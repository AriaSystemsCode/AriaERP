<%@ Language=VBScript %>
<%
Response.Buffer=true
IF Session("ID")="" And Session("rep")="" Then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
	</script>	
<%END IF 


IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF

constr =Application("DataConnectionString")

set Connt = server.CreateObject("ADODB.Connection")
connt.Open constr

%>

<html>
<head>
<LINK REL=stylesheet HREF="crmmain.css" TYPE="text/css">
<title>CRM - Return Authorization Request</title>
	<%IF strFile = "cust" Then%>
<script language="JavaScript">
<!--
function MM_findObj(n, d) { //v3.0
  var p,i,x;  if(!d) d=document; if((p=n.indexOf("?"))>0&&parent.frames.length) {
    d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}
  if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];
  for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document); return x;
}
function MM_swapImage() { //v3.0
  var i,j=0,x,a=MM_swapImage.arguments; document.MM_sr=new Array; for(i=0;i<(a.length-2);i+=3)
   if ((x=MM_findObj(a[i]))!=null){document.MM_sr[j++]=x; if(!x.oSrc) x.oSrc=x.src; x.src=a[i+2];}
}
function MM_swapImgRestore() { //v3.0
  var i,x,a=document.MM_sr; for(i=0;a&&i<a.length&&(x=a[i])&&x.oSrc;i++) x.src=x.oSrc;
}

function MM_preloadImages() { //v3.0
 var d=document; if(d.images){ if(!d.MM_p) d.MM_p=new Array();
   var i,j=d.MM_p.length,a=MM_preloadImages.arguments; for(i=0; i<a.length; i++)
   if (a[i].indexOf("#")!=0){ d.MM_p[j]=new Image; d.MM_p[j++].src=a[i];}}
}

function fwLoadMenus() {
  if (window.fw_menu_0) return;
  window.fw_menu_0 = new Menu("root",83,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_0.addMenuItem("Profile","location='custprof.asp'");
  fw_menu_0.addMenuItem("Statement","location='custstat.asp'");
   fw_menu_0.hideOnMouseOut=true;
  window.fw_menu_1 = new Menu("root",105,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_1.addMenuItem("Check Status","location='ordstatus.asp'");
  fw_menu_1.addMenuItem("Remote Order","location='ord1.asp'");
   fw_menu_1.hideOnMouseOut=true;
  window.fw_menu_2 = new Menu("root",100,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_2.addMenuItem("Check Status","location='returnstatus.asp'");
  fw_menu_2.addMenuItem("Request R/A","location='returnaddredirect.asp'");
   fw_menu_2.hideOnMouseOut=true;

  fw_menu_2.writeMenus();
} // fwLoadMenus()

//-->

</script>
<script language="JavaScript1.2" src="../fw_menu.js"></script>
</head>
<body topmargin="0" leftmargin="0" marginheight="0" marginwidth="0" bgcolor="#aecae6" onLoad="MM_preloadImages('../images/CustNav3_r5_c10overf2.gif','../images/CustNav3_r5_c11overf2.gif','../images/CustNav3_r5_c12overf2.gif','../images/CustNav3_r5_c13overf2.gif');">
<script language="JavaScript1.2">fwLoadMenus();</script>
<table border="0" cellpadding="0" cellspacing="0" width="750">
<!-- fwtable fwsrc="CustNav3.png" fwbase="CustNav3.gif" fwstyle="Dreamweaver" fwdocid = "742308039" fwnested="0" -->
  <tr>
   <td><img src="../images/spacer.gif" width="80" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="3" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="57" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="10" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="31" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="25" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="56" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="66" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="15" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="80" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="81" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="81" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="86" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="79" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="1" border="0"></td>
  </tr>

  <tr>
   <td colspan="14"><img name="CustNav3_r1_c1" src="../images/CustNav3_r1_c1.gif" width="750" height="8" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="8" border="0"></td>
  </tr>
  <tr>
   <td rowspan="2"><img name="CustNav3_r2_c1" src="../images/CustNav3_r2_c1.gif" width="80" height="59" border="0"></td>
   <td rowspan="2" colspan="2" background=../images/CustNav3_r2_c4.gif"><img name="CustNav3_r2_c2" src="../images/<%=Session("LogoPath")%>" width="60" height="59" border="0"></td>
   <td rowspan="2"><img name="CustNav3_r2_c4" src="../images/CustNav3_r2_c4.gif" width="10" height="59" border="0"></td>
   <td rowspan="2" colspan="2"><img name="CustNav3_r2_c5" src="../images/CustNav3_r2_c5.gif" width="56" height="59" border="0"></td>
   <td rowspan="2" colspan="2"><img name="CustNav3_r2_c7" src="../images/CustNav3_r2_c7.gif" width="122" height="59" border="0"></td>
   <td colspan="6"><img name="CustNav3_r2_c9" src="../images/CustNav3_r2_c9.gif" width="422" height="30" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="30" border="0"></td>
  </tr>
  <tr>
   <td colspan="6"><img name="CustNav3_r3_c9" src="../images/CustNav3_r3_c9.gif" width="422" height="29" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="29" border="0"></td>
  </tr>
  <tr>
   <td colspan="14"><img name="CustNav3_r4_c1" src="../images/CustNav3_r4_c1.gif" width="750" height="8" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="8" border="0"></td>
  </tr>
  <tr>
   <td colspan="2"><img name="CustNav3_r5_c1" src="../images/CustNav3_r5_c1.gif" width="83" height="20" border="0"></td>
   <td colspan="3"><a href="#" onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_0,100,95);" ><img name="CustNav3_r5_c3" src="../images/CustNav3_r5_c3.gif" width="98" height="20" border="0"></a></td>
   <td colspan="2"><a href="#" onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_1,181,95);" ><img name="CustNav3_r5_c6" src="../images/CustNav3_r5_c6.gif" width="81" height="20" border="0"></a></td>
   <td colspan="2"><a href="#" onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_2,262,95);" ><img name="CustNav3_r5_c8" src="../images/CustNav3_r5_c8.gif" width="81" height="20" border="0"></a></td>
   <td><a href="invoice.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('CustNav3_r5_c10','','../images/CustNav3_r5_c10overf2.gif',1);" ><img name="CustNav3_r5_c10" src="../images/CustNav3_r5_c10.gif" width="80" height="20" border="0"></a></td>
   <td><a href="ots.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('CustNav3_r5_c11','','../images/CustNav3_r5_c11overf2.gif',1);" ><img name="CustNav3_r5_c11" src="../images/CustNav3_r5_c11.gif" width="81" height="20" border="0"></a></td>
   <td><a href="catalog.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('CustNav3_r5_c12','','../images/CustNav3_r5_c12overf2.gif',1);" ><img name="CustNav3_r5_c12" src="../images/CustNav3_r5_c12.gif" width="81" height="20" border="0"></a></td>
   <td><a href="helpdesk.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('CustNav3_r5_c13','','../images/CustNav3_r5_c13overf2.gif',1);" ><img name="CustNav3_r5_c13" src="../images/CustNav3_r5_c13.gif" width="86" height="20" border="0"></a></td>
   <td><img name="CustNav3_r5_c14" src="../images/CustNav3_r5_c14.gif" width="79" height="20" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="20" border="0"></td>
	<TR>
		<TD colspan=14><img width="750" height=50 border="0" src="../images/requestra0001.jpg"></TD>
	</TR>

	</table>
	<%Else%>
<script language="JavaScript">
<!--
function MM_findObj(n, d) { //v3.0
  var p,i,x;  if(!d) d=document; if((p=n.indexOf("?"))>0&&parent.frames.length) {
    d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}
  if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];
  for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document); return x;
}
function MM_swapImage() { //v3.0
  var i,j=0,x,a=MM_swapImage.arguments; document.MM_sr=new Array; for(i=0;i<(a.length-2);i+=3)
   if ((x=MM_findObj(a[i]))!=null){document.MM_sr[j++]=x; if(!x.oSrc) x.oSrc=x.src; x.src=a[i+2];}
}
function MM_swapImgRestore() { //v3.0
  var i,x,a=document.MM_sr; for(i=0;a&&i<a.length&&(x=a[i])&&x.oSrc;i++) x.src=x.oSrc;
}

function MM_preloadImages() { //v3.0
 var d=document; if(d.images){ if(!d.MM_p) d.MM_p=new Array();
   var i,j=d.MM_p.length,a=MM_preloadImages.arguments; for(i=0; i<a.length; i++)
   if (a[i].indexOf("#")!=0){ d.MM_p[j]=new Image; d.MM_p[j++].src=a[i];}}
}

function fwLoadMenus() {
  if (window.fw_menu_0) return;
  window.fw_menu_0 = new Menu("root",105,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_0.addMenuItem("Check Status","location='repordstatus.asp'");
  fw_menu_0.addMenuItem("Remote Order","location='repord1.asp'");
   fw_menu_0.hideOnMouseOut=true;
  window.fw_menu_1 = new Menu("root",100,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_1.addMenuItem("Check Status","location='repchkra.asp'");
  fw_menu_1.addMenuItem("Request R/A","location='reprequestra.asp'");
   fw_menu_1.hideOnMouseOut=true;

  fw_menu_1.writeMenus();
} // fwLoadMenus()

//-->
</script>
<script language="JavaScript1.2" src="../fw_menu.js"></script>
</head>
<body topmargin="0" leftmargin="0" marginheight="0" marginwidth="0" bgcolor="#aecae6" onLoad="MM_preloadImages('../images/RepNav3_r5_c3overf2.gif','../images/RepNav3_r5_c10overf2.gif','../images/RepNav3_r5_c11overf2.gif','../images/RepNav3_r5_c12overf2.gif');">
<script language="JavaScript1.2">fwLoadMenus();</script>
<table border="0" cellpadding="0" cellspacing="0" width="750">
<!-- fwtable fwsrc="RepNav3.png" fwbase="RepNav3.gif" fwstyle="Dreamweaver" fwdocid = "742308039" fwnested="0" -->
  <tr>
   <td><img src="../images/spacer.gif" width="80" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="3" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="57" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="10" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="45" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="11" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="83" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="39" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="55" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="94" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="95" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="94" height="1" border="0"></td>
   <td><img src="../images/spacer.gif" width="84" height="1" border="0"></td>
   <td><img src="..images/spacer.gif" width="1" height="1" border="0"></td>
  </tr>

  <tr>
   <td colspan="13"><img name="RepNav3_r1_c1" src="../images/RepNav3_r1_c1.gif" width="750" height="8" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="8" border="0"></td>
  </tr>
  <tr>
   <td rowspan="2"><img name="RepNav3_r2_c1" src="../images/RepNav3_r2_c1.gif" width="80" height="59" border="0"></td>
   <td rowspan="2" colspan="2" background="../images/CustNav3_r2_c4.gif"><img name="RepNav3_r2_c2" src="../images/<%=Session("LogoPath")%>" width="60" height="59" border="0"></td>
   <td rowspan="2"><img name="RepNav3_r2_c4" src="../images/RepNav3_r2_c4.gif" width="10" height="59" border="0"></td>
   <td rowspan="2" colspan="2"><img name="RepNav3_r2_c5" src="../images/RepNav3_r2_c5.gif" width="56" height="59" border="0"></td>
   <td rowspan="2" colspan="2"><img name="RepNav3_r2_c7" src="../images/RepNav3_r2_c7.gif" width="122" height="59" border="0"></td>
   <td colspan="5"><img name="RepNav3_r2_c9" src="../images/RepNav3_r2_c9.gif" width="422" height="30" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="30" border="0"></td>
  </tr>
  <tr>
   <td colspan="5"><img name="RepNav3_r3_c9" src="../images/RepNav3_r3_c9.gif" width="422" height="29" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="29" border="0"></td>
  </tr>
  <tr>
   <td colspan="13"><img name="RepNav3_r4_c1" src="../images/RepNav3_r4_c1.gif" width="750" height="8" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="8" border="0"></td>
  </tr>
  <tr>
   <td colspan="2"><img name="RepNav3_r5_c1" src="../images/RepNav3_r5_c1.gif" width="83" height="20" border="0"></td>
   <td colspan="3"><a href="repcust.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('RepNav3_r5_c3','','../images/RepNav3_r5_c3overf2.gif',1);" ><img name="RepNav3_r5_c3" src="../images/RepNav3_r5_c3.gif" width="112" height="20" border="0"></a></td>
   <td colspan="2"><a href="#" onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_0,195,95);" ><img name="RepNav3_r5_c6" src="../images/RepNav3_r5_c6.gif" width="94" height="20" border="0"></a></td>
   <td colspan="2"><a href="#" onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_1,289,95);" ><img name="RepNav3_r5_c8" src="../images/RepNav3_r5_c8.gif" width="94" height="20" border="0"></a></td>
   <td><a href="repinvoice.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('RepNav3_r5_c10','','../images/RepNav3_r5_c10overf2.gif',1);" ><img name="RepNav3_r5_c10" src="../images/RepNav3_r5_c10.gif" width="94" height="20" border="0"></a></td>
   <td><a href="repots.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('RepNav3_r5_c11','','../images/RepNav3_r5_c11overf2.gif',1);" ><img name="RepNav3_r5_c11" src="../images/RepNav3_r5_c11.gif" width="95" height="20" border="0"></a></td>
   <td><a href="repcatalog.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('RepNav3_r5_c12','','../images/RepNav3_r5_c12overf2.gif',1);" ><img name="RepNav3_r5_c12" src="../images/RepNav3_r5_c12.gif" width="94" height="20" border="0"></a></td>
   <td><img name="RepNav3_r5_c13" src="../images/RepNav3_r5_c13.gif" width="84" height="20" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="20" border="0"></td>
  </tr>
	<TR>
		<TD colspan=14><img width="750" height=50 border="0" src="../images/requestra0001.jpg"></TD>
	</TR>

	</table>

	<%End IF%>


	<%IF compWork = "Y" Then%>



<%
Dim strSql ' as string

If Len(Trim(Session("OrderFlag"))) = 0 Then
	Set Session("rsReturnLine") = server.CreateObject("ADODB.recordset")
	Set rsReturnLine1 = server.CreateObject("ADODB.recordset")

  strSql = "select * from raline where .f."
  Session("rsReturnLine").open  strSql, connt, 2, 4
End IF
%>

<script language="JavaScript">
	function go(which) 
	{
	  n = which.selectedIndex;
	  str = which.title

	  if (n != 0)
	   {
	    which.form.submit();
	   }
	}

	function Up_lines(objInput)
	{
		objInput.Typeline.value = "U";
	}

	function Re_lines(objInput) 
	{
		objInput.Typeline.value = "R";
	}
</script>

<%' Script to check fields of quantity in the form


'RecordSets
set rsReason = server.CreateObject("ADODB.RecordSet")
set rsStyle = Server.CreateObject("ADODB.RecordSet")

'Queries
sqlReason = "SELECT Cdefcode, Cfld_name, Ccode_no, Cdiscrep FROM  Codes WHERE Cfld_name = 'REASON' ORDER BY Cdefcode"
sqlStyle = "SELECT Style, Desc1 FROM Style" 

'Open RecordSets
rsReason.Open sqlReason, connt
rsStyle.Open sqlStyle, connt

Dim DefReasonDes
rsReason.MoveFirst()



DefReasonDes = rsReason("Cdiscrep")
rsReason.MoveFirst()

Dim rs, RSColor, RSStymaj, rsRetStyStruct ' as ADODB.RECORDSET


set RSColor=server.CreateObject("ADODB.recordset")
strSql="select distinct style from style where cstymajor='" & request("menu1") & "'"
RSColor.open strSql,connt

set rsRetStyStruct=server.CreateObject("ADODB.recordset")
strSql="select ciseghead from icistru where citemrecty='U' And cisegno='1' And lsegendmaj=.T."
rsRetStyStruct.open strSql,connt 
%>

<Table Width=750  border=0>
<b><font color="#000080" face=Arial size="2" face="Arial">Return Authorization Details.</font></b>
</Table>

<form action="returnDetailRedirect.asp?firsttime=1" method="post" name="FORM1" id="FORM1">

<table border="1" width="750" bgcolor="#6495D0" bordercolor="#AECAE6">
        <tr>
            <td width="16%" bgcolor="#6495D0"><font face="Arial" size="2" color="#000080"><strong>Account<strong></font></td>
            <td colspan="7" width="16%" bgcolor="#6495D0"><font size="2" color="#FFFFFF" face="Arial">
            <%if len(trim(session("id")))>0 then%>
            <%=Session("RSCust").fields("account")%> - <%=Session("RSCust").fields("btname")%></font></td>
            <%else %>
             <%=Session("customerid")%> - <%=Session("RSCust").fields("btname")%></font></td>
            
            <%end if%>
        </tr>
        <tr>
            <td width="16%" bgcolor="#6495D0"><font face="Arial" size="2" color="#000080"><strong>Store</strong></font></td>
            <td width="16%" bgcolor="#6495D0"><font face="Arial" size="2" color="#000080"><strong>Location</strong></font></td>
            <td width="16%" bgcolor="#6495D0"><font face="Arial" size="2" color="#000080"><strong>Reason</strong></font></td>
            <td width="16%" bgcolor="#6495D0"><font face="Arial" size="2" color="#000080"><strong>Division</strong></font></td>
            <td width="16%" bgcolor="#6495D0"><font face="Arial" size="2" color="#000080"><strong>Entered</strong></font></td>
            <td width="16%" bgcolor="#6495D0"><font face="Arial" size="2" color="#000080"><strong>Void After</strong></font></td>
            <td width="16%" bgcolor="#6495D0"><font face="Arial" size="2" color="#000080"><strong>Total Qty.</strong></font></td>
            <td width="16%" bgcolor="#6495D0"><font face="Arial" size="2" color="#000080"><strong>Total Amount</strong></font></td>
        </tr>
        <tr><FONT color=#ffffff face=Arial size=2></FONT>
            <td width="16%" bgcolor="#6495D0"><font size="2" color="#FFFFFF" face="Arial"><%=Session("selectStore")%></font></td>
            <td width="16%" bgcolor="#6495D0"><font size="2" color="#FFFFFF" face="Arial"><%=Session("selectLocation")%></font></td>
            <td width="16%" bgcolor="#6495D0"><font size="2" color="#FFFFFF" face="Arial"><%=DefReasonDes%></font></td>
            <td width="16%" bgcolor="#6495D0"><font size="2" color="#FFFFFF" face="Arial">
            <%'Get Description of the Divisin
							Set rsDesc=Server.CreateObject ("ADODB.Recordset")
							MySQL= "SELECT Cdiscrep FROM Codes WHERE Cfld_name='CDIVISION' AND Cdefcode='N' AND Crltfield='N' AND Ccode_no='" & Trim(Session("selectDivision"))& "'"	
							rsDesc.Open MySQL, Connt
							if rsDesc.EOF AND rsDesc.BOF Then
							'error
							else
							Response.Write rsDesc("Cdiscrep")
							end if
							rsDesc.Close
						%>
							</font></td>
            <td width="16%" bgcolor="#6495D0"><font size="2" color="#FFFFFF" face="Arial"><%=Session("txtEntered")%></font></td>
            <td width="16%" bgcolor="#6495D0"><font size="2" color="#FFFFFF" face="Arial"><%=Session("txtVoid")%></font></td>
            <td width="16%" bgcolor="#6495D0"><input type="text" size="20" name="txtQty" value="<%=Session("TotalQty")%>" id="text1" style="WIDTH: 45px" readonly>
            </td>
            <td width="16%" bgcolor="#6495D0"><input type="text" size="20" name="txtAmount" value="<%=Session("TotalAmount")%>" id="text2" style="WIDTH: 45px" readonly> </td>
        </tr>
    </table>
    <BR>
    

    <table border="1" width=750  bgcolor="#6495D0" bordercolor="#AECAE6">
        
        <tr>
        
	       <%
	       Session("rsRetStyStruct").MoveFirst()
				IF Session("rsRetStyStruct").EOF And Session("rsRetStyStruct").BOF Then
				Else
					strTemp = "<TD bgColor=#6495D0 colspan=10><font face=""Arial"" size=""2"" color=""#000080""><strong>"
					Response.Write(strTemp)
					
					Session("rsRetStyStruct").MoveFirst


					DO While Not Session("rsRetStyStruct").Eof
						IF Len(Session("getstyle"))=0 Then
							strValue = ""
						Else
							strValue = Session(Trim(Session("rsRetStyStruct").fields("cisegsdes")))
						End IF
						strTemp = "<font face=Arial color=#000080 size=2>" & Trim(Session("rsRetStyStruct").fields("cisegsdes")) & "</font>" & "<INPUT name=" & Trim(Session("rsRetStyStruct").fields("cisegsdes")) & " size=""" & Session("rsRetStyStruct").fields("nisegsize") &  """ maxlength="""& Session("rsRetStyStruct").fields("nisegsize") & """ value=" & strValue & ">"
						strTemp = strTemp & " " & "<font face=Arial color=#000080 size=2>" & Trim(Session("rsRetStyStruct").fields("cisegsepr")) & "<Font>" & " "
						Response.Write(strTemp)
						Session("rsRetStyStruct").MoveNext
					Loop

				End IF
			%>
			 <font face="Arial" size="2" color="#000080"><strong>
				<input type="submit" name="button2" value="Get Style" id="button5" style="height: 24; width: 80"></font>
				
			 </td>
        </tr>
        
        <tr>
            <td  bgcolor="#6495D0">
				<p align="left"><font face="Arial" size="2" color="#000080"><strong>Description</strong></font></p>
            </td>
            <td colSpan="8" bgcolor="#6495D0">
				<p align="left"><font face="Arial" size="2" color="#000080"><strong><%=Session("LongDesc")%></strong></font></p>
            </td>
            
        </tr>
</form>
<%
Set RSOTS = server.CreateObject("ADODB.recordset")
strSql="select * from style where style='" & Session("getstyle") & "'"
RSOTS.open strSql,connt 
Dim intOTS ' as integer

IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
	Set RSScale = server.CreateObject("ADODB.recordset")
	strSql="select * from scale where type='S' And scale='" & RSOTS("scale") & "'"
	RSScale.open strSql,connt 
End IF

' Second Form start here ..
%>


<script Language="JavaScript"><!--
function FrontPage_Form2_Validator(theForm)
{

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord1.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord1.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord1.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord2.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord2.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord2.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord3.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord3.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord3.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord4.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord4.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord4.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord5.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord5.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord5.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord6.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord6.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord6.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord7.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord7.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord7.focus();
    return (false);
  }

  var checkOK = "0123456789-,";
  var checkStr = theForm.txtord8.value;
  var allValid = true;
  var decPoints = 0;
  var allNum = "";
  for (i = 0;  i < checkStr.length;  i++)
  {
    ch = checkStr.charAt(i);
    for (j = 0;  j < checkOK.length;  j++)
      if (ch == checkOK.charAt(j))
        break;
    if (j == checkOK.length)
    {
      allValid = false;
      break;
    }
    if (ch != ",")
      allNum += ch;
  }
  if (!allValid)
  {
    alert("Please enter only digit characters in the quantity field!");
    theForm.txtord8.focus();
    return (false);
  }

  var chkVal = allNum;
  var prsVal = parseInt(allNum);
  if (chkVal != "" && !(prsVal >= "0"))
  {
    alert("Please enter a value greater than or equal to \"0\" in the quantity field!");
    theForm.txtord8.focus();
    return (false);
  }
   var QtysSum;
  if (theForm.Typeline.value=="U")
  { 
	QtysSum = theForm.txtord1.value + theForm.txtord2.value + theForm.txtord3.value + theForm.txtord4.value + theForm.txtord5.value + theForm.txtord6.value + theForm.txtord7.value + theForm.txtord8.value ;
	if (QtysSum == 0)
	{	
		alert ("The number of items must be greater than zero!");
		return (false);
	}
  }
  if (theForm.Typeline.value=="R")
  {
	if (form2.StyleChoosed.value=="F")
	{
		alert("Please select the item you want to delete from the lower table and click “Remove Line”!");
		return false;
	}
  }
  return (true);
}




function CheckLineRecordset()
{
	if (form3.RecordsetEmptyFlag.value == "T")
	{ 
		alert ("Return Authorization request form is empty; can’t save!");
		return (false);
	}
	return (true)
}

//-->
</script>
  
 <form action="Retupdateline.asp" method="post" name="form2" id="FORM2" onsubmit="return FrontPage_Form2_Validator(this)" name="FrontPage_Form2">
  
		<input type="hidden" name="Typeline" value>
		<input type="hidden" name="SlctColor" value="<%=ucase(Session("getstyle"))%>">
		<%
		  if Trim(Session("getstyle")) = "" then
			styleischoosed = "F"
		  else
			styleischoosed = "T"
		  end if
		%>
		<input type="hidden" id=StyleChoosed name=StyleChoosed value="<%=styleischoosed%>">
        <tr>
            <td bgcolor="#6495D0"><p align="left"><font face="Arial" size="2" color="#000080"><strong>Reason</strong></font></p>
            </td>
        
            <td colspan="9" bgcolor="#6495D0">
				<p align="LEFT"><%
			'Reason Description to be written
							
							Dim ReasonDes1 
							rsReason.MoveFirst()
							
							Do While Not rsReason("Ccode_no") = Session("selectReason2")
								rsReason.MoveNext()
							Loop
							ReasonDes1 = rsReason("Cdiscrep")
							rsReason.MoveFirst()
			%> 
			<select name="SelectReason" size="1" id="select1">
                <option selected value="<%=Session("selectReason2")%>"><%=ReasonDes1%></option>
                <%rsReason.MoveNext() 
                Do While Not rsReason.EOF 
					if  Trim(rsReason("Ccode_no"))=Trim(Session("selectReason2")) then
						rsReason.MoveNext()
					else%>
						<option value="<%=rsReason("Ccode_no")%>"><%=rsReason("Cdiscrep")%></option>
					<%end if
					if rsReason.EOF then
					else 
						rsReason.MoveNext()
					end if
                 Loop%>
            </select>
            </p>
           </td>
        </tr>
        <tr>
<%intScaleCnt = cdbl("0")%>
            <td width="10%">&nbsp;</td>
            <td width="10%"><p align="left"><font face="Arial" size="2" color="#000080"><strong><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz1"))
					intScaleCnt = cdbl(RSScale("cnt"))
				Else
					Response.Write("Size1")
				End IF
			%></strong> </font></p>
            </td>
            <td width="10%"><p align="left"><font face="Arial" size="2" color="#000080"><strong><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz2"))
				Else
					Response.Write("Size2")
				End IF
			%> </strong></font></p>
            </td>
            <td width="10%"><p align="left"><font face="Arial" size="2" color="#000080"><strong><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz3"))
				Else
					Response.Write("Size3")
				End IF
			%> </strong></font></p>
            </td>
            <td width="10%"><p align="left"><font face="Arial" size="2" color="#000080"><strong><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz4"))
				Else
					Response.Write("Size4")
				End IF
			%> </strong></font></p>
            </td>
            <td width="10%"><p align="left"><font face="Arial" size="2" color="#000080"><strong><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz5"))
				Else
					Response.Write("Size5")
				End IF
			%></strong> </font></p>
            </td>
            <td width="10%"><p align="left"><font face="Arial" size="2" color="#000080"><strong><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz6"))
				Else
					Response.Write("Size6")
				End IF
			%> </strong></font></p>
            </td>
            <td width="10%"><p align="left"><font face="Arial" size="2" color="#000080"><strong><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz7"))
				Else
					Response.Write("Size7")
				End IF
			%> </strong></font></p>
            </td>
            <td width="10%"><p align="left"><font face="Arial" size="2" color="#000080"><strong><%IF Not RSOTS.EOF OR NOT RSOTS.BOF Then
					Response.Write(RSScale("sz8"))
				Else
					Response.Write("Size8")
				End IF
			%> </strong></font></p>
            </td>
        </tr>
        <tr>
            <td width="10%"><p align="left"><font face="Arial" size="2" color="#000080"><strong>Qty.</strong></font></p>
            </td>
            <td width="10%"><p align="left"><font size="2">
            
						<%strTemp = "Disabled"%>
						<%if Session("text1")="0" then
								Session("text1")=""
						 end if
						 if Session("text2")="0" then
							Session("text2")=""
						 end if
						 if Session("text3")="0" then
							Session("text3")=""
						 end if
						 if Session("text4")="0" then
							Session("text4")=""
						 end if
						 if Session("text5")="0" then
							Session("text5")=""
						 end if
						 if Session("text6")="0" then
							Session("text6")=""
						 end if
						 if Session("text7")="0" then
							Session("text7")=""
						 end if
						 if Session("text8")="0" then
							Session("text8")=""
						 end if
						%>
            <input type="text" size="7" name="txtord1" value="<%=Session("text1")%>" <%IF intScaleCnt=0   Then Response.Write(strTemp) End IF%> id="text3" maxlength="6"></font></p>
            </td>
            <td width="10%"><p align="left"><font size="2">
           
            <input type="text" size="7" name="txtord2" value="<%=Session("text2")%>" <%IF intScaleCnt<2   Then Response.Write(strTemp) End IF%> id="text3" maxlength="6"></font></p>
            </td>
            <td width="10%"><p align="left"><font size="2">
           
            <input type="text" size="7" name="txtord3" value="<%=Session("text3")%>" <%IF intScaleCnt<3   Then Response.Write(strTemp) End IF%> id="text3" maxlength="6"></font></p>
            </td>
            <td width="10%"><p align="left"><font size="2">
           
            <input type="text" size="7" name="txtord4" value="<%=Session("text4")%>" <%IF intScaleCnt<4   Then Response.Write(strTemp) End IF%> id="text17" maxlength="6"></font></p>
            </td>
            <td width="10"><p align="left"><font size="2">
          
            <input type="text" size="7" name="txtord5" value="<%=Session("text5")%>" <%IF intScaleCnt<5   Then Response.Write(strTemp) End IF%> id="text16" maxlength="6"></font></p>
            </td>
            <td width="10%"><p align="left"><font size="2">
          
            <input type="text" size="7" name="txtord6" value="<%=Session("text6")%>" <%IF intScaleCnt<6   Then Response.Write(strTemp) End IF%> id="text15" maxlength="6"></font></p>
            </td>
            <td width="10%"><p align="left"><font size="2">
          
            <input type="text" size="7" name="txtord7" value="<%=Session("text7")%>" <%IF intScaleCnt<7   Then Response.Write(strTemp) End IF%> id="text14" maxlength="6"></font></p>
            </td>
            <td width="10%"><p align="left"><font size="2">
         
            <input type="text" size="7" name="txtord8" value="<%=Session("text8")%>"<%IF intScaleCnt<8   Then Response.Write(strTemp) End IF%> id="text3" maxlength="6"></font></p>
            </td>
        </tr>
    </table>

 <TABLE border=0 cellPadding=1 cellSpacing=1  width=750>
  
  <TR>
    <TD width="20%" style="WIDTH: 20%">
      <p align="left">&nbsp;</p></TD>
    <TD width="25%" colSpan=2 style="WIDTH: 25%"></TD>
    <TD width="15%"></TD>
    <TD width="20%" colSpan=2><FONT size=2>
      <P align=right>
      <FONT size=2>
				
				<INPUT id=button3 name=button3 style="HEIGHT: 24px; WIDTH: 96px" 
				type="submit" value="Update Line" onclick="Up_lines(this.form)"></FONT>&nbsp;

				<INPUT id=button2 name=button2 style="HEIGHT: 24px; WIDTH: 95px" 
				type="submit" value="Remove Line" onclick="Re_lines(this.form)">
				
				</FONT></P></TD>
  
  </TR>
    </TABLE>
<TABLE  border=0 cellPadding=1 cellSpacing=1 width=750>
  
  <TR>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
   </TR>
    </TABLE>

  
 
</form>
<form action="actionretauthsave.asp" method="POST" name="form3" id="form3" onsubmit="return CheckLineRecordset()">


 <TABLE border=0 cellPadding=1 cellSpacing=1  width=750>
  
  <TR>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%"></TD>
    <TD style="WIDTH: 20%" width="20%">
      <p align="right"><FONT size=2>
      
				<INPUT id=button4 name=button3 style="HEIGHT: 24px; WIDTH: 96px" type="submit" value="Save R/A" >
				</FONT></TD>
		<%' to check if the recordset of the lines is empty or not ..%>
		<%if Session("rsReturnLine").BOF AND Session("rsReturnLine").EOF then
			EmptyFlag = "T"
		  else 
			EmptyFlag="F"
		  end if%>
		<input type="Hidden" name="RecordsetEmptyFlag" value="<%=EmptyFlag%>" >
   </TR>
   </Form>
</TABLE>


<table border="1" cellspacing="1" width=750 bgcolor="#6495D0">

    <tr>
        <td><font face="Arial" size="2" color="#000080"><strong><%=rsRetStyStruct("ciseghead")%></strong></font> </td>
        <td><font face="Arial" size="2" color="#000080"><strong>Description</strong></font>
        </td>
        <td><font face="Arial" size="2" color="#000080"><strong>Reason</strong></font>
        </td>
        <td><font face="Arial" size="2" color="#000080"><strong>Qty.</strong></font>
        </td>
        <td><font face="Arial" size="2" color="#000080"><strong>Price</strong></font>
        </td>
        <td><font face="Arial" size="2" color="#000080"><strong>Amount</strong></font>
        </td>
    </tr>
<%

          

          IF Not Session("rsReturnLine").EOF OR Not Session("rsReturnLine").BOF   Then
				Session("rsReturnLine").movefirst
          
						Do While Not Session("rsReturnLine").EOF
							strTemp = "<TR>"
							Response.Write(strTemp)

							strTemp = "<TD  bgColor=#ffffff><font size=""2"" color=""#000080"" face=""Arial"">" & "<A HREF=""ReturnDisplaySelectedRecord.asp?Style=" & Session("rsReturnLine").fields("style") & "&Reason=" & Session("rsReturnLine").Fields("Reason") & """>" & Session("rsReturnLine").fields("style") & "</A></font></TD>"
							Response.Write(strTemp)
							'Description
							Dim StyleDes 
							rsStyle.MoveFirst()
							
							Do While Not rsStyle("Style") = Session("rsReturnLine").Fields("style")
								rsStyle.MoveNext()
							Loop
							StyleDes = rsStyle("Desc1")
							rsReason.MoveFirst()
							
							
							strTemp = "<TD  bgColor=#ffffff><font size=""2"" color=""#000080"" face=""Arial"">" & StyleDes & "</font></TD>"
							Response.Write(strTemp)
							'Reason
							
							Dim ReasonDes 
							rsReason.MoveFirst()
							
							Do While Not rsReason("Ccode_no") = Session("rsReturnLine").Fields("Reason")
								rsReason.MoveNext()
							Loop
							ReasonDes = rsReason("Cdiscrep")
							rsReason.MoveFirst()
							
							strTemp = "<TD  bgColor=#ffffff><font size=""2"" color=""#000080"" face=""Arial""> &nbsp;" & ReasonDes &"</font></TD>"
							Response.Write(strTemp)
							
							strTemp = "<TD  bgColor=#ffffff><font size=""2"" color=""#000080"" face=""Arial"">" & Session("rsReturnLine").fields("totqty") & "</font></TD>"
							Response.Write(strTemp)
							
							strTemp = "<TD  bgColor=#ffffff><font size=""2"" color=""#000080"" face=""Arial"">" & Session("rsReturnLine").fields("price") & "</font></TD>"
							Response.Write(strTemp)

							
							intAmount = cdbl(Session("rsReturnLine").fields("totqty")) * cdbl(Session("rsReturnLine").fields("price"))
							strTemp = "<TD  bgColor=#ffffff><font size=""2"" color=""#000080"" face=""Arial"">" &intAmount & "</font></TD>"
							Response.Write(strTemp)

												
							Session("rsReturnLine").movenext

						Loop
					else
					'response.write("fady")
          End IF
          
End IF%>
</table>
<p>&nbsp;</p>
</body>
</html>
