<HTML>
<HEAD>

<Title></Title>
<%
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

' To handle the case of the rep.
IF len(Trim(Session("ID")))>0 Then
	CustID = Session("ID")
Else
	CustID = Session("customerid")
End IF

%>
<base target="_parent">
</HEAD>
<Body  leftmargin=0 topmargin=0 bgcolor="#aecae6">
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
  window.fw_menu_0 = new Menu("root",100,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_0.addMenuItem("Profile","location='custprof.asp'");
  
   fw_menu_0.hideOnMouseOut=true;
  window.fw_menu_1 = new Menu("root",130,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_1.addMenuItem("Check Status","location='ordstatus.asp'");
  fw_menu_1.addMenuItem("Remote Order","location='ord1.asp'");
  fw_menu_1.addMenuItem("Order Confirmation","location='ord_conf.asp'");
   fw_menu_1.hideOnMouseOut=true;
  window.fw_menu_2 = new Menu("root",100,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_2.addMenuItem("Check Status","location='returnstatus.asp'");
  fw_menu_2.addMenuItem("Request R/A","location='returnaddredirect.asp'");
   fw_menu_2.hideOnMouseOut=true;
	window.fw_menu_3 = new Menu("root",100,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_3.addMenuItem("Check Invoice","location='invoice.asp'");
  fw_menu_3.addMenuItem("View Statement","location='custstat.asp'");
   fw_menu_3.hideOnMouseOut=true;

  fw_menu_3.writeMenus();

} // fwLoadMenus()

//-->

</script>
<script language="JavaScript1.2" src="../fw_menu.js"></script>
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
   <td rowspan="2" colspan="2" background="../images/CustNav3_r2_c4.gif"><img name="CustNav3_r2_c2" src="../images/<%=Session("LogoPath")%>" width="60" height="59" border="0"></td>
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
   <td colspan="3"><a href="custprof.asp" onMouseOut="MM_swapImgRestore();" target="_parent" onMouseOver="MM_swapImage('RepNav2_r2_c2','','../images/RepNav2_r2_c2overf2.gif',1);" ><img name="RepNav2_r2_c2" src="../images/RepNav2_r2_c2.gif" width="98" height="20" border="0"></a></td>	
	<!--<td colspan="3"><a href="#" onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_0,81,95);" ><img name="CustNav3_r5_c3" src="images/CustNav3_r5_c3.gif" width="98" height="20" border="0"></a></td>-->
   <td colspan="2"><a href="#" onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_1,181,95);" ><img name="CustNav3_r5_c6" src="../images/CustNav3_r5_c6.gif" width="81" height="20" border="0"></a></td>
   <td colspan="2"><a href="#" onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_2,262,95);" ><img name="CustNav3_r5_c8" src="../images/CustNav3_r5_c8.gif" width="81" height="20" border="0"></a></td>
   <td colspan="1"><a href="#"  onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_3,342,95);" ><img name="CustNav3_r5_c10" src="../images/CustNav3_r5_c10.gif" width="82" height="20" border="0"></a></td>
   <td><a href="ots.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('CustNav3_r5_c11','','../images/CustNav3_r5_c11overf2.gif',1);" ><img name="CustNav3_r5_c11" src="../images/CustNav3_r5_c11.gif" width="81" height="20" border="0"></a></td>
   <td><a href="catalog.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('CustNav3_r5_c12','','../images/CustNav3_r5_c12overf2.gif',1);" ><img name="CustNav3_r5_c12" src="../images/CustNav3_r5_c12.gif" width="81" height="20" border="0"></a></td>
   <td><a href="helpdesk.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('CustNav3_r5_c13','','../images/CustNav3_r5_c13overf2.gif',1);" ><img name="CustNav3_r5_c13" src="../images/CustNav3_r5_c13.gif" width="86" height="20" border="0"></a></td>
   <td><a href="logoff.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('CustNav3_r5_c14','','../images/CustNav3_r5_c14overf2.gif',1);" ><img name="CustNav3_r5_c14" src="../images/CustNav3_r5_c14.gif" width="77" height="20" border="0"></a></td>
   <td><img src="../images/spacer.gif" width="1" height="20" border="0"></td>
<TR>
		<TD colspan=14><img width=750 height=50 border="0" src="../images/Catalog0001.jpg"></TD>
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
  
  window.fw_menu_0 = new Menu("root",115,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_0.addMenuItem("View Statement","location='custstat.asp'"); 
  fw_menu_0.addMenuItem("Contact mgt","location='custmanage.asp'");

   fw_menu_0.hideOnMouseOut=true;

  window.fw_menu_1 = new Menu("root",105,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_1.addMenuItem("Check Status","location='repordstatus.asp'");
  fw_menu_1.addMenuItem("Remote Order","location='repord1.asp'");
   fw_menu_1.hideOnMouseOut=true;
  window.fw_menu_2 = new Menu("root",100,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_2.addMenuItem("Check Status","location='repchkra.asp'");
  fw_menu_2.addMenuItem("Request R/A","location='reprequestra.asp'");
   fw_menu_2.hideOnMouseOut=true;
	window.fw_menu_3 = new Menu("root",100,19,"Arial, Helvetica, sans-serif",12,"#ffffff","#ffff00","#0066cc","#0099ff");
  fw_menu_3.addMenuItem("Check Invoice","location='repinvoice.asp'");
	fw_menu_3.addMenuItem("View Statement","location='custstat.asp'"); 
  
   fw_menu_3.hideOnMouseOut=true;

  fw_menu_3.writeMenus();

} // fwLoadMenus()

//-->
</script>
<script language="JavaScript1.2" src="fw_menu.js"></script>
 
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
   <td><img src="../images/spacer.gif" width="1" height="1" border="0"></td>
  </tr>

  <tr>
   <td colspan="13"><img name="RepNav3_r1_c1" src="../images/RepNav3_r1_c1.gif" width="750" height="8" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="8" border="0"></td>
  </tr>
  <tr>
   <td rowspan="2"><img name="RepNav3_r2_c1" src="../images/RepNav3_r2_c1.gif" width="80" height="59" border="0"></td>
   <td rowspan="2" colspan="2" background="../images/RepNav3_r2_c4.gif"><img name="RepNav3_r2_c2" src="../images/<%=Session("LogoPath")%>" width="60" height="59" border="0"></td>
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
   <!--<td colspan="3"><a href="#" onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_0,82,95);" ><img name="RepNav3_r5_c3" src="../images/RepNav3_r5_c3.gif" width="112" height="20" border="0"></a></td>-->
<td colspan="3"><a href="custmanage.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('RepNav4_r1_c2','','../images/RepNav4_r1_c2overf2.gif',1);" ><img name="RepNav4_r1_c2" src="../images/RepNav4_r1_c2.gif" width="112" height="20" border="0"></a></td>	
   <td colspan="2"><a href="#" onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_1,192,95);" ><img name="RepNav3_r5_c6" src="../images/RepNav3_r5_c6.gif" width="94" height="20" border="0"></a></td>
   <td colspan="2"><a href="#" onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_2,286,95);" ><img name="RepNav3_r5_c8" src="../images/RepNav3_r5_c8.gif" width="94" height="20" border="0"></a></td>
   <td colspan="1"><a href="#"  onMouseOut="FW_startTimeout();"  onMouseOver="window.FW_showMenu(window.fw_menu_3,380,95);" ><img name="CustNav3_r5_c10" src="../images/CustNav3_r5_c10.gif" width="94" height="20" border="0"></a></td>
   
	 <td><a href="repots.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('RepNav3_r5_c11','','../images/RepNav3_r5_c11overf2.gif',1);" ><img name="RepNav3_r5_c11" src="../images/RepNav3_r5_c11.gif" width="95" height="20" border="0"></a></td>
   <td><a href="repcatalog.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('RepNav3_r5_c12','','../images/RepNav3_r5_c12overf2.gif',1);" ><img name="RepNav3_r5_c12" src="../images/RepNav3_r5_c12.gif" width="94" height="20" border="0"></a></td>
   <td><a href="logoff.asp" onMouseOut="MM_swapImgRestore();"  onMouseOver="MM_swapImage('RepNav3_r5_c13','','../images/RepNav3_r5_c13overf2.gif',1);" ><img name="RepNav3_r5_c13" src="../images/RepNav3_r5_c13.gif" width="84" height="20" border="0"></td>
   <td><img src="../images/spacer.gif" width="1" height="20" border="0"></td>
  </tr>

  </table>
<table border="0" cellpadding="0" cellspacing="0" width="748">

<TR>
	<TD colspan=14 background="../images/bground.gif">
		<font color="white" size="2" face="Arial">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
              Your currently selected customer is <%=Session("customerid")%>
             <%response.write " - "&RSTemp("btname")%></font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp; <a href="repcust.asp" ><font color=white size="2" face="Arial"><b>Get Customer</b></font></a>
	</TD>
</TR>

<TR>
		<TD colspan=14><img width=750 height=50 border="0" src="../images/Catalog0001.jpg"></TD>
</TR>

</table>
 <%Set RSTemp=nothing
  ConnTemp.close
  Set ConnTemp=nothing
  %>
	<%End IF%>
</Body>
</HTML>