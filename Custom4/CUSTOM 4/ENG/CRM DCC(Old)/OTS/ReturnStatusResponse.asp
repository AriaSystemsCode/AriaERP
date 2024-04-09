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
%>

<html>
<head>
<LINK REL=stylesheet HREF="crmmain.css" TYPE="text/css">
<title>CRM - Check Return Authorization Status</title>
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
	<TD colspan=14><img width="750" height=50 border="0" src="../images/Checkrast0001.jpg"></TD>
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
   <td rowspan="2" colspan="2" background=../images/RepNav3_r2_c4.gif><img name="RepNav3_r2_c2" src="../images/<%=Session("LogoPath")%>" width="60" height="59" border="0"></td>
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
	<TD colspan=14><img width="750" height=50 border="0" src="../images/Checkrast0001.jpg"></TD>
</TR>

</table>

<%End IF%>


<%IF compWork = "Y" Then%>

<%
set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
set rsRanosMatch = server.CreateObject("ADODB.RecordSet")

BeginDate = (Trim(Ucase(Request.Form("txtBeginDate"))))
EndDate  = (Trim(Ucase(Request.Form("txtEndDate"))))
Status = Request.Form("selectStatus")
'Build the query according to entered values ..
if len(trim(session("rep")))>0 then
	strSQL = "SELECT DISTINCT Retauth.Rano,Retauth.Order, Retauth.Status, Retauth.Radate,Retauth.Void, Retauth.Store, Customer.Stname FROM Retauth, Customer WHERE Customer.Store=Retauth.Store AND Retauth.Account='" & Session("customerID") & "' AND Customer.Account ='" & Session("customerID") & "'"
else
	strSQL = "SELECT DISTINCT Retauth.Rano,Retauth.Order, Retauth.Status, Retauth.Radate,Retauth.Void, Retauth.Store, Customer.Stname FROM Retauth, Customer WHERE Customer.Store=Retauth.Store AND Retauth.Account='" & Session("ID") & "' AND Customer.Account ='" & Session("ID") & "'"
end if
if Trim(Request.Form("txtRanoNo"))<>"" then
		if len(trim(session("rep")))>0 then
			testexistSQL = "SELECT Cartons, Auth, Authamt, Rano,Status,Radate,Void, Store, Reason, Cdivision, Nreta_bud, Nreta_rec, Nreta_can, Nreta_opn, Nrtopnamt FROM Retauth WHERE Account='" & Session("customerID") & "' AND Rano='" & Request.Form("txtRanoNo") & "'"
	else
     	testexistSQL = "SELECT Cartons, Auth, Authamt, Rano,Status,Radate,Void, Store, Reason, Cdivision, Nreta_bud, Nreta_rec, Nreta_can, Nreta_opn, Nrtopnamt FROM Retauth WHERE Account='" & Session("ID") & "' AND Rano='" & Request.Form("txtRanoNo") & "'"	
	end if
	rsRanosMatch.Open testexistSQL, conn
	if rsRanosMatch.BOF AND rsRanosMatch.EOF then
		strSQL=strSQL & " AND Rano LIKE '" & Request.Form("txtRanoNo") & "%'"
	
	else
	
		'if len(trim(session("rep")))>0 then
		'	Response.Redirect ("repReturnStatusDetail.asp?RanoNo=" & Request.Form("txtRanoNo"))
		'else
		  Response.Redirect ("ReturnStatusDetail.asp?RanoNo=" & Request.Form("txtRanoNo"))
		'end if
	 end if
	 rsRanosMatch.close()
end if

if BeginDate = "" then
else
strSQL = strSQL & " AND Radate >={" & BeginDate & "}"
end if 
if EndDate ="" then
else
strSQL = strSQL & " AND Radate <={" & EndDate & "}"
end if
if Status = "ALL" then
else
strSQL = strSQL & " AND Retauth.Status='" & Status & "'"
end if



'the strSQL was build above .. 
'Now start paging.
Const NumPerPage = 25
Dim CurPage
If Request.QueryString("CurPage") = "" then
    CurPage = 1 'We're on the first page
Else
    CurPage = Request.QueryString("CurPage")
End if

set rsRanosMatchResult = server.CreateObject("ADODB.RecordSet")
rsRanosMatchResult.CursorLocation = 3 ' adUseClient
rsRanosMatchResult.CacheSize = NumPerPage

If Request.QueryString("CurPage") = "" then
	strSQL = strSQL
else
	strSQL = Request.form("strSQL")
end if

rsRanosMatchResult.Open strSQL, conn

%>

<BR>
  <table border="0" width="80%" cellspacing="0" cellpadding="0">
    <tr>
      <td width="100%">
      <% 
	if rsRanosMatchResult.EOF AND rsRanosMatchResult.BOF then%>	
<table border=0 >
<tr><td>
		<font face="Arial" size="2" color="#000080"><b>There are no records matching the entered selection criteria. </b></font>
</td></tr>
<tr><td><A HREF="returnStatus.asp"><font size=2 face=arial>back</font></A>
</td></tr>
</table>
	<%
	else
		
		rsRanosMatchResult.MoveFirst()
		rsRanosMatchResult.PageSize = NumPerPage

		Dim TotalPages
		TotalPages = rsRanosMatchResult.PageCount
		rsRanosMatchResult.AbsolutePage = CurPage

		'Counting variable for our recordset
		Dim count%>
	
<font face="Arial" size="2" color="#000080"><strong>The following Return Authorizations matched the entered selection criteria:</strong>

      </td>
    </tr>
  </table>

<br>
</p>
<table border="1" width="750" bgcolor="#6495D0" bordercolor="#AECAE6">
	<tr>
        <td bgcolor="#6495D0">
			<font face="Arial" size="2" color="#000080"><strong>Account<strong></font>
		</td>
        <td colspan=4 bgcolor="#6495D0">
			<font size="2" color="#FFFFFF" face="Arial">
			<%
			IF not trim(session("rep"))="" Then 
			Set rs=server.CreateObject("ADODB.recordset")
			sqls="Select * from  customer where account='"&request("txtcustid")&"'"
			rs.Open sqls,conn,1,3
			END IF%>
			<%if len(trim(session("rep")))>0 then
					if not rs.BOF and not rs.EOF then%>
						<%=session("customerid")%> - <%=rs("btname")%>
					<%end if%>
				
				
			<%else%>
			<%=Session("RSCust").fields("account")%> - <%=Session("RSCust").fields("btname")%>
			<%end if%>
			</font>
		</td>
    </tr>
	<tr>
        <td><font face="Arial" size="2" color="#000080"><strong>R/A #</strong></font>
        </td>
        <td><font face="Arial" size="2" color="#000080"><strong>Status</strong></font>
        </td>
        <td><font face="Arial" size="2" color="#000080"><strong>Entered</strong></font>
        </td>
        <td><font face="Arial" size="2" color="#000080"><strong>Void After</strong></font>
        </td>
              <td><font face="Arial" size="2" color="#000080"><strong>Store</strong></font>
        </td>
    </tr>	

	
	<%
		Count = 0
		Do while not rsRanosMatchResult.EOF And Count < rsRanosMatchResult.PageSize%>
			<tr>
				<td><font face="Arial" size="2" color="#ffffff">
					
					
					<A HREF="ReturnStatusDetail.asp?RanoNo=<%= rsRanosMatchResult("Rano")%>"><%= rsRanosMatchResult("Rano")%></A></font>
				</td>
				<td><font face="Arial" size="2" color="#ffffff">
					<%Select case rsRanosMatchResult("Status")
						case "O":
							Response.Write "Open"
						case "C":
							Response.Write "Complete"
					end select%></font>
				</td>
				<td><font face="Arial" size="2" color="#ffffff"><%= rsRanosMatchResult("Radate")%></font>
				</td>
				<td><font face="Arial" size="2" color="#ffffff"><%= rsRanosMatchResult("Void")%></font>
				</td>
				<td><font face="Arial" size="2" color="#ffffff">
					<% if Trim(rsRanosMatchResult("Store"))= "" then
							Response.Write "Main" & " - " & rsRanosMatchResult("Stname")
						else
							Response.Write rsRanosMatchResult("Store") & " - " & rsRanosMatchResult("Stname")
						
						end if%></font>
				</td>
			</tr>	
			<%
			Count = Count + 1
			rsRanosMatchResult.MoveNext()
		Loop
	
%>

<SCRIPT LANGUAGE=javascript>
<!--
function SubmitNavigation(formaction)
{
	document.form1.action = formaction;
	document.form1.submit();
	
}
//-->
</SCRIPT>
</table>
<table width =750>
<tr>
	<form action="" name="form1" id="form1" method="post">
	<%if trim(Request.QueryString ("curpage")) = "" then
		searchQuery = strSQL
	else
		searchQuery = Request.form("strSQL")
	end if
	%>
	<Input type = "HIDDEN" name ="strSQL" Value ="<%=searchQuery%>">
	<td colspan = 5 align="center">
	<%Response.Write("<font face=""Arial"" size=""2"" color=""#000080"">" & "Page " & CurPage & " of " & TotalPages & "<br></font>")
	if CurPage > 1 then
        'We are not at the beginning, show the prev button%>
        <a href="javascript:SubmitNavigation('ReturnStatusResponse.asp?CurPage=<%=curpage-1%>')"><IMG src="../images/back.gif"></a>
	<%End If

	if CInt(CurPage) <> CInt(TotalPages) then
        'We are not at the end, show a next button%>
       <a href="javascript:SubmitNavigation('ReturnStatusResponse.asp?CurPage=<%=curpage+1%>')" ><IMG src="../images/next.gif"></a>
    <%End If
%>
	</td>
	</form>
</tr>
</table>
<BR>
<%
	conn.Close
	set rsRanosMatchResult = nothing
	End IF
end if ' for rsRanoMatchResult.BOF and EOF
%>
</BODY>
</HTML>


 
