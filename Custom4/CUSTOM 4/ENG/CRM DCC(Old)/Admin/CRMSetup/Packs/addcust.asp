<%@ Language=VBScript %>
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>


<%
Response.Buffer =true
Response.Expires=-1

' Customer ===================================
Dim foxcon
Set Foxcon = server.CreateObject("ADODB.Connection")
foxcon.Open "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Session("Data") &";SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=Yes"

Dim SqlCon
Set SqlCon = server.CreateObject("ADODB.Connection")
SqlCon.Open Application("SqlServer")

Set RSGroup = server.CreateObject("ADODB.RecordSet")

strsql = "select * from custGroup where groupid = " & Session("CustGrp")
RSGroup.Open strsql,SqlCon

Const NumPerPage = 10
Dim CurPage

IF Request.QueryString("CurPage") = "" Then
	CurPage = 1
Else
	CurPage = Request.QueryString("CurPage")
End IF

Set RSCustomer = Server.CreateObject("ADODB.Recordset")
RSCustomer.CursorLocation = 3

IF request("txtCust") = "" Then
	strCustomer = "Select account,Btname From Customer where type='M' and status='A' order by account"
Else
	strCustomer = "Select account,Btname From Customer where type='M' and status='A' and account like '" & Ucase(request("txtCust")) & "%' order by account "
End IF
RSCustomer.Open strCustomer,foxcon

RSCustomer.CacheSize  = NumPerPage
RSCustomer.PageSize = NumPerPage
TotalPages = RSCustomer.PageCount 

IF request("type") = "N" Then
	RSCustomer.AbsolutePage = CurPage - 1
End IF

IF request("type") = "P" Then
	RSCustomer.AbsolutePage = CurPage + 1
End IF


Dim Count
Count = 0

Do while Not RSCustomer.EOF and count < NumPerPage
	IF request(RSCustomer.Fields("account").Value) = "ON" Then
		IF Session("StrAccount") = "" Then
			Session("StrAccount") = RSCustomer.Fields("account").Value 
		Else
			IF instr(1,Session("StrAccount"),RSCustomer.Fields("account").Value) = 0 Then
				Session("StrAccount") = Session("StrAccount") & "," & RSCustomer.Fields("account").Value
			End IF
		End IF
	Else
		IF Not session("FlagCust") Then
			Session("StrAccount") = replace(Session("StrAccount"),RSCustomer.Fields("account").Value,"")
		end IF
	End IF
	count = count + 1
	RSCustomer.MoveNext 
Loop
session("FlagCust") = False

Count = 0
IF Not(RSCustomer.EOF And RSCustomer.BOF) Then
	RSCustomer.MoveFirst 
End IF
RSCustomer.AbsolutePage = CurPage


%>
<html>
<head>
<title>CRM - Customer Groups' level</title>
<SCRIPT LANGUAGE=javascript>
<!--

function getPrev()
{
	document.CustFrm.action = "addcust.asp?type=P&CurPage=<%=CurPage - 1%>";
	document.CustFrm.submit();
	
}

function getNext()
{
	
	document.CustFrm.action = "addcust.asp?type=N&CurPage=<%=CurPage + 1%>";
	//savecust.asp?GrpID=<%Response.Write(Session("CustGrp"))%>
	document.CustFrm.submit();
}


function MM_showHideLayers() { //v3.0
  var i,p,v,obj,args=MM_showHideLayers.arguments;
  for (i=0; i<(args.length-2); i+=3) if ((obj=MM_findObj(args[i]))!=null) { v=args[i+2];
    if (obj.style) { obj=obj.style; v=(v=='show')?'visible':(v='hide')?'hidden':v; }
    obj.visibility=v; }
}


function MM_findObj(n, d) { //v4.0
  var p,i,x;  if(!d) d=document; if((p=n.indexOf("?"))>0&&parent.frames.length) {
    d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}
  if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];
  for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document);
  if(!x && document.getElementById) x=document.getElementById(n); return x;
}


//-->
</SCRIPT>
</head>
<Body bgcolor="#AECAE6">
<%

%>
<Font face=arial>
<table border="0" cellpadding="0" cellspacing="0" width="750">

  <tr>
   <td><img src="../../../images/spacer.gif" width="80" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="3" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="57" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="10" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="45" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="11" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="83" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="39" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="55" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="94" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="95" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="94" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="84" height="1" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="1" height="1" border="0"></td>
  </tr>

  <tr>
   <td colspan="13"><img name="RepNav3_r1_c1" src="../../../images/RepNav3_r1_c1.gif" width="750" height="8" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="1" height="8" border="0"></td>
  </tr>
  <tr>
   <td rowspan="2"><img name="RepNav3_r2_c1" src="../../../images/RepNav3_r2_c1.gif" width="80" height="59" border="0"></td>
   <%If trim(Session("LogoPath"))<>"" Then %>
   <td rowspan="2" colspan="2" background=../../../images/RepNav3_r2_c4.gif><img name="RepNav3_r2_c2" src="../../../images/<%=Session("LogoPath")%>" width="60" height="59" border="0"></td>
   <%Else%>
   <td rowspan="2" colspan="2"  background=../../../images/RepNav3_r2_c4.gif><img name="RepNav3_r2_c2" src="../../../images/AnimatedLogo.gif" width="60" height="59" border="0"></td>
   <%End If%>
   <td rowspan="2"><img name="RepNav3_r2_c4" src="../../../images/RepNav3_r2_c4.gif" width="10" height="59" border="0"></td>
   <td rowspan="2" colspan="2"><img name="RepNav3_r2_c5" src="../../../images/RepNav3_r2_c5.gif" width="56" height="59" border="0"></td>
   <td rowspan="2" colspan="2"><img name="RepNav3_r2_c7" src="../../../images/RepNav3_r2_c7.gif" width="122" height="59" border="0"></td>
   <td colspan="5"><img name="RepNav3_r2_c9" src="../../../images/RepNav3_r2_c9.gif" width="422" height="30" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="1" height="30" border="0"></td>
  </tr>
  <tr>
   <td colspan="5"><img name="RepNav3_r3_c9" src="../../../images/RepNav3_r3_c9.gif" width="422" height="29" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="1" height="29" border="0"></td>
  </tr>
  <tr>
   <td colspan="13"><img name="RepNav3_r4_c1" src="../../../images/RepNav3_r4_c1.gif" width="750" height="8" border="0"></td>
   <td><img src="../../../images/spacer.gif" width="1" height="8" border="0"></td>
  </tr>
  <tr>
   <td colspan="14"><img border="0" src="../../../images/crmsetup.jpg" width="750" height="50"></td>
  </tr>
</table>
<BR><SCRIPT LANGUAGE=javascript>
<!--
	
function getCustomer()
 {
document.forma.action="addcust.asp";
document.forma.method="POST";
document.forma.submit();
}

//-->
</SCRIPT>

</font>
<p>
<Font face=arial size=2>
<BR>
<%
IF Not(RSGroup.EOF And RSGroup.BOF ) Then
	Response.Write("Selected Group Name  - " & RSGroup.Fields("Description").Value)
End IF
%>
<BR>
<Form name="forma" ID="forma" method=post>
<table border=0 width="750">
<tr>
<td  valign="Top" bgcolor="#4269B8" width="103">
<center><font face="Arial" size="2" color="#000080"><b>Customer ID</b></font>&nbsp&nbsp
</td>
<td  valign="Top" bgcolor="#4269B8" width="68">
	<INPUT TYPE="text"  NAME="txtCust" size="5" maxlength="5" value="<%=Ucase(request("txtCust"))%>">
</td>
<td  valign="Top" bgcolor="#4269B8" width="369">
	<INPUT TYPE="submit" NAME="" style="width:125;" VALUE="Get Customer" onclick="getCustomer();">
</td>
</tr>
</table>
</form>
<BR>

<Form name="CustFrm" id="CustFrm" method=post action="savecust.asp?GrpID=<%Response.Write(Session("CustGrp"))%>&CurPage=<%=request("CurPage")%>" >

<Table width=750 border=1>
	<TR>
		<TD bgcolor="#4269B8" width=30>&nbsp;
		</TD>
		<TD bgcolor="#4269B8"><Font color="#000080">&nbsp;Customer Account</font>
		</TD>
		<TD bgcolor="#4269B8"><Font color="#000080">&nbsp;Customer Name</font>
		</TD>
	</TR>
	<%
	Do While Not RSCustomer.EOF and count<NumPerPage
		
	%>
	<TR>
		<TD bgcolor="ivory"><Font size=2>&nbsp;<input type="checkbox" name="<%=RSCustomer.Fields("account").Value%>" value="ON" <%
		IF instr(1,Session("StrAccount"),RSCustomer.Fields("account").Value) <> 0 Then
				'Session("StrAccount") = Session("StrAccount") & "," & RSCustomer.Fields("account").Value
				Response.Write(" checked ")
			End IF
			
		%>></font>
		</TD>
		<TD bgcolor="ivory"><font size=2>&nbsp;<%=RSCustomer.Fields("account").Value%></font>
		</TD>
		<TD bgcolor="ivory"><font size=2>&nbsp;<%=RSCustomer.Fields("btname").Value%></font>
		</TD>
	</TR>
	<%
		Count = Count + 1
		RSCustomer.MoveNext 
	Loop
	%>
</Table>
<Table width=750 border=0>

<TR>
	<TD align=right>
		<INPUT TYPE="submit" NAME="btn11" style="width:125;" VALUE="Submit">
	</TD>
</TR>
<TR><TD align=center colspan=1>
<%
IF CurPage > 1 then
%>
	<a href="javascript:getPrev()"> Previous </a>
<%End IF%>
<%
IF cint(CurPage) <> cint(TotalPages) Then%>
	<a href="javascript:getNext()" > Next </a>
	
	
<%End IF
%>
<BR>Page <%=CurPage%> of <%=TotalPages%>
</TD>
</TR>
</Table>
</Form>
</font>
</BODY>
</HTML>
