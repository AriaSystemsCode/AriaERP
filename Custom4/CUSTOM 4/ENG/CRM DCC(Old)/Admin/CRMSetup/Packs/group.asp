<%
Response.Buffer = True
%>
<html>
<head>
<title>CRM - Customer Groups' level</title>
<SCRIPT LANGUAGE=javascript>
<!--

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
<BR>
<form method="POST" action="groupresponse.asp?action=<%=request("action")%>" id=form1 name=form1>
  
  <Table  border="1" width="750">
  <TR>
		<TD width="250" bgcolor="#4269B8"><font color="#000080" face="Arial" size="2">Select Pack</font></TD>
		<TD width="450"><font face="Arial" size="2"><%
		Dim foxcon
		Set Foxcon = server.CreateObject("ADODB.Connection")
		foxcon.Open "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Session("Data") &";SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=Yes"
		
		Dim RSPacks
		Set RSPacks = server.CreateObject("ADODB.Recordset")
		strsql = "select * from spck_hdr where type='P' and account='*****'"
		RSPacks.Open strsql,foxcon
		'Response.Write(strsql)
		Dim SqlCon
		Set SQlcon = server.CreateObject("ADODB.Connection")
		SqlCon.Open Application("SqlServer")
		
		IF request("action") = "E" Then
			Set RSGroups = server.CreateObject("ADODB.Recordset")
			strsql = "select * from custgroup where groupid = " & request("rdoCustGrp")
			RSGroups.Open strsql,sqlcon
		%>
		<select name=lstpacks>
		<%
		IF Not(RSPacks.EOF and RSPacks.BOF) Then
			do while not RSPacks.EOF
				
				IF Trim(RSPacks.Fields("pack_id").Value) = Trim(RSGroups.Fields("packid").Value) Then
					Response.Write("<option value=" & Trim(RSPacks.Fields("pack_id").Value)  & " selected>" & RSPacks.Fields("pack_id").Value & " - " & RSPacks.Fields("desc").Value)
					strGrpDesc = Trim(RSGroups.Fields("description").Value)
				Else
					Response.Write("<option value=" & Trim(RSPacks.Fields("pack_id").Value)  & ">" & RSPacks.Fields("pack_id").Value & " - " & RSPacks.Fields("desc").Value)
				End IF
				RSPacks.MoveNext 
			loop
		End if
		
		
		%></select><input type=hidden name="txtgrpid" value="<%=request("rdoCustGrp")%>">
		<%
		Else%>
		<select name=lstpacks>
		<%
			IF Not(RSPacks.EOF and RSPacks.BOF) Then
				do while not RSPacks.EOF
						Response.Write("<option value=" & Trim(RSPacks.Fields("pack_id").Value)  & ">" & RSPacks.Fields("pack_id").Value & " - " & RSPacks.Fields("desc").Value)
					RSPacks.MoveNext 
				loop
			End if
		End IF%>
		</TD>
  </TR>
  <TR>
		<TD width="250" bgcolor="#4269B8"><font color="#000080" face="Arial" size="2">Group description</font></TD>
		<TD width="450"><Input name=txtgrpdesc value="<%=strGrpDesc%>"></TD>
  </TR>
  </Table>
  <Table  border="0" width="750">
	<TR>
		<TD align=right><input type=submit value="Submit" name=btn1></TD>
	</TR>
  </Table>

</form>
</font>
</Body>
</HTML>
