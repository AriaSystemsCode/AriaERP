<html>
<head>
<title></title>
<SCRIPT LANGUAGE=javascript>
<!--
function do_move()
{
	document.frm.action = "default.asp?type=M"
	document.frm.submit();
}
function do_moveall()
{
	document.frm.action = "default.asp?type=MA"
	document.frm.submit();
}
function do_remove()
{
	document.frm.action = "default.asp?type=R"
	document.frm.submit();
}

function do_removeall()
{
	document.frm.action = "default.asp?type=RA"
	document.frm.submit();
}

//-->
</SCRIPT>
<SCRIPT LANGUAGE=javascript>
<!--
function lfGetSysPath(frm)
{
	frm.action="PathFinder.asp?id=System"
	frm.submit()
	return true

}

function lfGetDataPath(frm)
{
	frm.action="PathFinder.asp?id=Data"
	frm.submit()
	return true
}

function lfGetLogoPath(frm)
{
	frm.action="PathFinder.asp?id=Logo"
	frm.submit()
	return true
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
<table border="0" cellpadding="0" cellspacing="0" width="750">
<!-- fwtable fwsrc="RepNav3.png" fwbase="RepNav3.gif" fwstyle="Dreamweaver" fwdocid = "742308039" fwnested="0" -->
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
<%

Set Con = server.CreateObject("ADODB.Connection")
con.Open Application("DataConnectionString")

Set rs1 = server.CreateObject("ADODB.Recordset")
	
IF isobject(Session("rs2")) Then
Else
	set Session("rs2") = server.CreateObject("ADODB.RecordSet")
	
	IF session("ShowCatalogVal") = "" Then
		strsql = "select * from customer where 1=0"
	Else
		strarGroup = split(session("ShowCatalogVal"),",")
		strGroups = ""
		for inttemp = 0 To Ubound(strarGroup)
			IF strGroups = "" then
				strGroups = "'" & strarGroup(inttemp) & "'"
			Else
				strGroups = strGroups & "," & "'" & strarGroup(inttemp) & "'"
			End IF
		next
		strsql = "select * from customer where Type='M' and status='A' and account in (" & strGroups & ")"
	End IF
	
	Session("rs2").Open strsql,con,2,4
	Session("rs2").ActiveConnection = nothing
End IF

			strsql = "select * from customer where Type='M' and Status='A' "
			IF Not(Session("rs2").Eof and Session("rs2").Bof) Then
				Session("rs2").movefirst
				Account = ""
				Do while not Session("rs2").eof
					IF strcode_no = "" Then
						strcode_no = "'" & Session("rs2").fields("account").value & "'"
					Else
						strcode_no = strcode_no & ",'" & Session("rs2").fields("Account").value & "'"
					End IF	
					Session("rs2").movenext
				Loop
				strsql = strsql & " and Account not in (" & strcode_no & ")"
			End IF
			rs1.Open strsql,con,2,4




Select case request("type")
	case "M"
		IF request("lstsource") <> "" Then
			rs1.Filter = "account='" & request("lstsource") & "'" 
			Session("rs2").Filter = "Account='" & request("lstsource") & "'" 
			IF (Session("rs2").EOF and Session("rs2").BOF) Then
				Session("rs2").addnew
				for inti=0 to rs1.Fields.count - 1
					Session("rs2").fields(rs1.Fields(inti).name).value = rs1.Fields(inti).value
				next
			End IF
			Session("rs2").Filter = "" 
			rs1.filter = "" 

			strsql = "select * from customer where Type='M' and Status='A' "
			IF Not(Session("rs2").Eof and Session("rs2").Bof) Then
				Session("rs2").movefirst
				strcode_no = ""
				Do while not Session("rs2").eof
					IF strcode_no = "" Then
						strcode_no = "'" & Session("rs2").fields("Account").value & "'"
					Else
						strcode_no = strcode_no & ",'" & Session("rs2").fields("Account").value & "'"
					End IF	
					Session("rs2").movenext
				Loop
				strsql = strsql & " and Account  not in (" & strcode_no & ")"
			End IF
			rs1.close
			rs1.Open strsql,con,2,4



		End IF
	case "MA"
		Session("rs2").Filter = "" 
		rs1.filter = "" 
		strsql = "select * from customer where Type='M' and Status='A'"
		Session("rs2").close
		Session("rs2").open strsql,con,2,4
		Session("rs2").ActiveConnection = nothing


			strsql = "select * from customer where Type='M' and Status='A'"
			IF Not(Session("rs2").Eof and Session("rs2").Bof) Then
				Session("rs2").movefirst
				strcode_no = ""
				Do while not Session("rs2").eof
					IF strcode_no = "" Then
						strcode_no = "'" & Session("rs2").fields("account").value & "'"
					Else
						strcode_no = strcode_no & ",'" & Session("rs2").fields("account").value & "'"
					End IF	
					Session("rs2").movenext
				Loop
				strsql = strsql & " and Account not in (" & strcode_no & ")"
			End IF
			rs1.close
			rs1.Open strsql,con,2,4
		
	case "R"
		IF request("lsttarget") <> "" Then
			Session("rs2").Filter = "account='" & request("lstTarget") & "'" 
			IF Not(Session("rs2").EOF and Session("rs2").BOF) Then								
				Session("rs2").movefirst
				Session("rs2").delete
				
			End IF
			Session("rs2").Filter = "" 
			rs1.filter = "" 

			strsql = "select * from customer where Type='M' and status='A'"
			IF Not(Session("rs2").Eof and Session("rs2").Bof) Then
				Session("rs2").movefirst
				strcode_no = ""
				Do while not Session("rs2").eof
					IF strcode_no = "" Then
						strcode_no = "'" & Session("rs2").fields("account").value & "'"
					Else
						strcode_no = strcode_no & ",'" & Session("rs2").fields("account").value & "'"
					End IF	
					Session("rs2").movenext
				Loop
				strsql = strsql & " and account  not in (" & strcode_no & ")"
			End IF
			rs1.close
			rs1.Open strsql,con,2,4


		End IF
	Case "RA"
		IF Not(Session("rs2").EOF and Session("rs2").BOF) Then
			Session("rs2").Movefirst
		End IF
		Do while Not Session("rs2").Eof
			Session("rs2").delete
			Session("rs2").movefirst
		Loop

			strsql = "select * from customer where Type='M' and status='A'"
			IF Not(Session("rs2").Eof and Session("rs2").Bof) Then
				Session("rs2").movefirst
				strcode_no = ""
				Do while not Session("rs2").eof
					IF strcode_no = "" Then
						strcode_no = "'" & Session("rs2").fields("Account").value & "'"
					Else
						strcode_no = strcode_no & ",'" & Session("rs2").fields("Account").value & "'"
					End IF	
					Session("rs2").movenext
				Loop
				strsql = strsql & " and Account not in (" & strcode_no & ")"
			End IF
			rs1.close
			rs1.Open strsql,con,2,4


End Select
RS = Move_IT(Session("rs2"),rs1,"Define Styles' Groups","Account","btname")
%>
































<%
Function Move_IT(RSTarget,RSSource,Title,key,KeyDesc)
%>

<table border="0" width="750">
  <tr>
    <td width="100%"><Form name=frm method=post action="crmsetup1.asp">

<div align="center">
  <center>
<table border="0" width="60%">
  <tr>
    <td width="100%" colspan="4">
      <p align="center"><font face=arial>&nbsp;<%=Title%></font></p>
    </td>
  </tr>
  <tr>
    <td width="40%" align="center">
        <select size="10" name="lstSource"  style="WIDTH: 200px;">
        <%
        Do while Not RSSource.Eof
					Response.Write("<option value=" & RSSource.Fields(key).Value & ">" & RSSource.Fields(KeyDesc).Value )
					RSSource.MoveNext 
        Loop
        %>
        </select>
    <td width="7%" align="center" colspan="2">
      <table border="0" width="100%">
        <tr>
          <td width="100%" align="center">
            <input type="button" value=">" name="btnmove" style="WIDTH: 60px; HEIGHT: 24px" size=30 onclick="do_move()">
        </tr>
        <tr>
          <td width="100%" align="center">
          	  <input type="button" value=">>" name="btnmoveall" style="WIDTH: 60px; HEIGHT: 24px" size=30 onclick="do_moveall()">
          </td>
        </tr>
        <tr>
          <td width="100%" align="center"><input type="button" value="<" name="B3" style="WIDTH: 60px; HEIGHT: 24px" size=30 onclick="do_remove()">
          </td>
        </tr>
        <tr>
          <td width="100%" align="center"><input type="button" value="<<" name="B3" style="WIDTH: 60px; HEIGHT: 24px" size=30 onclick="do_removeall()">
          </td>
        </tr>
      </table>
    </td>
    <td width="34%" align="center">
        <select size="10" name="lstTarget" style="WIDTH: 200px;">
        <%
        IF Not (RSTarget.Eof And RSTarget.BOF) Then
					RSTarget.movefirst
        End IF
        Do while Not RSTarget.Eof
					Response.Write("<option value=" & RSTarget.Fields(key).Value & ">" & RSTarget.Fields(KeyDesc).Value )
					RSTarget.MoveNext 
        Loop
        %>
        </select>
    </td>
  </tr>
  <tr>
    <td width="50%" colspan="2" align="right">
			<input type="submit" value="Submit" name="B3" style="WIDTH: 60px; HEIGHT: 24px" size=30>
    </td>
    <td width="50%" colspan="2">
			<input type="reset" value="Reset" name="B3" style="WIDTH: 60px; HEIGHT: 24px" size=30>
    </td>
  </tr>
  <tr>
    <td width="37%">&nbsp;</td>
    <td width="24%" colspan="2">&nbsp;</td>
    <td width="39%">&nbsp;</td>
  </tr>
</table>
  </center>
</div>
&nbsp;
</form>
</td>
  </tr>
</table>

<%
End Function
%>

