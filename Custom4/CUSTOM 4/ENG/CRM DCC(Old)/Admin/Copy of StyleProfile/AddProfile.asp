<%@LANGUAGE="VBSCRIPT" CODEPAGE="1252"%>
<!--#include file="../Connections/cnConn.asp" -->
<%Response.Buffer = true%>
<%
'Response.Write "<font size=3>" & session("strRmvStr")
'check if i m coming from the edit page
'Response.Write "<font size=3>" &session("strStyles") 
'Response.End 
if Request.QueryString ("code") <> "" or Request.QueryString ("selCode") <> "" then
	dim strCodeNo
	if Request.QueryString ("code") <> "" then
		strCodeNo = Request.QueryString ("code") 
	else
		strCodeNo = Request.QueryString ("selCode") 
	end if
	set cnnSQL = server.CreateObject ("ADODB.Connection")
	cnnSQL.Open Application("SqlServer") 
	Dim strSql, rsProfileHdr,strCode,strName
	
	set rsProfileHdr = Server.CreateObject("ADODB.RecordSet")
	set rsProfileDt = Server.CreateObject("ADODB.RecordSet")
	'check if the code exists before
	strSql = "Select * From styleProfileHeader Where cStyleProfileCode ='"&  trim(strCodeNo) & "'"
	'Response.Write strCodeNo
	'Response.end
	rsProfileHdr.Open strSql,cnnSQL,1,3
	'check if editing or just selecting
	if trim(Request.QueryString ("selCode"))  = "" then
		strCodeNo = rsProfileHdr("cStyleProfileCode") 
		strCode = rsProfileHdr("cStyleProfileCode") 
		strName = rsProfileHdr("cStyleProfileName")
	end if
	'check that i didn't get the records
	if session("strStyles") = "" then
		strSql = "Select * From styleProfiledetail Where cStyleProfileCode ='"& trim(strCodeNo) & "'"
		rsProfileDt.Open strSql,cnnSQL,1,3
		do while not rsprofiledt.EOF
			if session("strStyles") = "" then
				session("strStyles") = "'" & Trim(rsProfileDt("cstyle"))& "'"
			else
				session("strStyles") = session("strStyles") & ", " & "'" & Trim(rsProfileDt("cstyle")) & "'"
			end if
		rsprofiledt.MoveNext 
		loop
		rsprofiledt.close
	end if
	rsProfileHdr.Close
end if
'Response.Write "<br>" & "<font size=3>" &session("strStyles") 
'if Request.QueryString ("selectSty") <> "" then
	'Response.Write session("strStyles")& "<br>" &session("strRmvStr")
	'Response.End 
'else
	'session("strStyles") = ""
	'session("strRmvStr") = ""
'end if
%>
<html>
<head>
<title>Add New Profile</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<link href="../../Images/<%=session("theme")%>/Common.css" rel="stylesheet" type="text/css">
<script language="JavaScript" type="text/JavaScript">
<!--
function MM_findObj(n, d) { //v4.01
  var p,i,x;  if(!d) d=document; if((p=n.indexOf("?"))>0&&parent.frames.length) {
    d=parent.frames[n.substring(p+1)].document; n=n.substring(0,p);}
  if(!(x=d[n])&&d.all) x=d.all[n]; for (i=0;!x&&i<d.forms.length;i++) x=d.forms[i][n];
  for(i=0;!x&&d.layers&&i<d.layers.length;i++) x=MM_findObj(n,d.layers[i].document);
  if(!x && d.getElementById) x=d.getElementById(n); return x;
}

function MM_validateForm() { //v4.0
  var i,p,q,nm,test,num,min,max,errors='',args=MM_validateForm.arguments;
  for (i=0; i<(args.length-2); i+=3) { test=args[i+2]; val=MM_findObj(args[i]);
    if (val) { nm=val.name; if ((val=val.value)!="") {
      if (test.indexOf('isEmail')!=-1) { p=val.indexOf('@');
        if (p<1 || p==(val.length-1)) errors+='- '+nm+' must contain an e-mail address.\n';
      } else if (test!='R') { num = parseFloat(val);
        if (isNaN(val)) errors+='- '+nm+' must contain a number.\n';
        if (test.indexOf('inRange') != -1) { p=test.indexOf(':');
          min=test.substring(8,p); max=test.substring(p+1);
          if (num<min || max<num) errors+='- '+nm+' must contain a number between '+min+' and '+max+'.\n';
    } } } else if (test.charAt(0) == 'R') errors += '- '+nm+' is required.\n'; }
  } if (errors) alert('The following error(s) occurred:\n'+errors);
  document.MM_returnValue = (errors == '' && CheckPassWord());
}
function chkForm()
{
	if (document.form1.txtCode.value == "")
	{
		alert("Please enter the Profile code!");
		document.form1.txtCode.focus() ;
		return false;
	}
	if ("<%=trim(session("strStyles"))%>" == '')
	{
		alert("Please select the Style(s) assigned to the Profile!");
		return false;
	}
	
	document.form1.action = 'saveprofile.asp?code=<%=Request.QueryString ("code")%>';
	document.form1.submit();
}
function delProfile()
{
	
	document.form1.action = 'saveprofile.asp?del=T&code=<%=Request.QueryString ("code")%>';
	document.form1.submit();
}
function openwindow(strFile) 
{  
	window.open(strFile,'','toolbar=no,status=no,scrollbars=yes,location=no,resizable=yes,menubar=no,directories=no,top=' + ((window.screen.height-500)/2) + ',left=' + ((window.screen.width-800)/2) + ',width=800,height=500')
}
//-->
</script>
</head>

<body bgcolor="#3366CC" text="#FFFFF0">
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
 <form  method="POST" name="form1" >
<table width="80%" border="0" align="center">
  <tr> 
    <td> 
        <table width="100%" border="0" align="center">
		  <tr> 
			<td colspan="3" class="Dark_Cell"><div align="right"><a href="Default.asp"><font color="#FFFFFF">
			Back to Style Profile</font></a></div></td>
		  </tr>
		
          <tr> 
          <%if Request.QueryString ("code") = "" then%>
               <td colspan="3" class='Title'>Add Profile</td>  
          <%else%>
				<td colspan="3" class='Title'>Edit Profile</td> 
          <%end if%>
          </tr>
          <tr> 
            <td width="24%" class='Dark_Cell'>Profile Code*</td>
            <td colspan="2" class='Light_Cell'> 
				<input name="txtCode" type="text" <%if strCode= "" then%>value="<%=Request.Form ("txtCode")%>" <%else%>value="<%=strCode%>" readonly <%end if%>size="6" maxlength="10">             
				<%if Request.QueryString ("code") = "" then%>
					<input type=button value="Copy from Profile" onclick="openwindow('editprofile.asp?select=T');">
				<%end if%>
				<%'check if i m in the edit mode then add a button option to delete profile
				if Request.QueryString ("code") <> "" then%>
					<input type=button value="Delete" onclick="delProfile();">
				<%end if%>
            </td>
          </tr>
          <tr> 
            <td class='Dark_Cell'>Name</td>
            <td colspan="2" class='Light_Cell'>
				<input name="txtName" type="text" <%if strname= "" then%>value="<%=Request.Form ("txtName")%>"  <%else%>value="<%=strName%>" <%end if%>size="31" maxlength="30">
			</td>
          </tr>
		 <tr> 
			  <td class='Dark_Cell'>Style(s)*</td>
			  <td colspan="2" class='Light_Cell'>
					<input type=button value="Select/Update Style" onclick="openwindow('findstyle.asp?code=<%=Request.QueryString ("code")%>&first=T');">
			  </td>
		  </tr>  
          <tr> 
            <td colspan="3" class='Light_cell'>(*) Indicates Required Field</td></tr><tr>
              <td colspan="3"><div align="right">                  
                </div></td>
           </tr>   
           
        </table>
      </td>
  </tr>
  <%'wal check if there are styles added
  if trim(session("strStyles")) <> "" then
		'Response.Write "<font size=3>" &trim(session("strStyles"))
		'Response.End 
		set cnnDB=server.CreateObject("ADODB.connection")
		cnnDB.Open Application("DataConnectionString")
		
		Set RSCodes = Server.CreateObject("ADODB.RECORDSET")
		strSql = "Select ccode_no, cdiscrep, cdefcode from codes where cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam like 'NCSTYGROUP%' and cdiscrep <> '' Order By cCode_No"
		RSCodes.Open strSql,cnnDB
		
		dim rsResult
		set session("rsResult") = server.CreateObject ("ADODB.recordset")
		'open a recordset with the sleceted styles
		if session("strRmvStr") = "" then'check first if there are removed records
			arrStyle = split(trim(session("strStyles")),",")
			if ubound(arrStyle) > 0 then
				strsql = "select * from style where "
				for i=0 to ubound(arrStyle)
					strSql = strSql & "style =" & arrStyle(i)
					if i <> UBound(arrStyle) then
						strSql = strSql & " OR "
					End if
					
				next
				strsql = strsql & " order by style"
				'Response.Write "<font size=2>"&strsql
				'Response.End 
				session("rsResult").Open strsql,cnnDB,1,3
			else'only one record
				session("rsResult").Open "select * from style where style ='" &arrStyle(0)&"' ",cnnDB,1,3
			end if
			'session("rsResult").Open "select * from style where style in (" &trim(session("strStyles"))& ") order by style",cnnDB,1,3
		else
			arrStyle = split(trim(session("strStyles")),",")
			if ubound(arrStyle) > 0 then
				strsql = "select * from style where "
				for i=0 to ubound(arrStyle)
					strSql = strSql & "style =" & arrStyle(i)
					if i <> UBound(arrStyle) then
						strSql = strSql & " OR "
					End if
					
				next
				strsql = strsql & " AND style not in (" &session("strRmvStr")& ") order by style"
				'Response.Write "<font size=2>"&strsql
				'Response.End 
				session("rsResult").Open strsql,cnnDB,1,3
			else'only one record
				session("rsResult").Open "select * from style where style ='" &arrStyle(0)&"' AND style not in (" &session("strRmvStr")& ")",cnnDB,1,3
			end if
			'session("rsResult").Open "select * from style where style in (" &trim(session("strStyles"))& ") ",cnnDB,1,3
		end if
  %>
  <tr>       
  <td >
  <table border="1" width="100%" align=center cellpadding="0" cellspacing="0" "#111111">
	<TR>
		
		<!--TD width=5% class="dark_cell">></TD-->
		<TD width=22% class="dark_cell">Style</TD>
		<TD Width=45% class="dark_cell">Description</TD>
		<TD class=dark_cell>Group</TD>

	</TR>
  <%
	Do While Not session("rsResult").EOF	
  %>
		<TR>
			<!--TD class="light_cell" ><input type=checkbox name="chkID" onclick="chk(this);" value="<%=trim(session("rsResult")("cstymajor"))%>"></td-->
			<TD class="light_cell" ><%=trim(session("rsResult")("style"))%></TD>
			<TD class="light_cell" >&nbsp<%=trim(session("rsResult")("desc1"))%></TD>
			<%
				RSCodes.Filter = "ccode_no='"& Trim(session("rsResult").Fields("cstygroup").Value)& space(6-(len(Trim(session("rsResult").Fields("cstygroup").Value)))) &"'"
				if not rscodes.EOF then
					strGrp = rscodes.Fields("cdiscrep").Value
				end if
			%>
			<TD class="light_cell" >&nbsp<%=trim(strGrp)%></TD>
		</TR>
<%
	session("rsResult").MoveNext		
	Loop

%>
</table>
</td></tr>
  <%
  end if
  %>
  <tr>       
     <td ><div align="right"> 
   	      <input type="button" value="Submit" onclick="chkForm();">
          <input type="reset" name="Submit2" value="Reset">
        </div></td>
    </tr>
   <tr> 
    <td> 
        <table width="50%" border="0" align="center">
        </table>
    </td>
  </tr>
</table>
</form>
</body>
</html>
