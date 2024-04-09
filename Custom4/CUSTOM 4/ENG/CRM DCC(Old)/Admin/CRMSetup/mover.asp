<html>
<head>
<title></title>
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
<SCRIPT LANGUAGE=javascript>
<!--
function do_move()
{
	document.frm.action = "mover.asp?type=M"
	document.frm.submit();
}
function do_moveall()
{
	document.frm.action = "mover.asp?type=MA"
	document.frm.submit();
}
function do_remove()
{
	document.frm.action = "mover.asp?type=R"
	document.frm.submit();
}

function do_removeall()
{
	document.frm.action = "mover.asp?type=RA"
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

//-->
</SCRIPT>

</head>
<BODY BGCOLOR="#AECAE6" TOPMARGIN="0" LEFTMARGIN="0">
<!--TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
  <TR> 
	<td>
		<div border=1 id="Layer8" style="position:absolute; left:0; top:0; width:700; height:69; z-index:35; background-image: url(image/heder.JPG); layer-background-image: url(image/heder.JPG); border: 1px none #000000">
			<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"    codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" width="700" height="69" id=ShockwaveFlash1>
    <param name=movie value="../../banner.swf">
    <param name=quality value=high>
    <embed src="../../banner.swf" quality=high pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash" type="application/x-shockwave-flash" width="750" height="69">
    </embed> 
  </object>
		</div>
	</td>
  </TR>
</TABLE><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR-->
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
    <TD ROWSPAN=2> <IMG SRC="../../images/<%=Session("Theme")%>/Login1_05.jpg" WIDTH=60 HEIGHT=59></TD>
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
<%
'wma 05/25/2004 Session("CatDivision") = "" after update [start]
if trim(Request.form("lstDivision"))<>"" then
	'Session("CatDivision") = trim(Request.form("lstDivision"))
	Session("CatDivision") = trim(Request.form("lstDivision"))
end if
'wma 05/25/2004 Session("CatDivision") = "" after update [start]

Set Con = server.CreateObject("ADODB.Connection")
con.Open Application("DataConnectionString")

Set rs1 = server.CreateObject("ADODB.Recordset")
	
IF isobject(Session("rs2")) Then
Else
	set Session("rs2") = server.CreateObject("ADODB.RecordSet")
	
	IF session("ShowCatalogVal") = "" Then
		strsql = "select * from codes where cdefcode+crltfield+cfld_name='ZZZZZZZZ'"
	Else
		strarGroup = split(session("ShowCatalogVal"),",")
		strGroups = ""
		for inttemp = 0 To Ubound(strarGroup)
			
			IF strGroups = "" then
			 'strGroups = strGroups & " ccode_no='" & strarGroup(inttemp) & "'"
				strGroups = "'" & strarGroup(inttemp) & "'"
			Else
			'strGroups = strGroups & " or ccode_no='" & strarGroup(inttemp) & "'"
				strGroups = strGroups & "," & "'" & strarGroup(inttemp) & "'"
			End IF
		next
		strsql = "select * from codes where cdefcode+crltfield+cfld_name='NNCSTYGROUP' and " & MakeQuery("ccode_no", strGroups, 10, false)
	End IF
	Session("rs2").Open strsql,con,2,4
    'response.write("<BR>" & Session("rs2").recordcount)
	Session("rs2").ActiveConnection = nothing
End IF

			strsql = "select * from codes where cdefcode+crltfield+cfld_name='NNCSTYGROUP'"
			IF Not(Session("rs2").Eof and Session("rs2").Bof) Then
				Session("rs2").movefirst
				strcode_no = ""
				Do while not Session("rs2").eof
					IF strcode_no = "" Then
						strcode_no = "'" & Session("rs2").fields("ccode_no").value & "'"
					Else
						strcode_no = strcode_no & ",'" & Session("rs2").fields("ccode_no").value & "'"
					End IF	
					Session("rs2").movenext
				Loop
				strsql = strsql & " and " & makeQuery("ccode_no",  strcode_no , 10, true)
			End IF
			rs1.Open strsql,con,2,4

Select case request("type")
	case "M"
		IF request("lstsource") <> "" Then
			rs1.Filter = "ccode_no='" & request("lstsource") & "'" 
			'Check if exist in the target recordset
			Session("rs2").Filter = "ccode_no='" & request("lstsource") & "'" 
			IF (Session("rs2").EOF and Session("rs2").BOF) Then
				'Add to the target recordset
				Session("rs2").addnew
				for inti=0 to rs1.Fields.count - 1
					Session("rs2").fields(rs1.Fields(inti).name).value = rs1.Fields(inti).value
				next
			End IF
			Session("rs2").Filter = "" 
			rs1.filter = "" 

			'To delete from the source recordSet.
			strsql = "select * from codes where cdefcode+crltfield+cfld_name='NNCSTYGROUP' "
			IF Not(Session("rs2").Eof and Session("rs2").Bof) Then
				Session("rs2").movefirst
				strcode_no = ""
				Do while not Session("rs2").eof
					IF strcode_no = "" Then
						strcode_no = "'" & Session("rs2").fields("ccode_no").value & "'"
					Else
						strcode_no = strcode_no & ",'" & Session("rs2").fields("ccode_no").value & "'"
					End IF	
					Session("rs2").movenext
				Loop
				strsql = strsql & " and " & makeQuery("ccode_no", strcode_no , 10, true)
			End IF
			rs1.close
			rs1.Open strsql,con,2,4



		End IF
	case "MA"
		Session("rs2").Filter = "" 
		rs1.filter = "" 
		strsql = "select * from codes where cdefcode+crltfield+cfld_name='NNCSTYGROUP' "
		Session("rs2").close
		Session("rs2").open strsql,con,2,4
		Session("rs2").ActiveConnection = nothing


			strsql = "select * from codes where cdefcode+crltfield+cfld_name='NNCSTYGROUP' "
			IF Not(Session("rs2").Eof and Session("rs2").Bof) Then
				Session("rs2").movefirst
				strcode_no = ""
				Do while not Session("rs2").eof
					IF strcode_no = "" Then
						strcode_no = "'" & Session("rs2").fields("ccode_no").value & "'"
					Else
						strcode_no = strcode_no & ",'" & Session("rs2").fields("ccode_no").value & "'"
					End IF	
					Session("rs2").movenext
				Loop
				strsql = strsql & " and "  & makeQuery("ccode_no",  strcode_no , 10, true)
			End IF
			rs1.close
			rs1.Open strsql,con,2,4
		
	case "R"
		IF request("lsttarget") <> "" Then
			Session("rs2").Filter = "ccode_no='" & request("lstTarget") & "'" 
			IF Not(Session("rs2").EOF and Session("rs2").BOF) Then								
				Session("rs2").movefirst
				Session("rs2").delete
				
			End IF
			Session("rs2").Filter = "" 
			rs1.filter = "" 

			strsql = "select * from codes where cdefcode+crltfield+cfld_name='NNCSTYGROUP' "
			IF Not(Session("rs2").Eof and Session("rs2").Bof) Then
				Session("rs2").movefirst
				strcode_no = ""
				Do while not Session("rs2").eof
					IF strcode_no = "" Then
						strcode_no = "'" & Session("rs2").fields("ccode_no").value & "'"
					Else
						strcode_no = strcode_no & ",'" & Session("rs2").fields("ccode_no").value & "'"
					End IF	
					Session("rs2").movenext
				Loop
				'strsql = strsql & " and ccode_no  not in (" & strcode_no & ")"
				strsql = strsql & " and " & makeQuery("ccode_no", strcode_no, 10, true)
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

			strsql = "select * from codes where cdefcode+crltfield+cfld_name ='NNCSTYGROUP'"
			IF Not(Session("rs2").Eof and Session("rs2").Bof) Then
				Session("rs2").movefirst
				strcode_no = ""
				Do while not Session("rs2").eof
					IF strcode_no = "" Then
						strcode_no = "'" & Session("rs2").fields("ccode_no").value & "'"
					Else
						strcode_no = strcode_no & ",'" & Session("rs2").fields("ccode_no").value & "'"
					End IF	
					Session("rs2").movenext
				Loop
				'strsql = strsql & " and ccode_no  not in (" & strcode_no & ")"
				strsql = strsql & " and " & makeQuery("ccode_no", strcode_no, 10, true)
			End IF
			rs1.close
			rs1.Open strsql,con,2,4


End Select
RS = Move_IT(Session("rs2"),rs1,"Define Styles' Groups","ccode_no","cdiscrep")
%>













<%
Function Move_IT(RSTarget,RSSource,Title,key,KeyDesc)
%>

<table border="0" width="95%" align=center>
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
<%
function makeQuery(fieldname, ValuesList, NoOfItems, notflag)
	dim ValuesArray
	ValuesArray = split(ValuesList, ",", -1, 1)
	
	strQuery="( "
	for i=0 to ubound(ValuesArray) 
		if (i mod NoOfItems) = 0 then
			if i <> 0 then
				if notflag = false then
					strQuery = strQuery & ") or "
				else
					strQuery = strQuery & ") and "
				end if
			end if
			strQuery = strQuery & fieldname
			if notflag = false then
				strQuery = strQuery & " in ( "
			else
				strQuery = strQuery & " not in ( "
			end if
		end if
		strQuery = strQuery & " " & ValuesArray(i) & " "
		if (((i+1) mod NoOfItems) <> 0) and i <> ubound(ValuesArray)  then
			strQuery = strQuery & ","
		end if
		
		if i = ubound(ValuesArray) then
				strQuery = strQuery & ") "
		end if
	next 
	strQuery = strQuery & ")"
	makeQuery = strQuery
end function
%>