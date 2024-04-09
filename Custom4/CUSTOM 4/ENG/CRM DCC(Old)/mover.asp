<%
Function Move_IT(RSTarget,RSSource)
'Set Con = server.CreateObject("ADODB.Connection")
'con.Open Application("DataConnectionString")

set Session("RSSource") = server.CreateObject("ADODB.RecordSet")
set Session("RSSource") = RSSource
'strsql = "select * from customer"
'Session("RSSource").Open strsql,con,2,4

IF isobject(Session("RSTarget")) Then
Else
	set Session("RSTarget") = server.CreateObject("ADODB.RecordSet")
	set Session("RSTarget") = RSTarget
'	strsql = "select * from customer where 1=0"
'	Session("RSTarget").Open strsql,con,2,4
'	Session("RSTarget").ActiveConnection = nothing
End IF

IF Not(Session("RSTarget").EOF And Session("RSTarget").BOF) Then
	Session("RSTarget").movefirst
End IF
%>

<SCRIPT LANGUAGE=javascript>
<!--
function do_move()
{
	document.frm.action = "try_m.asp?type=M"
	document.frm.submit();
}
function do_moveall()
{
	document.frm.action = "try_m.asp?type=MA&source=<%=request("lstsource")%>"
	document.frm.submit();
}
function do_remove()
{
	document.frm.action = "try_m.asp?type=R"
	document.frm.submit();
}

function do_removeall()
{
	document.frm.action = "try_m.asp?type=RA"
	document.frm.submit();
}

//-->
</SCRIPT>


<Form name=frm method=post action="mover1.asp">
<div align="center">
  <center>
<table border="1" width="60%">
  <tr>
    <td width="40%" align="center">
        <select size="10" name="lstSource"  style="WIDTH: 200px;">
        <%
        Do while Not Session("RSSource").Eof
					Response.Write("<option value=" & Session("RSSource").Fields("Account").Value & ">" & Session("RSSource").Fields("Stname").Value )
					Session("RSSource").MoveNext 
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
        Do while Not Session("RSTarget").Eof
					Response.Write("<option value=" & Session("RSTarget").Fields("Account").Value & ">" & Session("RSTarget").Fields("Stname").Value )
					Session("RSTarget").MoveNext 
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
<%
End Function

function ititit()
Select case request("type")
	case "M"
		IF request("lstsource") <> "" Then
			Session("RSSource").Filter = "account='" & request("lstsource") & "'" 
			Session("RSTarget").Filter = "account='" & request("lstsource") & "'" 
			IF (Session("RSTarget").EOF and Session("RSTarget").BOF) Then
				Session("RSTarget").addnew
				for inti=0 to Session("RSSource").Fields.count - 1
					Session("RSTarget").fields(Session("RSSource").Fields(inti).name).value	 = Session("RSSource").Fields(inti).value
				next
				Session("RSTarget").updatebatch
			End IF
			Session("RSTarget").Filter = "" 
			Session("RSSource").filter = "" 
		End IF
		'Response.Redirect("mover.asp")
	case "ML"
		Set Session("RSTarget") = Session("RSSource")
		'Response.Redirect("mover.asp")
	case "R"
		IF request("lsttarget") <> "" Then
			Session("RSTarget").Filter = "account='" & request("lstTarget") & "'" 
			IF Not(Session("RSTarget").EOF and Session("RSTarget").BOF) Then
				Session("RSTarget").delete
				Session("RSTarget").updatebatch
				Session("RSTarget").movefirst
			End IF
			Session("RSTarget").Filter = "" 
			Session("RSSource").filter = "" 
		End IF
		'Response.Redirect("mover.asp")
	Case "RA"
		IF Not(Session("RSTarget").EOF and Session("RSTarget").BOF) Then
			Session("RSTarget").Movefirst
		End IF
		Do while Not Session("RSTarget").Eof
			Session("RSTarget").delete
			Session("RSTarget").movefirst
		Loop
		'Response.Redirect("mover.asp")
End Select
end function


%>