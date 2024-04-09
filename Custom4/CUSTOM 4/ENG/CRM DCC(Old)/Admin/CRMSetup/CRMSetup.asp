<%@ Language=VBScript %>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
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
function chkFrm()
{
	/*if (document.form1.txtDBName.value == '')
	{
		alert("Please enter the DB name!");
		document.form1.txtDBName.focus();
		return false;
	}*/
}
//-->
</SCRIPT>
<TITLE>CRM - CRM Setup</TITLE>
</HEAD>
<BODY BGCOLOR="#AECAE6" TOPMARGIN="0" LEFTMARGIN="0">

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


  <table bordercolor="#111111" border="0" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
  <TR>
	  <TD colspan=2 align=right><a href="../Default.asp"><font size=2 face=arial>Back to 
        Admin Page</font></a></TD>
  </TR>
  
  <TR>
	<th align=center colspan=2 ><font color=black size=4><u>You must open a new browser, for your changes 
      to take effect</u></th>
  </TR>
  </tr><td colspan=2><br></td></tr>
  <TR>
	<TD colspan=2 align=left><b>Fill in the fields below to configure the CRM module</b></TD>
	  
  </TR>
  </Table>
  
  
  		<%
		strAppUserVar = session("strAppUserVar")
		'Response.Write strAppUserVar
		'Response.End
		If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"ASETUP") <= 0 Then
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
		
  <P> 
    <FORM action="SaveSetup.asp" method=post id=form1 name=form1 onsubmit="return chkFrm();">
  
    <table bordercolor="#111111"  align=center border="1" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
      <TR> 
        
      <TD>Company ID</font></TD>
        <TD> 
          <INPUT id=txtCompID Size=3 name=txtCompID Value="<%Response.Write(Trim(Session("CompanyID")))%>" >
          <!--onFocus="MM_showHideLayers('CompID','','show','dataFiles','','hide','SysFiles','','hide','SQLServer','','hide','CustCanEdit','','hide','NotifMail','','hide','ConfMail','','hide','ShowOTS','','hide','PassField','','hide','Logo','','hide')"-->
          
        </TD>
      </TR>
      <TR> 
        
      <TD>Data Files Path</font></TD>
        <TD> 
          <INPUT id=flDataFiles Size=60 name="flDataFiles" Value="<%=Ucase(Trim(Session("DataPath")))%>" >
          <!--onFocus="MM_showHideLayers('CompID','','hide','dataFiles','','show','SysFiles','','hide','SQLServer','','hide','CustCanEdit','','hide','NotifMail','','hide','ConfMail','','hide','ShowOTS','','hide','PassField','','hide','Logo','','hide')" >-->
          <INPUT id=button1 name=button1 type=button value="..." onClick="return lfGetDataPath(this.form)">
        </TD>
      </TR>
      <TR> 
        
      <TD>System Files Path</font></TD>
        <TD> 
          <INPUT id=flSystemPath Size=60 name=flSystemPath Value="<%=Ucase(Trim(Session("SystemPath")))%>" >
          <!--onFocus="MM_showHideLayers('CompID','','hide','dataFiles','','hide','SysFiles','','show','SQLServer','','hide','CustCanEdit','','hide','NotifMail','','hide','ConfMail','','hide','ShowOTS','','hide','PassField','','hide','Logo','','hide')">-->
          <INPUT id=button2 name=button2 type=button value="..." onClick="return lfGetSysPath(this.form)">
        </TD>
      </TR>
      <!--TR> 
        
      <TD><font color="#FFFFF0">Logo Image Path</font></TD>
        <TD>
          <input id=LogoPath size=40 name=LogoPath value="<%=Ucase(Trim(Session("LogoPath")))%>" >
         
          <input id=button2 name=button22 type=button value="..." onClick="return lfGetLogoPath(this.form)">
        </TD>
      </TR-->
      <TR> 
        
      <TD>DB server Type</font></TD>
        <TD> 
          <Select name="lstSerType">
			<option value="SQL" <% if session("DBType") = "SQL" then Response.Write("Selected") end if%>>SQL
			<option value="ORACLE" <% if session("DBType") = "ORACLE" then Response.Write("Selected") end if%>>Oracle
          </select>
          
          
        </TD>

      <TR> 
        
      <TD>DB Server Name</font></TD>
        <TD> 
          <input type="text" name="txtServerName" size="20" value="<%Response.Write((Session("SQLServer")))%>" >
          <!--onFocus="MM_showHideLayers('CompID','','hide','dataFiles','','hide','SysFiles','','hide','SQLServer','','show','CustCanEdit','','hide','NotifMail','','hide','ConfMail','','hide','ShowOTS','','hide','PassField','','hide','Logo','','hide')">-->
        </TD>
      </TR>
	   <!--TR> 
        
      <TD><font color="#FFFFF0">DB Name</font></TD>
        <TD> 
          <input type="text" name="txtDBName" size="20" value="<%Response.Write((Session("DBName")))%>" >
          
        </TD>
      </TR-->
      <TR> 
        
      <TD>DB Username </font> </TD>
        <TD> 
          <input type="text" name="txtsqluser" size="20" value="<%response.write(Session("SqlUserName"))%>">
        </TD>
      </TR>
      <TR> 
        
      <TD>DB Password </font> </TD>
        <TD> 
          <input type="password" name="txtsqlpas" size="20" value="<%response.write(session("SqlPassWord"))%>">
        </TD>
      </TR>
      <TR> 
        <TD align="right"> 
          <INPUT id=reset1 name=reset1 type=reset value=Reset>
        </TD>
        <TD> 
          <INPUT id=submit1 name=submit1 type=submit value=Submit>
        </TD>
      </TR>
    </TABLE>
    <Table width="80%">
    <TR>
		<TD>
          <div id="CompID" style="position:absolute; left:107px; width:760px; height:27px; z-index:1; visibility: hidden">Please 
            enter the company name that you want the CRM module work on.</div>
          <div id="dataFiles" style="position:absolute; left:107px; width:760px; height:27px; z-index:2; visibility: hidden">Please 
            select the path where your data files are located.</div>
          <div id="SysFiles" style="position:absolute; left:107px; width:760px; height:27px; z-index:3; visibility: hidden">Please 
            select the path where your system files are located.</div>
          <div id="SQLServer" style="position:absolute; left:107px; width:760px; height:27px; z-index:4; visibility: hidden">Please 
            enter the name of your SQL server.</div>
          <div id="CustCanEdit" style="position:absolute; left:107px; width:760px; height:27px; z-index:5; visibility: hidden">By 
            selecting yes you will allow your customer to edit his profile.</div>
          <div id="NotifMail" style="position:absolute; left:107px; width:760px; height:27px; z-index:6; visibility: hidden">Please 
            enter the email address, where you will receive system notifications.</div>
          <div id="ConfMail" style="position:absolute; left:107px; width:760px; height:27px; z-index:7; visibility: hidden">Please 
            select one of the fields as defined in the customer screen, where 
            you will receive confirmation notifications.(You must specify the 
            exact email address in the customer screen).</div>
          <div id="ShowOTS" style="position:absolute; left:107px; width:760px; height:27px; z-index:8; visibility: hidden">Please 
            select weather you want to display Immediate, O.T.S., None or both.</div>
          <div id="PassField" style="position:absolute; left:107px; width:760px; height:27px; z-index:9; visibility: hidden">Please 
            select one of the fields as defined in the customer screen, to enter 
            the customer password. (You must specify the password in the customer 
            screen)</div>
          <div id="Logo" style="position:absolute; left:107px; width:760px; height:27px; z-index:10; visibility: hidden">Please 
            select the path where your Logo file is located, it should be 93 X 
            93 pixels transparent with GIF extension.</div>
        </TD>
    </TR>
    </Table>
  <p>&nbsp;</p></FORM>
</BODY>
</HTML>
