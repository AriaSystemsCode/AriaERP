<%
'WAL_05/17/2004 add code tp read values in setup file[start]
'get values seved in the file 
Dim objTxtFile
Set objTxtFile = Server.CreateObject("Scripting.FileSystemObject")
Dim strAppPath
Dim strFilePath

strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))

If Right(strAppPath,1) = "\" Then
	strFilePath = "admin\crmsetup\setup\setup.txt"
Else
	strFilePath = "\admin\crmsetup\setup\setup.txt"
End If
Set objTxtFile = objTxtFile.OpenTextFile(strAppPath & strFilePath,1)

strFile = objTxtFile.ReadAll
Dim strArSetups
	strArSetups = Split(strFile," AND ", -1 , 1)

	'Declare Vartiables To Hold the temporary key and values
	Dim strKey
	Dim strValue
	For intLoop = 0 To UBound(strArSetups)
		strArKeyValue = Split(strArSetups(intLoop) , "=" , -1 , 1)
		Application(strArKeyValue(0)) = strArKeyValue(1)
		Session(strArKeyValue(0)) = strArKeyValue(1)
	Next
	Application("DataConnectionString") = "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("DataPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"
	Application("SystemConnectionString") = "provider=MSDATASHAPE;Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("SystemPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"
objTxtFile.Close
Set objTxtFile = Nothing
'WAL_05/17/2004 add code tp read values in setup file[start]
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">

<title>CRM SETUP - Customer Groups' Level and Packs ID Assignment</title>
<SCRIPT LANGUAGE=javascript>
<!--
function do_Add()
{
	document.mainfrm.action = "group.asp?action=A"
	document.mainfrm.submit();
}

function do_AddCustomer()
{
alert(document.mainfrm.rdoCustGrp.checked)
	var total = 0;
//HDM incorrect code the radio is not validated at all, re-write this section
	
	if (document.mainfrm.rdoCustGrp.checked)
	{
		total=1;
	}
	else
	{
		total=0;
	}
	//var max = document.mainfrm.rdoCustGrp.length;
	/*for (var idx = 0; idx < max; idx++)
	{
		//if (eval("document.mainfrm.rdoCustGrp[" + idx + "].checked") == true )
		if (document.mainfrm.rdoCustGrp.checked)
			{
			total += 1;
			}
	}*/
	if (total==0 )
		{
			alert("Please,select one of the groups to be able to add customers");
			return false;
		}
	else
		{
			document.mainfrm.action = "getcust.asp"
			document.mainfrm.submit();
			return true;
		}
}

function do_Edit()
{
	var total = 0;
	var max = document.mainfrm.rdoCustGrp.length;
	for (var idx = 0; idx < max; idx++)
		if (eval("document.mainfrm.rdoCustGrp[" + idx + "].checked") == true )
			{
			total += 1;
			}
	if (total==0 )
		{
			alert("Please,select one of the groups to edit!");
		}
	else
		{
				document.mainfrm.action = "group.asp?action=E"
				document.mainfrm.submit();
		}

}
function do_Delete(frm)
{
	var total = 0;
	var max = document.mainfrm.rdoCustGrp.length;
	for (var idx = 0; idx < max; idx++)
		if (eval("document.mainfrm.rdoCustGrp[" + idx + "].checked") == true )
			{
			total += 1;
			}
	if (total==0 )
		{
			alert("Please,select one of the groups to delete!");
		}
	else
		{
				document.mainfrm.action = "groupresponse.asp?action=D"
				document.mainfrm.submit();
		}
		
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
<Font face=arial>
<table border="0" cellpadding="0" cellspacing="0" width="94%" align="center">
  <tr> 
    <td><img src="../../../images/<%=Session("THEME")%>/spacer.gif" width="80" height="1" border="0"></td>
    <td><img src="../../../images/<%=Session("THEME")%>/spacer.gif" width="60" height="1" border="0"></td>
    <td><img src="../../../images/<%=Session("THEME")%>/spacer.gif" width="10" height="1" border="0"></td>
    <td><img src="../../../images/<%=Session("THEME")%>/spacer.gif" width="47" height="1" border="0"></td>
    <td colspan="2"><img src="../../../images/<%=Session("THEME")%>/spacer.gif" width="6" height="1" border="0"></td>
    <td><img src="../../../images/<%=Session("THEME")%>/spacer.gif" width="125" height="1" border="0"></td>
    <td><img src="../../../images/<%=Session("THEME")%>/spacer.gif" width="422" height="1" border="0"></td>
    <td><img src="../../../images/<%=Session("THEME")%>/spacer.gif" width="1" height="1" border="0"></td>
  </tr>
  <tr> 
    <td colspan="4" height="8"><img name="Banner_r1_c1" src="../../../images/<%=Session("THEME")%>/Banner_r1_c1.gif" width="197" height="8" border="0"></td>
    <td width="3"></td>
    <td width="100%" valign="top"><img name="Banner_r1_c5" src="../../../images/<%=Session("THEME")%>/Banner_r1_c5.gif" width="100%" height="8" border="0"></td>
    <td colspan="2"><img name="Banner_r1_c6" src="../../../images/<%=Session("THEME")%>/Banner_r1_c6.gif" width="547" height="8" border="0"></td>
    <td><img src="../../../images/<%=Session("THEME")%>/spacer.gif" width="1" height="8" border="0"></td>
  </tr>
  <tr> 
    <td rowspan="2"><img name="Banner_r2_c1" src="../../../images/<%=Session("THEME")%>/Banner_r2_c1.gif" width="80" height="59" border="0"></td>
    <td rowspan="2"><img name="Banner_r2_c2" src="../../../images/<%=Session("THEME")%>/Banner_r2_c2.gif" width="60" height="59" border="0"></td>
    <td rowspan="2"><img name="Banner_r2_c3" src="../../../images/<%=Session("THEME")%>/Banner_r2_c3.gif" width="10" height="59" border="0"></td>
    <td rowspan="2"><img name="Banner_r2_c4" src="../../../images/<%=Session("THEME")%>/Banner_r2_c4.gif" width="47" height="59" border="0"></td>
    <td rowspan="2" colspan="2"><img name="Banner_r2_c5" src="../../../images/<%=Session("THEME")%>/Banner_r2_c5.gif" width="100%" height="59" border="0"></td>
    <td rowspan="2"><img name="Banner_r2_c6" src="../../../images/<%=Session("THEME")%>/Banner_r2_c6.gif" width="125" height="59" border="0"></td>
    <td><img name="Banner_r2_c7" src="../../../images/<%=Session("THEME")%>/Banner_r2_c7.gif" width="422" height="30" border="0"></td>
    <td><img src="../../../images/<%=Session("THEME")%>/spacer.gif" width="1" height="30" border="0"></td>
  </tr>
  <tr> 
    <td><img name="Banner_r3_c7" src="../../../images/<%=Session("THEME")%>/Banner_r3_c7.gif" width="422" height="29" border="0"></td>
    <td><img src="../../../images/<%=Session("THEME")%>/spacer.gif" width="1" height="29" border="0"></td>
  </tr>
  <tr> 
    <td colspan="4"><img name="Banner_r4_c1" src="../../../images/<%=Session("THEME")%>/Banner_r4_c1.gif" width="197" height="8" border="0"></td>
    <td colspan="2"><img name="Banner_r4_c5" src="../../../images/<%=Session("THEME")%>/Banner_r4_c5.gif" width="100%" height="8" border="0"></td>
    <td colspan="2"><img name="Banner_r4_c6" src="../../../images/<%=Session("THEME")%>/Banner_r4_c6.gif" width="547" height="8" border="0"></td>
    <td><img src="../../../images/<%=Session("THEME")%>/spacer.gif" width="1" height="8" border="0"></td>
  </tr>
</table>
<p>
<BR>
</p>
<p>Customer Groups' Level and Packs ID Assignment.</p>
    <%
    Dim SqlCon 
    Set SqlCon = server.CreateObject("ADODB.Connection")
    constr = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=CRM;Data Source=" & Trim(Session("SQLServer")) & ";UID="& Trim(Session("SqlUserName"))& ";PWD="&Trim(Session("SqlPassWord"))

    SqlCon.Open constr'Application("SqlServer")
    
    Dim RSCustGrp
    Set RSCustGrp = Server.CreateObject("ADODB.Recordset")
    strsql = "select * from custgroup order by GROUPID"
    RSCustGrp.Open strsql,sqlcon
    
    dim strPacks
    strPacks = ""
    Do While Not RSCustGrp.EOF 
			IF Not(RSCustGrp.EOF and RSCustGrp.BOF) Then
				IF strPacks = "" Then
					strPacks = "'" & RSCustGrp.Fields("PackID").Value & "'"
				Else
					strPacks = strPacks & ",'" & RSCustGrp.Fields("PackID").Value & "'"
				End IF
				
			End IF
			RSCustGrp.MoveNext 
		Loop
		
		Set FoxCon = Server.CreateObject("ADODB.Connection")
		FoxCon.Open Application("DataConnectionString")
		
		Set RSpacks = Server.CreateObject("ADODB.RecordSet")
		If Trim(strPacks) = "" Then
			Response.Write("No Styles found")
			Response.End 
		End If
		strsql = "select * from spck_hdr where account = '*****' and type='P' and pack_id in (" & strPacks & ")"
		'Response.Write(strsql)
		'Response.End 
		RSpacks.Open strsql,FoxCon
    
    IF Not(RSpacks.EOF and RSpacks.BOF) Then
			RSCustGrp.MoveFirst 
			RSpacks.MoveFirst 
    %>
		<form method="POST" action="getcust.asp" name="mainfrm">
		  <table border="1" width="95%" align=center>
				<tr>
				  <td width="21" bgcolor="#4269B8" align="center">&nbsp;</td>
				  <td width="350" bgcolor="#4269B8"><font color="#000080" face="Arial" size="2"><b>Group Description</b></font></td>
				  <td width="226" bgcolor="#4269B8"><font color="#000080" face="Arial" size="2"><b>Pack ID</b></font></td>
				  <td width="226" bgcolor="#4269B8"><font color="#000080" face="Arial" size="2"><b>Pack Description</b></font></td>
				</tr>

		<%	Do While not RSpacks.EOF
		
					RSCustGrp.Filter = "PackID='" & RSpacks.Fields("Pack_ID").Value & "'"
    %>
				<tr>
				  <td width="21" align="center" bgcolor="ivory"><input type="radio" value="<%=RSCustGrp.Fields("GroupID").Value%>" name="rdoCustGrp" id="rdoCustGrp"></td>
				  <td width="350" bgcolor="ivory"><Font size=2>&nbsp;<%=RSCustGrp.Fields("Description").Value%></font></td>
				  <td width="226" bgcolor="ivory"><Font size=2>&nbsp;<%=RSCustGrp.Fields("PackID").Value%></font></td>
					<td width="226" bgcolor="ivory"><Font size=2>&nbsp;<%=RSpacks.Fields("desc").Value%></font></td>				  
				</tr>
    <%
				RSpacks.MoveNext 
			Loop
			RSCustGrp.Filter = ""
			%>
				<TR><TD Colspan=4 align=center>
				<p>	<input type="submit" value="Add" name="B1" onclick="do_Add()">
						<input type="submit" value="Edit" name="B2" onclick="do_Edit()">
						<input type="submit" value="Delete" name="B3" onclick="do_Delete(this.form)">
						<input type="submit" value="Assign Customer to Group" name="B4" onclick="return do_AddCustomer()">
						</p>
				</TR>
			</table>
		</form>
    <%Else%>
		<form method="POST" action="default.asp" Action="group.asp" name="mainfrm">
		  <table border="1" width="750">
				<tr>
					<TD align=center> There are no group found.</TD>
				</TR>
				<TR><TD Colspan=4 align=center>
					<p><input type="submit" value="Add" name="B1" onclick="do_Add()"></p>
				</TR>
			</table>
		</form>
				
    <%End IF%>
</font>
</Body>
</HTML>