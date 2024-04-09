<%@ Language=VBScript %>
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>

<%
Response.Buffer=true
If Trim(Session("ID")) = "" and Trim(Session("rep"))= "" Then
'	Response.redirect "../login.asp"%>
<script language="javascript">
	parent.location.href = "../login.asp"
</script>
<%End if
session("Curr_disc")=0
'NEK 1/6/2003[Start] Returns the Recordset of a specific Code in Codes.dbf
function getCodesFromFile(CodeName)
	Set objFileSystem = Nothing
	Dim objFile
	Set objFile = Server.CreateObject("Scripting.FileSystemObject")
	strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))
	If Right(strAppPath,1) = "\" Then
		strFilePath = "admin\crmsetup\setup\setup.txt"
	Else
		strFilePath = "\admin\crmsetup\setup\setup.txt"
										
	End If
	Set objTxtFile = objFile.OpenTextFile(strAppPath & strFilePath,1)
	Dim strLine

	strFile = objTxtFile.ReadAll
	
	Dim strArSetups
	strArSetups = Split(strFile," AND ")

	'Declare Vartiables To Hold the temporary key and values
	Dim strKey
	Dim strValue
	For intLoop = 0 To UBound(strArSetups)
	strArKeyValue = Split(strArSetups(intLoop) , "=" )
		if Trim(strArKeyValue(0)) = Trim(CodeName) then
			getCodesFromFile = strArKeyValue(1)
		end if 
	Next

	
	
end function
'NEK 1/6/2003[End]

function getCodes(CodeName,CodeNo) 
	set Codesconn = server.CreateObject("adodb.connection") 
	Codesconn.Open Application("DataConnectionString")
	'Response.Write "CodeNo== " & CodeNo
	if Trim(CodeNo) = "" then
		strsql = "select Cdiscrep , Ccode_no from Codes where cdefcode+crltfield+cfld_name = 'NN" & CodeName & "'"
	else
		strsql = "select Cdiscrep , Ccode_no from Codes where cdefcode+ccode_no+crltfield+cfld_name = 'N" & CodeNo & Space(6 - len(CodeNo)) & "N" & CodeName & "'"
	end if 
	set rsCodes = server.CreateObject("adodb.recordset")
	'Response.Write strsql
	rsCodes.Open strsql , CodesConn,2, 4
	set getCodes = rsCodes
end function 
'WAL_ function to get def code
function getDefCode(CodeName) 
dim rsDef
set rsDef = server.CreateObject ("ADODB.RECORDSET")
rsDef.Open "Select Ccode_no from Codes where cdefcode+crltfield+cfld_name like 'DN" & CodeName & "'", Conn
if not rsDef.EOF then
	getDefCode = trim(rsDef("ccode_no"))
else
	getDefCode = ""
end if
end function

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	'IF Trim(Session("customerid")) = ""  Then
	'	Response.Redirect("../repcust.asp")
	'END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF
IF Trim(Session("customerid")) <> ""  Then
	if not Session("rsCust").eof  then
		if Session("rsCust").fields("Status")="P" then
			Response.Redirect("../Common/Msgs.asp")
		end if
	end if
End if
 %>
<html>
<head>
<Title>CRM - Check Order Status</Title>
<LINK rel="stylesheet" type="text/css" href="../images/<%=Session("Theme")%>/Order.css">
</head>

<body bgcolor="#aecae6" topmargin="0" leftmargin="0">
<%if trim(UCase(Session("DateFormat")))="" then
DateFormat()
end if
'Response.Write "<font color=white size=20>Date Format :"&Session("DateFormat")&"</font>"
%>

<!--#include file="../common/checkDateFormat.asp"-->
<script LANGUAGE="javascript" src="checkForm.js">
</script>
<script language="Javascript">
function dateValidate(formId,len)
{
//		alert(formId);
	if(formId=="text2")
	{
		
		if((len==2)||(len==5))
		{
			form1.text2.value = form1.text2.value+"/";
		}
	}
	if(formId=="text3")
	{
		if((len==2)||(len==5))
		{
	
			form1.text3.value = form1.text3.value+"/";
		}
	}
}
</script>
<SCRIPT LANGUAGE = JScript>
function FormCheck()
{
// check to see if the Number entered is a valid number.
	var checkOK = " ";
	var checkStr = document.form1.text1.value;
	var allValid = true;
	var allSpace = "";
	for (i = 0;  i < checkStr.length;  i++)
	{
		ch = checkStr.charAt(i);
		for (j = 0;  j < checkOK.length;  j++)
			if (ch == checkOK.charAt(j))
			break;
		if (j == checkOK.length)
		{
		allValid = false;
		break;
		}
		if (ch != ",")
		allSpace += ch;
	}
	if (!allValid)
	{
		var checkOK = "0123456789";
		var checkStr = document.form1.text1.value;
		var allValid2 = true;
		var allNum = "";
		for (i = 0;  i < checkStr.length;  i++)
		{
			ch = checkStr.charAt(i);
			for (j = 0;  j < checkOK.length;  j++)
				if (ch == checkOK.charAt(j))
				break;
			if (j == checkOK.length)
			{
			allValid2 = false;
			break;
			}
			if (ch != ",")
			allNum += ch;
		}
		if (!allValid2)
		{
			alert("The Order must be a number!");
			document.form1.text1.focus();
			return (false);
		}
	}
	
//check to see if the first date is spaces .. 
	var checkOK = " ";
	var checkStr = document.form1.text2.value;
	var allValid = true;
	var allSpace = "";
	for (i = 0;  i < checkStr.length;  i++)
	{
		ch = checkStr.charAt(i);
		for (j = 0;  j < checkOK.length;  j++)
			if (ch == checkOK.charAt(j))
			break;
		if (j == checkOK.length)
		{
		allValid = false;
		break;
		}
		if (ch != ",")
		allSpace += ch;
	}
	if (!allValid)
	{
		//check if the date is date .. 
		var t;
		if (document.form1.text2.value!="")
		{
			t = Date.parse(document.form1.text2.value)
			if (!t) 
			{
				alert("Please enter valid date or leave blank!");
				document.form1.text2.focus();
				return false;
			}
		}
	}
	
//check to see if the second date is spaces .. 
	var checkOK = " ";
	var checkStr = document.form1.text3.value;
	var allValid = true;
	var allSpace = "";
	for (i = 0;  i < checkStr.length;  i++)
	{
		ch = checkStr.charAt(i);
		for (j = 0;  j < checkOK.length;  j++)
			if (ch == checkOK.charAt(j))
				break;
		if (j == checkOK.length)
		{
			allValid = false;
			break;
		}
		if (ch != ",")
		allSpace += ch;
	}
	if (!allValid)
	{
		var b;
		if (document.form1.text3.value!="")
		{
			b = Date.parse(document.form1.text3.value)
			if (!b)
			{	
				alert("Please enter valid date or leave blank!");
				document.form1.text3.focus();
				return false;
			}
		}
	}
		
	return true;
}

</SCRIPT>
<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<P><BR><BR><Br></P>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><br></p>

<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
	<TD colspan=13>
	<%IF Trim(Session("customerid")) <> ""  Then%>
		<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=session("rscust").fields("btname").value%></P>
	<%else%>
		&nbsp
	<%end if%>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
</TR>
</table>
 <%
End IF%>
<div align="center">
<center>
	<Table width=95% border=1 height="50">
		<TR>
		<TD class="title">Get Order</TD>
		</TR>
	</Table>
</center>
</div>
<%
IF compWork = "Y" Then

set conn=server.CreateObject("ADODB.connection")
conn.Open Application("DataConnectionString")

'RecordSets
set rsOrderLength = server.CreateObject("ADODB.RecordSet")

strSQL = "SELECT Nfld_wdth FROM Sequence WHERE Cseq_type+cseq_group like 'ORDER%'"
rsOrderLength.Open strSQL, conn
%>
<br>
<FORM action="orderList.asp" method=post id=form1 name=form1 onsubmit="return FormCheck(form1)">
<%
If Trim(Session("Rep")) = "" Then
	strAppUserVar = Session("ID")
Else
	strAppUserVar = Session("Rep")
End If
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"EDITORD") <= 0 Then
%>
	<Table border=0 width=95% align=center>
		<tr>
		<td class="Title"><font color="Yellow">
			You're not authorized to view this page</font>
		</td>
		</tr>
	</Table>
<%Response.End 
End If
%>

<Table border=0 width=95% align=center>
<tr>
	<td>
	  <table border="0" width="95%" cellspacing="0" cellpadding="0" align=center>
			<tr>
			  <td width="100%"><strong>Enter your searching criteria to select the order(s) you want to modify or just click the search button for all:</strong></td>
			</tr>
	  </table>
	</tD>
</tr>

 <TR>
 <TD>
    <div align="center">
      <center>
    <table border="1" bordercolor="#111111" width=95% cellspacing="0" style="border-collapse: collapse" cellpadding="0">
        <tr>
            <td class="dark_cell"><strong>Order#</strong></td>
			<td >
					<table border=0 width="100%" cellspacing=0 cellpadding=0>
						<tr>
							<td class="light_cell" width="10%" nowrap >&nbsp;<INPUT id=text1 name="txtOrderNoStart" maxlength="6" size="10">&nbsp;&nbsp;</td>
							<td class="light_cell" width="5%" nowrap ><strong>To</strong></td>
							<td width="80%" class="light_cell">&nbsp;<INPUT id=text6 name="txtOrderNoEnd" maxlength="6" size="10"></td>
						</tr>
						</table>
            </td>
		</tr>
        <tr>
			<td class="dark_cell"><strong>Entered date</strong></td>
            <td >
				<table border=0 width=100% cellspacing=0 cellpadding=0>
					<tr>
						<Td class="light_cell" width="10%" nowrap>
						<%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
							&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')">&nbsp;&nbsp; 
						<%else%>
            				&nbsp;<INPUT id=text2 name="txtBeginDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text2.id,form1.text2.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')">&nbsp;&nbsp; 
						<%end if%>
					</td>
					<td class="light_cell" width="5%" nowrap><strong>To</strong> </td>
					<td width="80%" class="light_cell">
							<%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
								&nbsp;<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')"> 
							<%else%>
								&nbsp;<INPUT id=text3 name="txtEndDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text3.id,form1.text3.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')"> 
							<%end if%>	
		            </td>
				</tr>
		   	  </table>
			</td>	    
        </tr>
         <tr>
			<td class="dark_cell" ><strong>Start Ship Date</strong></td>
			<td >
					<table border=0 width="100%" cellspacing=0 cellpadding=0>
							<tr>
								<Td class="light_cell" width="10%" nowrap>
									<%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
										&nbsp;<INPUT id=text4 name="txtStartShipDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text4.id,form1.text4.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')">&nbsp;&nbsp; 
									<%else%>
            							&nbsp;<INPUT id=text4 name="txtStartShipDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text4.id,form1.text4.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')">&nbsp;&nbsp;
									<%end if%>
							    </td>
							    <td class="light_cell" width="5%" nowrap ><strong>To</strong></td>
							    <td width="80%" class="light_cell">
									<%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
										&nbsp;<INPUT id=text5 name="txtEndShipDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text5.id,form1.text5.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')"> 
									<%else%>
										&nbsp;<INPUT id=text5 name="txtEndShipDate" maxlength=10 size=10 value="" onKeyDown="dateValidate(form1.text5.id,form1.text5.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')"> 
									<%end if%></td>
							</tr>
					</table>
            </td>
         </tr>  
		<tr>
			<td class="dark_cell"><strong>Expected Ship Date</strong></td>
            <td class="light_cell" >
				<%if (trim(UCase(Session("DateFormat")))=trim(UCase("british"))) then%>
				    &nbsp;<INPUT id=text7 name="txtCompleteDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text7.id,form1.text7.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'3')" onChange="DateFormat(this,this.value,event,true,'3')"> 
				<%else%>
					&nbsp;<INPUT id=text7 name="txtCompleteDate" maxlength=10 size=10  value="" onKeyDown="dateValidate(form1.text7.id,form1.text7.value.length);" onKeyUp="DateFormat(this,this.value,event,false,'1')" onChange="DateFormat(this,this.value,event,true,'1')"> 
				<%end if%>
            </td>
        </tr>
        <%Seasons = Session("StyleColor")
        SeasonsArr=split(Seasons,",")
        %>
         
            		<%if ubound(SeasonsArr) > 0  then%>
            			<tr>
						<td class="dark_cell"><strong>Season</strong></td>
						<td class="light_cell" >
							&nbsp;<select name="selectSeason">
            			<option value="All">All</option>
            		<%for intSeasonLoop = lbound(SeasonsArr) to ubound(SeasonsArr)
            				set rsSeasons = getCodes("SEASON",SeasonsArr(intSeasonLoop))
            				if Trim(SeasonsArr(intSeasonLoop)) = Trim(rsSeasons.Fields("Ccode_no").Value) then %>
            					<option value="<%=Trim(rsSeasons.Fields("Ccode_no").Value)%>"><%=Trim(rsSeasons.Fields("Cdiscrep").Value)%></option>
            				<%end if 
					next%>
							</select>
						   </td>
						</tr>
            		<%else 
            			if Trim(UCase(Seasons))= "NONE" then%>
            				<tr>
							<td class="dark_cell"><strong>Season</strong></td>
							<td class="light_cell" >
								&nbsp;<select name="selectSeason">
            				<option value="All">All</option>
            				<%set rsSeasons = getCodes("SEASON","")
            				do while not rsSeasons.eof %>
            					<option value="<%=Trim(rsSeasons.Fields("Ccode_no").Value)%>"><%=Trim(rsSeasons.Fields("Cdiscrep").Value)%></option>
            				<%rsSeasons.movenext
            				loop%>
            				</select>
							</td>
							</tr>
            			<%else
            				set rsSeasons = getCodes("SEASON",Trim(UCase(Seasons)))%>
            					
            				<%if not rsSeasons.eof then %>
            					<input type=hidden name="selectSeason" value="<%=Trim(rsSeasons.Fields("Ccode_no").Value)%>">
            					<!--option value="<%=Trim(rsSeasons.Fields("Ccode_no").Value)%>"><%=Trim(rsSeasons.Fields("Cdiscrep").Value)%></option-->
	            			<%end if 
	            		end if 	
	            	end if 	%>
               
        <%Divisions = Session("CatDivision")
        DivisionsArr=split(Divisions,",")%>

            
				    <%if ubound(DivisionsArr)> 0 then%>
						<tr>
						<td class="dark_cell"><strong>Division</strong></td>
						<td class="light_cell" >	
						&nbsp;<select name="selectDivision" colspan=2>
            			<option value="All">All</option>
            			<%for intDivLoop = lbound(DivisionsArr) to ubound(DivisionsArr)
            				set rsDivisions = getCodes("CDIVISION",DivisionsArr(intDivLoop))
            			if Trim(DivisionsArr(intDivLoop)) = Trim(rsDivisions.Fields("Ccode_no").Value) then %>
            				<option value="<%=Trim(rsDivisions.Fields("Ccode_no").Value)%>"><%=Trim(rsDivisions.Fields("CDiscrep").Value)%></option>
            			<%end if 
            		next%>
            			</select>
					    </td>
					</tr>
	           	<%	else 
    		           if Trim(UCase(Divisions))= "NONE" then%>
    						<tr>
    						<td class="dark_cell"><strong>Division</strong></td>
							<td class="light_cell" >	
							&nbsp;<select name="selectDivision" colspan=2>
            				<option value="All">All</option>
            				<%set rsDivisions = getCodes("CDIVISION","")
            				do while not rsDivisions.eof %>
            					<option value="<%=Trim(rsDivisions.Fields("Ccode_no").Value)%>"><%=Trim(rsDivisions.Fields("Cdiscrep").Value)%></option>
            				<%rsDivisions.movenext
            				loop%>
            			</select>
					    </td>
					    </tr>
					   <%else	
            			  set rsDivisions = getCodes("CDIVISION",Trim(UCase(Divisions)))
            			  if not rsDivisions.eof then %>
            					<input type=hidden name="selectDivision" value="<%=Trim(rsDivisions.Fields("Ccode_no").Value)%>">
            					<!--option value="<%=Trim(rsDivisions.Fields("Ccode_no").Value)%>"><%=Trim(rsDivisions.Fields("Cdiscrep").Value)%></option-->
            			<%end if 
            		end if 
            	end if 	 %>
         <tr>
			<td class="dark_cell"><strong>All Customers</strong></td>
        	<td class="light_cell">
				<input type=checkbox name=chkCust <%if trim(custid) = "" then%>checked<%end if%>>
			</td>
		</tr>
        <tr>
			<td colspan="2" align="right" class="dark_cell">
				<INPUT type="submit" value="Search" id=submit1 name=submit1>
				<INPUT type="reset" value="Reset" id=reset1 name=reset1> 
            
			</td>
        </tr>
    </table>
      </center>
    </div>
</TD></TR></Table>
</FORM>

<%End IF%>
</body>
<%
conn.Close 
set conn=nothing
set rsOrderLength=nothing
Session("BeginDate") = ""
Session("EndDate") = ""
Session("Status") = ""
Session("txtOrderNoStart") = ""
Session("txtOrderNoEnd") = ""
Session("Season") = ""
Session("Division") = ""
Session("txtStartShipDate") = ""
Session("txtEndShipDate") = ""
Session("txtCompleteDate") = ""

%>