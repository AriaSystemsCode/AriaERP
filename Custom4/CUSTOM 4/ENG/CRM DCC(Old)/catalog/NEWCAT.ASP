<%
Response.Buffer=True
Response.Expires=-1

If Trim(Session("ID")) = "" And Session("rep")= "" Then
'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../login.asp"
	</script>	
<%End if

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
if not Session("rsCust").eof  then
	if Session("rsCust").fields("Status")="P" then
		Response.Redirect("../Common/Msgs.asp")
	end if
end if
Session("StoreValue") = ""
Session("Type") = "R"
%>
<Html>
<Head>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/Catalog.css" TYPE="text/css">


<%

'Response.Write("<font color='red' size=50>Catalog Val" &session("showcatalogval")&"</font>")
arrFound= split(session("showcatalogval"),",")
redim arrGroups(ubound(arrFound),2)

For intcount =0 to ubound(arrFound)
	arrGroups(intcount,0) = arrFound(intcount)
Next

const NumOfGrp = 3

' ARD - This part is to find which group has Styles and which is not and prevent those are empty from showing [Start]
Dim FoxCon
Set FoxCon = server.CreateObject("ADODB.Connection")
FoxCon.Open Application("DataConnectionString")

Dim strSeason, strDivision
strSeason = Trim(Session("StyleColor"))
strDivision = Trim(Session("CatDivision"))


'If Trim(UCase(strSeason)) = "NONE" Then
'	strSeasonCondition = ""
'Else
'	strSeasonCondition = " Season='" & strSeason & "'"
'End If
'If Trim(UCase(strSeason)) = "NONE" or len(Trim(UCase(strSeason)))>4Then
If Trim(UCase(strSeason)) = "NONE" or instr(Trim(UCase(strSeason)),",") <>0 Then
	'Multiple Seasons WMA 5/24/2004 START
	'strSeasonCondition = " Season in('" & Replace(Trim(Session("StyleColor")),",","','") & "') "	
	strSeasConArr  = split(Session("StyleColor"),",")
	intSeasConCount = int(UBound(strSeasConArr)/24)
	strSeasonCondition = strSeasonCondition & " AND ("
	for i = 0 to  intSeasConCount 
		for j = i*24 to (i*24+24) -1
			if j = UBound(strSeasConArr) then
				strSQLall = strSQLall + "'" &  strSeasConArr(j) & "'"
			elseif j < UBound(strSeasConArr) then
				if ((j+1) mod 24 = 0 and j<>0) then
					strSQLall = strSQLall + "'" &  strSeasConArr(j) & "'"			
				else
					strSQLall = strSQLall + "'" &  strSeasConArr(j) & "',"			
				end if	
			end if		
		next		
		if i =0 then 'first item
			strSeasonCondition = strSeasonCondition & " Season in("& strSQLall &") " 
		else
			strSeasonCondition = strSeasonCondition & " or Season in(" & strSQLall & ") " 
		end if
		strSQLall = ""
	next
	strSeasonCondition = strSeasonCondition & " )"
    'Multiple Seasons WMA 5/24/2004 END		
    
Else
	strSeasonCondition = " Season='" & strSeason & "'"
End If

If Trim(UCase(strDivision)) = "NONE" Then
	strDivisionCondition = ""
Else
	If Trim(strSeasonCondition) ="" Then
		strDivisionCondition = " cdivision='" & strDivision & "'"
	Else
		strDivisionCondition = " And cdivision='" & strDivision & "'"
	End If
End If
Dim strSeasDivWhere

strSeasDivWhere = strSeasonCondition & strDivisionCondition
If trim(strSeasDivWhere) <> "" And Left(Trim(strSeasDivWhere),3) <> "AND" Then
	strSeasDivWhere = " AND " & strSeasDivWhere
End If
'Response.Write(strSeasDivWhere)
Dim RSGroups
Set RSGroups = Server.CreateObject("ADODB.Recordset")
For intcount =0 to ubound(arrGroups,1)
	'HDM E302082,1 [Start] 12/23/2002 Add Order by style no.
	'strsql = "select * from style where style.Status+style.cstygroup='A" & Trim(arrGroups(intcount,0)) & "'" & strSeasDivWhere
	strsql = "select * from style where style.Status+style.cstygroup='A" & Trim(arrGroups(intcount,0)) & "'" & strSeasDivWhere & " order by style"
	'HDM E302082,1 [End]
'	Response.Write(strsql&"<BR>")
	'Response.End 
	RSGroups.Open strsql,FoxCon
'	Response.Write "<font size=5>RSGroups="&RSGroups.RecordCount&"</font>" 
	IF Not(RSGroups.EOF and RSGroups.BOF) Then
		if strGroupFound = "" Then
			strGroupFound = RSGroups.Fields("cstygroup").Value 
		Else
			strGroupFound = strGroupFound & "," & RSGroups.Fields("cstygroup").Value 
		End IF
	End IF
	RSGroups.Close 
Next
strvalidGroup = Split(strGroupFound,",")
'Response.Write "<font size=3>strvalidGroup==" & ubound(strvalidGroup)&"</font>"
'ARD - [End]
strTemp = ""
IF ubound(strvalidGroup)+1 = 0 Then	
'Response.Write "<font size=5>IN IF </font>"%>
	<SCRIPT LANGUAGE=javascript>
<!--

function hideloadingmsg() 

{
	document.all.loadingmsg.style.display = 'none';
    document.all.loadingmsg.style.visibility = 'hidden';

}

//-->

</SCRIPT>

	</head>
	<body onLoad="hideloadingmsg()">
<div name='loadingmsg' id='loadingmsg' align=center style='display:inline;visibility:visible'>
	<p align=center>
		<A align=center style="font-family=verdana;font-size=12px;font-weight=bold;"><font color=red>Please standby, ..Loading data from server.</font></A>
	</p>
</DIV>

	<%IF strFile = "cust" Then%>
	<SCRIPT LANGUAGE="JavaScript1.2"
	        SRC="../HM_Loader_Cust.js"
	        TYPE='text/javascript'>
	</SCRIPT>
	<p><BR><BR><BR></p>
	<%Else%>
	<SCRIPT LANGUAGE="JavaScript1.2"
	        SRC="../HM_Loader_Sales.js"
	        TYPE='text/javascript'>
	</SCRIPT>
	<p><br><br><BR></p>
	<%End if%>
	<Table width=95% align=center>
	<TR>
	<TD>
	<Font face=arial size=2>
	Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	</TR>
	</Table>
	</font>
	<Table width=95% align=center border=1>
	<TR>
		<TD class=title><%=session("CatalogField")%></TD>
	</TR>
	</Table>

	<%
	'wal_130674 correct check on privilage case multi user
	if Application("CustomerLoginUsing")= "User"  then
		strAppUserVar = session("userID")
	ElseIf Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
	If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"CATALOG") <= 0 Then
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
	<Table width=95% align=center >
	<TR>
		<TD align=center><b>No records found in selected group(s).</b></TD>
	</TR>
	</Table>
	</body>
	</html>
<%else
intresult = int(100/(ubound(strvalidGroup)+1))
'END IF 

'intresult = int(100/(ubound(strvalidGroup)+1))
' ARD - This part is to find which group has Styles and which is not and prevent those are empty from showing [End]


'Response.Write("<br><br><br><font size=3>"&strSQL&"</font><br>")

 Const NumPerPage = 3
  Dim CurPage
  Dim intUpLimit
  Dim intLowLimit
  
  IF Request.QueryString("CurPage") = "" Then
		'ARD
		CurPage = 1
  Else
		CurPage = Request.QueryString("CurPage")
  End IF
  
  intLowLimit = (CurPage * NumPerPage) - NumPerPage
  intUpLimit  = intLowLimit + NumPerPage-1
  IF intUpLimit > ubound(strvalidGroup)  Then
		intUpLimit = ubound(strvalidGroup) 
  End IF
  %>
<SCRIPT LANGUAGE=javascript>
<!--
		var Grpnames = new Array();
		var intCurpage = new Array();
			<%For Intcont=0 to ubound(strvalidGroup)%>
				Grpnames[<%=Intcont%>] = '<%response.write("GRP"&Trim(strvalidGroup(Intcont)))%>'
				intCurpage[<%=Intcont%>] = 1
			<%Next%>
//-->
</SCRIPT>
<!--Create a new arrya to hold the groups names-->
<SCRIPT LANGUAGE=javascript>
var NameGRPDESC = new Array();
</SCRIPT>
<%
  Set RSstyles = Server.CreateObject("ADODB.Recordset")

For intcount = intLowLimit to intUpLimit
		'HDM E302082,1 [Start] 12/23/2002 Add Order by style no.
		'strsql = "select DISTINCT style.cstymajor,Style.desc, codes.cdiscrep from style, codes where style.cstygroup='" & strvalidGroup(intcount) & "' and codes.ccode_no='" & strvalidGroup(intcount) & "'" & strSeasDivWhere &" group by cstymajor"
		strsql = "select DISTINCT style.cstymajor,Style.desc, codes.cdiscrep from style, codes where style.cstygroup='" & strvalidGroup(intcount) & "' and codes.ccode_no='" & strvalidGroup(intcount) & "' And codes.cfld_name='CSTYGROUP' " & strSeasDivWhere &" group by cstymajor order by style"
		'HDM E302082,1 [End]
		'Response.Write(strsql)
		'Response.End 
		RSstyles.Open strsql,FoxCon
		%>
		<SCRIPT LANGUAGE=javascript>
		<!--
			var <%response.write("GRP"&Trim(strvalidGroup(intcount)))%> = new Array();
			var <%Response.Write("GRP"&Trim(strvalidGroup(intcount))& "DESC")%> = new Array();

			<%intcounter = 0%>
		  <%Response.Write("NameGRPDESC")%>[<%=intcount%>] = '<%=Trim(RSstyles.fields("cdiscrep").value)%>'

			<%Do while not RSstyles.EOF %>
				<%response.write("GRP"&Trim(strvalidGroup(intcount)))%>[<%=intcounter%>] = '<%=Trim(RSstyles.fields("cstymajor").value)%>'
				<%Response.Write("GRP"&Trim(strvalidGroup(intcount))& "DESC")%>[<%=intcounter%>] = '<%=Trim(RSstyles.fields("Desc").value)%>'
				
				<%intcounter = intcounter + 1%>
				<%RSstyles.MoveNext%>
			<%Loop%>
		//-->
		</SCRIPT>
		
		<%
		RSstyles.Close 
	Next

%>

<SCRIPT LANGUAGE=javascript>
<!--
function do_load()
{
	//for(intcont=0 ; intcont < Grpnames.length  ; intcont++)
	for(intcont=<%=intLowLimit%> ; intcont < <%=intUpLimit + 1%> ; intcont++)
	{
		strtry = eval('Grpnames[' + intcont + ']')
		if(eval(strtry).length > 5)
			{
				intarcount = 5
				objnext = eval('document.frm01.next' + intcont)
				objnext.src = '../images/<%=Session("Theme")%>/next.gif'
			}
		else
			intarcount = eval(strtry).length
		for(intiner=0 ;intiner< intarcount  ; intiner++)
		{
			var strtemp = eval('Grpnames[' + intcont + ']') + '['+ intiner +']'	
			var strStyMajInfo = eval('Grpnames[' + intcont + ']') + 'DESC' +'['+ intiner +']'
			var strImgDesc
			
			//To put the style esc. [Start]
			strImgDesc = ''
			<%
			
			Select Case Session("M_STYINFO")
				Case "C"
					%>
					strStyMajInfo = eval(strtemp)
					<%
				Case "D"
					%>
					strStyMajInfo = eval(strStyMajInfo)
					<%
				Case "B"
				%>
					strStyMajInfo = eval(strtemp) + ' - ' + eval(strStyMajInfo)
				<%
			End Select
			%>
			objDiv = eval('DIV' + intcont + intiner)
			eval('objDiv').innerText = strStyMajInfo
			//[End]
			objImg = eval('document.frm01.img' + intcont + intiner)
			objImg.src = '../styimg/' + eval(strtemp) + '.jpg'
			
			objHref = eval('document.all.a' +  intcont + intiner)
			//objHref.href = 'catstyle.asp?style=' + eval(strtemp) + '&GRP=' + eval('Grpnames[' + intcont + ']')
			var strGrpName = eval('Grpnames[' + intcont + ']');
			strGrpName = strGrpName.substr(3,strGrpName.length)
			objHref.href = 'catstyle.asp?PageID=C&style=' + eval(strtemp) + '&GRP=' + strGrpName
			
			
		}
	}
}


function move_next(GrpNum)
{
	strtry = eval('Grpnames[' + GrpNum + ']');
	intGrpCount = eval(strtry).length;
	intCurrSty = 5*(intCurpage[GrpNum] - 1) + 1;
	
	
	if(intCurrSty + 4 < intGrpCount )
	{
		intCurpage[GrpNum] ++;
		intCurrSty = intCurrSty + 5;
	
		if(intCurrSty > intGrpCount)
			intGrpEnd = intGrpCount + 1;
		else
			if(intCurrSty + 5 > intGrpCount)
				intGrpEnd = intGrpCount + 1;
			else
				intGrpEnd = intCurrSty + 5;
		
		var intcounter = 0;
		for(intcont=0 ;intcont<5  ; intcont++)
		{
			objHref = eval('document.all.a' +  GrpNum + intcont)
			objHref.href = '#';
		}
		
		for(intcont = intCurrSty ; intcont < intGrpEnd ; intcont++)
		{
			objImg = eval('document.frm01.img' + GrpNum + intcounter);
			inttemp = intcont - 1;
			var strtemp = eval('Grpnames[' + GrpNum + ']') + '['+ inttemp +']';
			objImg.src = '../styimg/' + eval(strtemp) + '.jpg';
			objHref = eval('document.all.a' +  GrpNum + intcounter)

			var strStyMajInfo = eval('Grpnames[' + GrpNum + ']') + 'DESC' +'['+ inttemp +']'
			var strImgDesc
			
			//To put the style esc. [Start]
			strImgDesc = ''
			<%
			
			Select Case Session("M_STYINFO")
				Case "C"
					%>
					strStyMajInfo = eval(strtemp)
					<%
				Case "D"
					%>
					strStyMajInfo = eval(strStyMajInfo)
					<%
				Case "B"
				%>
					strStyMajInfo = eval(strtemp) + ' - ' + eval(strStyMajInfo)
				<%
			End Select
			%>

			objDiv = eval('DIV' + GrpNum + intcounter)
			eval('objDiv').innerText = strStyMajInfo
			
			//HDM[Start]

			var strGrpName = eval('Grpnames[' + GrpNum + ']')
			strGrpName = strGrpName.substr(3,strGrpName.length)
			//objHref.href = 'catstyle.asp?style=' + eval(strtemp) + '&GRP=' + strGrpName
			objHref.href = 'catstyle.asp?PageID=C&style=' + eval(strtemp) + '&GRP=' + strGrpName
			intcounter ++;




		}
			
		for(intcont = intcounter ; intcont < 5 ; intcont++)
		{
			objImg = eval('document.frm01.img' + GrpNum + intcont);
			objImg.src = '../images/<%=Session("Theme")%>/transperant.gif';
			
			objDiv = eval('DIV' + GrpNum + intcont);
			eval('objDiv').innerText = "";

		}
		

		if(intCurrSty + 5 < intGrpCount)
		{
			objnext = eval('document.frm01.next' + GrpNum)
			objnext.src = '../images/<%=Session("Theme")%>/next.gif'
		}
		else
		{
			objnext = eval('document.frm01.next' + GrpNum)
			objnext.src = '../images/<%=Session("Theme")%>/transperant.gif'
		}

		objback = eval('document.frm01.back' + GrpNum)
		objback.src = '../images/<%=Session("Theme")%>/back.gif'
	}	
}

function move_back(GrpNum)
{
	
	if(intCurpage[GrpNum] - 1 > 0)
	{
		intCurpage[GrpNum] --;
		strtry = eval('Grpnames[' + GrpNum + ']');
		intGrpCount = eval(strtry).length;
		intCurrSty = 5*(intCurpage[GrpNum] - 1) + 1;

		intGrpEnd = intCurrSty + 4 ;

		var intcounter = 0;
		for(intcont = intCurrSty - 1 ; intcont < intGrpEnd ; intcont++)
		{
			objImg = eval('document.frm01.img' + GrpNum + intcounter);
			inttemp = intcont ;
			var strtemp = eval('Grpnames[' + GrpNum + ']') + '['+ inttemp +']';

			var strStyMajInfo = eval('Grpnames[' + GrpNum + ']') + 'DESC' +'['+ inttemp +']'
			var strImgDesc
			
			strImgDesc = ''
			<%
			
			Select Case Session("M_STYINFO")
				Case "C"
					%>
					strStyMajInfo = eval(strtemp)
					<%
				Case "D"
					%>
					strStyMajInfo = eval(strStyMajInfo)
					<%
				Case "B"
				%>
					strStyMajInfo = eval(strtemp) + ' - ' + eval(strStyMajInfo)
				<%
			End Select
			%>


			objDiv = eval('DIV' + GrpNum + intcounter)
			eval('objDiv').innerText = strStyMajInfo
			
			objImg.src = '../styimg/' + eval(strtemp) + '.jpg';
			
			objHref = eval('document.all.a' +  GrpNum + intcounter)
			
			
			
			var strGrpName = eval('Grpnames[' + GrpNum + ']')
			strGrpName = strGrpName.substr(3,strGrpName.length)
			objHref.href = 'catstyle.asp?PageID=C&style=' + eval(strtemp) + '&GRP=' + strGrpName
			
			
			
			//objHref.href = 'catstyle.asp?style=' + eval(strtemp) + '&GRP=' + strGrpName
			
			
			intcounter ++;
		}
			
		for(intcont = intcounter ; intcont < 5 ; intcont++)
		{
			objImg = eval('document.frm01.img' + GrpNum + intcont);
			objImg.src = '../images/<%=Session("Theme")%>/transperant.gif';
		}
		
		if(intGrpEnd  < intGrpCount)
		{
			objnext = eval('document.frm01.next' + GrpNum)
			objnext.src = '../images/<%=Session("Theme")%>/next.gif'
		}
		else
		{
			objnext = eval('document.frm01.next' + GrpNum)
			objnext.src = '../images/<%=Session("Theme")%>/transperant.gif'
		}

		if(intCurpage[GrpNum] > 1)
		{
			objback = eval('document.frm01.back' + GrpNum)
			objback.src = '../images/<%=Session("Theme")%>/back.gif'
		}
		else
		{
			objback = eval('document.frm01.back' + GrpNum)
			objback.src = '../images/<%=Session("Theme")%>/transperant.gif'
		}
	}	
	
}
//-->
</SCRIPT>


  <%  
  
%>
</head>
<body  >
<%IF strFile = "cust" Then%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Cust.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><BR><BR><BR></p>
<%Else%>
<SCRIPT LANGUAGE="JavaScript1.2"
        SRC="../HM_Loader_Sales.js"
        TYPE='text/javascript'>
</SCRIPT>
<p><br><br><BR></p>
<table border="0" cellpadding="0" cellspacing="0" width="95%" align=center>
<TR>
	<!-- ARD -->
	<TD colspan=13>
	<P>Your currently selected <%=session("CustField")%> is <%=Session("customerid")%> - <%=Session("rscust")("btname")%></P>
	</TD>
	<TD align=right><input type=button onclick="javascript:window.location.href='repcust.asp';" value="Get <%=session("CustField")%>" id=button1 name=button1></TD>
	<!-- ARD -->
</TR>
</table>
 <%End IF%>

<Table width=95% height=50 align=center border=1>
<TR>
<TD class=title>Catalogue</TD>
</TR>
</Table>
	<%
	If Trim(Session("Rep")) = "" Then
		strAppUserVar = Session("ID")
	Else
		strAppUserVar = Session("Rep")
	End If
	If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"CATALOG") <= 0 Then
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

<Table width=95% align=center border=0>
<TR>
<TD  align=right>
	<a href="CatSearch.asp">Search Catalogue</a>
	<a href="../order/orderhsave.asp?From=Cat">Check shopping cart</a> </TD>
</TR>
</Table>


<Table width=95% align=center>
<TR>
<TD align=center>
<Form name=frm01 method=post>

<%
	
	'ARD
	'for intloop=0 to NumOfGrp-1
	For intloop=intLowLimit to intUpLimit
		intresult = intloop mod 2
		IF intresult > 0 Then
			Response.Write("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
		End IF
	%>
	<table width=750 align=center border=0>
	<TR>
		<%
		IF intresult > 0 Then
			Response.Write("<TD>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<TD>")
		End IF
		
		Dim intfrstelmnt
		intfrstelmnt = NumOfGrp * (CurPage - 1) + 1
		For intinerloop=0 to 4%>	
			<td VALIGN='TOP' rowspan=2>
			<a id="a<%=intloop%><%=intinerloop%>" >
				<img name="img<%=intloop%><%=intinerloop%>" id="img<%=intloop%><%=intinerloop%>" width=83 height=109 src="../images/<%=Session("Theme")%>/transperant.gif" border=0>
			</a><br>
			<Div id="DIV<%=intloop%><%=intinerloop%>" name="DIV<%=intloop%><%=intinerloop%>">
			</Div>
			</td>
		<%Next%>
	</TR>
	<tr>
	<td>
		<table border=0>
		<tr><td colspan=2 align=center><Div><b><Script Language="JAVAScript">document.write(NameGRPDESC[<%=intloop%>]);</Script></b></Div></td></tr></tr>
		<TD><img name="back<%=intloop%>" id="back<%=intloop%>" onclick="javascript:move_back(<%=intloop%>)" src="../images/<%=Session("Theme")%>/transperant.gif" width=70 height=20></TD>
		<TD><img name="next<%=intloop%>" id="next<%=intloop%>" onclick="javascript:move_next(<%=intloop%>)" src="../images/<%=Session("Theme")%>/transperant.gif" width=70 height=20></TD>
		</tr>
		</table>
	</td>
	</TR>
	</Table>
	<%
	Next
%>

</form>
</TD>
</TR>
<TR><TD ALIGN=CENTER>

<%
'WMH Outer Paging [Start]
IF Cint(CurPage) > 1 THEN	
	'show prev %>
	<a href="newcat.asp?CurPage=<%=CurPage-1%>"><img border=0 src="../images/<%=Session("Theme")%>/back.gif"></a>&nbsp;
<%END IF
'Show next Button

IF Cint(CurPage)< Cint(Ubound(strvalidGroup)/NumOfGrp)+1 Then%>
	<a href="newcat.asp?CurPage=<%=CurPage+1%>"><img border=0 src="../images/<%=Session("THEME")%>/next.gif"></a>
<%
End IF
'WMH Outer Paging [End]
%>

</TD></TR>

</Table>
<SCRIPT LANGUAGE=javascript>
<!--
do_load()
//-->
</SCRIPT>
<%END IF %>
<p>
</body>
</html>