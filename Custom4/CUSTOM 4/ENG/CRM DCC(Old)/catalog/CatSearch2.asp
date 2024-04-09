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


'Session("ConnectionString") = Application("DataConnectionString")
Session("ConnectionString") = "Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB= "& Application("DataPath") &";SourceType=DBF;Exclusive=No;BackgroundFetch=NO;Collate=Machine;Null=No;Deleted=Yes"
Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open(Session("ConnectionString"))

Set RSCodes = Server.CreateObject("ADODB.RECORDSET")
strSql = "Select ccode_no, cdiscrep, cdefcode from codes where cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam like 'NCSTYGROUP%'"
RSCodes.Open strSql,Conn


IF Not IsObject(Session("RSStyStruct")) Then
	Set Session("RSStyStruct") = Server.CreateObject("ADODB.RecordSet")
	strsql = "select * from icistru where citemrecty+cisegno like'U%'"
	Session("RSStyStruct").open strsql,conn
End IF

%>
<Html>
<Head>
<link REL="stylesheet" HREF="../images/<%=Session("Theme")%>/Catalog.css" TYPE="text/css">


<%

'WMH Start New Search [Start] ////////////////////////////////////////////////
'ARD - Search result [Start]

'WMH Outer Paging [Start]
const NumOfGrp = 3
Const NumPerPage = 3
Dim CurPage
Dim intUpLimit
Dim intLowLimit
'WMH Outer Paging [End]

If Request("search") = "Y" then
	Dim rsStySearch 
	Set rsStySearch = server.CreateObject("adodb.recordset")
	if Trim(Request("Group"))="ALL" then
		strsql = "Select * from style where status+cstymajor like 'A" & UCASE(Trim(Request(strName))) & "%' and " &makeQuery("CSTYgroup", session("showcatalogval"), 10, false) 
	else
		strsql = "Select * from style where status+cstymajor like 'A" & UCASE(Trim(Request(strName))) & "%' and cstygroup = '" & Request("Group")& "'"
	end if

	rsStySearch.CursorType = 3
	rsStySearch.Open strsql	,conn
	Response.Write(rsStySearch.RecordCount )

	intRecordCount = rsStySearch.RecordCount
	if rsStySearch.RecordCount = 1 then	
		Response.Redirect("Catstyle.asp?style=" & Trim(rsStySearch("cstymajor")) & "&GRP=" & trim(rsStySearch("cstygroup")))
	else
'*******************************************************************
		if Trim(Request("Group"))="ALL" then
			arrFound= split(session("showcatalogval"),",")
			%>
			<SCRIPT LANGUAGE=javascript>
			<!--
					var Grpnames = new Array();
					var intCurpage = new Array();
						<%For Intcont=0 to ubound(arrFound)%>
							Grpnames[<%=Intcont%>] = '<%=Trim(arrFound(Intcont))%>'
							intCurpage[<%=Intcont%>] = 1
						<%Next%>
			//-->
			</SCRIPT>
			<%
  
			IF Request.QueryString("CurPage") = "" Then
				CurPage = 1
			Else
				CurPage = Request.QueryString("CurPage")
			End IF
  

			intLowLimit = (CurPage * NumPerPage) - NumPerPage
			intUpLimit  = intLowLimit + NumPerPage-1
			IF intUpLimit > ubound(arrFound)  Then
				intUpLimit = ubound(arrFound) 
			End IF
				
			For intcount = intLowLimit to intUpLimit
				rsStySearch.Filter = "CSTYGROUP = '" & Trim(arrFound(intcount)) & "'"
				if not(rsStySearch.EOF and rsStySearch.BOF) then
				%>
					<SCRIPT LANGUAGE=javascript>
					<!--
						var <%=Trim(arrFound(intcount))%> = new Array();
						var <%=Trim(arrFound(intcount))& "DESC"%> = new Array();
						<%intcounter = 0%>
						<%Do while not rsStySearch.EOF %>
							<%=Trim(arrFound(intcount))%>[<%=intcounter%>] = '<%=Trim(rsStySearch.fields("cstymajor").value)%>'
							<%=Trim(arrFound(intcount))& "DESC"%>[<%=intcounter%>] = '<%=Trim(rsStySearch.fields("Desc").value)%>'
							<%intcounter = intcounter + 1%>
							<%rsStySearch.MoveNext%>
						<%Loop%>
					//-->
					</SCRIPT>
				<%
				End if
			Next
'WMH [Start]
'		for intloop=0 to 2
	For intloop=intLowLimit to intUpLimit
''		intresult = intloop mod 2
'WMH [Start]
		%>
	
	
	<table width=750 align=center>
	<TR>
		<%
		IF intresult > 0 Then
			Response.Write("<TD>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<TD>")
		End IF
		
		Dim intfrstelmnt
		intfrstelmnt = NumOfGrp * (CurPage - 1) + 1
		For intinerloop=0 to 4%>	
			<td><a id="a<%=intloop%><%=intinerloop%>" ><img name="img<%=intloop%><%=intinerloop%>" id="img<%=intloop%><%=intinerloop%>" width=83 height=109 src="../images/<%=Session("Theme")%>/transperant.gif" border=0></a><br>
			<Div id="DIV<%=intloop%><%=intinerloop%>" name="DIV<%=intloop%><%=intinerloop%>">
			</Div>
			</td>
		<%Next%>
	<TD><img name="back<%=intloop%>" id="back<%=intloop%>" onclick="javascript:move_back(<%=intloop%>)" src="../images/<%=Session("Theme")%>/transperant.gif" width=70 height=20></TD>
	<TD><img name="next<%=intloop%>" id="next<%=intloop%>" onclick="javascript:move_next(<%=intloop%>)" src="../images/<%=Session("Theme")%>/transperant.gif" width=70 height=20></TD>
	</TR>
	</Table>
		<%Next%>
	
		<SCRIPT LANGUAGE=javascript>
		<!--
		do_load()
		//-->
		</SCRIPT>

		<%		
		
'*******************************************************************			
		else

			%>
			<SCRIPT LANGUAGE=javascript>
			<!--
					var Grpnames = new Array();
					var intCurpage = new Array();
							Grpnames[0] = '<%=Request("Group")%>'
							intCurpage[0] = 1
			//-->
			</SCRIPT>
			<%
  
			IF Request.QueryString("CurPage") = "" Then
				CurPage = 1
			Else
				CurPage = Request.QueryString("CurPage")
			End IF
 
			intLowLimit = (CurPage * NumPerPage) - NumPerPage
			intUpLimit  = intLowLimit + NumPerPage-1
				
				if not(rsStySearch.EOF and rsStySearch.BOF) then
				%>
					<SCRIPT LANGUAGE=javascript>
					<!--
						var <%=Request("Group")%> = new Array();
						var <%=Request("Group")& "DESC"%> = new Array();
						<%intcounter = 0%>
						<%Do while not rsStySearch.EOF %>
							<%=Request("Group")%>[<%=intcounter%>] = '<%=Trim(rsStySearch.fields("cstymajor").value)%>'
							<%=Request("Group")& "DESC"%>[<%=intcounter%>] = '<%=Trim(rsStySearch.fields("Desc").value)%>'
							<%intcounter = intcounter + 1%>
							<%rsStySearch.MoveNext%>
						<%Loop%>
					//-->
					</SCRIPT>
<%
			end if		
'WMH [Start]
'		for intloop=0 to 2
	For intloop=intLowLimit to intUpLimit
''		intresult = intloop mod 2
'WMH [END]
		%>
	
	
	<table width=750 align=center>
	<TR>
		<%
		IF intresult > 0 Then
			Response.Write("<TD>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<TD>")
		End IF
		
		
		intfrstelmnt = NumOfGrp * (CurPage - 1) + 1
		For intinerloop=0 to 4%>	
			<td><a id="a<%=intloop%><%=intinerloop%>" ><img name="img<%=intloop%><%=intinerloop%>" id="img<%=intloop%><%=intinerloop%>" width=83 height=109 src="../images/<%=Session("Theme")%>/transperant.gif" border=0></a><br>
			<Div id="DIV<%=intloop%><%=intinerloop%>" name="DIV<%=intloop%><%=intinerloop%>">
			</Div>
			</td>
		<%Next%>
	<TD><img name="back<%=intloop%>" id="back<%=intloop%>" onclick="javascript:move_back(<%=intloop%>)" src="../images/<%=Session("Theme")%>/transperant.gif" width=70 height=20></TD>
	<TD><img name="next<%=intloop%>" id="next<%=intloop%>" onclick="javascript:move_next(<%=intloop%>)" src="../images/<%=Session("Theme")%>/transperant.gif" width=70 height=20></TD>
	</TR>
<TR><TD ALIGN=CENTER>



</TD></TR>	
	</Table>
		<%Next

%>

<SCRIPT LANGUAGE=javascript>
<!--
do_loadSingle()
//-->
</SCRIPT>


<%
'*******************************************************************			
		end if
	end if

end if

'ARD - Search result [Start]
%>
<%
'WMH Start New Search [End] ////////////////////////////////////////////////



%>



<SCRIPT LANGUAGE=javascript>
<!--
function do_load()
{
	for(intcont=0 ; intcont < 3 ; intcont++)
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
			objHref.href = 'catstyle.asp?style=' + eval(strtemp) + '&GRP=' + eval('Grpnames[' + intcont + ']')
			
		}
	}
}

function do_loadSingle()
{
	var intCountLimit
	for(intcont=0 ; intcont < 3  ; intcont++)
	{
		for(inerloop = 0 ; inerloop <5 ; inerloop++)
		{
			strtry = eval('Grpnames[0]')
			
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
			
			objHref.href = 'catstyle.asp?style=' + eval(strtemp) + '&GRP=' + eval('Grpnames[' + GrpNum + ']')
			intcounter ++;
		}
			
		for(intcont = intcounter ; intcont < 5 ; intcont++)
		{
			objImg = eval('document.frm01.img' + GrpNum + intcont);
			objImg.src = '../images/<%=Session("Theme")%>/transperant.gif';
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
			objHref.href = 'catstyle.asp?style=' + eval(strtemp) + '&GRP=' + eval('Grpnames[' + GrpNum + ']')
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
	<P>Your currently selected customer is <%=Session("customerid")%> - <%=Session("rscust")("btname")%></P>
	</TD>
	<TD align=right><a href="repcust.asp">Get Customer</a></TD>
	<!-- ARD -->
</TR>
</table>
 <%End IF%>


<!--WMH Top Form-------------------------------------------------------------- -->
<BR><BR>
<form action="CatSearch2.asp?search=Y" method="post" name ="frmMain" id="form1">
	<p><font size="2" face="Aria"><strong> </p>
    <div align="center">
      <center>
   <table bordercolor="#111111" border="1" width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0" >
		 <tr>
			<td Align="left" valign="center" width=50% class="dark_cell">

			<%

				IF Not(Session("RSStyStruct").EOF And Session("RSStyStruct").BOF) Then
					Session("RSStyStruct").MoveFirst
					DO While Not Session("RSStyStruct").Eof
						if Trim(Request("firsttime"))<>"" then
							strValue = Request.Form(Trim(Session("RSStyStruct").fields("cisegsdes")))
							Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) = strValue
						else
							strValue = Session(Trim(Session("RSStyStruct").fields("cisegsdes")))
						end if
						strRequiredStyle = Trim(strRequiredStyle) & Trim(strValue)
						strRequiredStyle = Ucase(strRequiredStyle) & "%"
						if Session("RSStyStruct")("lSegEndMaj") = true then
							if Request("search")="Y" then
								strTemp = "<font face=Arial color=#000080 size=2><b>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "</b></font><INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Ucase(Request(Trim(Session("RSStyStruct").Fields("cisegsdes").Value))) & ">"
								strName = Trim(Session("RSStyStruct").fields("cisegsdes"))
							else 
								strTemp = "<font face=Arial color=#000080 size=2><b>" & Trim(Session("RSStyStruct").fields("cisegsdes")) & "<b></font><INPUT name=" & Trim(Session("RSStyStruct").fields("cisegsdes")) & " size=""" & Session("RSStyStruct").fields("nisegsize") &  """ maxlength="""& Session("RSStyStruct").fields("nisegsize") & """ value=" & Session(Trim(Session("RSStyStruct").fields("cisegsdes"))) & ">"
								strName = Trim(Session("RSStyStruct").fields("cisegsdes"))
							end if
							Response.Write(strTemp)
						end if
						strRequiredStyle = strRequiredStyle & Trim(Session("RSStyStruct").fields("cisegsepr"))
						Session("RSStyStruct").MoveNext
					Loop
				End IF
				
			%> &nbsp;</td>
      <%IF Session("M_STYVIEW") = "G" Then%>
      <td Align="left" valign="center" colSpan=1 class="dark_cell">
      <font face=Arial size=2><b>Group</b></font>
				<SELECT name=Group size=1> 
					<OPTION selected value="ALL">All
				<%IF Not RSCodes.EOF And Not RSCodes.BOF Then%>
					<%Dim strTemp%>
					<%RSCodes.MoveFirst 
						
						arrFound= split(session("showcatalogval"),",")
						for intcount =0 to ubound(arrFound)
							
							RSCodes.Filter = "ccode_no='"& arrFound(intcount) & space(6 - len(arrFound(intcount))) & "'"
							if Request("Group") =  arrFound(intcount) Then
								Response.Write("<option value=""" & Trim(RSCodes.Fields("ccode_no").Value )& """ selected>" & RSCodes.Fields("cdiscrep").Value )
							Else
								Response.Write("<option value=""" & Trim(RSCodes.Fields("ccode_no").Value )& """>" & RSCodes.Fields("cdiscrep").Value )
							End IF
							
							RSCodes.Filter = ""
						next
						
				END IF%>
					</SELECT>
				
			</td>
			<%End IF%>
			<td align=left class="dark_cell"><input type="submit" name="B2" value="Search"><input type="reset" name="B3" value="Reset">
			</td>
     </tr>
    </table>
    </form>
<!--WMH End Top Form-------------------------------------------------------------- -->




<Table width=95% align=center border=0>
<TR>
<TD  align=right>
	<a href="CatSearch.asp">Search Catalog</a> 
	<a href="catcustord.asp">Check shopping cart</a> </TD>
</TR>
</Table>


<Table width=95% align=center border=0>
<TR>
<TD align=center>

</TD>
</TR>
<TR><TD ALIGN=CENTER>

<%
'WMH Outer Paging [Start]
IF Cint(CurPage) > 1 THEN	
	'show prev %>
	<a href="CatSearch2.asp?CurPage=<%=CurPage-1%>"><img src="../images/<%=Session("Theme")%>/back.gif"></a>&nbsp;
<%END IF
'Show next Button

IF Cint(CurPage)< Cint(Ubound(arrFound)/NumOfGrp) Then%>
	<a href="CatSearch2.asp?CurPage=<%=CurPage+1%>"><img src="../images/<%=Session("THEME")%>/next.gif"></a>
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

</body>
</html>


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
		strQuery = strQuery & "'" & ValuesArray(i) & "'"
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