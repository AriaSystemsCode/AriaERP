<%
Response.Buffer=True
Response.Expires=-1

If Trim(Session("ID")) = "" And Session("rep")= "" Then
'Response.redirect "../default.asp"%>
	<script language="javascript">
		parent.location.href ="../default.asp"
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

Dim RSGroups
Set RSGroups = Server.CreateObject("ADODB.Recordset")
For intcount =0 to ubound(arrGroups,1)
	strsql = "select * from style where style.cstygroup='" & Trim(arrGroups(intcount,0)) & "'"
	RSGroups.Open strsql,FoxCon
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
'ARD - [End]
strTemp = ""
IF ubound(strvalidGroup)+1 = 0 Then
	%>
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
	Your currently selected customer is <%=Session("customerid")%> - <%=Session("rscust").fields("btname").value%>
	</TD>
	<TD align=right><a href="repcust.asp">Get Customer</a></TD>
	</TR>
	</Table>
	</font>
	<Table width=95% align=center border=1>
	<TR>
		<TD class=title>Catalog</TD>
	</TR>
	</Table>
	<Table width=95% align=center >
	<TR>
		<TD align=center><b>No records found in selected group(s).</b></TD>
	</TR>
	</Table>
	</body>
	</html>
	<%
	 
End IF
intresult = int(100/(ubound(strvalidGroup)+1))
' ARD - This part is to find which group has Styles and which is not and prevent those are empty from showing [End]

 Const NumPerPage = 3
  Dim CurPage
  Dim intUpLimit
  Dim intLowLimit
  
  IF Request.QueryString("CurPage") = "" Then
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
				Grpnames[<%=Intcont%>] = '<%=Trim(strvalidGroup(Intcont))%>'
				intCurpage[<%=Intcont%>] = 1
			<%Next%>
//-->
</SCRIPT>

<%
  Set RSstyles = Server.CreateObject("ADODB.Recordset")

For intcount = intLowLimit to intUpLimit
		strsql = "select DISTINCT style.cstymajor,Style.desc, codes.cdiscrep from style, codes where style.cstygroup='" & strvalidGroup(intcount) & "' and codes.ccode_no='" & strvalidGroup(intcount) & "'"
		RSstyles.Open strsql,FoxCon
		%>
		<SCRIPT LANGUAGE=javascript>
		<!--
			var <%=Trim(strvalidGroup(intcount))%> = new Array();
			var <%=Trim(strvalidGroup(intcount))& "DESC"%> = new Array();
			<%intcounter = 0%>
			<%Do while not RSstyles.EOF %>
				<%=Trim(strvalidGroup(intcount))%>[<%=intcounter%>] = '<%=Trim(RSstyles.fields("cstymajor").value)%>'
				<%=Trim(strvalidGroup(intcount))& "DESC"%>[<%=intcounter%>] = '<%=Trim(RSstyles.fields("Desc").value)%>'
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
	for(intcont=0 ; intcont < Grpnames.length  ; intcont++)
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
			objImg = eval('document.frm01.img' + intcont + intiner)
			objImg.src = '../styimg/' + eval(strtemp) + '.jpg'
			objHref = eval('document.all.a' +  intcont + intiner)
			objHref.href = 'catstyle.asp?style=' + eval(strtemp) + '&GRP=' + eval('Grpnames[' + intcont + ']')
			
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

<Table width=95% height=50 align=center border=1>
<TR>
<TD class=title>Catalog</TD>
</TR>
</Table>

<Table width=95% align=center border=0>
<TR>
<TD  align=right><a href="catcustord.asp">Check shopping cart</a> </TD>
</TR>
</Table>


<Table width=95% align=center border=0>
<TR>
<TD align=center>
<Form name=frm01 method=post>

<%
	
	If Request.QueryString("CurPage") = "" Then
		CurPage = 1
	Else
		CurPage = Request.QueryString("CurPage")
	End IF

	for intloop=0 to NumOfGrp-1
		intresult = intloop mod 2
		IF intresult > 0 Then
			Response.Write("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
		End IF
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
			<td><a id="a<%=intloop%><%=intinerloop%>" ><img name="img<%=intloop%><%=intinerloop%>" id="img<%=intloop%><%=intinerloop%>" width=83 height=109 src="../images/<%=Session("Theme")%>/transperant.gif" border=0></a></td>
		<%Next%>
	<TD><img name="back<%=intloop%>" id="back<%=intloop%>" onclick="javascript:move_back(<%=intloop%>)" src="../images/<%=Session("Theme")%>/transperant.gif" width=70 height=20></TD>
	<TD><img name="next<%=intloop%>" id="next<%=intloop%>" onclick="javascript:move_next(<%=intloop%>)" src="../images/<%=Session("Theme")%>/transperant.gif" width=70 height=20></TD>
	</TR>
	</Table>
	<%
	Next
%>
</form>
</TD>
</TR></Table>
<SCRIPT LANGUAGE=javascript>
<!--
do_load()
//-->
</SCRIPT>

</body>
</html>