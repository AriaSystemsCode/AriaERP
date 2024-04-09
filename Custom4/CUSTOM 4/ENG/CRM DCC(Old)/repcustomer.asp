<%@ Language=VBScript %>
<% session("customerid")=Request("custid")

'session("customerid")=Request.form("id")
'Response.Write session("customerid")
'if Trim(Session("ID")) = "" then
	'Response.redirect "default.htm"
'end if
%>
<%set conn1=server.CreateObject("ADODB.connection")
set session("rs1")=server.CreateObject("ADODB.recordset")
conn1.Open "dsn=crm"
sqls="select * from customer where account='"&request("custid")&"'"
session("rs1").Open sqls,conn1,1,3

%>
<html>

<head><LINK REL=stylesheet HREF="crmmain.css" TYPE="text/css">
<meta http-equiv="Content-Type" content="text/html; charset=windows-1256">
<meta name="GENERATOR" content="Microsoft FrontPage 4.0">
<title>CRM - Check Invoice</title>
</head>
<SCRIPT LANGUAGE = JScript>
function getcustomer()
{document.location ="findcustomer.asp"

}

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
			alert("The Invoice must be a number");
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
				alert("please enter valid date or let it empty");
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
				alert("please enter valid date or let it empty");
				return false;
			}
		}
	}
		
	return true;
}
</SCRIPT>


<body bgcolor="#aecae6" topmargin="0" leftmargin="0" background="images/Tile1.gif">
<div align="center">
  <center>
<table border="0" width="100%" cellspacing="0" cellpadding="0">
  <tr>
    <td width="100%" align="center">
<p>
<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"   codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" WIDTH=80% HEIGHT=125 id="ShockwaveFlash1">
 <param name="_cx" value="13653">
 <param name="_cy" value="3307">
 <param name="Movie" value="flash/rebNav.swf">
 <param name="Src" value="flash/rebNav.swf">
 <param name="WMode" value="Transparent">
 <param name="Play" value="0">
 <param name="Loop" value="0">
 <param name="Quality" value="High">
 <param name="SAlign" value>
 <param name="Menu" value="0">
 <param name="Base" value>
 <param name="Scale" value="ExactFit">
 <param name="DeviceFont" value="0">
 <param name="EmbedMovie" value="0">
 <param name="BGColor" value="AECAE6">
 <param name="SWRemote" value><embed src="flash/rebNav.swf" loop="false" menu="false" quality="medium" wmode="transparent" bgcolor="#AECAE6" WIDTH="100%" HEIGHT="172" TYPE="application/x-shockwave-flash" PLUGINSPAGE="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash">
</object>
</p>
    </td>
  </tr>
  
  
</table>
  </center>
</div>
<br>
<div align="center">
  <center>
  <table border="0" width="80%" cellspacing="0" cellpadding="0">
    <tr>
      <td width="100%"><font face="Arial" size="2" color="#000080">
<b>
Your Customer is now <%=request("custid")%>-<%=session("rs1").fields("btname")%></b>
<%session("customer")=session("rs1").fields("account")

'session("customer")="MA100"
%>
</font></td>
    </tr>
  </table>
  </center>
</div>

<p>



</p>
<%'conn1.close
'set conn1=nothing
'set rs1=nothing
%>
</body>

</html>
