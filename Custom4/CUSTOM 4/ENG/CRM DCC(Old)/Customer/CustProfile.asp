<%@ Language=VBScript %>
<SCRIPT language=JavaScript>
function go(which, num, win) 
{
  n = which.selectedIndex;
  str = which.title
  if (n != 0)

       which.form.submit();

}

</SCRIPT>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<Title>CRM - Customer Profile</Title>
</HEAD>
<BODY bgcolor="#aecae6" leftmargin="0" topmargin="0">
<Center>
<!-- URL's used in the movie-->
<A HREF=helpdesk.asp></A>
<A HREF=return.asp></A> 
<A HREF=catalog.asp></A> 
<A HREF=OTS.asp></A> 
<A HREF=order.asp></A> 
<A HREF=custprof.asp></A> 
<!-- text used in the movie-->
<!--ustomer elationship anagement -->
<OBJECT classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
  codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0"
 WIDTH=780 HEIGHT=140 id=ShockwaveFlash1>
 <PARAM NAME=movie VALUE="CustNav.swf"> 
 <PARAM NAME=loop VALUE=false> 
 <PARAM NAME=menu VALUE=false> 
 <PARAM NAME=quality VALUE=medium> 
 <PARAM NAME=wmode VALUE=transparent> 
 <PARAM NAME=bgcolor VALUE=#AECAE6> 
 <EMBED src="CustNav.swf" loop=false 
 menu=false quality=medium wmode=transparent bgcolor=#AECAE6  WIDTH=100% HEIGHT=172 TYPE="application/x-shockwave-flash" PLUGINSPAGE="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash"></EMBED>
</OBJECT>


</center>
<form name=form action="custprofile.asp" method="post">
<%
'account="MA100"
account=Session("ID")
set conn=server.CreateObject("ADODB.connection")
set rs=server.CreateObject("ADODB.recordset")
conn.Open "dsn=CRM;uid=aria;pwd=aria"
store=Request("store")
select case store
case ""
	sqlstat="select distinct * from customer where type like 'M'  and account='" & account & "'"
	
case else
	sqlstat="select  * from customer where type like 'S' and account='"&account&"' and store='"&store&"'"
	
end select
rs.open sqlstat,conn,1,3
%>
<CENTER><FONT color=navy size=5><B>Customer Profile</B></FONT></CENTER><br><table  width="100%"  border=1 bgcolor="#6495d0" bordercolor="#aecae6">
<%do while not rs.eof%>  <tr> <td Align="center" valign="center" width="16%"><b><font color="Ivory" size="2" face="arial">Account</font></b></td>
    <td Align="center" valign="center" width="16%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("account")%></font></td>

    <%set dbrs=server.CreateObject("ADODB.recordset")
	sqls="select distinct store from customer where type like 'S' and account='"&account&"'"
	dbrs.open sqls,conn,1,3
	%>


			<%if  dbrs.EOF and dbrs.BOF then

					strTemp = "<td Align=""center"" width=""17%""><b><font color=""Ivory"" size=""2"" face=""arial"">"

					Response.Write(strTemp)

					strTemp = "<td Align=""center"" width=""17%"">"

					Response.Write(strTemp)

			

				else

					Dim strTemp ' as string

					strTemp = "<td Align=""center"" width=""17%""><b><font color=""Ivory"" size=""2"" face=""arial"">"

					Response.Write(strTemp)

					Response.Write("Store")

						'Response.Write(request("store"))
						'strTemp = "<BR>"
						'Response.Write(strTemp)
						'Response.Write(dbrs("store"))

					strTemp = "</b></td>"
				
					Response.Write(strTemp)

						dbrs.movefirst

						strTemp = "<td Align=""center"" width=""17%""><select name=store onchange=""go(this)"">"
						Response.Write(strTemp)
						
						strTemp = "<option selected value=" & store & ">" & store
						Response.Write(strTemp)
						
							
							do while not dbrs.EOF
							
						if trim(request("store")) = trim(dbrs("store")) then
							dbrs.MoveNext 
							'Response.Write("ana moosh store")
						else
							strTemp = "<option value=" & dbrs("store") & ">" & dbrs("store")
							Response.Write(strTemp)
							dbrs.MoveNext 
						end if
								'strTemp="<OPTION value="&dbrs("store")&">"
								'Response.Write(strTemp)
								'if store<>dbrs("store") then
								'Response.Write dbrs("store")
								'end if
								'if (store=dbrs("store")) then
								'dbrs.MoveNext
							'end if
							loop				
				end if

			   dbrs.Close
			set dbrs=nothing

			%>

    

    

				

   </TD>
    <td Align="center" valign="center" width="17%"><b><font color="Ivory" size="2" face="arial">Phone</font></b></td>
    <td Align="center" valign="center" width="17%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("phone1")%></font></td>
  </tr>
  <tr>
    <td Align="center" valign="center" width="16%"><font color="Ivory" size="2" face="arial"><b>Name</b></font></td>
    <td Align="center" valign="center" width="16%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("stname")%></font></td>
    <td width="17%">&nbsp;</td>
    <td width="17%">&nbsp;</td>
    <td Align="center" valign="center" width="17%"><b><font color="Ivory" size="2" face="arial">Status</font></b></td>
    <td Align="center" valign="center" width="17%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("status")%></font></td>
  </tr>
  <tr>
    <td Align="center" valign="center" width="16%"><b><font color="Ivory" size="2" face="arial">Address</font></b></td>
    <td Align="center" valign="center" width="16%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("caddress1")%></font></td>
    <td width="17%">&nbsp;</td>
     <td width="17%">&nbsp;</td>   

     <%

     	Dim strTemp3' as string



			if not len(trim(rs("dba")))=0 then

				strTemp3= "<td Align=""center"" width=""17%""><b><font color=""Ivory"" size=""2"" face=""arial"">"

  			Response.Write(strTemp3)

				Response.Write("DBA")

				strTemp3= "</b></td>"

  			Response.Write(strTemp3)

	  		strTemp3= "<td Align=""center"" width=""17%"" bgcolor=""Ivory""><font color=""navy"" size=""2"" face=""Arial"">"

		  	Response.Write(strTemp3)

			  Response.Write rs("dba")

			  strTemp3= "</font></td>"

		  	Response.Write(strTemp3)

				end if

			

		%>

  </tr>
  <tr>
    <td width="16%">&nbsp;</td>
    <td Align="center" valign="center" width="16%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("caddress2")%></font></td>
    <td width="17%">&nbsp;</td>
    <td width="17%">&nbsp;</td>
    <td Align="center" valign="center" width="17%"><b><font color="Ivory" size="2" face="arial">Buyer</font></b></td>
    <td Align="center" valign="center" width="17%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("buyer")%></font></td>
  </tr>
  <tr>
    <td Align="center" valign="center" width="16%"><b><font color="Ivory" size="2" face="arial">City</font></b></td>
    <td Align="center" valign="center" width="16%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("caddress3")%></font></td>
    <td width="17%">&nbsp;</td>
    <td width="17%">&nbsp;</td>
    <td Align="center" valign="center" width="17%"><b><font color="Ivory" size="2" face="arial">B.Kper</font></b></td>
    <td Align="center" valign="center" width="17%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("keeper")%></font></td>
  </tr>
  <tr>
    <td Align="center" valign="center" width="16%"><b><font color="Ivory" size="2" face="arial">State</font></b></td>
    <td Align="center" valign="center" width="16%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("caddress4")%></font></td>
    <td Align="center" valign="center" width="17%"><b><font color="Ivory" size="2" face="arial">Zip</font></b></td>
    <td Align="center" valign="center" width="17%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("caddress5")%></font></td>
    <td Align="center" valign="center" width="17%"><b><font color="Ivory" size="2" face="arial">Phone2</font></b></td>
    <td Align="center" valign="center" width="17%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("phone2")%></font></td>
  </tr>
  <tr>
    <td Align="center" valign="center" width="16%"><b><font color="Ivory" size="2" face="arial">Country</font></b></td>
    <td Align="center" valign="center" width="16%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("caddress6")%></font></td>
    <td width="17%">&nbsp;</td>
    <td width="17%">&nbsp;</td>
    <td Align="center" valign="center" width="17%"><b><font color="Ivory" size="2" face="arial">Fax</font></b></td>
    <td Align="center" valign="center" width="17%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><%Response.Write rs("fax")%></font></td>
  </tr>



<%rs.movenext
loop
%></table></form>
<%rs.Close
conn.Close
set rs=nothing
set conn=nothing
%>
</BODY>
</HTML>

