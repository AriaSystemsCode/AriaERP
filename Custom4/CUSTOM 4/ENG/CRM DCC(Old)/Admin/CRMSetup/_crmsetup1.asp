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

function do_mover()
{
	document.form1.action = "mover.asp"
	document.form1.submit();
}

function do_mover2()
{
	document.form1.action = "mover2.asp"
	document.form1.submit();
}

//Use Online Payment Section
function UseOnlinePayment(useOnline)
{
	if (useOnline == false)
	{
		document.form1.lstGateWay.disabled = true;
		document.form1.txtDepositPercent.value = '0';
		document.form1.txtDepositPercent.disabled = true;
		document.form1.txtSSLURL.value = '';
		document.form1.txtSSLURL.disabled = false;		

		document.form1.radUseExtSSLTrue.checked = false;
		document.form1.radUseExtSSLFalse.checked = true;				
		document.form1.radUseExtSSLTrue.disabled = true;						
//		document.form1.radUseExtSSLFalse.disabled = true;				
		
		document.form1.radUseDepositTrue.checked = false;
		document.form1.radUseDepositFalse.checked = true;				
		document.form1.radUseDepositTrue.disabled = true;						
//		document.form1.radUseDepositFalse.disabled = true;				
		
		document.form1.radAllowEditDepositTrue.checked = false;
		document.form1.radAllowEditDepositFalse.checked = true;
		document.form1.radAllowEditDepositTrue.disabled = true;		
//		document.form1.radAllowEditDepositFalse.disabled = true;												
	}
	else //true
	{
		document.form1.lstGateWay.disabled = false;
		document.form1.txtDepositPercent.value = '50';
		document.form1.txtDepositPercent.disabled = false;
		//document.form1.txtSSLURL.value = '';
		document.form1.txtSSLURL.disabled = true;		
		
		document.form1.radUseExtSSLTrue.checked = false;
		document.form1.radUseExtSSLFalse.checked = true;				
		document.form1.radUseExtSSLFalse.disabled = false;		
		document.form1.radUseExtSSLTrue.disabled = false;						

		document.form1.radUseDepositTrue.checked = true;
		document.form1.radUseDepositFalse.checked = false;				
		document.form1.radUseDepositFalse.disabled = false;		
		document.form1.radUseDepositTrue.disabled = false;						
		
		document.form1.radAllowEditDepositTrue.checked = true;
		document.form1.radAllowEditDepositFalse.checked = false;
		document.form1.radAllowEditDepositTrue.disabled = false;		
		document.form1.radAllowEditDepositFalse.disabled = false;												
	}
}


//Use Ext. URL
function UseExtSSL(useOnline)
{
	if (useOnline == false)
	{
		document.form1.txtSSLURL.value = '';
		document.form1.txtSSLURL.disabled = true;
	
	}
	else //true
	{
		//document.form1.txtSSLURL.value = '50';
		document.form1.txtSSLURL.disabled = false;

	}
}	

//Use Deposit
function UseDeposit(useOnline)
{
	if (useOnline == false)
	{
		document.form1.txtDepositPercent.value = '0';
		document.form1.txtDepositPercent.disabled = true;
		
		document.form1.radAllowEditDepositTrue.checked = false;
		document.form1.radAllowEditDepositFalse.checked = true;
		document.form1.radAllowEditDepositTrue.disabled = true;		
//		document.form1.radAllowEditDepositFalse.disabled = true;
		
	}
	else //true
	{
		document.form1.txtDepositPercent.value = '50';
		document.form1.txtDepositPercent.disabled = false;

		document.form1.radAllowEditDepositTrue.checked = true;
		document.form1.radAllowEditDepositFalse.checked = false;
		document.form1.radAllowEditDepositTrue.disabled = false;		
		document.form1.radAllowEditDepositFalse.disabled = false;												
	}
}	
//-->
</SCRIPT>
<TITLE>CRM - CRM Setup</TITLE>
</HEAD>
<BODY BGCOLOR="#AECAE6" TOPMARGIN="0" LEFTMARGIN="0">
<%
''get values seved in the file 
'Dim objTxtFile
'Set objTxtFile = Server.CreateObject("Scripting.FileSystemObject")
'Dim strAppPath
'Dim strFilePath
'strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))

'If Right(strAppPath,1) = "\" Then
'	strFilePath = "admin\crmsetup\setup\setup.txt"
'Else
'	strFilePath = "\admin\crmsetup\setup\setup.txt"
'End If
'Set objTxtFile = objTxtFile.OpenTextFile(strAppPath & strFilePath,1)

'strFile = objTxtFile.ReadAll
Dim strArSetups

'wma save only in second page
'strArSetups = Split(strFile," AND ", -1 , 1)
strArSetups = Split(Session("strLine")," AND ", -1 , 1)

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

'objTxtFile.Close
'Set objTxtFile = Nothing

%>

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
  <P> 
    <FORM action="SaveSetup1.asp" method=post id=form1 name=form1>
  
   <%
        
	  	on error resume next
		set conn=server.CreateObject("ADODB.connection")
		conn.Open Application("DataConnectionString")'strConn
		if len(err.Description) > 0 then
			Response.Write err.Description 
			Response.Write ("<br><a href=javascript:window.history.back();>Back</a>")
			Response.End
		end if
		on error goto 0
       set RScodes = server.createobject("ADODB.Recordset")
       'WAL_ add code for "TERMS"
       strsql = "select * from codes where (cdefcode='N' or cdefcode='D') and (cfld_name='SEASON' or cfld_name='CDIVISION' or cfld_name='CTERMCODE' or cfld_name='CARPTYPE') and crltfield='N'"
       on error resume next
       RScodes.open strsql,conn,2,4
       'set conn = nothing
        if len(err.Description) > 0 then
			if instr(1,err.Description,"does not exist") then
				   Response.Write "<font size=2>" & err.Description  & "<b><br> Please go back and correct the data path.</b><br>"
				   Response.Write ("<br><a href=javascript:window.history.back();>Back</a>")
				   Response.End
			else
				   Response.Write "<font size=2>" &  err.Description  & "<br>"
				   Response.Write ("<br><a href=javascript:window.history.back();>Back</a>")
				   Response.End
			end if
       end if
       on error goto 0
       rscodes.addnew
       rscodes.fields("cdefcode") = "N"
       rscodes.fields("cfld_name")= "SEASON"
       rscodes.fields("Cdiscrep") = "None"
       rscodes.fields("ccode_no") = "NONE"

       rscodes.addnew
       rscodes.fields("cdefcode") = "N"
       rscodes.fields("cfld_name")= "CDIVISION"
       rscodes.fields("Cdiscrep") = "None"
       rscodes.fields("ccode_no") = "NONE"
       
       
       RSCODES.filter = "cfld_name='SEASON' and cdefcode='N'"
             
        
                  
        IF isobject(Session("rs3")) Then
			strSeason = ""
			IF Not (Session("rs3").eof and Session("rs3").BOF) Then
				Session("rs3").movefirst
			End IF
			do while not Session("rs3").eof
				IF strSeason = "" Then
					strSeason = Trim(Session("rs3").fields("ccode_no").value)
				Else
					strSeason = strSeason & "," & Trim(Session("rs3").fields("ccode_no").value)
				End IF
				Session("rs3").movenext
			Loop
        Else
			strSeason = Session("StyleColor") 'session("ShowCatalogVal")
        End IF
        
        %>   
		
        <table bordercolor="#111111" border="1" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
		<TR> 
        <TD width="171">Select Season</TD>
        <TD width="563">
          <input type="text" name="txtSeason" size="40" value="<%=strSeason%>" readonly><input type="button" value="..." name="btnmover2" onclick="do_mover2()">
        </TD>
      </TR>
      <TR> 
        <TD width="171">Select Division</TD>
        <TD width="563"> 
          <select size="1" name="lstDivision" style="width:200"><%
			
			RSCODES.filter = "cfld_name='CDIVISION' and cdefcode='N'"
			do while not rscodes.EOF
			IF trim(Session("CatDivision")) = Trim(rscodes.fields("ccode_no").value) Then
   		      	response.write("<option value="&rscodes.fields("ccode_no").value & " selected>" & rscodes.fields("cdiscrep").value)
   		   Else
   		   		response.write("<option value=" & Trim(rscodes.fields("ccode_no").value) & " >" & rscodes.fields("cdiscrep").value)
   		   End IF  	
	          rscodes.movenext
          loop
          rscodes.filter=""
          rscodes.movefirst
          %>

          </select>
        </TD>
      </TR>

      <%IF Session("M_STYVIEW") = "P" Then%>
      <TR> 
        <TD width="171"> 
          Define Styles' Packs
        </TD>
        <TD width="563"> 
          <a href="packs/"  >Adjust Packs</a>
        </TD>
      </TR>

      <%Else%>
      <TR> 
        <TD width="171"> 
          Define Styles' Groups
        </TD>
        <TD width="563"><%
        IF isobject(Session("rs2")) Then
					strgrp = ""
					IF Not (Session("rs2").eof and Session("rs2").BOF) Then
						Session("rs2").movefirst
					End IF
					do while not Session("rs2").eof
						IF strgrp = "" Then
							strgrp = Trim(Session("rs2").fields("ccode_no").value)
						Else
							strgrp = strgrp & "," & Trim(Session("rs2").fields("ccode_no").value)
						End IF
						Session("rs2").movenext
					Loop
        Else
					strgrp = session("ShowCatalogVal")
        End IF
        %> 
          <input type="text" name="txtgrp" size="40" value="<%=strgrp%>" readonly><input type="button" value="..." name="btnmover1" onclick="do_mover()">
        </TD>
      </TR>


      <%End IF%>
      <TR> 
        <TD width="171">Default Term Code</TD>
        <TD width="563"> 
          <select size="1" name="lstTerm" style="width:200"><%
			'get def code 
			RSCODES.filter = "cfld_name='CTERMCODE' and cdefcode='D'"
			strDef = Trim(rscodes.fields("ccode_no").value)
			rscodes.filter=""
			rscodes.movefirst
			RSCODES.filter = "cfld_name='CTERMCODE' and cdefcode='N'"
			do while not rscodes.EOF
				IF Session("TermCode") = Trim(rscodes.fields("ccode_no").value) or strDef = Trim(rscodes.fields("ccode_no").value)Then
   				  	response.write("<option value="&rscodes.fields("ccode_no").value & " selected>" & rscodes.fields("cdiscrep").value)
   				Else
   		   			response.write("<option value=" & Trim(rscodes.fields("ccode_no").value) & " >" & rscodes.fields("cdiscrep").value)
   				End IF  	
	        rscodes.movenext
            loop
			rscodes.filter=""
			rscodes.movefirst
          %>

          </select>
        </TD>
      </TR>
      <TR> 
        <TD width="171">Default Warehouse</TD>
        <TD width="563"> 
		  <%
			Dim rsWarHous
			set rsWarHous = server.CreateObject ("ADODB.Recordset")
			rsWarHous.Open "Select * from warehous",conn
          %>
          <select size="1" name="lstWareHous" style="width:200">
			<%if rsWarHous.EOF then%>
				<option value="">None
			<%else%>
				<%do while not rsWarHous.EOF%>
					<option value="<%=rsWarhous("cwareCode")%>" <%IF trim(Session("WareHous")) = Trim(rsWarhous("cwareCode")) Then%> selected <%end if%>><%=rsWarhous("cdesc")%>
				<%rsWarHous.MoveNext 
				  loop%>
			<%end if%>
          </select>
        </TD>
      </TR>
      <tr></tr>
       <TR> 
        <TD width="171">Customer Login Using</TD>
        <TD width="563"> 
          <select size="1" name="lstCustomerLoginUsing" style="width:200">
			<option value="PhoneNumber" <%if trim(session("CustomerLoginUsing")) = "PhoneNumber" then %>selected<%end if%> >Phone Number</option>
			<option value="CustomerId" <%if trim(session("CustomerLoginUsing")) = "CustomerId" then %>selected<%end if%> >Customer Id</option>			
			<!--wal_127795 add option for multi user login-->
			<option value="User" <%if trim(session("CustomerLoginUsing")) = "User" then %>selected<%end if%> >Multi User</option>						
	       </select>
        </TD>
      </TR>
      
      <TR> 
        <TD width="171">Customer ID</TD>
        <TD width="563"> 
          <select size="1" name="lstCustomerCodeType" style="width:200">
			<option value="Sequential" <%if trim(session("CustomerCodeType")) = "Sequential" then %>selected<%end if%> >Sequential Id</option>
			<option value="Mannual" <%if trim(session("CustomerCodeType")) = "Mannual" then %>selected<%end if%> >Mannualy</option>
	       </select>
        </TD>
      </TR>      
      <tr></tr>
      <TR> 
        <TD width="171">Bank code for Cash Recite</TD>
        <TD width="563"> 
        <%Dim rsBanks
          set rsBanks = server.CreateObject ("Adodb.Recordset")
          rsBanks.Open "select * from apBanks order by cbnklndes",conn
        %>
          <select size="1" name="lstBanks" style="width:200"><%
			if not rsBanks.EOF then
				do while not rsBanks.EOF
					IF Session("BankCode") = Trim(rsBanks.fields("cbnkCode").value) Then
   					  	response.write("<option value="&rsBanks.fields("cbnkCode").value & " selected>" & rsBanks.fields("cbnklndes").value)
   					Else
   		   				response.write("<option value=" & Trim(rsBanks.fields("cbnkCode").Value) & " >" & rsBanks.fields("cbnklndes").value)
   					End IF  	
				rsBanks.movenext
				loop
			else
				response.write("<option value=''>None")
			end if
          %>

          </select>
        </TD>
      </TR>
      <TR> 
        <TD width="171">Payment Type</TD>
        <TD width="563"> 
          <select size="1" name="lstPay" style="width:200"><%
			'get def code 
			RSCODES.filter = "cfld_name='CARPTYPE' and cdefcode='D'"
			IF trim(Session("ARPTYPE")) = "" then
				strDef = Trim(rscodes.fields("ccode_no").value)
			Else
				strDef = trim(Session("ARPTYPE"))
			End if
			rscodes.filter=""
			rscodes.movefirst
			RSCODES.filter = "cfld_name='CARPTYPE' and cdefcode='N'"
			do while not rscodes.EOF
				IF trim(strDef) = Trim(rscodes.fields("ccode_no").value)Then
   		   			response.write("<option value='"&rscodes.fields("ccode_no").value & "' selected>" & rscodes.fields("cdiscrep").value)
   		   		Else
   		   			response.write("<option value=" & Trim(rscodes.fields("ccode_no").value) & " >" & rscodes.fields("cdiscrep").value)
   				End IF  	
	        rscodes.movenext
            loop
			rscodes.filter=""
			rscodes.movefirst
          %>

          </select>
        </TD>
      </TR>
      <TR> 
        <TD width="171">Use Taxes</TD>
        <TD width="563"> 
			<input type=radio name=radTaxes value="T" <%if session("UseTaxes") = "T" or session("UseTaxes") = "" then%>checked<%end if%>>Yes
			<input type=radio name=radTaxes value="F" <%if session("UseTaxes") = "F" then%>checked<%end if%>>No
     
		</TD>
      </TR>
      <TR> 
        <TD width="171">Priority Shipping charge</TD>
        <TD width="563"> 
			$<input type=text name=txtPri <%if trim(session("PriChg")) = "" then%>value="0"<%else%>value="<%=trim(session("PriChg"))%>"<%end if%> size=5>
		</TD>
      </TR>
       <TR> 
        <TD width="171">Drop Ship charge</TD>
        <TD width="563"> 
			$<input type=text name=txtDrop <%if trim(session("DropChg")) = "" then%>value="0"<%else%>value="<%=trim(session("DropChg"))%>"<%end if%> size=5>
		</TD>
      </TR>
       <TR> 
        <TD width="171">Use UPS charge</TD>
        <TD width="563"> 
			<input type=radio name=radUPS value="T" <%if session("UPS") = "T" or session("UPS") = "" then%>checked<%end if%>>Yes
			<input type=radio name=radUPS value="F" <%if session("UPS") = "F" then%>checked<%end if%>>No
     
		</TD>
      </TR>
      <TR> 
        <TD width="171">Show Weight field</TD>
        <TD width="563"> 
			<input type=radio name=radWeight value="T" <%if session("Weight") = "T" or session("Weight") = "" then%>checked<%end if%>>Yes
			<input type=radio name=radWeight value="F" <%if session("Weight") = "F" then%>checked<%end if%>>No
     
		</TD>
      </TR>
      <TR> 
        <TD width="171">Show Carton field</TD>
        <TD width="563"> 
			<input type=radio name=radCarton value="T" <%if session("Carton") = "T" or session("Carton") = "" then%>checked<%end if%>>Yes
			<input type=radio name=radCarton value="F" <%if session("Carton") = "F" then%>checked<%end if%>>No
     
		</TD>
      </TR>
      <tr></tr>
       <!--wal_127343 add setup for Cust PO-->     
      <TR> 
        <TD width="171">Use Customer PO#</TD>
        <TD width="563"> 
			<input type=radio name=radCustPO value="T" <%if session("CustPO") = "T" or session("CustPO") = "" then%>checked<%end if%>>Yes
			<input type=radio name=radCustPO value="F" <%if session("CustPO") = "F" then%>checked<%end if%>>No
        </TD>
      </TR>
      <%'wal_ issue# 128815 add Contract ID on setup[start]%>
      <TR> 
        <TD width="171">Use Contract Number</TD>
        <TD width="563"> 
			<input type=radio name=radContID value="T" <%if session("ContID") = "T" then%>checked<%end if%>>Yes
			<input type=radio name=radContID value="F" <%if session("ContID") = "F" or session("ContID") = ""then%>checked<%end if%>>No
        </TD>
      </TR>
      <%'wal_ issue# 128815 add Contract ID on setup[end]%>
       <TR> 
        <TD width="171">Show Ship Via(in Customer Section)</TD>
        <TD width="563"> 
			<input type=radio name=radShipVia value="T" <%if session("ShowShipVia") = "T" or session("ShowShipVia") = "" then%>checked<%end if%>>Yes
			<input type=radio name=radShipVia value="F" <%if session("ShowShipVia") = "F" then%>checked<%end if%>>No
        </TD>
      </TR>
      <TR> 
        <TD width="171">Show Expected Ship(in Customer Section)</TD>
        <TD width="563"> 
			<input type=radio name=radExpected value="T" <%if session("ShowExpected") = "T" or session("ShowExpected") = "" then%>checked<%end if%>>Yes
			<input type=radio name=radExpected value="F" <%if session("ShowExpected") = "F" then%>checked<%end if%>>No
        </TD>
      </TR>
      <TR> 
        <TD width="171">Add Order Size per Line</TD>
        <TD width="563"> 
			<input type=radio name=radLine value="T" <%if session("SizePerLine") = "T" or session("SizePerLine") = "" then%>checked<%end if%>>Yes
			<input type=radio name=radLine value="F" <%if session("SizePerLine") = "F" then%>checked<%end if%>>No
        </TD>
      </TR>
       <TR> 
        <TD width="171">Show OTS Availability</TD>
        <TD width="563"> 
			<input type=radio name=radOTS value="F" <%if session("ShowOTS") = "F" then%>checked<%end if%>>Yes
			<input type=radio name=radOTS value="T" <%if session("ShowOTS") = "T" or session("ShowOTS") = "" then %>checked<%end if%>>No
        </TD>
      </TR>
      <TR> 
        <TD width="171">Use Code filter in Catalog</TD>
        <TD width="563"> 
			<input type=radio name=radCode value="T" <%if session("UseCode") = "T" then%>checked<%end if%>>Yes
			<input type=radio name=radCode value="F" <%if session("UseCode") = "F" or session("UseCode") = "" then %>checked<%end if%>>No
        </TD>
      </TR>
      <TR> 
        <TD width="171">Low Availability Quantity</TD>
        <TD width="563"> 
			<input type=text name=txtQty <%if trim(session("LowQty")) = "" then%>value="2"<%else%>value="<%=trim(session("LowQty"))%>"<%end if%> size=5>
		</TD>
      </TR>
      <TR> 
        <TD width="171">No. of Expected Ship Days</TD>
        <TD width="563"> 
			<input type=text name=txtDays <%if trim(session("Days")) = "" then%>value="7"<%else%>value="<%=trim(session("Days"))%>"<%end if%> size=5>
		</TD>
      </TR>

	<!-- Online Payment Section -->      
      <TR> 
     </TR>
      <TR> 
        <TD width="171">Use Online Payment</TD>
        <TD width="563"> 
			<input type=radio name=radUseOnlinePayment value="T" <%if session("UseOnlinePayment") = "T" or session("UseOnlinePayment") = "" then%>checked<%end if%>  onclick="UseOnlinePayment(true)">Yes
			<input type=radio name=radUseOnlinePayment value="F" <%if session("UseOnlinePayment") = "F" then%>checked<%end if%>  onclick="UseOnlinePayment(false)">No
		</TD>
      </TR>
      <TR> 
        <TD width="171">Online Payment Gateway</TD>
        <TD width="563"> 
          <select size="1" name="lstGateWay" style="width:200" <%if session("UseOnlinePayment") = "F" then Response.Write "disabled" %>>
			<option value="PlugNPay.com (MemberShip Managment)" <%if session("GateWay") = "PlugNPay.com (MemberShip Managment)" then%>selected<%end if%> >PlugNPay.com (Membership Managment)</option>
			<!--option value="PlugNPay.com (Direct Link)" <%if session("GateWay") = "PlugNPay.com (Direct Link)" then%>selected<%end if%> >PlugNPay.com (Direct Link)</option-->			
	       </select>
        </TD>
      </TR>
      <TR> 
        <TD width="171">Use External SSL Server</TD>
        <TD width="563"> 
			<input type=radio name=radUseExtSSL id=radUseExtSSLTrue value="T" <%if session("UseExtSSL") = "T"  then%>checked<%end if%>  <%if session("UseOnlinePayment") = "F" then Response.Write "disabled" %>   onclick="UseExtSSL(true)">Yes
			<input type=radio name=radUseExtSSL id=radUseExtSSLFalse value="F" <%if session("UseExtSSL") = "F" or session("UseExtSSL") = "" then%>checked<%end if%>   onclick="UseExtSSL(false)">No
		</TD>
      </TR>
      <TR> 
        <TD width="171">SSL URL</TD>
        <TD width="563"> 
			<input type=text size=40 name=txtSSLURL <%if trim(session("SSLURL")) = "" then%>value=""<%else%>value="<%=trim(session("SSLURL"))%>"<%end if%> size=5  <%if session("UseOnlinePayment") = "F" or session("UseExtSSL") = "F" then Response.Write "disabled" %>> ex: https://www.ariany.com/SSLFolder
		</TD>
      </TR>                  
      <TR> 
        <TD width="171">Use Deposit</TD>
        <TD width="563"> 
			<input type=radio name=radUseDeposit id=radUseDepositTrue value="T" <%if session("UseDeposit") = "T" or session("UseDeposit") = "" then%>checked<%end if%>  <%if session("UseOnlinePayment") = "F" then Response.Write "disabled" %>   onclick="UseDeposit(true)">Yes
			<input type=radio name=radUseDeposit id=radUseDepositFalse value="F" <%if session("UseDeposit") = "F" then%>checked<%end if%>   onclick="UseDeposit(false)">No
		</TD>
      </TR>
      <TR> 
        <TD width="171">Deposit Percent</TD>
        <TD width="563"> 
			<input type=text name=txtDepositPercent <%if trim(session("DepositPercent")) = "" then%>value="50"<%else%>value="<%=trim(session("DepositPercent"))%>"<%end if%> size=5  <%if session("UseOnlinePayment") = "F" or session("UseDeposit") = "F" then Response.Write "disabled" %>>%
		</TD>
      </TR>            
      <TR> 
        <TD width="171">Allow Edit Deposit</TD>
        <TD width="563"> 
			<input type=radio name=radAllowEditDeposit id=radAllowEditDepositTrue value="T" <%if session("AllowEditDeposit") = "T" or session("AllowEditDeposit") = "" then%>checked<%end if%>  <%if  session("UseOnlinePayment") = "F" or session("UseDeposit") = "F"  then Response.Write "disabled" %>>Yes
			<input type=radio name=radAllowEditDeposit id=radAllowEditDepositFalse value="F" <%if session("AllowEditDeposit") = "F" then%>checked<%end if%> >No
		</TD>
      </TR>      
      <TR> 
        <TD width="171">Show Notepad field</TD>
        <TD width="563"> 
			<input type=radio name=radNote value="T" <%if session("ShowNotes") = "T"  then%>checked<%end if%>>Yes
			<input type=radio name=radNote value="F" <%if session("ShowNotes") = "F" or session("ShowNotes") = "" then%>checked<%end if%> >No
		</TD>
      </TR>     
	<!-- Online Payment Section -->            
	
      <TR> 
        <TD align="right"> 
          <INPUT id=reset1 name=reset1 type=reset value=Reset>
        </TD>
        <TD width="563"> 
          <INPUT id=submit1 name=submit1 type=submit value=Submit>
        </TD>
        
      </TR>
    </TABLE>
    <%
'    Response.Write "<font size=5>DepositPercent= "  & session("DepositPercent") & "<hr>"
'    Response.Write "<font size=5>UseDeposit= "  & session("UseDeposit") & "<hr>"
'    Response.Write "<font size=5>GateWay= "  & session("GateWay") & "<hr>"
'    Response.Write "<font size=5>AllowEditDeposit= "  & session("AllowEditDeposit") & "<hr>"
'    Response.Write "<font size=5>UseOnlinePayment= "  & session("UseOnlinePayment") & "<hr>" 
    %>
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
