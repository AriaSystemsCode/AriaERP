<%
'WAL_05/17/2004 add code tp read values in setup file[start]
'get values seved in the file 
Dim strAppPath
Dim strFilePath

strAppPath = Trim(Request.ServerVariables("APPL_PHYSICAL_PATH"))

If Right(strAppPath,1) = "\" Then
	strFilePath = "admin\crmsetup\setup\setup.txt"
Else
	strFilePath = "\admin\crmsetup\setup\setup.txt"
End If

'Response.Write(strAppPath & strFilePath)
Dim objFile
Set objFile = Server.CreateObject("Scripting.FileSystemObject")
on error resume next
Set objTxtFile = objFile.OpenTextFile(strAppPath & strFilePath,1)
	
If err.number<>0 then
		response.write "You must setup CRM first."
		response.write "<br><a href=""../CrmSetup\crmSetup.asp"">Setup CRM</a>"
		Response.End
else
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
End If
On Error GoTo 0
'WAL_05/17/2004 add code tp read values in setup file[end]
%>
<HTML>
<HEAD>
<TITLE>CRM - Admin</TITLE>
<LINK rel="stylesheet" type="text/css" href="../../images/<%=Session("THEME")%>/Common.css">
</HEAD>
<BODY BGCOLOR="#aecae6" TOPMARGIN="0" LEFTMARGIN="0">
<!--TABLE WIDTH=95% BORDER=0 CELLPADDING=0 CELLSPACING=0 align=center>
  <TR> 
	<td>
		<div border=1 id="Layer8" style="position:absolute; left:0; top:0; width:700; height:69; z-index:35; background-image: url(image/heder.JPG); layer-background-image: url(image/heder.JPG); border: 1px none #000000">
			<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"    codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=5,0,0,0" width="700" height="69" id=ShockwaveFlash1>
    <param name=movie value="../../banner.swf">
    <param name=quality value=high>
    <embed src="../../banner.swf" quality=high pluginspage="http://www.macromedia.com/shockwave/download/index.cgi?P1_Prod_Version=ShockwaveFlash" type="application/x-shockwave-flash" width="750" height="69">
    </embed> 
  </object>
		</div>
	</td>
  </TR>
</TABLE><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR><BR-->
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

  <%
 Set objXML = Server.CreateObject("Msxml2.DOMDocument")
 Set objLst = Server.CreateObject("Msxml2.DOMDocument")
 Set objHdl = Server.CreateObject("Msxml2.DOMDocument")

 CurFName = Request.ServerVariables ("SCRIPT_NAME")
 CurFName = mid(CurFName,instrrev(CurFName,"/")+1)

 path = server.MapPath (CurFName)
 path = mid(path,1,instrrev(path,"\"))

 set ado_stream = Server.CreateObject("ADODB.Stream")
 ado_stream.Type = 2
 ado_stream.Charset = "ascii"
 ado_stream.LineSeparator = -1
 ado_stream.Open 
 
 objXML.async = False
 objXML.Load (Server.MapPath("theme.xml"))

 If objXML.parseError.errorCode <> 0 Then
	 'handle the error
	 Response.Write "<b>Error at line: "
	 Response.Write objXML.parseError.Line & "</b><br>"
 End If
		
 Set objLst = objXML.getElementsByTagName("NAME")
 noOfHeadlines = objLst.length
 
 For i=0 To noOfHeadlines - 1 ' Loop for all THEMES
  Set objHdl = objLst.item(i)
  IF strHeaders = "" then
		strHeaders = objHdl.childNodes(0).text
  Else
		strHeaders = strHeaders & "," & objHdl.childNodes(0).text 
  End IF
 Next
 
 arrThems = Split(strHeaders,",")
%>
  <BR>
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
	<TD colspan=2 align=left><b>Fill in the fields below to configure the CRM layout</b></TD>
	  
  </TR>
  </Table>
</div>

<%
strAppUserVar = session("strAppUserVar")
If Trim(Application(strAppUserVar & "Lvl")) = "O" And InStr(1,Application(strAppUserVar),"ALAYOUT") <= 0 Then
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
		
		
<Form name=mainfrm method=post action="savetheme.asp">
  <div align="left">
    <table bordercolor="#111111" border="1" align=center width=95% style="border-collapse: collapse" cellpadding="0" cellspacing="0">
<tr> 
        <td width="317" >Choose Theme</td>
        <td width="810" >
          <select name="lstTheme">
            <%
      For intCount = 0 to ubound(arrThems)
				IF Session("THEME") = arrThems(intCount) Then
					Response.Write("<option value=""" & arrThems(intCount) & """ selected>" & arrThems(intCount))
				Else
					Response.Write("<option value=""" & arrThems(intCount) & """>" & arrThems(intCount))
				End IF
				
      Next
      
      %>
          </Select></td>
      </tr>
        <TR> 
     </TR>
      <%'WAL_E302083,1 add input text for Customer and Store Fields [start]%>
      <tr> 
        <td width="317" >Customer Field Label</td>
        <td width="810" > 
          <input type=text name = "txtCust" maxlength="30"<%if trim(Session("CustField")) = "" then%>value="Customer"<%else%>value="<%=Session("CustField")%>"<%end if%>> 
          <input type=hidden name = "hidCust" maxlength="30"<%if trim(Session("CustField")) = "" then%>value="Customer"<%else%>value="<%=Session("CustField")%>"<%end if%>> 
        </td>
      </tr>
      <tr> 
        <td width="317" >BookKeeper Field Label</td>
        <td width="810" > 
          <input type=text name = "txtKeeper" maxlength="30"<%if trim(Session("keeperField")) = "" then%>value="Bookkeeper"<%else%>value="<%=Session("keeperField")%>"<%end if%>> 
          <input type=hidden name = "hidKeeper" maxlength="30"<%if trim(Session("keeperField")) = "" then%>value="Bookkeeper"<%else%>value="<%=Session("keeperField")%>"<%end if%>> 
        </td>
      </tr>
       <tr> 
        <td width="317" >Buyer Field Label</td>
        <td width="810" > 
          <input type=text name = "txtBuyer" maxlength="30"<%if trim(Session("BuyerField")) = "" then%>value="Buyer"<%else%>value="<%=Session("BuyerField")%>"<%end if%>> 
          <input type=hidden name = "hidBuyer" maxlength="30"<%if trim(Session("BuyerField")) = "" then%>value="Buyer"<%else%>value="<%=Session("BuyerField")%>"<%end if%>> 
        </td>
      </tr>
     
       <tr> 
        <td width="317" >DBA Field Label</td>
        <td width="810" > 
          <input type=text name = "txtDBA" maxlength="30"<%if trim(Session("DBAField")) = "" then%>value="DBA"<%else%>value="<%=Session("DBAField")%>"<%end if%>> 
          <input type=hidden name = "hidDBA" maxlength="30"<%if trim(Session("DBAField")) = "" then%>value="DBA"<%else%>value="<%=Session("DBAField")%>"<%end if%>> 
        </td>
      </tr>
       <tr> 
        <td width="317" >Phone Field Label</td>
        <td width="810" > 
          <input type=text name = "txtPhone" maxlength="30"<%if trim(Session("PhoneField")) = "" then%>value="Phone"<%else%>value="<%=Session("PhoneField")%>"<%end if%>> 
          <input type=hidden name = "hidPhone" maxlength="30"<%if trim(Session("PhoneField")) = "" then%>value="Phone"<%else%>value="<%=Session("PhoneField")%>"<%end if%>> 
        </td>
      </tr>
       <tr> 
        <td width="317" >Phone2 Field Label</td>
        <td width="810" > 
          <input type=text name = "txtPhone2" maxlength="30"<%if trim(Session("Phone2Field")) = "" then%>value="Phone2"<%else%>value="<%=Session("Phone2Field")%>"<%end if%>> 
          <input type=hidden name = "hidPhone2" maxlength="30"<%if trim(Session("Phone2Field")) = "" then%>value="Phone2"<%else%>value="<%=Session("Phone2Field")%>"<%end if%>> 
        </td>
      </tr>
       <tr> 
        <td width="317" >Fax Field Label</td>
        <td width="810" > 
          <input type=text name = "txtFax" maxlength="30"<%if trim(Session("FaxField")) = "" then%>value="Fax"<%else%>value="<%=Session("FaxField")%>"<%end if%>> 
          <input type=hidden name = "hidFax" maxlength="30"<%if trim(Session("FaxField")) = "" then%>value="Fax"<%else%>value="<%=Session("FaxField")%>"<%end if%>> 
        </td>
      </tr>
      
        <TR> 
     </TR>
    
      <tr> 
        <td width="317" >Catalog Field Label</td>
        <td width="810" > 
          <input type=text name = "txtCatalog" maxlength="30"<%if trim(Session("CatalogField")) = "" then%>value="Catalog"<%else%>value="<%=Session("CatalogField")%>"<%end if%>> 
          <input type=hidden name = "hidCatalog" maxlength="30"<%if trim(Session("CatalogField")) = "" then%>value="Catalog"<%else%>value="<%=Session("CatalogField")%>"<%end if%>> 
        </td>
      </tr>
       <tr> 
        <td width="317" >OTS Field Label</td>
        <td width="810" > 
          <input type=text name = "txtOTS" maxlength="30"<%if trim(Session("OTSField")) = "" then%>value="OTS"<%else%>value="<%=Session("OTSField")%>"<%end if%>> 
          <input type=hidden name = "hidOTS" maxlength="30"<%if trim(Session("OTSField")) = "" then%>value="OTS"<%else%>value="<%=Session("OTSField")%>"<%end if%>> 
        </td>
      </tr>
       <tr></tr>
      <tr> 
        <td width="317" >Store Field Label</td>
        <td width="810" > 
          <input type=text name = "txtStore" maxlength="30"<%if trim(Session("StoreField")) = "" then%>value="Store"<%else%>value="<%=Session("StoreField")%>"<%end if%>> 
          <input type=hidden name = "hidStore" maxlength="30"<%if trim(Session("StoreField")) = "" then%>value="Store"<%else%>value="<%=Session("StoreField")%>"<%end if%>> 
        </td>
      </tr>
       <%'wal_ issue# 128815 add Contract ID on setup[start]%>
       <tr> 
        <td width="317" >Contract Field Label</td>
        <td width="810" > 
          <input type=text name = "txtContract" maxlength="30" <%if trim(Session("ContractField")) = "" then%>value="Contract#"<%else%>value="<%=Session("ContractField")%>"<%end if%>> 
          <input type=hidden name = "hidContract" maxlength="30" <%if trim(Session("ContractField")) = "" then%>value="Contract#"<%else%>value="<%=Session("ContractField")%>"<%end if%>> 
        </td>
      </tr>
       <tr> 
        <td width="317" >City Field Label</td>
        <td width="810" > 
          <input type=text name = "txtCity" maxlength="30"<%if trim(Session("CityField")) = "" then%>value="City"<%else%>value="<%=Session("CityField")%>"<%end if%>> 
          <input type=hidden name = "hidCity" maxlength="30"<%if trim(Session("CityField")) = "" then%>value="City"<%else%>value="<%=Session("CityField")%>"<%end if%>> 
        </td>
      </tr>
       <tr> 
        <td width="317" >State Field Label</td>
        <td width="810" > 
          <input type=text name = "txtState" maxlength="30"<%if trim(Session("StateField")) = "" then%>value="State"<%else%>value="<%=Session("StateField")%>"<%end if%>> 
          <input type=hidden name = "hidState" maxlength="30"<%if trim(Session("StateField")) = "" then%>value="State"<%else%>value="<%=Session("StateField")%>"<%end if%>> 
        </td>
      </tr>
       <tr> 
        <td width="317" >Zip Code Field Label</td>
        <td width="810" > 
          <input type=text name = "txtZip" maxlength="30"<%if trim(Session("ZipField")) = "" then%>value="Zip Code"<%else%>value="<%=Session("ZipField")%>"<%end if%>> 
          <input type=hidden name = "hidZip" maxlength="30"<%if trim(Session("ZipField")) = "" then%>value="Zip Code"<%else%>value="<%=Session("ZipField")%>"<%end if%>> 
        </td>
      </tr>
      <tr> 
        <td width="317" >Billing Field Label</td>
        <td width="810" > 
          <input type=text name = "txtBilling" maxlength="30"<%if trim(Session("BillingField")) = "" then%>value="Billing"<%else%>value="<%=Session("BillingField")%>"<%end if%>> 
          <input type=hidden name = "hidBilling" maxlength="30"<%if trim(Session("BillingField")) = "" then%>value="Billing"<%else%>value="<%=Session("BillingField")%>"<%end if%>> 
        </td>
      </tr>
      <tr> 
        <td width="317" >Ship To Field Label</td>
        <td width="810" > 
          <input type=text name = "txtShipTo" maxlength="30"<%if trim(Session("ShipToField")) = "" then%>value="Ship To"<%else%>value="<%=Session("ShipToField")%>"<%end if%>> 
          <input type=hidden name = "hidShipTo" maxlength="30"<%if trim(Session("ShipToField")) = "" then%>value="Ship To"<%else%>value="<%=Session("ShipToField")%>"<%end if%>> 
        </td>
      </tr>
      <tr> 
        <td width="317" >Shipping Field Label</td>
        <td width="810" > 
          <input type=text name = "txtShipping" maxlength="30"<%if trim(Session("ShippingField")) = "" then%>value="Shipping"<%else%>value="<%=Session("ShippingField")%>"<%end if%>> 
          <input type=hidden name = "hidShipping" maxlength="30"<%if trim(Session("ShippingField")) = "" then%>value="Shipping"<%else%>value="<%=Session("ShippingField")%>"<%end if%>> 
        </td>
      </tr>
	   <tr> 
        <td width="317" >Note1 Field Label</td>
        <td width="810" > 
          <input type=text name = "txtNote1" maxlength="30"<%if trim(Session("Note1Field")) = "" then%>value="Note1"<%else%>value="<%=Session("Note1Field")%>"<%end if%>> 
          <input type=hidden name = "hidNote1" maxlength="30"<%if trim(Session("Note1Field")) = "" then%>value="Note1"<%else%>value="<%=Session("Note1Field")%>"<%end if%>> 
        </td>
      </tr>
       <tr> 
        <td width="317" >Note2 Field Label</td>
        <td width="810" > 
          <input type=text name = "txtNote2" maxlength="30"<%if trim(Session("Note2Field")) = "" then%>value="Note2"<%else%>value="<%=Session("Note2Field")%>"<%end if%>> 
          <input type=hidden name = "hidNote2" maxlength="30"<%if trim(Session("Note2Field")) = "" then%>value="Note2"<%else%>value="<%=Session("Note2Field")%>"<%end if%>> 
        </td>
      </tr>
       <tr> 
        <td width="317" >Style Field Label</td>
        <td width="810" > 
          <input type=text name = "txtStyle" maxlength="30"<%if trim(Session("StyleField")) = "" then%>value="Style"<%else%>value="<%=Session("StyleField")%>"<%end if%>> 
          <input type=hidden name = "hidStyle" maxlength="30"<%if trim(Session("StyleField")) = "" then%>value="Style"<%else%>value="<%=Session("StyleField")%>"<%end if%>> 
        </td>
      </tr>
      <tr></tr>
      <tr> 
        <td width="317" >Merchandise Amount Field Label</td>
        <td width="810" > 
          <input type=text name = "txtAmount" maxlength="30"<%if trim(Session("AmountField")) = "" then%>value="Merchandise Amount"<%else%>value="<%=Session("AmountField")%>"<%end if%>> 
          <input type=hidden name = "hidAmount" maxlength="30"<%if trim(Session("AmountField")) = "" then%>value="Merchandise Amount"<%else%>value="<%=Session("AmountField")%>"<%end if%>> 
        </td>
      </tr>
      <tr> 
        <td width="317" >Freight Field Label</td>
        <td width="810" > 
          <input type=text name = "txtFreight" maxlength="30"<%if trim(Session("FreightField")) = "" then%>value="Freight"<%else%>value="<%=Session("FreightField")%>"<%end if%>> 
          <input type=hidden name = "hidFreight" maxlength="30"<%if trim(Session("FreightField")) = "" then%>value="Freight"<%else%>value="<%=Session("FreightField")%>"<%end if%>> 
        </td>
      </tr>
      <tr> 
        <td width="317" >Balance To Pay Field Label</td>
        <td width="810" > 
          <input type=text name = "txtBalance" maxlength="30"<%if trim(Session("BalanceField")) = "" then%>value="Balance To Pay"<%else%>value="<%=Session("BalanceField")%>"<%end if%>> 
          <input type=hidden name = "hidBalance" maxlength="30"<%if trim(Session("BalanceField")) = "" then%>value="Balance To Pay"<%else%>value="<%=Session("BalanceField")%>"<%end if%>> 
        </td>
      </tr>
      <%'WAL_E302083,1 add input text for Customer and Store Fields [start]%>
      <tr> 
       
      </tr>
      <tr> 
        <td width="317" align=right >
        <input type="reset" name="Reset" value="Reset">
        </td>
        <td width="810" align=left>
		  <input type=submit name=btn1 value="Submit">
        </td>
      </tr>
    </table>
  </div>
</form>
</BODY>
</HTML>

