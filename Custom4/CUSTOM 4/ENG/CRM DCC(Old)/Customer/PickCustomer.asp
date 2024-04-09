<%@LANGUAGE="VBSCRIPT" CODEPAGE="1252"%>

<%
Dim rsTypes
Dim rsTypes_numRows
MM_cnFox_STRING = Application("DataConnectionString")
Set rsTypes = Server.CreateObject("ADODB.Recordset")
rsTypes.ActiveConnection = MM_cnFox_STRING
rsTypes.Source = "SELECT distinct cwbtype  FROM suwbfed where not empty(cwbtype)"
rsTypes.CursorType = 0
rsTypes.CursorLocation = 2
rsTypes.LockType = 1
rsTypes.Open()

rsTypes_numRows = 0
%>
<%
Dim rsResults__MMColParam
rsResults__MMColParam = "1"
If (Request("lstType") <> "") Then 
  rsResults__MMColParam = Request("lstType")
End If
%>
<%
Dim rsResults
Dim rsResults_numRows

Set rsResults = Server.CreateObject("ADODB.Recordset")
rsResults.ActiveConnection = MM_cnFox_STRING
'prepare the filter
Dim strWhere
strWhere = ""
If Trim(Request("lstType")) = "*" or Trim(Request("lstType")) = ""Then
'Do Nothing
	strTypeWhr = " Where cwbtype <> '' "
Else
	strTypeWhr = " Where cwbtype = '" & Request("lstType") &"'"
End If
Dim strCompWhere
strCompWhere = ""
Dim strEmailWhere
strEmailWhere = ""
Dim strPhoneWhere
strPhoneWhere = ""
Dim strFaxWhere
strFaxWhere = ""
If Trim(Request("txtComp")) <> "" Then
	strCompWhere = " And cWCompany Like '%"& Trim(Request("txtComp")) &"%'"
End If
If Trim(Request("txtEmail")) <> "" Then
	strEmailWhere = " And cwe_mail = '"& Trim(Request("txtEmail")) &"'"
End If
If Trim(Request("txtPhone")) <> "" Then
	strPhoneWhere = " And cWphone ='"& Trim(Request("txtPhone")) &"'"
End If
If Trim(Request("txtFax")) <> "" Then
	strFaxWhere = " And cwFax = '"& Trim(Request("txtFax")) &"'"
End If

strWhere = strWhere & strTypeWhr & strCompWhere & strEmailWhere & strPhoneWhere & strFaxWhere

'rsResults.Source = "SELECT * FROM suwbfed WHERE cwbtype = '" + Replace(rsResults__MMColParam, "'", "''") + "' ORDER BY dadd_date ASC"
rsResults.Source = "SELECT * FROM suwbfed " & strWhere &" ORDER BY dadd_date ASC"

rsResults.CursorType = 0
rsResults.CursorLocation = 2
rsResults.LockType = 1
rsResults.Open()

rsResults_numRows = 0
%>
<%
Dim Repeat1__numRows
Dim Repeat1__index

Repeat1__numRows = 10
Repeat1__index = 0
rsResults_numRows = rsResults_numRows + Repeat1__numRows
%>
<%
'  *** Recordset Stats, Move To Record, and Go To Record: declare stats variables

Dim rsResults_total
Dim rsResults_first
Dim rsResults_last

' set the record count
rsResults_total = rsResults.RecordCount

' set the number of rows displayed on this page
If (rsResults_numRows < 0) Then
  rsResults_numRows = rsResults_total
Elseif (rsResults_numRows = 0) Then
  rsResults_numRows = 1
End If

' set the first and last displayed record
rsResults_first = 1
rsResults_last  = rsResults_first + rsResults_numRows - 1

' if we have the correct record count, check the other stats
If (rsResults_total <> -1) Then
  If (rsResults_first > rsResults_total) Then
    rsResults_first = rsResults_total
  End If
  If (rsResults_last > rsResults_total) Then
    rsResults_last = rsResults_total
  End If
  If (rsResults_numRows > rsResults_total) Then
    rsResults_numRows = rsResults_total
  End If
End If
%>
<%
Dim MM_paramName 
%>
<%
' *** Move To Record and Go To Record: declare variables

Dim MM_rs
Dim MM_rsCount
Dim MM_size
Dim MM_uniqueCol
Dim MM_offset
Dim MM_atTotal
Dim MM_paramIsDefined

Dim MM_param
Dim MM_index

Set MM_rs    = rsResults
MM_rsCount   = rsResults_total
MM_size      = rsResults_numRows
MM_uniqueCol = ""
MM_paramName = ""
MM_offset = 0
MM_atTotal = false
MM_paramIsDefined = false
If (MM_paramName <> "") Then
  MM_paramIsDefined = (Request.QueryString(MM_paramName) <> "")
End If
%>
<%
' *** Move To Record: handle 'index' or 'offset' parameter

if (Not MM_paramIsDefined And MM_rsCount <> 0) then

  ' use index parameter if defined, otherwise use offset parameter
  MM_param = Request.QueryString("index")
  If (MM_param = "") Then
    MM_param = Request.QueryString("offset")
  End If
  If (MM_param <> "") Then
    MM_offset = Int(MM_param)
  End If

  ' if we have a record count, check if we are past the end of the recordset
  If (MM_rsCount <> -1) Then
    If (MM_offset >= MM_rsCount Or MM_offset = -1) Then  ' past end or move last
      If ((MM_rsCount Mod MM_size) > 0) Then         ' last page not a full repeat region
        MM_offset = MM_rsCount - (MM_rsCount Mod MM_size)
      Else
        MM_offset = MM_rsCount - MM_size
      End If
    End If
  End If

  ' move the cursor to the selected record
  MM_index = 0
  While ((Not MM_rs.EOF) And (MM_index < MM_offset Or MM_offset = -1))
    MM_rs.MoveNext
    MM_index = MM_index + 1
  Wend
  If (MM_rs.EOF) Then 
    MM_offset = MM_index  ' set MM_offset to the last possible record
  End If

End If
%>
<%
' *** Move To Record: if we dont know the record count, check the display range

If (MM_rsCount = -1) Then

  ' walk to the end of the display range for this page
  MM_index = MM_offset
  While (Not MM_rs.EOF And (MM_size < 0 Or MM_index < MM_offset + MM_size))
    MM_rs.MoveNext
    MM_index = MM_index + 1
  Wend

  ' if we walked off the end of the recordset, set MM_rsCount and MM_size
  If (MM_rs.EOF) Then
    MM_rsCount = MM_index
    If (MM_size < 0 Or MM_size > MM_rsCount) Then
      MM_size = MM_rsCount
    End If
  End If

  ' if we walked off the end, set the offset based on page size
  If (MM_rs.EOF And Not MM_paramIsDefined) Then
    If (MM_offset > MM_rsCount - MM_size Or MM_offset = -1) Then
      If ((MM_rsCount Mod MM_size) > 0) Then
        MM_offset = MM_rsCount - (MM_rsCount Mod MM_size)
      Else
        MM_offset = MM_rsCount - MM_size
      End If
    End If
  End If

  ' reset the cursor to the beginning
  If (MM_rs.CursorType > 0) Then
    MM_rs.MoveFirst
  Else
    MM_rs.Requery
  End If

  ' move the cursor to the selected record
  MM_index = 0
  While (Not MM_rs.EOF And MM_index < MM_offset)
    MM_rs.MoveNext
    MM_index = MM_index + 1
  Wend
End If
%>
<%
' *** Move To Record: update recordset stats

' set the first and last displayed record
rsResults_first = MM_offset + 1
rsResults_last  = MM_offset + MM_size

If (MM_rsCount <> -1) Then
  If (rsResults_first > MM_rsCount) Then
    rsResults_first = MM_rsCount
  End If
  If (rsResults_last > MM_rsCount) Then
    rsResults_last = MM_rsCount
  End If
End If

' set the boolean used by hide region to check if we are on the last record
MM_atTotal = (MM_rsCount <> -1 And MM_offset + MM_size >= MM_rsCount)
%>
<%
' *** Go To Record and Move To Record: create strings for maintaining URL and Form parameters

Dim MM_keepNone
Dim MM_keepURL
Dim MM_keepForm
Dim MM_keepBoth

Dim MM_removeList
Dim MM_item
Dim MM_nextItem

' create the list of parameters which should not be maintained
MM_removeList = "&index="
If (MM_paramName <> "") Then
  MM_removeList = MM_removeList & "&" & MM_paramName & "="
End If

MM_keepURL=""
MM_keepForm=""
MM_keepBoth=""
MM_keepNone=""

' add the URL parameters to the MM_keepURL string
For Each MM_item In Request.QueryString
  MM_nextItem = "&" & MM_item & "="
  If (InStr(1,MM_removeList,MM_nextItem,1) = 0) Then
    MM_keepURL = MM_keepURL & MM_nextItem & Server.URLencode(Request.QueryString(MM_item))
  End If
Next

' add the Form variables to the MM_keepForm string
For Each MM_item In Request.Form
  MM_nextItem = "&" & MM_item & "="
  If (InStr(1,MM_removeList,MM_nextItem,1) = 0) Then
    MM_keepForm = MM_keepForm & MM_nextItem & Server.URLencode(Request.Form(MM_item))
  End If
Next

' create the Form + URL string and remove the intial '&' from each of the strings
MM_keepBoth = MM_keepURL & MM_keepForm
If (MM_keepBoth <> "") Then 
  MM_keepBoth = Right(MM_keepBoth, Len(MM_keepBoth) - 1)
End If
If (MM_keepURL <> "")  Then
  MM_keepURL  = Right(MM_keepURL, Len(MM_keepURL) - 1)
End If
If (MM_keepForm <> "") Then
  MM_keepForm = Right(MM_keepForm, Len(MM_keepForm) - 1)
End If

' a utility function used for adding additional parameters to these strings
Function MM_joinChar(firstItem)
  If (firstItem <> "") Then
    MM_joinChar = "&"
  Else
    MM_joinChar = ""
  End If
End Function
%>
<%
' *** Move To Record: set the strings for the first, last, next, and previous links

Dim MM_keepMove
Dim MM_moveParam
Dim MM_moveFirst
Dim MM_moveLast
Dim MM_moveNext
Dim MM_movePrev

Dim MM_urlStr
Dim MM_paramList
Dim MM_paramIndex
Dim MM_nextParam

MM_keepMove = MM_keepBoth
MM_moveParam = "index"

' if the page has a repeated region, remove 'offset' from the maintained parameters
If (MM_size > 1) Then
  MM_moveParam = "offset"
  If (MM_keepMove <> "") Then
    MM_paramList = Split(MM_keepMove, "&")
    MM_keepMove = ""
    For MM_paramIndex = 0 To UBound(MM_paramList)
      MM_nextParam = Left(MM_paramList(MM_paramIndex), InStr(MM_paramList(MM_paramIndex),"=") - 1)
      If (StrComp(MM_nextParam,MM_moveParam,1) <> 0) Then
        MM_keepMove = MM_keepMove & "&" & MM_paramList(MM_paramIndex)
      End If
    Next
    If (MM_keepMove <> "") Then
      MM_keepMove = Right(MM_keepMove, Len(MM_keepMove) - 1)
    End If
  End If
End If

' set the strings for the move to links
If (MM_keepMove <> "") Then 
  MM_keepMove = MM_keepMove & "&"
End If

MM_urlStr = Request.ServerVariables("URL") & "?" & MM_keepMove & MM_moveParam & "="

MM_moveFirst = MM_urlStr & "0"
MM_moveLast  = MM_urlStr & "-1"
MM_moveNext  = MM_urlStr & CStr(MM_offset + MM_size)
If (MM_offset - MM_size < 0) Then
  MM_movePrev = MM_urlStr & "0"
Else
  MM_movePrev = MM_urlStr & CStr(MM_offset - MM_size)
End If

%>
<SCRIPT LANGUAGE=javascript>
<!--
function ReturnResults(val,Name,Mail)
{
	var val;
	opener.document.frmadd.txtName.value = val;
	opener.document.frmadd.txtContact.value = Name;
	opener.document.frmadd.txtMail.value = Mail;
	opener.document.frmadd.action = 'repAddcust.asp?CustID='+val;
	opener.document.frmadd.submit();
	window.close();
}
//-->
</SCRIPT>

<SCRIPT RUNAT=SERVER LANGUAGE=VBSCRIPT>					
function DoDateTime(str, nNamedFormat, nLCID)				
	dim strRet								
	dim nOldLCID								
										
	strRet = str								
	If (nLCID > -1) Then							
		oldLCID = Session.LCID						
	End If									
										
	On Error Resume Next							
										
	If (nLCID > -1) Then							
		Session.LCID = nLCID						
	End If									
										
	If ((nLCID < 0) Or (Session.LCID = nLCID)) Then				
		strRet = FormatDateTime(str, nNamedFormat)			
	End If									
										
	If (nLCID > -1) Then							
		Session.LCID = oldLCID						
	End If									
										
	DoDateTime = strRet							
End Function									
</SCRIPT>									
<html>
<head>
<title>Pick Prospect</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<script language="JavaScript" type="text/JavaScript">
<!--
function MM_callJS(jsStr) { //v2.0
  return eval(jsStr)
}
function chkFrm()
{
	if (document.frmadd.txtEmail.value != "")
	{
		var strEmail=document.frmadd.txtEmail .value;
		var strLength=strEmail.length;
		var strIndex=strEmail.indexOf('@',0);
		if((1 > strIndex) || (strIndex > strEmail.lastIndexOf('.')) || (strLength == ( strEmail.lastIndexOf('.')+1)))
		{
				alert("Enter valid email!");
				document.frmadd.txtEmail .focus ();
				return false;
		}
	}
	var object;
	object = document.frmadd.txtPhone;
	if (object.value != "")
	{
		if (!checkNumber(object))
		{
			alert(" Please Enter Numbers Only!!");
			object.focus();
			return(false);
		}
	}	
	if ((document.frmadd.txtFax.value)!= "")
	{
		if (!checkNumber(document.frmadd.txtFax))
		{
			alert(" Please Enter Numbers Only!!");
			document.frmadd.txtFax.focus();
			return(false);
		}
	}
}
function checkNumber(stringObject)
{		
	var Flag;
	Flag=false ;
	for (x=0;x<=stringObject.value.length-1;x++)
	{
		if ((stringObject.value.charAt(x) == "-") || (stringObject.value.charAt(x) == "/") || (stringObject.value.charAt(x) == "."))
			return true;
		else
		{
			if (isNaN(stringObject.value.charAt(x)))
				return false;
		}
	}
	return true;
}
//-->
</script>
</head>

<body>
<form name="frmadd" method="post" action="PickCustomer.asp">
  <table width="100%" border="0">
    <tr> 
      <td colspan="8"><div align="center"><strong><font size="2" face="Verdana, Arial, Helvetica, sans-serif">Pick 
          customer info.</font></strong></div></td>
    </tr>
    <tr> 
      <td><strong><font size="2" face="Verdana, Arial, Helvetica, sans-serif">Type</font></strong></td>
      <td> <strong><font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
        <select name="lstType" id="lstType">
          <option value="*" <%If (Not isNull(request("lstType"))) Then If ("*" = CStr(request("lstType"))) Then Response.Write("SELECTED") : Response.Write("")%>>All</option>
          <%
While (NOT rsTypes.EOF)
%>
          <option value="<%=(rsTypes.Fields.Item("cwbtype").Value)%>" <%If (Not isNull(request("lstType"))) Then If (CStr(rsTypes.Fields.Item("cwbtype").Value) = CStr(request("lstType"))) Then Response.Write("SELECTED") : Response.Write("")%> ><%=(rsTypes.Fields.Item("cwbtype").Value)%></option>
          <%
  rsTypes.MoveNext()
Wend
If (rsTypes.CursorType > 0) Then
  rsTypes.MoveFirst
Else
  rsTypes.Requery
End If
%>
        </select>
        </font></strong></td>
      <td><strong><font size="2" face="Verdana, Arial, Helvetica, sans-serif">Company</font></strong></td>
      <td > <strong><font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
        <input name="txtComp" type="text" id="txtComp" value="<%= Request("txtComp") %>">
        </font></strong></td>
      <td ><font size="2" face="Arial, Helvetica, sans-serif"><strong>Email</strong></font></td>
      <td ><font size="2" face="Arial, Helvetica, sans-serif"> 
        <input name="txtEmail" type="text" id="txtEmail" value="<%= Request("txtEmail") %>">
        </font></td>
      <td rowspan="2"><div align="center"><strong><font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
          <input type="submit" name="Submit" value="Submit" onclick="chkFrm();">
          </font></strong></div></td>
    </tr>
    <tr> 
      <td colspan="2">&nbsp;</td>
      <td><strong><font size="2" face="Arial, Helvetica, sans-serif">Phone</font></strong></td>
      <td><input name="txtPhone" type="text" id="txtPhone" value="<%= Request("txtPhone") %>" ></td>
      <td><strong><font size="2" face="Arial, Helvetica, sans-serif">Fax</font></strong></td>
      <td><input name="txtFax" type="text" id="txtFax" value="<%= Request("txtFax") %>" ></td>
      <td>&nbsp;</td>
    </tr>
    <tr> 
      <td colspan="8"> <table width="100%" border="0">
          <tr> 
            <td><strong><font size="2" face="Verdana, Arial, Helvetica, sans-serif">Type</font></strong></td>
            <td><strong><font size="2" face="Verdana, Arial, Helvetica, sans-serif">Company</font></strong></td>
            <td><strong><font size="2" face="Verdana, Arial, Helvetica, sans-serif">Contact</font></strong></td>
            <td><strong><font size="2" face="Verdana, Arial, Helvetica, sans-serif">Date</font></strong></td>
          </tr>
          <%if rsResults.EOF then%>
			<tr> 
            <td colspan="4"> <div align="center"><font color=red> No records</font></div>
            </td>
            </tr>
          <%else
While ((Repeat1__numRows <> 0) AND (NOT rsResults.EOF)) 
%>
          <tr> 
            <td><font size="2" face="Verdana, Arial, Helvetica, sans-serif"><%=(rsResults.Fields.Item("cwbtype").Value)%></font></td>
            <td><font size="2" face="Verdana, Arial, Helvetica, sans-serif"><a href="javascript:ReturnResults('<%=(rsResults.Fields.Item("cwcompany").Value)%>','<%=(rsResults.Fields.Item("cwname").Value)%>','<%=(rsResults.Fields.Item("cwe_mail").Value)%>')"><%=(rsResults.Fields.Item("cwcompany").Value)%></a></font></td>
            <td><font size="2" face="Verdana, Arial, Helvetica, sans-serif"><%=(rsResults.Fields.Item("cwname").Value)%></font></td>
            <td><font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
              <%If rsResults.Fields.Item("dadd_date").Value <> "12:00:00 AM" Then Response.Write(rsResults.Fields.Item("dadd_date").Value) Else Response.Write("N/A") %>
              </font></td>
          </tr>
          <% 
  Repeat1__index=Repeat1__index+1
  Repeat1__numRows=Repeat1__numRows-1
  rsResults.MoveNext()
Wend
end if
%>
          <tr> 
            <td colspan="4"> <div align="center"> 
                <table border="0" width="50%" align="center">
                  <tr> 
                    <td width="23%" align="center"> <font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
                      <% If MM_offset <> 0 Then %>
                      <a href="<%=MM_moveFirst%>">First</a> 
                      <% End If ' end MM_offset <> 0 %>
                      </font></td>
                    <td width="31%" align="center"> <font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
                      <% If MM_offset <> 0 Then %>
                      <a href="<%=MM_movePrev%>">Previous</a> 
                      <% End If ' end MM_offset <> 0 %>
                      </font></td>
                    <td width="23%" align="center"> <font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
                      <% If Not MM_atTotal Then %>
                      <a href="<%=MM_moveNext%>">Next</a> 
                      <% End If ' end Not MM_atTotal %>
                      </font></td>
                    <td width="23%" align="center"> <font size="2" face="Verdana, Arial, Helvetica, sans-serif"> 
                      <% If Not MM_atTotal Then %>
                      <a href="<%=MM_moveLast%>">Last</a> 
                      <% End If ' end Not MM_atTotal %>
                      </font></td>
                  </tr>
                </table>
              </div></td>
          </tr>
        </table></td>
    </tr>
    <tr> 
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td colspan="3">&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
    </tr>
    <tr> 
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td colspan="3">&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
    </tr>
  </table>
</form>
</body>
</html>
<%
rsTypes.Close()
Set rsTypes = Nothing
%>
<%
rsResults.Close()
Set rsResults = Nothing
%>

