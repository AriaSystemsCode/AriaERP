<%@ Language=VBScript %>
<%
Response.Buffer =true
Response.Expires=-1
%>


<%
'////////////FIll Session Array With Selected Customer ID//////////
Dim intCtr
Dim intCtrT
Dim arrCustID()
Dim arrCustName()
Dim TempArray()
Dim intSeleCustID
Dim intSeleCustID_Old

intSeleCustID=0
intSeleCustID_Old=0
intCtr=Session("RintCounter")
intSeleCustID=Session("RintSeleCustID")
intSeleCustID_Old=Session("RintSeleCustID")

 
If intSeleCustID <> 0 Then'and intSeleCustID<>"" Then 
' There are already one or more customer id stored in this session
	ReDim arrCustID(intCtr+intSeleCustID ,2)
	Redim TempArray(intCtr+intSeleCustID ,2)
else
' Session is empty..no customer ids are stored till now
    intSeleCustID=0
    intSeleCustID_Old=0
	ReDim arrCustID(intCtr ,2)
	Redim TempArray(intCtr ,2)
End IF
Session("TempCust")=TempArray'/////Temporary Array to Keep checked Customers//////////
'/////We should fill aaCustID with old stored Cust IDs before we fill
'/////it with new selected ids
if intSeleCustID>0 then
	For intCtrT=0 To intSeleCustID-1
	       TempArray(intCtrT ,0)=Session("RCustID")(intCtrT ,0)
	       TempArray(intCtrT ,1)=Session("RCustID")(intCtrT ,1)
	       arrCustID(intCtrT ,0)=Session("RCustID")(intCtrT ,0)
	       arrCustID(intCtrT ,1)=Session("RCustID")(intCtrT ,1)
	       'Response.Write ("ARDxxxxx"&TempArray(intCtrT ,1))
	Next
	Session("TempCust")=TempArray
	'Response.Write (session("TempCust")(0 ,0))
end if

'//////////////Add New Selected IDs to arrCustID/////////////////////////////////////////

If intSeleCustID=0  Then
	'Response.Write ("<br>HI FIRST")
	For intCtrT=1 To intCtr
	  If Request.Form("ID" & intCtrT)<>"" Then
	     arrCustID(intSeleCustID ,0)=Request.Form("ID" & intCtrT)
	     arrCustID(intSeleCustID ,1)=Request.Form("Name" & intCtrT)
	     intSeleCustID=intSeleCustID+1
	  End If
	Next
Else
'	Response.Write ("<br>HI NOT FIRST")
	Dim test
	For intCtrT=1 To intCtr
	   test=true
	   test=CheckID(Request.Form("ID" & intCtrT))
	 '  Response.Write ("<BR>test="&test)
	  If Request.Form("ID" & intCtrT)<>"" Then
		 If test then
	     arrCustID(intSeleCustID ,0)=Request.Form("ID" & intCtrT)
	     arrCustID(intSeleCustID ,1)=Request.Form("Name" & intCtrT)
	     intSeleCustID=intSeleCustID+1
	     End If
	  End If
	Next
End If

'Response.Write ("<HR>After intSeleCustID="&intSeleCustID&"<HR>")
Session("RCustID")=arrCustID  '//Fill Session Array With New Array
Session("RintSeleCustID")=intSeleCustID 
''////////////FIll Session Array With Selected Customer ID//////////

'/////////////////Check If This ID is already stored in the session array or not
'/////////////////before saving it////////////////////
Dim intTest
function CheckID(custID)
CheckID=true
'Response.Write("<BR>intSeleCustID="&intSeleCustID)
	if intSeleCustID>0 then
		For intTest=0 To intSeleCustID_Old - 1
			if (custID =Session("RCustID")(intTest ,0)) then
			    CheckID=false
			    Exit Function 
			end if   
		Next
	end if

end function
'////////////////////////////////////////////////////////
%>

<%
Response.Write(Request.QueryString ("XX"))
strBeginDate=Request.Form ("txtBegindate")
strEndDate=Request.Form ("txtEnddate")
Dim strConn
Dim strSelect
Dim strCustCriteria
Dim rsCustomer,rsCategory
Dim intCounter,ctr
Dim strNextPrev,rowcount
Dim strFirstRec,strLastRec

strConn= "dsn=WebTrack1;uid=aria;pwd=aria;Deleted=Yes"
Session("ConnectionString") = strConn
Set Conn = Server.CreateObject("ADODB.Connection")
Conn.Open strConn
strCustCriteria=Request.Form("txtCust")
PageCounter=Request.Form("PageCounter")

strSelect=""
strSelect="select * from customer where status = 'A' and type = 'M' and Ccont_code = 'USA' and Account like '"&Ucase(strCustCriteria)&"%' order by Account"

Set rsCustomer = Server.CreateObject("ADODB.Recordset")
rsCustomer.Open strSelect,Conn,1,3
'/////Get # of records to display per page//////////
NumPerPage =cint(Request.Form ("select2"))
if NumPerPage = 0 then NumPerPage = 20
count=PageCounter
rowcount=rsCustomer.recordcount
strFirstRec =   ((NumPerPage * count)+1)
strLastRec  =	NumPerPage * (count + 1)
'///////End//////////

'//////////////////TEMP R/////////////////////

If (count=null) or (count = 0) Then
      If rowcount > NumPerPage	Then
   	        
		 strNextPrev="<b><a href=""javascript:getNextPrev(1)"">" _
		             & "Next Records</a> </b>"
         strFromTo="<b>Results 1  To"& NumPerPage & "</b>"		
      Else	'//No Of records < NumPerPage
 			
         strFromTo="<b>Results From 1 To  " & rowcount & "</b>"						 
      End if
Else '//if count >0
	
		If (rowcount / NumPerPage)  > (count+1) Then
   	          
			strNextPrev="<b><a href=""javascript:getNextPrev(" & (count - 1) & ")"">" _
       	                &  "Prev Records</a> " _
                        &  "&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp" & "<a href=""javascript:getNextPrev(" & (count + 1) & ")"">" _
						&  "Next Records</a></b>"							
         strFromTo="<p><b>Results From " & (strFirstRec) & " To " & (strLastRec)& "</b>"								
			
		Elseif (rowcount / NumPerPage)  = count Then
   	            
			strNextPrev="<b><a href=""javascript:getNextPrev(" & (count - 1) & ")"">" _
						& "Prev Records </a></b>"							         			
            strFromTo="<p><b>Results From " & (strFirstRec) & " To " & (strLastRec) & "</b>"															
   	
	   Elseif  int(rowcount / NumPerPage) = int( count) Then '//For last group of records NumPerPage or <NumPerPage
   				
			strNextPrev="<b><a href=""javascript:getNextPrev(" & (count - 1) & ")"">" _
						& "Prev Records </a></b>"							         			
            strFromTo="<p><b>Results From  " & (strFirstRec) & " To " & (rowcount) & "</b>"															
	
	   Elseif  (int(rowcount / NumPerPage)- int( count))=1 Then '//added by me 28-3
   				
			strNextPrev="<b><a href=""javascript:getNextPrev(" & (count - 1) & ")"">" _
						& "Prev Records </a></b>"							         			
            strFromTo="<p><b>Results From  " & (strFirstRec) & " To " & (rowcount) & "</b>"															
					
      End if
End if
'//////////////////TEMP R/////////////////////
%>
<HTML>
<HEAD>
<style type="text/css">
a:link 
     {  color: blue;
        text-decoration: underline
     }
a:visited 
     {
       color: blue;
       text-decoration: underline
     }
A:hover 
	{ 
   	text-decoration	:	none; 
	color		:	#CC00CC;
	background	:	none;
	}

</style>
<script language="javascript">
function getNextPrev(intPageCounter)
{
//document.frmNextPrev.select2.value =intPageCounter;
document.frmNextPrev.PageCounter.value=intPageCounter;
document.frmNextPrev.action='selectCustNext.asp';
document.frmNextPrev.method="POST";
document.frmNextPrev.submit();
}

function returnBack()
{
document.frmNextPrev.action ='FillSession.asp?BegDate=<%=strBeginDate%>&EndDate=<%=strEnddate%>';
document.frmNextPrev.method="POST";
document.frmNextPrev.submit();
//return(true)
}

</script>
</HEAD>
<!--Bgcolor=mistyrose-->
<BODY  Bgcolor=White>
<FORM NAME="frmNextPrev" METHOD="POST">
<input type="Hidden" name="PageCounter" value="<%=PageCounter%>">
<input type="Hidden" name="txtCust" value="<%=strCustCriteria%>">

<center><font color=navy size=+2><b><u>Select One Or More Customer</u></b></font></center>

<center><br>
<TABLE BORDER="1" width=50%>

<%
 intCounter=0
 ctr=0
 Do while Not (rsCustomer.EOF)
 	ctr = ctr + 1
    If ctr<=strLastRec and  ctr>=strFirstRec Then
			intCounter=intCounter+1 
			Response.Write("<TR>")
			Response.Write("<TD bgColor=#ccccff >")
			Response.Write("<font color=navy size=2>")
			Response.Write(rsCustomer("Account") & " -- " & rsCustomer("Btname"))
			Response.Write("</font>")
			Response.Write("</TD>")
			Response.Write("<TD bgColor=#ccccff >")
			if  Session("RintSeleCustID")= 0 then
			  Response.Write("<input type=""checkbox"" NAME=""ID" & intCounter & """ VALUE=""" & rsCustomer("Account") & """ >")
			else
				'Dim i
				'i=0  
					For intCtrCust = 0 to ubound(Session("TempCust"))
					'For each intCtrCust in Session("TempCust")
						if rsCustomer("Account") = Session("TempCust")(intCtrCust ,0)  then 
							flag=false
							exit for
						else
							flag=true
						end if
						'i=i+1
						Next 
					if flag=false then
						Response.Write("<input type=""checkbox"" NAME=""ID" & intCounter & """ VALUE=""" & rsCustomer("Account") & """checked=true>")
					else
						Response.Write("<input type=""checkbox"" NAME=""ID" & intCounter & """ VALUE=""" & rsCustomer("Account") & """ >")
					end if
        'end if				
			end if
			Response.Write("<input type=""HIDDEN"" NAME=""Name" & intCounter & """ VALUE=""" & rsCustomer("Btname") & """>")
			Response.Write("</TD>")
			Response.Write("</TR>")
	End If
 rsCustomer.MoveNext()
 Loop

  Session("RintCounter")=intCounter
%>

<%
rsCustomer.Close
Conn.Close
Set rsCustomer = Nothing
%>

<tr>
<td colspan=2  bgColor=#ffccff align=center valign="middle">
<!--Response.Write("<b>Total No. Of Records:" & rowcount & "</b><br>")-->
<%Response.Write ("<u>" & strFromTo & "<b> Of " & rowcount & "</u></b>")%>
</td>
</tr>
<%if rowcount >20 then%>
		<TR>
		<TD  bgColor=#ccccff>
		<%Response.Write ("" & strNextPrev & "<br>")%>
		</td>
		<td  bgColor=#ccccff valign="top">
										<font face="Arial"  color=navy size=2><strong>Display</strong></font>
									
										   <select id="select2" name="select2" >
												<option value="20">20</option>
										    <option <%if NumPerPage=30 then Response.Write "Selected"%> value="30">30</option>
												<option <%if NumPerPage=40 then Response.Write "Selected"%> value="40">40</option>
												<option <%if NumPerPage=50 then Response.Write "Selected"%> value="50">50</option>
											</select>
										<font face="Arial" color=navy size=2><strong>Customer Per Page </strong></font>
				                    </td>
		</tr>
<% end if%>
<tr>
<TD colspan=2 bgColor=#ffccff  valign="middle" align="center">
<Input type="button" value="OK"  style="width:90;"  name=submit1 onClick="returnBack();">
&nbsp;
<Input type="reset"  style="width:90;" value="Clear All" id=reset1 name=reset1>
&nbsp;
<Input type="button" value="Check All"  style="width:90;" onClick="checkAll();">
</CENTER>
</TD>

</TABLE>
</FORM>

<%
Response.Write ("<script language=""javascript"">")
Response.Write ("function checkAll()")
Response.Write ("{")
Response.Write ("var ctr=0;")
Response.Write ("for (ctr=1;ctr<=" & intCounter & ";ctr++) ")
Response.Write ("{")
Response.Write ("eval(""document.frmNextPrev.ID"" +ctr).checked=true;")
Response.Write ("}")
Response.Write ("}")
Response.Write ("</script>")
%>
</BODY>
</HTML>
