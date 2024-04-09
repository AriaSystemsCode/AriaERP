<%@ Language=VBScript %>

<%Response.Buffer = true%>
<%Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>
<%
if Trim(Session("ID")) = "" and trim(Session("customerid"))="" then
	'Response.redirect "../default.asp"%>
	<script language="javascript">
	parent.location.href="../login.asp"
	</script>
<%end if

IF Len(trim(session("ID")))>0 Then
	strFile = "cust"
	compWork = "Y"
	custid = Session("ID")
END IF
	
IF Len(Trim(Session("rep")))>0 Then
	IF Trim(Session("customerid")) = "" Then
		Response.Redirect("../repcust.asp")
	else
		custid = Ucase(Trim(Session("customerid")))
	END IF
	strFile = "reb"
	compWork = "Y"
	custid = Session("customerid")
End IF


if trim (Session("rep")) = "" then
	CurCust = Session("ID")
else 
	CurCust = Session("CustomerID")
end if


Dim conFox
Set conFox = server.CreateObject("adodb.connection")
conFox.Open Application("DataConnectionString")

if Request("lstRefe") = "NEW"  then
	Dim rsProfValue
	Set rsProfValue = server.CreateObject("ADODB.recordset")
	strsql = "select * from arPrFCod where cpro_code+cpro_value = 'ZZZZZZ'"
	rsProfValue.Open strsql,conFox,1,3
	
	rsProfValue.AddNew 
	rsProfValue.Fields("Cpro_code").Value = Request("lstProf")
	rsProfValue.Fields("Cpro_value").Value = Request("txtRef")
	rsProfValue.Update 
else
end if



Dim rsProfile
Set rsProfile = server.CreateObject("ADODB.recordset")
if len(Request("Val")) > 0 then
	strsql = "select * from profile where cconttype+ccont_id+store+cpro_code+cpro_value like 'C"&CurCust&"%' and cpro_code+cpro_value ='" & Request("Val")& "'"
else
	strsql = "select * from profile where cconttype+ccont_id+store+cpro_code+cpro_value like 'ZZZ'"
end if
'Response.Write(strsql)
'Response.End 
rsProfile.Open strsql,conFox,1,3

if rsProfile.EOF and rsProfile.BOF then
	rsProfile.AddNew 
	rsProfile.Fields("cconttype").Value = "C"
	rsProfile.Fields("ccont_id").Value = CurCust
	rsProfile.Fields("cpro_code").Value = Request("lstProf")
	IF Request("lstRefe") = "NEW" then
		rsProfile.Fields("cpro_value").Value = Request("txtref")
	else
		rsProfile.Fields("cpro_value").Value = Request("lstRefe")
	end if
	
	rsProfile.Fields("dpro_date").Value = Request("txtDate")
	rsProfile.Fields("cadd_time").Value = time()
	rsProfile.Fields("dadd_date").Value = date()
	rsProfile.Update 
else
	rsProfile.Fields("dpro_date").Value = Request("txtDate")
	rsProfile.Fields("cadd_time").Value = time()
	rsProfile.Fields("dadd_date").Value = date()
	rsProfile.Update 
end if
Response.Redirect ("custaddprof.asp")
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
</HEAD>
<BODY>

<P>&nbsp;</P>

</BODY>
</HTML>
