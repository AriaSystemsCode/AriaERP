<%@ Language=VBScript %>
<% Response.CacheControl = "no-cache" %>
<% Response.AddHeader "Pragma", "no-cache" %>
<% Response.Expires = -1 %>

<%
Response.Buffer = True
Dim SqlCon
Set SqlCon = server.CreateObject("ADODB.Connection")
SqlCon.Open Application("SqlServer")


	Session("CustGrp") = request("rdoCustGrp")


Set RsAccount = server.CreateObject("ADODB.Recordset")
strsql = "select * from custclassification where custgroup = " & Session("CustGrp")
RSAccount.Open strsql,SqlCon

Session("StrAccount") = ""

IF Not(RSAccount.EOF And RSAccount.BOF) Then
	Do While Not RSAccount.EOF 
		IF Session("StrAccount") = "" Then
			Session("StrAccount") = RSAccount.Fields("custid").Value 
		Else
				Session("StrAccount") = Session("StrAccount") & "," & RSAccount.Fields("custid").Value
		End IF
		RSAccount.MoveNext 
	Loop
End IF
session("FlagCust") = True
'Response.Write(Session("CustGrp"))
Response.Redirect("addcust.asp")
%>