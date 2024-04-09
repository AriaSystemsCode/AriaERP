<%
Response.Buffer = True
Dim SqlCon
Set SQlcon = server.CreateObject("ADODB.Connection")
SqlCon.Open Application("SqlServer")
'Response.Write(Application("SqlServer"))
'Response.End 

Dim RSGroup
Set RSGroup = server.CreateObject("ADODB.Recordset")

Select Case Request("action")
	case "A"
		strsql = "select * from custgroup where 1=0"
		RSGroup.Open strsql,sqlcon,2,4
		IF RSGroup.EOF And RSGroup.BOF Then
			RSGroup.AddNew 
			RSGroup.Fields("Description").Value = Request("txtgrpdesc")
			RSGroup.Fields("PackID").Value = request("lstpacks")
			RSGroup.UpdateBatch 
			RSGroup.Close  
		End IF
	case "E"
		strsql = "select * from custgroup where groupid=" & request("txtgrpid")
		RSGroup.Open strsql,SqlCon,2,4
		RSGroup.Fields("description").Value = request("txtgrpdesc")
		RSGroup.Fields("packid").Value = request("lstpacks")
		RSGroup.UpdateBatch 
		RSGroup.Close 
	case "D"
		strsql = "select * from custgroup where groupid = " & request("rdoCustGrp")
		RSGroup.Open strsql,sqlcon,2,4
		IF Not (RSGroup.EOF And RSGroup.BOF) Then
			RSGroup.Delete 
			RSGroup.UpdateBatch 
		End IF
		
		
End Select
Response.Redirect("default.asp")
%>