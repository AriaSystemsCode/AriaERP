<%	Response.Buffer = true
'Response.Write "OUT"
	'Response.end
	'If Request.QueryString("PageID")="C" Then
		Response.Redirect("Catalog/Catalog.asp?PageID="&Request.QueryString("PageID"))
	'Else
		'Response.Redirect("Catalog/CatSearch.asp")
	'End If
	
	
%>