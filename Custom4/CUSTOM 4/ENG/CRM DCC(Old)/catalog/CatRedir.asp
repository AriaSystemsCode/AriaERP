<%Response.Buffer = true
'Response.Write "IN "
	'Response.write Request.QueryString("PageID")
	'Response.end
	If Request.QueryString("PageID")="C" Then
		'Response.Redirect("../Catalog/NewCat.asp")
		Response.Redirect("Catalog.asp?PageID="&Request.QueryString("PageID"))
	Else
		Response.Redirect("../Catalog/CatSearch.asp")
	End If
	'Response.Redirect("Catalog.asp?PageID="&Request.QueryString("PageID"))
%>