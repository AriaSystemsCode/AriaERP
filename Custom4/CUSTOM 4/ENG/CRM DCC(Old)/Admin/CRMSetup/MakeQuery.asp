

<%
Response.Write makeQuery("hi", " m , f , g , h , k", 2)

function makeQuery(fieldname, ValuesList, NoOfItems)
	dim ValuesArray
	ValuesArray = split(ValuesList, ",", -1, 1)
	
	strQuery="( "
	for i=0 to ubound(ValuesArray) 
		if (i mod NoOfItems) = 0 then
			if i <> 0 then
				strQuery = strQuery & ") or "
			end if
			strQuery = strQuery & fieldname & " in ( "
		end if
		
		strQuery = strQuery & " '" & ValuesArray(i) & "' "
		if (((i+1) mod NoOfItems) <> 0) and i <> ubound(ValuesArray)  then
			strQuery = strQuery & ","
		end if
		
		if i = ubound(ValuesArray) then
				strQuery = strQuery & ") "
		end if
	next 
	strQuery = strQuery & ")"
	makeQuery = strQuery
end function
%>