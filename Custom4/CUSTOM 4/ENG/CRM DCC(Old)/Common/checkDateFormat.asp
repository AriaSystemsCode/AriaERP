<%function DateFormat()
	set conn = server.createObject("adodb.connection")
	conn.open Application("SystemConnectionString")
		sqlgetFormat = "select ccont_code, cdate_type from sycint where ccont_code in (select ccont_code from syccomp where ccomp_id='"&Session("CompanyID")&"')"
		set rsgetFormat = server.createobject("adodb.recordset")
		rsgetFormat.Open sqlgetFormat , conn
		if not rsgetFormat.eof then
			Session("DateFormat")= rsgetFormat("cdate_type")
		end if
end function%>