
<%
Response.Buffer = True
Response.Expires = -1

'*:***************************************************************************
'*: Active Server Page : glbscrpt.asp
'*: File descreption   : To convert the physical file name to the description
'*: System             : WEB
'*: Module             : Customer Relationship Management
'*: Developer          : Ahmed M. Reda (ARD)
'*:***************************************************************************
Dim Conn ' as connection
Set Conn = server.CreateObject("ADODB.Connection")
Conn.Open Application("DataConnectionString")

Dim SYSConn ' as connection
Set SYSConn = Server.CreateObject("ADODB.Connection")
SYSConn.Open Application("SystemConnectionString")
	
'*!****************************************************************************
'*! Name      : fld_desc
'*! Developer : Ahmed M. Reda (ARD)
'*! Date      : 11/01/2001
'*! Purpose   : Function to be used to return the description of any field
'*!****************************************************************************
'*! Example   : fld_desc(strfld)
'*!****************************************************************************
Function fld_desc(strfld)
	'Response.Write(strfld)
	Dim RSFields ' as RecordSet
	Set RSFields = Server.CreateObject("ADODB.RecordSet")
	strsql = "select cfld_head from sydfield where cfld_name='" & cstr(strfld) & "'"
	RSFields.Open strsql,SYSConn
	fld_desc = (RSFields.Fields("cfld_head"))
End Function


'*!****************************************************************************
'*! Name      : getCode_desc
'*! Developer : Ahmed M. Reda (ARD)
'*! Date      : 11/01/2001
'*! Purpose   : Function to be used to return the description of any field
'*!****************************************************************************
'*! Example   : fld_desc(strfld)
'*!****************************************************************************
Function getCode_desc(category,code)
	Dim RSCodes ' as RecordSet
	Set RSCodes = Server.CreateObject("ADODB.RecordSet")
	strsql = "select * from codes where cfld_name='" & cstr(category) & "' and ccode_no='" & cstr(code) & "'"
	RSCodes.Open strsql,Conn
	'HDM Check if not EOF
	If RSCodes.EOF And RSCodes.BOF Then
		getCode_desc = ""
	Else
		getCode_desc = (RSCodes.Fields("cdiscrep"))
	End If
End Function
%>