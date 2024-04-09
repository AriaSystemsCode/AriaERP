<%
function Add2Log(Contact,Account,TransType,TransNum,Memo)
	set uiAdd = server.CreateObject ("TransLogUI.TransUI")
	'''Aria-dev: ConStr = "DRIVER={SQL Server};SERVER=ARIA-DEV;DATABASE=CRM;UID="&Trim(Session("SqlUserName"))&";PWD="&Trim(Session("SqlPassWord"))
	'ARD
	'ConStr = "DRIVER={SQL Server};SERVER=" & Trim(Session("SQLServer")) & ";DATABASE=CRM;UID="&Trim(Session("SqlUserName"))&";PWD="&Trim(Session("SqlPassWord"))
	ConStr = Application("SqlServer")
	'ARD
	uiAdd.ConParameter = ConStr
	uiAdd.Add 
	uiAdd.Contact = Contact
	uiAdd.Account = Account
	uiAdd.TransType = TransType
	uiAdd.TransNumber = TransNum
	uiAdd.Memo = Memo
	
	uiAdd.Save 

	set uiAdd = nothing
end function
%>
