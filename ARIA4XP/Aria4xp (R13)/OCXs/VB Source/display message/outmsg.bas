Attribute VB_Name = "Module1"

Sub Main()
tcmsgid = Command()
Dim myOlApp, myNamespace, oMessage As Object
Set myOlApp = CreateObject("Outlook.Application")
Set myNamespace = myOlApp.GetNamespace("MAPI")
myNamespace.session.Logon
Set oMessage = myNamespace.GetItemFromID(tcmsgid)
oMessage.Display (1)

End Sub
