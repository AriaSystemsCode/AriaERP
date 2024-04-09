Attribute VB_Name = "Module1"

Sub Main()
  Dim strComParm As String
  Dim astrArguments() As String
  Dim lngCounter As Long
   
  
  strComParm = Command()
  
  astrArguments = Split(strComParm, ",", -1, vbTextCompare)
  If UBound(astrArguments) < 3 Then
    MsgBox "Wrong number of parameters, cannot proceed."
  Else
    SendMail astrArguments(0), astrArguments(1), astrArguments(2), astrArguments(3), astrArguments(4)
  End If
End Sub


Sub SendMail(ByVal strFromEmail As String, ByVal strToEmail As String, strSubject As String, OutgoingFileDir As String, CurrMailServ As String)
MsgBox strFromEmail + " " + strToEmail + " " + strSubject + " " + CurrMailServ

Dim CurrMail As New ASPEMAILLib.MailSender
CurrMail.From = strFromEmail
CurrMail.Subject = strSubject
CurrMail.AddAddress strToEmail
CurrMail.AddAttachment OutgoingFileDir
CurrMail.Host = CurrMailServ
CurrMail.Send

End Sub
