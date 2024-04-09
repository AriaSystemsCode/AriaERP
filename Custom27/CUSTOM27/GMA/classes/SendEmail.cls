VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "SendEmail"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public Function emailfile(ByVal strEmail As String, strSubject As String, OutgoingFileDir As String) As Boolean
Dim strReg, strReg1 As String

strReg = GetVAlue("", "", "\Software\Microsoft\VisualFoxPro\6.0\Options\OLEList\{20C62CA0-15DA-101B-B9A8-444553540000}")
strReg1 = GetVAlue("", "", "\Software\Microsoft\VisualFoxPro\6.0\Options\OLEList\{20C62CAB-15DA-101B-B9A8-444553540000}")
 
On Error GoTo ErrorLn
Dim oSESSION, omessage As Object
If False And (Len(Trim(strReg)) = 0 Or Len(Trim(strReg1)) = 0) Then
  MsgBox "You have no licenses to log on mail system.", vbCritical + vbOKOnly, "Caution"
Else
  Set oSESSION = CreateObject("MSMAPI.MAPISESSION")
  Set omessage = CreateObject("MSMAPI.MAPIMESSAGES")
  emailfile = False
  oSESSION.Action = 1
  If oSESSION.SessionID <> 0 Then
    omessage.SessionID = oSESSION.SessionID
    omessage.Compose
    omessage.RecipIndex = omessage.RecipCount
    omessage.RecipType = 1
    omessage.RecipAddress = strEmail
    omessage.Action = 13
    omessage.MsgSubject = strSubject
    omessage.AttachmentIndex = omessage.AttachmentCount
    omessage.AttachmentName = "GMA_PO.ZIP"
    omessage.AttachmentPathName = OutgoingFileDir
    omessage.AttachmentPosition = omessage.AttachmentIndex
    omessage.AttachmentType = 0
    omessage.MsgNoteText = "*"
    omessage.Send
    omessage.Fetch
    emailfile = True
  Else
    emailfile = False
  End If
  Set oSESSION = Nothing
  Set omessage = Nothing
  MsgBox "File " & OutgoingFileDir & " has been sent to " & Trim$(strEmail), vbOKOnly, "Send Product Activities"
End If
On Error GoTo 0

Exit Function
ErrorLn:
MsgBox "Error while loggin to mail system or mail system Not Installed"
Set oSESSION = Nothing
Set omessage = Nothing
emailfile = False
End Function

