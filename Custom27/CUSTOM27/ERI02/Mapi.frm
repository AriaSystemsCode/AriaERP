VERSION 5.00
Object = "{20C62CAE-15DA-101B-B9A8-444553540000}#1.1#0"; "MSMAPI32.OCX"
Begin VB.Form Mapi 
   ClientHeight    =   810
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   1875
   LinkTopic       =   "Form1"
   ScaleHeight     =   810
   ScaleWidth      =   1875
   StartUpPosition =   3  'Windows Default
   Begin MSMAPI.MAPIMessages oMessage 
      Left            =   960
      Top             =   120
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
      AddressEditFieldCount=   1
      AddressModifiable=   0   'False
      AddressResolveUI=   0   'False
      FetchSorted     =   0   'False
      FetchUnreadOnly =   0   'False
   End
   Begin MSMAPI.MAPISession oSession 
      Left            =   240
      Top             =   120
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
      DownloadMail    =   0   'False
      LogonUI         =   -1  'True
      NewSession      =   0   'False
   End
End
Attribute VB_Name = "Mapi"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_Load()
Dim lcFile As String
Dim lnI As Integer
Dim cMsgContents As String

'a command line file contains the names of styles/colors added
lcFile = Command$()
 
'The maillist.txt file must contain the following list
'wendy@ericjavits.com
'fatima@ericjavits.com
'edson@ericjavits.com
'frank@ericjavits.com
'geta@ericjavits.com
'nyshowroom@ericjavits.com
'topexec9@aol.com

If Trim(lcFile) <> "" Then
  lnFileNumber = FreeFile()
  Open lcFile For Input As #lnFileNumber
  While Not EOF(lnFileNumber)
    Line Input #lnFileNumber, lcLine
    cMsgContents = cMsgContents & Chr(13) & lcLine
  Wend
  Close #lnFileNumber
  Kill lcFile
End If

'Populate array of mail list
lnFileNumber = FreeFile()
Open "MailList.txt" For Input As #lnFileNumber

On Error GoTo ErrorLn
  
oSession.Action = 1
If oSession.SessionID <> 0 Then
  While Not EOF(lnFileNumber)
    Line Input #lnFileNumber, lcLine
    If Trim(lcLine) <> "" Then
      oMessage.SessionID = oSession.SessionID
      oMessage.Compose
      oMessage.RecipIndex = oMessage.RecipCount
      oMessage.RecipType = 1
      oMessage.RecipAddress = lcLine
      oMessage.Action = 13
      oMessage.MsgSubject = "STYLE CHANGE ALERT"
      oMessage.MsgNoteText = cMsgContents
      oMessage.Send
    End If
  Wend
Else
  MsgBox "Can not log to mail system."
End If

Close #lnFileNumber
On Error GoTo 0

Unload Me

Exit Sub

ErrorLn:
MsgBox Err.Number & ":" & Err.Description
    
End Sub
