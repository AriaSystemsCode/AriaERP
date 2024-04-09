VERSION 5.00
Begin VB.Form frmReceive 
   Caption         =   "Receive Incoming Product Activities"
   ClientHeight    =   3420
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   8160
   LinkTopic       =   "Form1"
   ScaleHeight     =   3420
   ScaleWidth      =   8160
   StartUpPosition =   3  'Windows Default
   Begin VB.Frame Frame2 
      Caption         =   "Receive In Directory:"
      Height          =   2475
      Left            =   60
      TabIndex        =   4
      Top             =   195
      Width           =   3270
      Begin VB.TextBox txtReceive 
         Enabled         =   0   'False
         Height          =   345
         Left            =   90
         TabIndex        =   7
         Top             =   285
         Width           =   3060
      End
      Begin VB.DriveListBox drvReceive 
         Height          =   315
         Left            =   90
         TabIndex        =   6
         Top             =   660
         Width           =   3060
      End
      Begin VB.DirListBox DirReceive 
         Height          =   1215
         Left            =   90
         TabIndex        =   5
         Top             =   1035
         Width           =   3060
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "Received Messages"
      Height          =   2475
      Left            =   3390
      TabIndex        =   2
      Top             =   195
      Width           =   4695
      Begin VB.ListBox lstFiles 
         Height          =   2010
         Left            =   105
         TabIndex        =   3
         Top             =   315
         Width           =   4485
      End
   End
   Begin VB.CommandButton cmdClose 
      Caption         =   "&Close"
      Height          =   540
      Left            =   4695
      TabIndex        =   1
      Top             =   2820
      Width           =   1170
   End
   Begin VB.CommandButton cmdReceive 
      Caption         =   "&Receive"
      Height          =   480
      Left            =   2550
      TabIndex        =   0
      Top             =   2835
      Width           =   1155
   End
End
Attribute VB_Name = "frmReceive"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cmdClose_Click()
WritePrivateProfileString "Receive Activity", "ReceiveDir", Me.txtReceive.Text, App.Path & "\RcvActi.ini"
Unload Me
End Sub

Private Sub cmdReceive_Click()

Dim strReg, strReg1 As String
Dim intLine As Integer

On Error GoTo ErrorLn

strReg = GetVAlue("", "wjsjjjlqmjpjrjjjvpqqkqmqukypoqjquoun", "\899B3E80-6AC6-11cf-8ADB-00AA00C00905")
strReg1 = GetVAlue("", "konhqhioohihphkouimonhqhvnwiqhhhnjti", "\C4145310-469C-11d1-B182-00A0C922E820")

If Len(Trim(strReg)) = 0 Or Len(Trim(strReg1)) = 0 Then
  MsgBox "You have no licenses to log on mail system.", vbCritical + vbOKOnly, "Caution"
Else
  Dim oFileSystemObject, oSourceFile, oTargetFile
  Set oFileSystemObject = CreateObject("Scripting.FileSystemObject")
  Dim oSESSION, omessage As Object
  Dim strSourceFile, strSourceDir, strTargetFile, strLine As String
  strSourceDir = Me.txtReceive
  strTargetFile = "INCOME.EDI"
  '* Create INCOME.EDI file if does not exist
  
  Set oTargetFile = oFileSystemObject.OpenTextFile(strSourceDir & IIf(Right(strSourceDir, 1) = "\", "", "\") & strTargetFile, 8, True)
  Dim Msgcounter, AttCounter As Integer
  Set oSESSION = CreateObject("MSMAPI.MAPISESSION")
  Set omessage = CreateObject("MSMAPI.MAPIMESSAGES")
  oSESSION.Action = 1
  If oSESSION.SESSIONID <> 0 Then
    omessage.SESSIONID = oSESSION.SESSIONID
    omessage.FetchUnreadOnly = True
    omessage.Action = 1
    For Msgcounter = 0 To omessage.MSGCOUNT - 1
      omessage.MSGINDEX = Msgcounter
      If Left$(omessage.MsgSubject, 16) = "Product Activity" Then
        For AttCounter = 0 To omessage.AttachmentCount - 1
          omessage.AttachmentIndex = AttCounter
          '* Open the source file for read
          strSourceFile = omessage.AttachmentPathName
          Me.lstFiles.AddItem omessage.MsgSubject
          Set oSourceFile = oFileSystemObject.OpenTextFile(strSourceFile, 1, False)
          Do While oSourceFile.AtEndOfStream <> True
            strLine = oSourceFile.ReadLine & Chr(13)
            oTargetFile.Write strLine
          Loop
          oSourceFile.Close
        Next AttCounter
      End If
    Next Msgcounter
    oTargetFile.Close
  End If
  Set omessage = Nothing
  Set oTargetFile = Nothing
  Set oSourceFile = Nothing
  Set oFileSystemObject = Nothing
  oSESSION.SignOff
  Set oSESSION = Nothing
  Me.cmdReceive.Enabled = False
End If
Exit Sub
ErrorLn:
MsgBox "Error while loggin to mail system or mail system Not Installed"
Set omessage = Nothing
Set oSESSION = Nothing
Set oTargetFile = Nothing
Set oSourceFile = Nothing
Set oFileSystemObject = Nothing
End Sub

Private Sub DirReceive_Change()
Me.txtReceive = Me.DirReceive.Path
End Sub

Private Sub drvReceive_Change()
Me.DirReceive.Path = Me.drvReceive.Drive
End Sub

Private Sub Form_Load()
Dim lcreturn As String

If Dir(App.Path & "\RcvActi.ini") = "RcvActi.ini" Then
  lcreturn = Space(100)
  GetPrivateProfileString "Receive Activity", "ReceiveDir", "NONE", lcreturn, 160, App.Path & "\RcvActi.ini"
  lcreturn = Trim$(lcreturn)
  If Left$(lcreturn, 4) <> "NONE" Then
    Me.DirReceive.Path = lcreturn
    Me.txtReceive.Text = lcreturn
    Me.drvReceive.Drive = Left$(Me.txtReceive, 1)
  End If
End If
End Sub
