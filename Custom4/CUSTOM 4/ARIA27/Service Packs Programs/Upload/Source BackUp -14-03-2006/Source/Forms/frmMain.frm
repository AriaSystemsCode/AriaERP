VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{91FB09A2-B504-11D1-ADB8-00A024122F93}#1.0#0"; "xFTPPro.ocx"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Upload"
   ClientHeight    =   5625
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   10215
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   5625
   ScaleWidth      =   10215
   StartUpPosition =   1  'CenterOwner
   Begin XFTPPROLib.XFTPPro ftpConnector 
      Left            =   8100
      Top             =   90
      _Version        =   65536
      _ExtentX        =   847
      _ExtentY        =   847
      _StockProps     =   0
   End
   Begin VB.Frame fraStep 
      BorderStyle     =   0  'None
      Height          =   1980
      Index           =   3
      Left            =   9840
      TabIndex        =   15
      Top             =   6720
      Width           =   3660
      Begin VB.CommandButton cmdViewLastResponse 
         Caption         =   "View Last Server Response..."
         Height          =   315
         Left            =   735
         TabIndex        =   23
         Top             =   1665
         Width           =   2925
      End
   End
   Begin VB.Frame fraStep 
      BorderStyle     =   0  'None
      Height          =   1980
      Index           =   2
      Left            =   9360
      TabIndex        =   12
      Top             =   6840
      Width           =   3660
      Begin VB.Label lblSign 
         AutoSize        =   -1  'True
         Caption         =   "The Service Pack Team."
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   195
         Left            =   1650
         TabIndex        =   14
         Top             =   1665
         Width           =   2010
      End
      Begin VB.Label lblThank 
         AutoSize        =   -1  'True
         Caption         =   "Thank you for using this program."
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   195
         Left            =   810
         TabIndex        =   13
         Top             =   1425
         Width           =   2850
      End
   End
   Begin VB.Frame fraStep 
      BorderStyle     =   0  'None
      Height          =   4140
      Index           =   0
      Left            =   2400
      TabIndex        =   5
      Top             =   720
      Width           =   7740
      Begin VB.Frame fraCustomers 
         Caption         =   "  Customer List  "
         Height          =   2055
         Left            =   120
         TabIndex        =   30
         Top             =   2040
         Width           =   7575
         Begin VB.ListBox lstCustomers 
            Height          =   1635
            Left            =   120
            Style           =   1  'Checkbox
            TabIndex        =   34
            Top             =   240
            Width           =   6255
         End
         Begin VB.CommandButton cmdSelectAll 
            Caption         =   "Select All"
            Height          =   375
            Left            =   6480
            TabIndex        =   33
            Top             =   240
            Width           =   975
         End
         Begin VB.CommandButton cmdClear 
            Caption         =   "Clear"
            Height          =   375
            Left            =   6480
            TabIndex        =   32
            Top             =   1200
            Width           =   975
         End
         Begin VB.CommandButton cmdInvert 
            Caption         =   "Invert"
            Height          =   375
            Left            =   6480
            TabIndex        =   31
            Top             =   720
            Width           =   975
         End
      End
      Begin VB.Frame fraFixes 
         Caption         =   "  Fixes List  "
         Height          =   2055
         Left            =   120
         TabIndex        =   24
         Top             =   0
         Width           =   7575
         Begin VB.ListBox lstExes 
            Height          =   1620
            Left            =   1200
            TabIndex        =   28
            Top             =   240
            Width           =   1575
         End
         Begin VB.TextBox txtResponse 
            Height          =   1425
            Left            =   3000
            MultiLine       =   -1  'True
            ScrollBars      =   2  'Vertical
            TabIndex        =   27
            Top             =   450
            Width           =   4455
         End
         Begin VB.CommandButton cmdAttach 
            Caption         =   "&Attach ..."
            Height          =   375
            Left            =   120
            TabIndex        =   26
            Top             =   240
            Width           =   975
         End
         Begin VB.CommandButton cmdRemove 
            Caption         =   "&Remove"
            Height          =   375
            Left            =   120
            TabIndex        =   25
            Top             =   720
            Width           =   975
         End
         Begin VB.Label lblResponse 
            AutoSize        =   -1  'True
            Caption         =   "Response (Editable Text)"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            ForeColor       =   &H000000C0&
            Height          =   195
            Left            =   3000
            TabIndex        =   29
            Top             =   240
            Width           =   2130
         End
      End
      Begin VB.ComboBox cboCustomers 
         Height          =   315
         Left            =   4200
         Style           =   2  'Dropdown List
         TabIndex        =   8
         Top             =   240
         Visible         =   0   'False
         Width           =   3330
      End
      Begin VB.TextBox txtFile 
         Enabled         =   0   'False
         Height          =   315
         Left            =   150
         Locked          =   -1  'True
         TabIndex        =   7
         Top             =   225
         Visible         =   0   'False
         Width           =   3045
      End
      Begin VB.CommandButton cmdBrowse 
         Caption         =   "..."
         Height          =   315
         Left            =   3270
         TabIndex        =   6
         Top             =   225
         Visible         =   0   'False
         Width           =   390
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Upload this file"
         Height          =   195
         Left            =   150
         TabIndex        =   9
         Top             =   0
         Visible         =   0   'False
         Width           =   1050
      End
   End
   Begin VB.CommandButton cmdUpload 
      Caption         =   "Upload"
      Height          =   315
      Left            =   7545
      TabIndex        =   4
      Top             =   5235
      Width           =   1230
   End
   Begin VB.CommandButton cmdClose 
      Cancel          =   -1  'True
      Caption         =   "Close"
      CausesValidation=   0   'False
      Height          =   315
      Left            =   8880
      TabIndex        =   3
      Top             =   5235
      Width           =   1230
   End
   Begin VB.Frame fraStep 
      BorderStyle     =   0  'None
      Height          =   4140
      Index           =   1
      Left            =   840
      TabIndex        =   0
      Top             =   3960
      Width           =   7740
      Begin VB.Frame fraProgress 
         BorderStyle     =   0  'None
         Height          =   1875
         Left            =   480
         TabIndex        =   16
         Top             =   2010
         Width           =   7485
         Begin MSComctlLib.ProgressBar cntTherm 
            Height          =   285
            Left            =   0
            TabIndex        =   17
            Top             =   1470
            Width           =   6615
            _ExtentX        =   11668
            _ExtentY        =   503
            _Version        =   393216
            Appearance      =   1
         End
         Begin VB.Label lblAccount 
            AutoSize        =   -1  'True
            Caption         =   "Account:"
            Height          =   195
            Left            =   0
            TabIndex        =   39
            Top             =   0
            Width           =   645
         End
         Begin VB.Label lblCustAcc 
            AutoSize        =   -1  'True
            Caption         =   "Label2"
            ForeColor       =   &H8000000D&
            Height          =   195
            Left            =   720
            TabIndex        =   38
            Top             =   0
            Width           =   465
         End
         Begin VB.Label lblIssue 
            AutoSize        =   -1  'True
            Caption         =   "Issue #:"
            Height          =   195
            Left            =   0
            TabIndex        =   37
            Top             =   303
            Width           =   615
         End
         Begin VB.Label lblIssueNo 
            AutoSize        =   -1  'True
            Caption         =   "IssueNo"
            ForeColor       =   &H8000000D&
            Height          =   195
            Left            =   720
            TabIndex        =   36
            Top             =   303
            Width           =   585
         End
         Begin VB.Label lblSoFar 
            AutoSize        =   -1  'True
            Caption         =   "Bytes Transfered so Far:"
            Height          =   195
            Left            =   0
            TabIndex        =   22
            Top             =   1215
            Width           =   1800
         End
         Begin VB.Label lblTotal 
            AutoSize        =   -1  'True
            Caption         =   "File Size:"
            Height          =   195
            Left            =   0
            TabIndex        =   21
            Top             =   960
            Width           =   630
         End
         Begin VB.Label lblFileName 
            AutoSize        =   -1  'True
            Caption         =   "File Name:"
            Height          =   195
            Left            =   0
            TabIndex        =   20
            Top             =   606
            Width           =   750
         End
         Begin VB.Label lblSoFarNum 
            Alignment       =   1  'Right Justify
            AutoSize        =   -1  'True
            Caption         =   "0000"
            Height          =   195
            Left            =   6255
            TabIndex        =   19
            Top             =   1215
            Width           =   360
         End
         Begin VB.Label lblTotalNum 
            Alignment       =   1  'Right Justify
            AutoSize        =   -1  'True
            Caption         =   "0000"
            Height          =   195
            Left            =   6255
            TabIndex        =   18
            Top             =   960
            Width           =   360
         End
      End
      Begin VB.Image imgStatus 
         Height          =   480
         Index           =   2
         Left            =   0
         Top             =   1080
         Width           =   480
      End
      Begin VB.Label lblStatus 
         AutoSize        =   -1  'True
         Caption         =   "Register Tracking Entries"
         Height          =   195
         Index           =   2
         Left            =   480
         TabIndex        =   35
         Top             =   1200
         Width           =   1785
      End
      Begin VB.Label lblStatus 
         AutoSize        =   -1  'True
         Caption         =   "Connecting to Aria server"
         Height          =   195
         Index           =   0
         Left            =   480
         TabIndex        =   2
         Top             =   120
         Width           =   1785
      End
      Begin VB.Label lblStatus 
         AutoSize        =   -1  'True
         Caption         =   "Uploading Individual Fixes"
         Height          =   195
         Index           =   1
         Left            =   480
         TabIndex        =   1
         Top             =   660
         Width           =   1860
      End
      Begin VB.Image imgStatus 
         Height          =   480
         Index           =   1
         Left            =   0
         Top             =   540
         Width           =   480
      End
      Begin VB.Image imgStatus 
         Height          =   480
         Index           =   0
         Left            =   0
         Top             =   0
         Width           =   480
      End
   End
   Begin MSComDlg.CommonDialog cntDialog 
      Left            =   7530
      Top             =   75
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin MSComctlLib.ImageList imlStatus 
      Left            =   0
      Top             =   0
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   2
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmMain.frx":0442
            Key             =   "Active"
            Object.Tag             =   "Done"
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmMain.frx":075C
            Key             =   ""
         EndProperty
      EndProperty
   End
   Begin VB.Label lblDesc 
      Caption         =   "The 'Upload Tracking Entries' program is uploading XXXXXXXX.XXX to XXXXXXXXXXXXXXXX"
      Height          =   435
      Left            =   2400
      TabIndex        =   11
      Top             =   360
      Width           =   7890
   End
   Begin VB.Label lblTitle 
      AutoSize        =   -1  'True
      Caption         =   "Uploading is in progress"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   2400
      TabIndex        =   10
      Top             =   135
      Width           =   2010
   End
   Begin VB.Line linSep 
      BorderColor     =   &H00808080&
      Index           =   1
      X1              =   120
      X2              =   10100
      Y1              =   5145
      Y2              =   5145
   End
   Begin VB.Line linSep 
      BorderColor     =   &H00FFFFFF&
      Index           =   0
      X1              =   120
      X2              =   10100
      Y1              =   5160
      Y2              =   5160
   End
   Begin VB.Image imgUpload 
      BorderStyle     =   1  'Fixed Single
      Height          =   4830
      Left            =   120
      Picture         =   "frmMain.frx":0A76
      Stretch         =   -1  'True
      Top             =   135
      Width           =   2175
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Enum enmPage
  [envPGetInfo] = 0
  [envPProgress] = 1
  [envPSuccessful] = 2
  [envPError] = 3
  [envPCustomer] = 4
End Enum

Private Enum enmAction
  [envAConnect] = 0
  [envAUpload] = 1
  [envARegister] = 2
End Enum

Private Const mcnsTotalPages   As Byte = 4
Private Const mcnsTotalActions As Byte = 3
Private Const mcnsUserCode     As String = "ARIANY\SPACKR"
Private Const mcnsPassword     As String = "SPR"

Private mintConnectorResponse  As Integer
Private mintCurPage            As enmPage
Private mbolCanExit            As Boolean
Private mbolFileUpLoadComplete As Boolean
Private mstrLastResponse       As String

'' MBADRAN START
Private mcolExes As Dictionary         '' MBADRAN
Private mobjConFox As ADODB.Connection
Private mobjConSql As ADODB.Connection
Private rsCustomers As ADODB.Recordset
Private rsIssueHdr As ADODB.Recordset
Private mResponseText As String
Private mintIndividualCustomers As Integer
Private mastrMailTo() As String
Private mastrIssue() As String

Private Sub StepSet(ByVal evlPage As enmPage)
  lblTitle.Caption = ""
  lblDesc.Caption = ""
  Select Case evlPage
    Case envPGetInfo
      lblTitle.Caption = "Set uploading information"
      lblDesc.Caption = "Please specifiy the file(s) to be " & _
                        "uploaded and then select the " & _
                        "customer(s) to receive the updates..."
    
    Case envPProgress
      Dim strFileName As String
      strFileName = Right(Trim(txtFile.Text), 12)
      
      lblTitle = "Uploading is in progress"
      'lblDesc.Caption = "The '" & App.Title & "' program " & _
                        "is uploading to " & _
                        cboCustomers.List(cboCustomers.ListIndex)
      lblDesc.Caption = "The '" & App.Title & "' program " & _
                        "is uploading."
    
    Case envPSuccessful
      lblTitle = "Uploading is done"
      lblDesc.Caption = "The uploading process has completed successfully."
      
    Case envPError
      lblTitle = "Error"
      lblDesc.Caption = "The '" & App.Title & "' program failed " & _
                        "to upload the file because of an error…" & vbCrLf & vbCrLf & _
                        "Please retry again later..." & vbCrLf & vbCrLf

    Case Else
      lblTitle = Space(0)
      lblDesc.Caption = Space(0)
  End Select
  
  Dim bytPage As Byte
  For bytPage = 0 To mcnsTotalPages - 1
'    fraStep(bytPage).Left = 2340
'    fraStep(bytPage).Top = 1305
    
    fraStep(bytPage).Left = 2400
    fraStep(bytPage).Top = 720
    
    fraStep(bytPage).Visible = bytPage = evlPage
  Next bytPage
  
  mintCurPage = evlPage
  fraStep(evlPage).Refresh
  Call Refresh
  Call SetUploadStatus
End Sub

'' ADD BY
Private Sub cmdAttach_Click()
  cntDialog.Flags = cdlOFNHideReadOnly Or cdlOFNFileMustExist
  
  '' Attach Free File ...
  
  'cntDialog.InitDir = "P:\TRACKING\SPACK\FIXES\"
  
  cntDialog.Filter = "Bugs|BB??????.???|" & _
                     "Enhancements|EE??????.???|" & _
                     "Custom Programs|CC??????.???|" & _
                     "New Developments|NN??????.???|" & _
                     "EDI Mapping|*.EXE|" & _
                     "Free Files|*.*"
  Call cntDialog.ShowOpen
  
  If Len(Trim(cntDialog.FileName)) > 0 Then
    txtFile.Text = Trim(cntDialog.FileName)
    Dim strTrackingEntry As String
    Dim strIndFix As String
    Dim strEntryType As String
    Dim bolEntry As Boolean
    strIndFix = Mid(txtFile.Text, InStrRev(txtFile.Text, "\") + 1)
    strEntryType = Mid(Mid(strIndFix, 1, InStrRev(strIndFix, ".") - 1), 3)
    If IsNumeric(strEntryType) Then
      strTrackingEntry = Mid(Mid(strIndFix, 1, InStrRev(strIndFix, ".") - 1), 2)
      bolEntry = True
    Else
      strTrackingEntry = strIndFix
    End If
            
    If mcolExes.Exists(strTrackingEntry) Then
      Call MsgBox(strTrackingEntry & " entry already in the fixes list!", vbInformation)
      Exit Sub
    End If
    
    ''-- Get the associated Open Issue If there
    Dim strOpenIssue As String
    Dim strEntryInfo As String
    strOpenIssue = IIf(bolEntry, GetOpenIssueHeader(strTrackingEntry), "")
    
    strEntryInfo = txtFile.Text & "|" & strOpenIssue & "|"
    
    Dim strResponse As String
    If bolEntry Then
      strResponse = GetResponse(strIndFix, Mid$(strOpenIssue, 1, 6))
''      strResponse = "Kindly be informed that the individual fix " & _
'        strIndFix & " is now available for download on the Aria FTP site." & _
'        vbCrLf & _
'        "You can download it using the Download " & _
'        "Service Pack program just as normal."
    End If
    
    strEntryInfo = strEntryInfo & strResponse & "|"
     
    '' Add the key to the collection.
    Call mcolExes.Add(strTrackingEntry, strEntryInfo)
    Call lstExes.AddItem(strTrackingEntry)
    lstExes.ListIndex = lstExes.ListCount - 1
   
    ' Associated customer for this open issue.
    If Len(Trim(strOpenIssue)) > 0 Then
      ' Check the open issue customer as default.
      Dim strCustomer As String * 5
      strCustomer = Mid(strOpenIssue, 8)
      rsCustomers.MoveFirst
      rsCustomers.Find "Account='" & strCustomer & "'"
      If Not rsCustomers.EOF Then
        lstCustomers.Selected(rsCustomers.AbsolutePosition - 1) = True
      End If
    End If
    Call CheckList
  End If
End Sub

Private Sub cmdClear_Click()
  Dim intListIndex As Integer
  intListIndex = lstCustomers.ListIndex
  Dim intItem As Integer
  For intItem = 0 To lstCustomers.ListCount - 1
    lstCustomers.Selected(intItem) = False  '' Check this item.
  Next intItem
  lstCustomers.ListIndex = intListIndex
End Sub

Private Sub cmdInvert_Click()
  Dim intListIndex As Integer
  intListIndex = lstCustomers.ListIndex
  Dim intItem As Integer
  For intItem = 0 To lstCustomers.ListCount - 1
    lstCustomers.Selected(intItem) = Not lstCustomers.Selected(intItem)  '' Check this item.
  Next intItem
  lstCustomers.ListIndex = intListIndex
End Sub

Private Sub cmdRemove_Click()
  If lstExes.ListIndex < 0 Then
    Exit Sub
  End If
  Dim strIndex As String
  Dim intChoice As Integer
  strIndex = lstExes.List(lstExes.ListIndex)
  If MsgBox("Are you sure you want to remove " & strIndex, vbQuestion + vbYesNo) = vbYes Then
    Dim intNextIndex As Integer
    If lstExes.ListIndex = lstExes.ListCount - 1 Then
      intNextIndex = lstExes.ListCount - 2
    Else
      intNextIndex = lstExes.ListIndex
    End If
    lstExes.RemoveItem (lstExes.ListIndex)
    mcolExes.Remove (strIndex)
    If lstExes.ListCount = 0 Then
      Call lstExes_Click
      lstCustomers.Enabled = False
    Else
      lstExes.ListIndex = intNextIndex
    End If
    Call SetUploadStatus
    Call CheckList
  End If
End Sub

Private Sub CheckList()
  Dim bolMakeEnable As Boolean
  bolMakeEnable = (lstExes.ListCount > 0)
  lstExes.Enabled = bolMakeEnable
  txtResponse.Enabled = bolMakeEnable
  cmdRemove.Enabled = bolMakeEnable
  lstCustomers.Enabled = bolMakeEnable
End Sub
Private Sub cmdSelectAll_Click()
  Dim intListIndex As Integer
  intListIndex = lstCustomers.ListIndex
  Dim intItem As Integer
  For intItem = 0 To lstCustomers.ListCount - 1
    lstCustomers.Selected(intItem) = True  '' Check this item.
  Next intItem
  lstCustomers.ListIndex = intListIndex
End Sub

Private Sub cmdViewLastResponse_Click()
  Call MsgBox("Last response from the server was:" & vbCrLf & mstrLastResponse, vbInformation)
End Sub

Private Sub Form_Load()
  mbolCanExit = True
  Caption = App.Title
  cntTherm.Min = 0
  Set mcolExes = New Dictionary  '' MBADRAN
  
  Call StepSet(envPGetInfo)
  fraProgress.Visible = False
  Call CheckList
End Sub

Private Sub SetUploadStatus()
  'MBADRAN
  'cmdUpload.Enabled = mintCurPage = envPGetInfo And _
                      Len(Trim(txtFile.Text)) > 0 And _
                      cboCustomers.ListIndex > 0
  'cmdNext.Enabled = cmdUpload.Enabled
  If mintCurPage <> envPGetInfo Then
    cmdUpload.Enabled = False
    Exit Sub
  End If
  
  If mcolExes.Count = 0 Then
    cmdUpload.Enabled = False
    Exit Sub
  End If
  
  Dim bolEnabled As Boolean
  Dim strTrackInfo As String
  Dim intAttachment As Integer
  Dim astrTrackInfo() As String
  For intAttachment = 0 To lstExes.ListCount - 1
    strTrackInfo = mcolExes.Item(lstExes.List(intAttachment))
    astrTrackInfo = Split(strTrackInfo, "|")
    If Len(Trim(astrTrackInfo(UBound(astrTrackInfo)))) > 0 Then
      bolEnabled = True
      Exit For
    End If
  Next intAttachment
  cmdUpload.Enabled = bolEnabled
End Sub

Public Function DataLoad(ByRef strError As String) As Boolean
  Dim bolRetValue As Boolean
  bolRetValue = True
  
  Call cboCustomers.AddItem("Select a customer...")
  cboCustomers.ListIndex = 0
  Call SetUploadStatus
  
  On Error GoTo ErrHand
  
  'Dim mobjConFox As ADODB.Connection
  Set mobjConFox = New ADODB.Connection
  mobjConFox.ConnectionString = "DSN=" & cnsDSNName
  Call mobjConFox.Open

  Set rsCustomers = New ADODB.Recordset
  With rsCustomers
    .CursorLocation = adUseClient
    .CursorType = adOpenStatic
    .LockType = adLockOptimistic
    
    Dim strSQL As String
    strSQL = "SELECT Account, BtName as Name " & _
             "WHERE TYPE = 'M' AND " & _
             "Status <> 'P' AND " & _
             "Status <> 'X' " & _
             "FROM Customer ORDER BY Account"
    
    Call .Open(strSQL, mobjConFox)
    Do While Not .EOF()
      Call cboCustomers.AddItem(.Fields("Account").Value & " - " & .Fields("Name").Value)
      Call lstCustomers.AddItem(.Fields("Account").Value & " - " & .Fields("Name").Value)
      Call .MoveNext
    Loop
    '.Close
  End With
  
  '' MBADRAN Close when unloading the form.
  'Set rsCustomers = Nothing
  'mobjConFox.Close
  'Set mobjConFox = Nothing
  lstCustomers.Enabled = False
  cmdClear.Enabled = False
  cmdSelectAll.Enabled = False
  cmdInvert.Enabled = False
  
  On Error GoTo 0
  
  DataLoad = bolRetValue
  Exit Function

ErrHand:
  strError = "The following error has occured during the process " & _
             "of loading the list of the customers from the " & _
             "tracking customer file..." & vbCrLf & vbCrLf & _
             "Error Number: " & Err.Number & vbCrLf & _
             "Description: " & Err.Description & vbCrLf & vbCrLf & _
             "Cannot proceed."
             
  DataLoad = False
  Exit Function
End Function

Private Sub cboCustomers_Click()
  Call SetUploadStatus
End Sub

Private Sub cmdBrowse_Click()
  cntDialog.Flags = cdlOFNHideReadOnly Or cdlOFNFileMustExist
  cntDialog.Filter = "Bugs|BB??????.???|" & _
                     "Enhancements|EE??????.???|" & _
                     "Custom Programs|CC??????.???|" & _
                     "New Developments|NN??????.???"
  Call cntDialog.ShowOpen
  
  If Len(Trim(cntDialog.FileName)) > 0 Then
    txtFile.Text = Trim(cntDialog.FileName)
    Call SetUploadStatus
  End If
End Sub

Private Sub cmdClose_Click()
  Call Unload(Me)
End Sub

Private Sub cmdUpload_Click()
  'fraCustomerStatus.Visible = False
  Call StepSet(envPProgress)
'  If Connect() Then
'    If UpLoad() Then
'      Call StepSet(envPSuccessful)
'    Else
'      Call StepSet(envPError)
'    End If
'  Else
'    Call StepSet(envPError)
'  End If
  
  If Not Connect() Then
    Call StepSet(envPError)
    Exit Sub
  End If
  
  cmdClose.Enabled = False
  mbolCanExit = False
  'fraCustomerStatus.Visible = True
  
  Dim bolUploadFixes As Boolean
  bolUploadFixes = UpLoadFixes()
  
  'fraCustomerStatus.Visible = False
  fraProgress.Visible = False
  
  If bolUploadFixes Then
    ftpConnector.ABOR
    Call WaitUntilFTPFinishsExecuting
    ftpConnector.QUIT
    Call WaitUntilFTPFinishsExecuting
    Call ActionActivate(envARegister)
    cmdClose.Caption = "&Finish"
    Call SendResponses
    
    Call StepSet(envPSuccessful)
  Else
    Call StepSet(envPError)
  End If
  
  cmdClose.Enabled = True
  mbolCanExit = True

End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
  If Not mbolCanExit Then
    If UnloadMode = vbFormControlMenu Then
      Cancel = True
    Else
      'Added by MBADRAN
      Set rsCustomers = Nothing
      mobjConFox.Close
      Set mobjConFox = Nothing
    End If
    
  End If
End Sub

Private Sub ftpConnector_OnAsyncEvent(ByVal Filesize As Long, _
                                      ByVal BytesSoFar As Long, _
                                      ByVal Command As String, _
                                      ByVal Complete As Boolean)
  If Complete Then
    mbolFileUpLoadComplete = (Filesize = BytesSoFar)
  End If
  Call ShowDownloadStatus(Filesize, BytesSoFar)
End Sub

Private Sub ftpConnector_OnHostResponse(ByVal Code As Integer, ByVal Command As String, ByVal Response As String)
  mstrLastResponse = Response
End Sub

Private Sub ftpConnector_OnLogin(ByVal ResultCode As XFTPPROLib.cnstxFtpErrors)
  mintConnectorResponse = ResultCode
End Sub

Private Function GetFTPErrorDescription(ByVal intErr As Integer) As String
  Select Case intErr
    Case xFTPERR_AREADYCONNECTED
      GetFTPErrorDescription = "A connection already exists."
    Case xFTPERR_ACCOUNTNEEDED
      GetFTPErrorDescription = "Server awaits an account name"
    Case xFTPERR_LOGINREFUSED
      GetFTPErrorDescription = "The ACCOUNT/USER/PASSWD has been rejected."
    Case xFTPERR_NOTINITIALIZED
      GetFTPErrorDescription = "Session has not been initialized by FtpInit (internal error)"
    Case xFTPERR_NOTCONNECTED
      GetFTPErrorDescription = "User is not connected to a remote host"
    Case xFTPERR_SENDREFUSED
      GetFTPErrorDescription = "Can not send the data (network is down)"
    Case xFTPERR_CANTCREATESOCKET
      GetFTPErrorDescription = "The socket has not been created"
    Case xFTPERR_CONNECTREJECTED
      GetFTPErrorDescription = "Connect has been rejected (server is not a FTP server, ...)"
    Case xFTPERR_CANTCONNECT
      GetFTPErrorDescription = "The connect has failed"
    Case xFTPERR_TIMEOUT
      GetFTPErrorDescription = "The connect has timed-out"
    Case xFTPERR_INVALIDPARAMETER
      GetFTPErrorDescription = "Bad Parameters"
    Case xFTPERR_SERVERCANTEXECUTE
      GetFTPErrorDescription = "Directory does not exist."
    Case xFTPERR_NOREPLY
      GetFTPErrorDescription = "No reply has been received."
    Case xFTPERR_UNEXPECTEDANSWER
      GetFTPErrorDescription = "Received a reply. But this reply is not a valid FTP answer."
    Case xFTPERR_FILELOCKED
      GetFTPErrorDescription = "File is locked."
    Case xFTPERR_CMDNOTIMPLEMENTED
      GetFTPErrorDescription = "Command not implemented."
    Case Else
      GetFTPErrorDescription = "Unknown error."
  End Select
End Function

Private Sub WaitUntilFTPFinishsExecuting()
  Do While ftpConnector.StillExecuting
   DoEvents
  Loop
End Sub

Private Sub ActionActivate(ByVal evlAction As enmAction)
  Dim bytAction As Byte
  For bytAction = 0 To mcnsTotalActions - 1
    Select Case bytAction
      Case Is < evlAction
        Set imgStatus(bytAction) = imlStatus.ListImages.Item(2).Picture
        lblStatus(bytAction).FontBold = False
      Case Is = evlAction
        Set imgStatus(bytAction) = imlStatus.ListImages.Item(1).Picture
        lblStatus(bytAction).FontBold = True
      Case Is > evlAction
        Set imgStatus(bytAction) = Nothing
        lblStatus(bytAction).FontBold = False
    End Select
  Next bytAction
End Sub

Private Function Connect() As Boolean
  Call ActionActivate(envAConnect)
  
  mintConnectorResponse = 0
  Screen.MousePointer = vbHourglass
  
  cmdClose.Enabled = False
  mbolCanExit = False
  
  On Error GoTo ErrHand
  
  With ftpConnector
    .RemoteHost = LoadSetting(HOST_SERVER)
    Call WaitUntilFTPFinishsExecuting
    
    .LongDirs = False
    Call WaitUntilFTPFinishsExecuting
    
    .UserName = mcnsUserCode
    Call WaitUntilFTPFinishsExecuting
    
    .Password = mcnsPassword
    Call WaitUntilFTPFinishsExecuting
    
    .Timeout = 120
    Call WaitUntilFTPFinishsExecuting
    
    Call .Connect
    Call WaitUntilFTPFinishsExecuting
  End With
  Screen.MousePointer = vbDefault
  
  On Error GoTo 0
  
  cmdClose.Enabled = True
  mbolCanExit = True
  
  Dim bolRetValue As Boolean
  If mintConnectorResponse <> xFTPERR_OK Then
    bolRetValue = False
    Call MsgBox("The following FTP error occured while traing to login to Aria server... " & _
                vbCrLf & vbCrLf & _
                "Error Number: " & mintConnectorResponse & vbCrLf & _
                "Description: " & GetFTPErrorDescription(mintConnectorResponse) & _
                vbCrLf & vbCrLf & _
                "Cannot proceed.", vbCritical)
  Else
    bolRetValue = True
  End If
  
  mintConnectorResponse = 0
  Connect = bolRetValue
  Exit Function
  
ErrHand:
  Call MsgBox("The following error has occured while traing to login to Aria server..." & _
              vbCrLf & vbCrLf & _
              "Error Number: " & Err.Number & vbCrLf & _
              "Description: " & Err.Description & vbCrLf & vbCrLf & _
              "Cannot proceed.", vbCritical)
             
  Connect = False
  Exit Function
End Function

'Private Function UpLoad() As Boolean
'  Call ActionActivate(envAUpload)
'  lblFileName.Caption = "Uploading '" & Right(Trim(UCase(txtFile.Text)), 12) & "'"
'
'  Dim objFileSys As Scripting.FileSystemObject
'  Set objFileSys = New Scripting.FileSystemObject
'
'  Dim objFile As Scripting.File
'  Set objFile = objFileSys.GetFile(txtFile.Text)
'  lblTotalNum.Caption = ConvertToText(objFile.Size)
'  cntTherm.Max = objFile.Size
'  cmdClose.Enabled = False
'  mbolCanExit = False
'
'  Dim bolRetValue As Boolean
'  bolRetValue = False
'
'  Dim strLocalFile As String
'  strLocalFile = Trim(UCase(txtFile.Text))
'
'  Dim strFileName As String
'  strFileName = Right(strLocalFile, 12)
'
'  Dim strCustCode As String
'  strCustCode = UCase(Trim(Left(cboCustomers.List(cboCustomers.ListIndex), 5)))
'
'  '' MBADRAN
'  Dim strRemoteFile As String
'  strRemoteFile = "/SP/MBADRAN/" & strCustCode & "/" & strFileName
'
'  If ftpConnector.Connected Then
'    mbolFileUpLoadComplete = False
'
'    On Error GoTo ErrHand
'    Call WaitUntilFTPFinishsExecuting
'    Call ShowDownloadStatus(0, 0)
'    Call ftpConnector.STOR(strLocalFile, strRemoteFile)
'    Call WaitUntilFTPFinishsExecuting
'    On Error GoTo 0
'
'    ftpConnector.ABOR
'    Call WaitUntilFTPFinishsExecuting
'
'    ftpConnector.QUIT
'    Call WaitUntilFTPFinishsExecuting
'
'    bolRetValue = mbolFileUpLoadComplete
'    Caption = App.Title
'  Else
'    Call MsgBox("Connection to Aria server loast. Cannot proceed.", vbCritical)
'    bolRetValue = False
'  End If
'
'  cmdClose.Enabled = True
'  mbolCanExit = True
'
'  UpLoad = bolRetValue
'  Exit Function
'
'ErrHand:
'  Call MsgBox("The following error occured while traing to upload the file... " & _
'              vbCrLf & vbCrLf & _
'              "Error Number: " & Err.Number & vbCrLf & _
'              "Description: " & GetFTPErrorDescription(Err.Number) & _
'              vbCrLf & vbCrLf & _
'              "Cannot proceed.", vbCritical)
'  UpLoad = False
'  cmdClose.Enabled = True
'  mbolCanExit = True
'  Exit Function
'End Function
Private Function UpLoad(ByVal strFullFile As String, _
                        ByVal strCustCode As String) As Boolean
  
  'Call ActionActivate(envAUpload)
  lblFileName.Caption = "Uploading '" & Right(Trim(UCase(strFullFile)), 12) & "'"
  
  Dim objFileSys As Scripting.FileSystemObject
  Set objFileSys = New Scripting.FileSystemObject
  
  Dim objFile As Scripting.File
  Set objFile = objFileSys.GetFile(strFullFile)
  lblTotalNum.Caption = ConvertToText(objFile.Size)
  cntTherm.Max = objFile.Size
  
  Dim bolRetValue As Boolean
  bolRetValue = False
  
  Dim strLocalFile As String
  strLocalFile = Trim(UCase(strFullFile))
  
  Dim strFileName As String
  strFileName = objFile.Name
    
  Dim bolIsFree As Boolean
  If Len(strFileName) = 12 Then
    bolIsFree = Not IsNumeric(Mid$(strFileName, 3, 6))
  Else
    bolIsFree = True
  End If
  
  'Dim strCustCode As String
  'strCustCode = UCase(Trim(Left(cboCustomers.List(cboCustomers.ListIndex), 5)))
  
  '' MBADRAN
  Dim strRemoteFile As String
  Dim strRemoteFolder As String
  'strRemoteFolder = "/SP/MBADRAN/" & strCustCode & "/"
  strRemoteFolder = "/SP/" & strCustCode & "/"
  strRemoteFile = strRemoteFolder & IIf(bolIsFree, "Free/", "") & strFileName
  
  If ftpConnector.Connected Then
    mbolFileUpLoadComplete = False
   
    On Error GoTo ErrHand
    Call WaitUntilFTPFinishsExecuting
    
    'Badran Create Customer Folder if not exists.
    Call ftpConnector.MKD(strRemoteFolder)
    Call WaitUntilFTPFinishsExecuting
    If bolIsFree Then
      'Badran Create Customer Free Folder if not exists.
      Call ftpConnector.MKD(strRemoteFolder & "Free/")
      Call WaitUntilFTPFinishsExecuting
    End If
    
    Call ShowDownloadStatus(0, 0)
    Call ftpConnector.STOR(strLocalFile, strRemoteFile)
    Call WaitUntilFTPFinishsExecuting
    On Error GoTo 0
  
    bolRetValue = mbolFileUpLoadComplete
    Caption = App.Title
  Else
    Call MsgBox("Connection to Aria server lost. Cannot proceed.", vbCritical)
    cmdClose.Enabled = True
    mbolCanExit = True
    bolRetValue = False
  End If
  
  UpLoad = bolRetValue
  Exit Function

ErrHand:
  Call MsgBox("The following error occured while traing to upload the file... " & _
              vbCrLf & vbCrLf & _
              "Error Number: " & Err.Number & vbCrLf & _
              "Description: " & GetFTPErrorDescription(Err.Number) & _
              vbCrLf & vbCrLf & _
              "Cannot proceed.", vbCritical)
  UpLoad = False
  cmdClose.Enabled = True
  mbolCanExit = True
  Exit Function
End Function '


Private Sub ShowDownloadStatus(ByVal lngTotal As Double, ByVal lngSoFar As Double)
  If Not fraProgress.Visible Then
    fraProgress.Visible = True
  End If
  
  lblSoFarNum.Caption = ConvertToText(lngSoFar)
  lblSoFarNum.Refresh
  
  If lngTotal > 0 Then
    Caption = lblFileName.Caption & " - (" & _
              Format(((lngSoFar / lngTotal) * 100), "#0") & _
              " % Completed)"
    cntTherm.Value = lngSoFar
  End If
End Sub

Private Function ConvertToText(ByVal lngValue As Double) As String
  Dim lngRetValue As Double
  Dim strSuffix As String
  
  If Int(lngValue / 1024) = 0 Then
     lngRetValue = lngValue
     strSuffix = "bytes"
  Else
    If Int(lngValue / (1024 ^ 2)) = 0 Then
      lngRetValue = lngValue / 1024
      strSuffix = "KB"
    Else
      lngRetValue = lngValue / (1024 ^ 2)
      strSuffix = "MB"
    End If
  End If
  
  Dim strRetValue As String
  strRetValue = Format(lngRetValue, "#########0.00")
  strRetValue = strRetValue & Space(1) & strSuffix
  
  ConvertToText = strRetValue
End Function

Private Function GetOpenIssueHeader(ByVal strTrackID As String) As String
  If mobjConSql Is Nothing Then
    Set mobjConSql = New ADODB.Connection
    mobjConSql.ConnectionString = cnSQLConnectionStr
    Call mobjConSql.Open
    Set rsIssueHdr = New ADODB.Recordset
    With rsIssueHdr
      .CursorLocation = adUseClient
      .CursorType = adOpenStatic
      .LockType = adLockOptimistic
    End With
  End If
  
  Dim strSQL As String
'  strSQL = "SELECT cIssueNo, cCust_ID, dRespDate, lstARespDate, cRespType, cWe_Mail FROM SuIssHdr " & _
'             "WHERE cTrackRef = '" & strTrackID & "'"
  'CUSTPRG
  Dim strIssType As String
  Select Case Left$(strTrackID, 1)
    Case "C"
      strIssType = "CUSTPRG"
    Case "B"
      strIssType = "BUG"
    Case "N"
      strIssType = "NEWDEV"
    Case "E"
      strIssType = "ENHANCE"
  End Select
  strSQL = "SELECT cIssueNo, cCust_ID, dRespDate, lstARespDate, cRespType, cWe_Mail, LastLineNo, cMod_ID FROM SuIssHdr " & _
             "WHERE cTrackRef = '" & Mid$(strTrackID, 2) & "' AND " & _
               "ServiceID = 'SWDEV' AND cIssType = '" & strIssType & "'"
  
  If rsIssueHdr.Source <> "" Then
    rsIssueHdr.Close
  End If
  Call rsIssueHdr.Open(strSQL, mobjConSql)
  
  GetOpenIssueHeader = ""
  If Not rsIssueHdr.EOF Then
    Do While Not rsIssueHdr.EOF()
      GetOpenIssueHeader = GetOpenIssueHeader & _
          IIf(Len(GetOpenIssueHeader) = 0, "", ",") & HandleIssue()
      rsIssueHdr.MoveNext
    Loop
    
  End If
End Function

Private Sub lstCustomers_ItemCheck(Item As Integer)
  Dim strTrackInfo As String
  Dim strCustID As String
  strTrackInfo = mcolExes.Item(lstExes.List(lstExes.ListIndex))
  Dim astrTrackInfo() As String
  astrTrackInfo = Split(strTrackInfo, "|")
  strCustID = Mid(lstCustomers.List(Item), 1, 5)
  If lstCustomers.Selected(Item) Then
    mcolExes.Item(lstExes.List(lstExes.ListIndex)) = strTrackInfo & "," & strCustID
    mintIndividualCustomers = mintIndividualCustomers + 1
  Else
    mcolExes.Item(lstExes.List(lstExes.ListIndex)) = Replace(strTrackInfo, "," & strCustID, "")
    mintIndividualCustomers = mintIndividualCustomers - 1
  End If
  Call SetUploadStatus

  cmdClear.Enabled = (lstCustomers.SelCount > 0)
  cmdSelectAll.Enabled = (lstCustomers.ListCount <> lstCustomers.SelCount)
  cmdInvert.Enabled = True

End Sub

Private Sub lstExes_Click()
  Call RefreshEntry
End Sub

Private Sub RefreshEntry()
  
  If lstExes.ListIndex < 0 Then
    txtFile.Text = ""
    lblResponse.Caption = "Response (Editable Text)"
    txtResponse.Text = ""
  End If
  
  Dim strTrackInfo As String
  strTrackInfo = mcolExes.Item(lstExes.List(lstExes.ListIndex))
  If Trim(strTrackInfo) = "" Then
    Exit Sub
  End If
  Dim astrTrackInfo() As String
  Dim intInfo As Integer
  Dim strIssue As String
  astrTrackInfo = Split(strTrackInfo, "|")
  
  '' Information saved like (separated by pipe)
  '1. Full file path
  '2. Open Issue ID.
  '3. Response
  '4. Customer list comma separator.
  intInfo = LBound(astrTrackInfo)
  txtFile.Text = astrTrackInfo(intInfo)
  strIssue = astrTrackInfo(intInfo + 1)
  lblResponse.Caption = "Response " + IIf(strIssue = "      ", "", " on Isuue " & Mid(strIssue, 1, 6)) & " (Editable Text)"
  txtResponse.Text = astrTrackInfo(intInfo + 2)
  Call txtResponse_Validate(False)

 ' Clear customer list.
  Dim intItem As Integer
  If lstCustomers.SelCount > 0 Then
    For intItem = 0 To lstCustomers.ListCount - 1
      lstCustomers.Selected(intItem) = False
    Next intItem
  End If
  lstCustomers.ListIndex = 0
  
  Dim strCustomers As String
  strCustomers = astrTrackInfo(UBound(astrTrackInfo))
  If strCustomers = "" Then
    cmdClear.Enabled = False
    cmdSelectAll.Enabled = True
    cmdInvert.Enabled = True
    Exit Sub
  End If
    
  ' Select the customer list associated with this entry.
  For intItem = 0 To lstCustomers.ListCount - 1
    If InStrRev(strCustomers, "," & Mid(lstCustomers.List(intItem), 1, 5)) > 0 Then
      lstCustomers.Selected(intItem) = True  '' Check this item.
    End If
  Next intItem
  lstCustomers.ListIndex = 0
  cmdClear.Enabled = True
  cmdSelectAll.Enabled = (lstCustomers.ListCount <> lstCustomers.SelCount)
  cmdInvert.Enabled = True
End Sub

Private Sub txtResponse_GotFocus()
  mResponseText = Trim(txtResponse.Text)  '' Save old response.
End Sub

Private Sub txtResponse_Validate(Cancel As Boolean)
  txtResponse.Text = Trim(txtResponse.Text)
  If mResponseText = txtResponse.Text Then
    Exit Sub
  End If
    
  Dim intTextLen As Integer
  intTextLen = Len(txtResponse.Text)
  If intTextLen >= 2000 Then
    Call MsgBox("Response text exceeds 2000 characters by (" & intTextLen - 1999 & ") .", vbCritical)
    Cancel = True
    Exit Sub
  End If
  
  If InStr(1, txtResponse.Text, "'") > 0 Then
    Call MsgBox("Can't accept the character (') within the text. Please Replace it.", vbCritical)
    Cancel = True
    Exit Sub
  End If
  
  ' Text was changed, then save the new text.
  Dim strTrackInfo As String
  Dim astrTrackInfo() As String
  Dim intInfo As Integer
  strTrackInfo = mcolExes.Item(lstExes.List(lstExes.ListIndex))
  astrTrackInfo = Split(strTrackInfo, "|")
  intInfo = LBound(astrTrackInfo)
  strTrackInfo = Trim(astrTrackInfo(intInfo)) & "|" & _
                 Trim(astrTrackInfo(intInfo + 1)) & "|" & _
                 Trim(txtResponse.Text) & "|" & _
                 Trim(astrTrackInfo(intInfo + 3))
  mcolExes.Item(lstExes.List(lstExes.ListIndex)) = strTrackInfo
End Sub

Private Function UpLoadFixes() As Boolean
  Call ActionActivate(envAUpload)
  
  Dim intFix As Integer
  Dim strFileName As String
  Dim strCustomer As String * 5
  Dim strOpenIssue As String * 6
  Dim strResponse As String
  Dim strCustomerList As String
  Dim astrTrackInfo() As String
  Dim aCustomers() As String
  Dim strFolder As String * 5
  Dim intFolder As Integer
  Dim strLongCust As String
  ReDim mastrMailTo(0, 3)
  ReDim mastrIssue(0, 3)
  
  'loop fixes
  For intFix = 0 To lstExes.ListCount - 1
    astrTrackInfo = Split(Trim(mcolExes.Item(lstExes.List(intFix))), "|")
    strCustomerList = astrTrackInfo(3)
    If Len(Trim(strCustomerList)) > 0 Then
      strFileName = astrTrackInfo(0)
      If Len(Trim(astrTrackInfo(1))) = 0 Then
        strOpenIssue = ""
        strCustomer = ""
        lblIssueNo.Caption = ""
      Else
        strOpenIssue = Mid(astrTrackInfo(1), 1, 6)
        strCustomer = Right(astrTrackInfo(1), 5)
        strLongCust = ""
        rsCustomers.MoveFirst
        rsCustomers.Find "Account='" & strCustomer & "'"
        If Not rsCustomers.EOF Then
          strLongCust = lstCustomers.List(rsCustomers.AbsolutePosition - 1)
        End If
        lblIssueNo.Caption = strOpenIssue & ", For : " & strLongCust
      End If
      strResponse = astrTrackInfo(2)
       
      strCustomerList = Mid(strCustomerList, 2)
      aCustomers = Split(strCustomerList, ",")
      'loop customer list for this fix.
      For intFolder = LBound(aCustomers) To UBound(aCustomers)
        strFolder = aCustomers(intFolder)
        If Len(Trim(strFolder)) > 0 Then
          If strCustomer = strFolder Then
            lblCustAcc.Caption = strLongCust
          Else
            rsCustomers.MoveFirst
            rsCustomers.Find "Account='" & strFolder & "'"
            If rsCustomers.EOF Then
              lblCustAcc.Caption = strFolder
            Else
              lblCustAcc.Caption = lstCustomers.List(rsCustomers.AbsolutePosition - 1)
            End If
          End If
          
          'Upload this file.
          If UpLoad(strFileName, strFolder) Then
            Call RegisterEntry(strFileName, lblCustAcc.Caption, strResponse, IIf(strFolder = strCustomer, strOpenIssue, ""))
          Else
            UpLoadFixes = False
            Exit Function
          End If
          
        End If
      Next intFolder
    End If
  
  Next intFix
  
  UpLoadFixes = True

End Function

Private Function RegisterEntry(ByVal strFileName As String, _
                               ByVal strCustomer As String, _
                               ByVal strResponse As String, _
                               Optional ByVal strIssue As String) As Boolean
  '' Send the response
  If Len(Trim(strIssue)) = 0 Then
    '' Send Mail to customer Case ..... BEGIN
    Dim strMail As String
    strMail = GetEMailAddress("Send To " & strCustomer)
  
    If Len(strMail) > 0 Then
'      strResponse = "Dear Customer," & vbCrLf & strResponse & vbCrLf & vbCrLf & _
'                    "Tip: This is an automatically generated message, " & _
'                    "please do NOT reply to this message." & vbCrLf & vbCrLf & _
'                    "Thank you for contacting Aria Systems, Inc." & vbCrLf & _
'                    "Online Support Team."
      
      If Len(Trim(mastrMailTo(0, 0))) > 0 Then
        Call DynaArray("MailTo")
      End If
      
      mastrMailTo(UBound(mastrMailTo), 0) = strFileName
      mastrMailTo(UBound(mastrMailTo), 1) = strCustomer
      mastrMailTo(UBound(mastrMailTo), 2) = strResponse
      mastrMailTo(UBound(mastrMailTo), 3) = strMail
    End If
    '' Send Mail to customer Case ..... END
  
  Else  ' Open Issue case
    If Len(Trim(mastrIssue(0, 0))) > 0 Then
      Call DynaArray("Issue")
    End If
    mastrIssue(UBound(mastrIssue), 0) = strFileName
    mastrIssue(UBound(mastrIssue), 1) = strCustomer
    mastrIssue(UBound(mastrIssue), 2) = strResponse
    mastrIssue(UBound(mastrIssue), 3) = strIssue
  End If
End Function

Private Function IsValidMail(ByVal strMail As String) As Boolean
  strMail = Trim(strMail)
  ' Empty or has a space.
  If (Len(strMail) = 0) Or (InStr(1, strMail, " ") > 0) Then
    IsValidMail = False
    Exit Function
  End If
  Dim intAt As Integer
  intAt = InStr(1, strMail, "@")
  ' If Not found or it's the first character or it's the last character.
  If (intAt <= 1) Or (intAt = Len(strMail)) Then
    IsValidMail = False
    Exit Function
  End If
    
  ' If the dot is before the @
  'AMH [Start]
  'If InStr(1, strMail, ".") < intAt Then
  '  IsValidMail = False
  '  Exit Function
  'End If
  'AMH [End]
  strMail = Mid$(strMail, intAt + 1)
    
  ' If still have another @
  If InStr(1, strMail, "@") > 0 Then
    IsValidMail = False
    Exit Function
  End If
   
  intAt = InStr(1, strMail, ".")
  ' If It's the first character after @ or not exists or it's the last character.
  If (intAt <= 1) Or (intAt = Len(strMail)) Then
    IsValidMail = False
    Exit Function
  End If
  IsValidMail = True  ' Pass the validation.
End Function

Private Sub SendResponses()
  Dim intToUpdate As Integer
  Dim strExe As String
  Dim strCust As String * 5
  Dim strResponse As String
  Dim strEntry As String * 7
  Dim intBatch As Integer
  Dim strSqlInsert As String
  
  ' Update Tracking Entries and Register the entry to this customer.
  If Len(Trim(mastrMailTo(0, 0))) > 0 Then
    Dim strEmail As String
    
    ' Loop all the lines to update the entry and the upload dictionary.
    For intToUpdate = LBound(mastrMailTo) To UBound(mastrMailTo)
      strExe = Trim(mastrMailTo(intToUpdate, 0))
      If Len(Trim(strExe)) <> 0 Then
        strCust = Trim(mastrMailTo(intToUpdate, 1))
        strResponse = Trim(mastrMailTo(intToUpdate, 2))
        strEmail = Trim(mastrMailTo(intToUpdate, 3))
        
        intBatch = Val(Right(strExe, 3))
        strEntry = Mid(Right(strExe, 11), 1, 7)
        
        strSqlInsert = "INSERT INTO SUUPLOAD " & _
          "(nBatchNo,cTrackNo,cCust_ID,cEmail_Add,dGenDate) VALUES (" & intBatch & ",'" & strEntry & "','" & _
          strCust & "','" & strEmail & "'," & _
            Date2String(Date) & ")"
        mobjConFox.Execute (strSqlInsert)
        Call SendMail(strExe, strResponse, strEmail)  ' Send response mail.
      End If
    Next intToUpdate
  End If
  
  ' Update Open Issues If there.
  If Len(Trim(mastrIssue(0, 0))) > 0 Then
    ' Loop all the lines to update the entry and the upload dictionary.
    For intToUpdate = LBound(mastrIssue) To UBound(mastrIssue)
      strExe = Trim(mastrIssue(intToUpdate, 0))
      If Len(Trim(strExe)) <> 0 Then
        strCust = Trim(mastrIssue(intToUpdate, 1))
        strResponse = Trim(mastrIssue(intToUpdate, 2))
        strEmail = "Open Issue # " & Trim(mastrIssue(intToUpdate, 3))
        intBatch = Val(Right(strExe, 3))
        strEntry = Mid(Right(strExe, 11), 1, 7)
        
        'Initialize other fields --- By mhm
        'strSqlInsert = "INSERT INTO SUUPLOAD " & _
          "(nBatchNo,cTrackNo,cCust_ID,cEmail_Add,dGenDate) VALUES (" & intBatch & ",'" & strEntry & "','" & _
          strCust & "','" & strEmail & "'," & _
            Date2String(Date) & ")"
        strSqlInsert = "INSERT INTO SUUPLOAD " & _
          "(nBatchNo,cTrackNo,cCust_ID,cEmail_Add,dGenDate,mresponse,dspackdate,cadd_user,cadd_time,dadd_date,llok_stat,clok_user,dlok_date,clok_time) VALUES (" & intBatch & ",'" & strEntry & "','" & _
          strCust & "','" & strEmail & "'," & _
            Date2String(Date) & ",' '" & " " & ",{}" & ",' '" & ",{}" & ",{}" & ",0" & ",' '" & ",{}" & ",{}" & " " & ")"
            
            
        mobjConFox.Execute (strSqlInsert)
          
        ' Send the Open Issue
        Call SendIssue(strExe, strResponse, Trim(mastrIssue(intToUpdate, 3)))
         
      End If
    Next intToUpdate
  End If
  
  'Update Tracking entries
  For intToUpdate = 0 To lstExes.ListCount - 1
    strEntry = lstExes.List(intToUpdate)
    If Len(strEntry) <> 0 Then
      Call UpdateEntry(strEntry)
    End If
  Next intToUpdate
  
End Sub

Private Function Date2String(ByVal dDate As Date, Optional ByVal bolSQL As Boolean) As String
  If Len(Trim(dDate)) = 0 Then
    dDate = Date
  End If
  Date2String = IIf(bolSQL, "", "{") & Month(dDate) & "/" & Day(dDate) & "/" & Year(dDate) & IIf(bolSQL, "", "}")
End Function

Private Sub SendMail(ByVal strExe As String, ByVal strResponse As String, strMail As String)
  Dim objMail As ASPEMAILLib.MailSender
  Set objMail = New ASPEMAILLib.MailSender
  With objMail
    '.Host = "ARIAWEB.ARIA.COM.EG"
    .Host = "ARIAWEB"
    .From = "SUPPORT@ARIA.COM.EG"
    .FromName = "Aria Systems Support team"
    .Subject = "Individual Fix " & Right(strExe, 12) & " Now Available for Download."
    .Body = strResponse
    Call .AddAddress(strMail)
    Call .AddReplyTo(strMail)
    Call .Send
  End With
  Set objMail = Nothing     'Clear memory
End Sub

Private Sub SendIssue(ByVal strExe As String, ByVal strResponse As String, strIssue As String)
  Dim strSQLCommand As String
  Dim intNextLine As Integer
  Dim strModID As String
  'Get Last Line No ..... BEGIN
  '
  'Stop
  strSQLCommand = "SELECT LastLineNo, cMod_ID FROM suIssHdr WHERE (CIssueNo = '" & strIssue & "')"
  Set rsIssueHdr = New ADODB.Recordset
  Call rsIssueHdr.Open(strSQLCommand, mobjConSql)
  intNextLine = rsIssueHdr.Fields("LastLineNo") + 1
  strModID = Left$(rsIssueHdr.Fields("cMod_ID"), 2)
  'Get Last Line No ..... END
  
  ' Update the Header (Include the last line no ..... BEGIN)
  'strSQLCommand = "UPDATE suIssHdr SET dRespDate = '" & Date2String(Date, True) & _
      "',lstARespDate = '" & Date2String(Date, True) & _
      "',CRespType = 'Y' WHERE (CIssueNo = '" & strIssue & "')"
  strSQLCommand = "UPDATE suIssHdr SET dRespDate = '" & Date2String(Date, True) & _
      "',lstARespDate = '" & Date2String(Date, True) & _
      "',CRespType = 'Y', LastLineNo = " & intNextLine & " WHERE (CIssueNo = '" & strIssue & "')"
  ' Update the Header (Include the last line no ..... END)
  
  Call mobjConSql.Execute(strSQLCommand)
  
'(Evaluate the response length to update the NotePad File) ..... BEGIN
'  ' Calculate the new line no.
'  Dim rsMaxLine As ADODB.Recordset
'  Set rsMaxLine = New ADODB.Recordset
'  With rsMaxLine
'    .CursorLocation = adUseClient
'    .CursorType = adOpenStatic
'    .LockType = adLockOptimistic
'  End With
'  Call rsMaxLine.Open("SELECT MAX(intLineNo) + 1 AS nMaxLine From SuIssDt WHERE (CIssueNo = '" & _
'    strIssue & "')", mobjConSql)
'  Dim intNextLine As Integer
'  intNextLine = IIf(IsNull(rsMaxLine.Fields(0).Value), 1, IsNull(rsMaxLine.Fields(0).Value))
'strResponse
Dim intResponses As Integer
Dim intRespLine As Integer
Dim strResp As String
intResponses = (Len(strResponse) / 5000) + 1
For intRespLine = 1 To intResponses
  strResp = Mid$(strResponse, 1 + ((intRespLine - 1) * 5000), 5000)
  strSQLCommand = "INSERT INTO NotePade " & _
    "(CIssueNo,NoteLineNo,Internal,mNote,intLineNo) VALUES ('" & _
    strIssue & "', " & intRespLine & ", 0, '" & strResp & "'," & _
    intNextLine & ")"
  Call mobjConSql.Execute(strSQLCommand)  '' Add this line.
Next intRespLine

'(Evaluate the response length to update the NotePad File) ..... END
  
  
  ' Update the detail table.
  ' now update the subject instead of the response ..... BEGIN
  strResponse = "Your Issue " & strIssue & " on the " & strModID & " Module is Completed."
  ' now update the subject instead of the response ..... END

  ' Update the details table ..... BEGIN
  strSQLCommand = "INSERT INTO SuIssDt " & _
      "(CIssueNo, cRespType, cRespBy, DRespDate, tRespTime, intLineNo, NotificationSent, Hidden,mRespSubject) " & _
      "VALUES ('" & strIssue & "','Y','ARIA','" & _
      Date2String(Date, True) & "','" & FormatDateTime(Now, vbShortTime) & "'," & intNextLine & ",1,0,'" & strResponse & "')"

  ' Updating will send the Open Issue to the customer.
  Call mobjConSql.Execute(strSQLCommand)
  ' Update the details table ..... END
  
  'Call rsMaxLine.Close
  'Set rsMaxLine = Nothing
  
  ' Update the Notification table ..... BEGIN
  ' AMH Update the Notification table in case of custom program [Start]
  Dim strEntry As String
  strEntry = Mid(Right(strExe, 11), 1, 7)
  If strEntry = "C" Then
     strSQLCommand = "INSERT INTO NotificationLog " & _
       "(cIssueNo,nWarningTimes,cCountStatus,dCountInit,nCountPeriod) VALUES ('" & _
       strIssue & "',0,'A','" & Date2String(Date, True) & "',2)"
    
     On Error Resume Next  '' AMH. Till discus it with Hossam.
     Call mobjConSql.Execute(strSQLCommand)
  End If
  ' AMH [End]
  ' strSQLCommand Update the Notification table ..... END
  
End Sub

Private Sub UpdateEntry(ByVal strEntry As String)
  Dim strID As String
  strID = Right$(strEntry, 6)
  If Not IsNumeric(strID) Then
    Exit Sub
  End If
  
  Dim strUpdateSQL As String
  Dim strTable As String
  Dim strType As String
    
  strType = Mid$(strEntry, 1, 1)
  Select Case strType
    Case "B"
      strUpdateSQL = "cBug_ID='" & strID & "'"
      strTable = "BUG"
    
    Case "E"
      strUpdateSQL = "cEnh_ID='" & strID & "'"
      strTable = "ENHANCE"
    
    Case "C"
      strUpdateSQL = "ccPrgID='" & strID & "'"
      strTable = "CUSTPRG"
    
    Case "N"
      strUpdateSQL = "cPrg_ID='" & strID & "'"
      strTable = "SUNWDVLP"
  
  End Select
  strUpdateSQL = "UPDATE " & strTable & _
      " SET lDailySent=.F., lTransmit=.T., lHavAttch=.T. WHERE " & strUpdateSQL

  mobjConFox.Execute (strUpdateSQL)  '' Update the entry.
  
End Sub

Private Sub DynaArray(strArray As String)
  Dim aTempArray() As String
  Dim intIndex As Integer
  If strArray = "MailTo" Then
    ReDim aTempArray(UBound(mastrMailTo), 3)
    For intIndex = 0 To UBound(mastrMailTo)
      aTempArray(intIndex, 0) = mastrMailTo(intIndex, 0)
      aTempArray(intIndex, 1) = mastrMailTo(intIndex, 1)
      aTempArray(intIndex, 2) = mastrMailTo(intIndex, 2)
      aTempArray(intIndex, 3) = mastrMailTo(intIndex, 3)
    Next intIndex
    ReDim mastrMailTo(UBound(mastrMailTo) + 1, 3)
    For intIndex = 0 To UBound(mastrMailTo) - 1
      mastrMailTo(intIndex, 0) = aTempArray(intIndex, 0)
      mastrMailTo(intIndex, 1) = aTempArray(intIndex, 1)
      mastrMailTo(intIndex, 2) = aTempArray(intIndex, 2)
      mastrMailTo(intIndex, 3) = aTempArray(intIndex, 3)
    Next intIndex
  End If
  
  If strArray = "Issue" Then
    ReDim aTempArray(UBound(mastrIssue), 3)
    For intIndex = 0 To UBound(mastrIssue)
      aTempArray(intIndex, 0) = mastrIssue(intIndex, 0)
      aTempArray(intIndex, 1) = mastrIssue(intIndex, 1)
      aTempArray(intIndex, 2) = mastrIssue(intIndex, 2)
      aTempArray(intIndex, 3) = mastrIssue(intIndex, 3)
    Next intIndex
    ReDim mastrIssue(UBound(mastrMailTo) + 1, 3)
    For intIndex = 0 To UBound(mastrIssue) - 1
      mastrIssue(intIndex, 0) = aTempArray(intIndex, 0)
      mastrIssue(intIndex, 1) = aTempArray(intIndex, 1)
      mastrIssue(intIndex, 2) = aTempArray(intIndex, 2)
      mastrIssue(intIndex, 3) = aTempArray(intIndex, 3)
    Next intIndex
  End If
End Sub


Private Function GetResponse(ByVal strFix2UpLoad As String, strOpenIssue As String) As String
  Dim strEntType As String * 1
  Dim strEntry As String * 6
  
  Dim strBuildNo As String * 3
  Dim strSPackNo As String * 2
  Dim strBody As String
  Dim strLineFeed As String
  
  strEntType = Mid$(strFix2UpLoad, 1, 1)
  strEntry = Mid$(strFix2UpLoad, 3, 6)
  'strBuildNo = "040"
  strBuildNo = "041"
  strSPackNo = "01"
  strLineFeed = vbCrLf
    
  GetResponse = "Dear Customer," & strLineFeed & strLineFeed
  If strEntType = "C" Then
    strBody = "Kindly be informed that your Custom Program " & strEntry & _
      " has been completed, and its installation file " & strFix2UpLoad & _
      " is now available for download from the Aria FTP site. Your system should be upgraded to Build " & strBuildNo
      '" and updated with Service Pack " & strSPackNo &
      strBody = strBody & _
      " otherwise you will not be able to install this file. Use the Download Service Pack program to download the file, and after the file is downloaded successfully, use the Install Service Packs/Fixes program to install it into your system." & strLineFeed & _
      "After you install the program, please take the time to use it thoroughly to see if it is working properly within your system or not. Should there be any problems, please add a new response on the open issue " & strOpenIssue & _
      " through our Open Issues Web site, and describe these problems clearly in your response. If you wish to send us any relating documents that may help our support team in diagnosing and fixing these problems, please send these documents as an email attachment to attachments@aria.com.eg. However, if the program works properly without any problems, please respond to confirm that the program meets your approval so that we may close the issue. If we don’t receive any response from you within 2 weeks from sending this email, we will assume that you totally approve the program, and as a result, the issue " & strOpenIssue & _
      " will be closed." & strLineFeed & _
      "Reference: To know how to use the Download Service Pack program and the Install Service Packs/Fixes program, read the (New Service Pack Program) document - Chapters 1 and 2, available in the Downloads section on our Support page at: http://www.ariany.com/support.asp" & strLineFeed & strLineFeed

  Else
  
'Kindly be informed that your Issue # {issue number} on the {module name} module has been completed and its fix file {fix number} is now available for download from the Aria FTP site. The description of this issue says: "enter description here".

'Your system should be upgraded to Build XX and updated with Service Pack XX otherwise you will not be able to install this fix file. Use the Download Service Pack program to download the file, and after the file is downloaded successfully, use the Install Service Packs/Fixes program to install it into your system. To know how to use the Download Service Pack program and the Install Service Packs/Fixes program, read the 'Aria Service Packs and Individual Fixes' document - Chapters 1 and 3, available in the Downloads section on our Support page at: <http://www.ariany.com/support.asp>.
    
    strBody = "Kindly be informed that your Issue " & strOpenIssue & _
      " has been completed and its fix file " & strFix2UpLoad & _
      " is now available for download from the Aria FTP site. Your system should be upgraded to Build " & strBuildNo & _
      " and updated with Service Pack " & strSPackNo & _
      " otherwise you will not be able to install this file. Use the Download Service Pack program to download the file, and after the file is downloaded successfully, use the Install Service Packs/Fixes program to install it into your system. To know how to use the Download Service Pack program and the Install Service Packs/Fixes program, read the (New Service Pack Program) document - Chapters 1 and 2, available in the Downloads section on our Support page at: http://www.ariany.com/support.asp." & strLineFeed & strLineFeed
  
  End If
  GetResponse = Trim(GetResponse) & Trim(strBody) & strLineFeed & _
      "Thank you for your cooperation!" & strLineFeed & _
      "Sincerely" & strLineFeed & _
      "Aria Online Support Team"
  
End Function  ' end of GetResponse. '

Private Function GetEMailAddress(ByVal strTitle As String) As String
  Dim strMail As String
  Do While True
    strMail = InputBox("Send mail To:", strTitle)
    If IsValidMail(strMail) Then
      GetEMailAddress = Trim(strMail)
      Exit Do
    Else
      If MsgBox("Invalid email address. Enter new mail.", vbQuestion + vbRetryCancel) = vbCancel Then
        Exit Do
      End If
    End If
  Loop
End Function

Private Function HandleIssue() As String
  Dim strCurrentIssueNo As String * 6
  strCurrentIssueNo = rsIssueHdr.Fields("cIssueNo").Value
  HandleIssue = strCurrentIssueNo & "-" & rsIssueHdr.Fields("cCust_ID").Value
  
  ' Add the email address if not found
  Dim strEmail As String
  Dim strSQL As String

  strEmail = Trim(rsIssueHdr.Fields("cWe_Mail").Value)
  If Len(strEmail) = 0 Then
    strEmail = GetEMailAddress("Contact mail for account: " & _
      Trim(rsIssueHdr.Fields("cCust_ID").Value) & _
      ", in issue: " & strCurrentIssueNo)
      
    If Len(strEmail) > 0 Then
      ' Update Email information into the issue header.
      strSQL = "Update SuIssHdr SET Cwe_mail = '" & strEmail & _
        "' WHERE (CIssueNo = '" & strCurrentIssueNo & "')"
         
      ' Issue the update command.
      Call mobjConSql.Execute(strSQL)
    End If
  End If
End Function
