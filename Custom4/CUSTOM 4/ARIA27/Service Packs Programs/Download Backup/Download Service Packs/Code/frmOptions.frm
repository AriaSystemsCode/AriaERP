VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Begin VB.Form frmOptions 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Options"
   ClientHeight    =   4920
   ClientLeft      =   2565
   ClientTop       =   1500
   ClientWidth     =   6150
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   178
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmOptions.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4920
   ScaleWidth      =   6150
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin TabDlg.SSTab tbsOptions 
      Height          =   4245
      Left            =   120
      TabIndex        =   23
      TabStop         =   0   'False
      Top             =   150
      Width           =   5895
      _ExtentX        =   10398
      _ExtentY        =   7488
      _Version        =   393216
      Style           =   1
      Tabs            =   2
      TabHeight       =   494
      WordWrap        =   0   'False
      ShowFocusRect   =   0   'False
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      TabCaption(0)   =   "General"
      TabPicture(0)   =   "frmOptions.frx":000C
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "picTabs(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Connection"
      TabPicture(1)   =   "frmOptions.frx":0028
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "picTabs(1)"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).ControlCount=   1
      Begin VB.PictureBox picTabs 
         BorderStyle     =   0  'None
         Enabled         =   0   'False
         Height          =   3810
         Index           =   1
         Left            =   -74970
         ScaleHeight     =   3810
         ScaleWidth      =   5850
         TabIndex        =   27
         TabStop         =   0   'False
         Top             =   300
         Width           =   5850
         Begin VB.Frame fraLoop 
            Height          =   1275
            Left            =   150
            TabIndex        =   33
            Top             =   2280
            Width           =   5580
            Begin VB.TextBox txtTimes 
               Height          =   285
               Left            =   2265
               TabIndex        =   19
               Top             =   840
               Width           =   900
            End
            Begin VB.TextBox txtSeconds 
               Height          =   285
               Left            =   150
               TabIndex        =   18
               Top             =   840
               Width           =   585
            End
            Begin VB.CheckBox chkTryAgain 
               Caption         =   "When Connection Fails, Try to Automatically Connect Again"
               Height          =   210
               Left            =   150
               TabIndex        =   17
               Top             =   225
               Width           =   4575
            End
            Begin VB.Label lblEvery 
               AutoSize        =   -1  'True
               Caption         =   "Times."
               Height          =   195
               Index           =   2
               Left            =   3225
               TabIndex        =   37
               Top             =   885
               Width           =   465
            End
            Begin VB.Label lblEvery 
               AutoSize        =   -1  'True
               Caption         =   "Seconds."
               Height          =   195
               Index           =   1
               Left            =   825
               TabIndex        =   36
               Top             =   885
               Width           =   660
            End
            Begin VB.Label lblEvery 
               AutoSize        =   -1  'True
               Caption         =   "Trails:"
               Height          =   195
               Index           =   3
               Left            =   2265
               TabIndex        =   35
               Top             =   585
               Width           =   435
            End
            Begin VB.Label lblEvery 
               AutoSize        =   -1  'True
               Caption         =   "Every:"
               Height          =   195
               Index           =   0
               Left            =   150
               TabIndex        =   34
               Top             =   585
               Width           =   480
            End
         End
         Begin VB.Frame fraProxy 
            Height          =   1230
            Left            =   135
            TabIndex        =   30
            Top             =   1005
            Width           =   5580
            Begin VB.TextBox txtFTP 
               Height          =   285
               Left            =   165
               TabIndex        =   15
               Top             =   750
               Width           =   4200
            End
            Begin VB.TextBox txtPort 
               Height          =   285
               Left            =   4515
               TabIndex        =   16
               Top             =   750
               Width           =   900
            End
            Begin VB.CheckBox chkUseProxy 
               Caption         =   "Connect Using Proxy"
               Height          =   210
               Left            =   165
               TabIndex        =   14
               Top             =   240
               Width           =   1845
            End
            Begin VB.Label lblFTP 
               AutoSize        =   -1  'True
               Caption         =   "FTP Address:"
               Height          =   195
               Left            =   165
               TabIndex        =   32
               Top             =   540
               Width           =   960
            End
            Begin VB.Label lblPort 
               AutoSize        =   -1  'True
               Caption         =   "Port:"
               Height          =   195
               Left            =   4515
               TabIndex        =   31
               Top             =   540
               Width           =   360
            End
         End
         Begin VB.Frame fraHostServer 
            Height          =   930
            Left            =   120
            TabIndex        =   28
            Top             =   60
            Width           =   5580
            Begin VB.TextBox txtHostServer 
               Height          =   285
               Left            =   165
               TabIndex        =   12
               Top             =   450
               Width           =   3465
            End
            Begin VB.CommandButton cmdResetHostServer 
               Caption         =   "Use Default"
               Height          =   312
               Left            =   3735
               TabIndex        =   13
               Top             =   435
               Width           =   1710
            End
            Begin VB.Label lblHostServer 
               AutoSize        =   -1  'True
               Caption         =   "Host Server:"
               Height          =   195
               Left            =   165
               TabIndex        =   29
               Top             =   195
               Width           =   915
            End
         End
      End
      Begin VB.PictureBox picTabs 
         BorderStyle     =   0  'None
         Enabled         =   0   'False
         Height          =   3810
         Index           =   0
         Left            =   15
         ScaleHeight     =   3810
         ScaleWidth      =   5850
         TabIndex        =   24
         TabStop         =   0   'False
         Top             =   300
         Width           =   5850
         Begin VB.CheckBox chkSelect 
            Caption         =   "Select What to Download"
            Height          =   225
            Left            =   300
            TabIndex        =   10
            Top             =   3025
            Width           =   2115
         End
         Begin VB.CheckBox chkInterActive 
            Caption         =   "Interactive Log Screen"
            Height          =   225
            Left            =   300
            TabIndex        =   8
            Top             =   2720
            Width           =   1935
         End
         Begin VB.CheckBox chkAutoRun 
            Caption         =   "Update Service Packs Automatically When Finished Downloading"
            Height          =   225
            Left            =   300
            TabIndex        =   11
            Top             =   3330
            Width           =   5145
         End
         Begin VB.CheckBox chkShowIntroduction 
            Caption         =   "Skip Introduction Screen"
            Height          =   225
            Left            =   300
            TabIndex        =   6
            Top             =   2415
            Width           =   2055
         End
         Begin VB.CheckBox chkSavePassword 
            Caption         =   "Remember Password"
            Height          =   225
            Left            =   3750
            TabIndex        =   9
            Top             =   2715
            Width           =   1815
         End
         Begin VB.CheckBox chkSaveUser 
            Caption         =   "Remember User ID"
            Height          =   225
            Left            =   3750
            TabIndex        =   7
            Top             =   2415
            Width           =   1650
         End
         Begin VB.Frame fraDownLoad 
            Height          =   2205
            Left            =   135
            TabIndex        =   25
            Top             =   60
            Width           =   5580
            Begin VB.CommandButton cmdDefAriaPath 
               Caption         =   "Use Default"
               Height          =   312
               Left            =   3735
               TabIndex        =   5
               Top             =   1755
               Width           =   1710
            End
            Begin VB.CommandButton cmdAriaDir 
               Caption         =   "..."
               Height          =   285
               Left            =   5115
               TabIndex        =   4
               TabStop         =   0   'False
               Top             =   1380
               Width           =   330
            End
            Begin VB.TextBox txtAriaPath 
               BackColor       =   &H8000000F&
               Height          =   285
               Left            =   165
               Locked          =   -1  'True
               TabIndex        =   3
               TabStop         =   0   'False
               Top             =   1380
               Width           =   4890
            End
            Begin VB.CommandButton cmdResetDirectory 
               Caption         =   "Use Default"
               Height          =   312
               Left            =   3735
               TabIndex        =   2
               Top             =   825
               Width           =   1710
            End
            Begin VB.CommandButton cmdDir 
               Caption         =   "..."
               Height          =   285
               Left            =   5115
               TabIndex        =   1
               TabStop         =   0   'False
               Top             =   450
               Width           =   330
            End
            Begin VB.TextBox txtDownloadTo 
               BackColor       =   &H8000000F&
               Height          =   285
               Left            =   165
               Locked          =   -1  'True
               TabIndex        =   0
               TabStop         =   0   'False
               Top             =   450
               Width           =   4890
            End
            Begin VB.Label lblAriaPath 
               AutoSize        =   -1  'True
               Caption         =   "Aria Advantage Series System Files Folder:"
               Height          =   195
               Left            =   165
               TabIndex        =   38
               Top             =   1125
               Width           =   3090
            End
            Begin VB.Label lblDownloadTo 
               AutoSize        =   -1  'True
               Caption         =   "Download Service Packs to this Folder:"
               Height          =   195
               Left            =   165
               TabIndex        =   26
               Top             =   195
               Width           =   2775
            End
         End
      End
   End
   Begin VB.CommandButton cmdApply 
      Caption         =   "Apply"
      Enabled         =   0   'False
      Height          =   312
      Left            =   4920
      TabIndex        =   22
      Top             =   4500
      Width           =   1095
   End
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   312
      Left            =   3720
      TabIndex        =   21
      Top             =   4500
      Width           =   1095
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   312
      Left            =   2490
      TabIndex        =   20
      Top             =   4500
      Width           =   1095
   End
End
Attribute VB_Name = "frmOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private mbolModified As Boolean
Private mbolInit As Boolean
Private Sub chkAutoRun_Click()
  Call ShowModified
End Sub
Private Sub chkInterActive_Click()
  Call ShowModified
End Sub
Private Sub chkSavePassword_Click()
  Call ShowModified
End Sub
Private Sub chkSaveUser_Click()
  Call ShowModified
End Sub
Private Sub chkSelect_Click()
  Call ShowModified
End Sub
Private Sub chkShowIntroduction_Click()
  Call ShowModified
End Sub
Private Sub chkTryAgain_Click()
  Call ShowModified
  Call SetReConnect
End Sub
Private Sub chkUseProxy_Click()
  Call ShowModified
  Call SetConnectionSettings
End Sub
Private Sub SetReConnect()
  lblEvery(0).Enabled = chkTryAgain
  lblEvery(1).Enabled = chkTryAgain
  lblEvery(2).Enabled = chkTryAgain
  lblEvery(3).Enabled = chkTryAgain
  txtSeconds.Enabled = chkTryAgain
  txtTimes.Enabled = chkTryAgain
End Sub
Private Sub SetConnectionSettings()
  lblFTP.Enabled = chkUseProxy
  txtFTP.Enabled = chkUseProxy
  lblPort.Enabled = chkUseProxy
  txtPort.Enabled = chkUseProxy
  If chkUseProxy = vbUnchecked Then
    txtFTP = vbNullString
    txtPort = vbNullString
  End If
End Sub
Private Sub cmdApply_Click()
  If txtTimes <= 0 Then
    txtTimes = 0
    chkTryAgain = vbUnchecked
  End If
  
  Call SetSetting(DOWNLOAD_TO, Trim$(txtDownloadTo))
  Call SetSetting(ARIA_SYS_PATH, Trim$(txtAriaPath))
  Call SetSetting(SKIP_INTRO, chkShowIntroduction)
  Call SetSetting(SAVE_ID, chkSaveUser)
  Call SetSetting(SAVE_PASSWORD, chkSavePassword)
  Call SetSetting(SELECT_TO_DOWN, chkSelect)
  Call SetSetting(AUTO_UPDATE, chkAutoRun)
  Call SetSetting(INTERACTIVE, chkInterActive)
  Call SetSetting(HOST_SERVER, Trim$(txtHostServer))
  Call SetSetting(USE_PROXY, chkUseProxy)
  Call SetSetting(FTP_ADDRESS, Trim$(txtFTP))
  Call SetSetting(PORT, Trim$(txtPort))
  Call SetSetting(RETRY_CONNECT, chkTryAgain)
  Call SetSetting(TIMES, txtTimes)
  Call SetSetting(NUM_SECONDS, txtSeconds)
  
  mbolModified = False
  cmdApply.Enabled = False
End Sub
Private Sub cmdAriaDir_Click()
  Dim strPath As String
  strPath = GetDir(hWnd, "Please select 'Aria Advantage Series' system files folder")
  If Len(Trim$(strPath)) Then
    txtAriaPath = strPath
    Call ShowModified
  End If
End Sub
Private Sub cmdDefAriaPath_Click()
  Dim strDefDir As String
  strDefDir = GetDefault(ARIA_SYS_PATH)
  If UCase(Trim$(strDefDir)) <> UCase(Trim$(txtAriaPath)) Then
    txtAriaPath = strDefDir
    Call ShowModified
  End If
End Sub
Private Sub cmdResetDirectory_Click()
  Dim strDefDir As String
  strDefDir = GetDefault(DOWNLOAD_TO)
  If UCase(Trim$(strDefDir)) <> UCase(Trim$(txtDownloadTo)) Then
    txtDownloadTo = strDefDir
    Call ShowModified
  End If
End Sub
Private Sub cmdCancel_Click()
  Unload Me
End Sub
Private Sub cmdDir_Click()
  Dim strPath As String
  strPath = GetDir(hWnd, "Please select where to download the service packs")
  If Len(Trim$(strPath)) Then
    txtDownloadTo = strPath
    Call ShowModified
  End If
End Sub
Private Sub cmdOk_Click()
  If mbolModified Then
    Call cmdApply_Click
  End If
  Unload Me
End Sub
Private Sub cmdResetHostServer_Click()
  Dim strServer As String
  strServer = GetDefault(HOST_SERVER)
  If UCase(Trim$(strServer)) <> UCase(Trim$(txtHostServer)) Then
    txtHostServer = strServer
    Call ShowModified
  End If
End Sub
Private Sub Form_Load()
  Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2

  tbsOptions.Tab = 0
  picTabs(0).Enabled = True
  
  mbolModified = False
  mbolInit = True
  
  txtDownloadTo = LoadSetting(DOWNLOAD_TO)
  txtAriaPath = LoadSetting(ARIA_SYS_PATH)
  chkShowIntroduction = LoadSetting(SKIP_INTRO)
  chkSaveUser = LoadSetting(SAVE_ID)
  chkSavePassword = LoadSetting(SAVE_PASSWORD)
  chkSelect = LoadSetting(SELECT_TO_DOWN)
  chkAutoRun = LoadSetting(AUTO_UPDATE)
  chkInterActive = LoadSetting(INTERACTIVE)
  txtHostServer = LoadSetting(HOST_SERVER)
  chkUseProxy = LoadSetting(USE_PROXY)
  txtFTP = LoadSetting(FTP_ADDRESS)
  txtPort = LoadSetting(PORT)
  chkTryAgain = LoadSetting(RETRY_CONNECT)
  txtTimes = LoadSetting(TIMES)
  txtSeconds = LoadSetting(NUM_SECONDS)
  
  Call SetConnectionSettings
  Call SetReConnect
  
  mbolInit = False
End Sub
Private Sub ShowModified()
  If Not mbolModified And Not mbolInit Then
    mbolModified = True
    cmdApply.Enabled = True
  End If
End Sub
Private Sub tbsOptions_Click(PreviousTab As Integer)
  picTabs(tbsOptions.Tab).Enabled = True
  picTabs(PreviousTab).Enabled = False
End Sub
Private Sub txtAriaPath_Change()
  Call ShowModified
End Sub
Private Sub txtAriaPath_Validate(Cancel As Boolean)
  If Len(Trim(txtAriaPath)) Then
    If Not DirectoryExist(txtAriaPath) Then
      MsgBox "Not a valid directory. Cannot accept.", vbCritical + vbOKOnly
      Cancel = True
    End If
  End If
End Sub
Private Sub txtDownloadTo_Change()
  Call ShowModified
End Sub
Private Sub txtDownloadTo_Validate(Cancel As Boolean)
  If Len(Trim(txtDownloadTo)) Then
    If Not DirectoryExist(txtDownloadTo) Then
      MsgBox "Not a valid directory. Cannot accept.", vbCritical + vbOKOnly
      Cancel = True
    End If
  End If
End Sub
Private Sub txtFTP_Change()
  Call ShowModified
End Sub
Private Sub txtHostServer_Change()
  Call ShowModified
End Sub
Private Sub txtPort_Change()
  Call ShowModified
End Sub
Private Sub txtSeconds_Change()
  Call ShowModified
End Sub
Private Sub txtTimes_Change()
  Call ShowModified
End Sub
Private Function DirectoryExist(ByVal strDir As String) As Boolean
  On Error GoTo ErrorTrap
  ChDir strDir
  DirectoryExist = True
  Exit Function

ErrorTrap:
  If Err.Number = 76 Then
    DirectoryExist = False
    Exit Function
  Else
    MsgBox "Error : " & Err.Number & vbCrLf & Err.Description, vbCritical + vbOKOnly
    Err.Clear
  End If
End Function
