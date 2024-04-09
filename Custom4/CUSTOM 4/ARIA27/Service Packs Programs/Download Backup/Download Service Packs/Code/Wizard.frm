VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{91FB09A2-B504-11D1-ADB8-00A024122F93}#1.0#0"; "XFTPPRO.OCX"
Object = "{48E59290-9880-11CF-9754-00AA00C00908}#1.0#0"; "MSINET.OCX"
Begin VB.Form frmWizard 
   Appearance      =   0  'Flat
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Wizard"
   ClientHeight    =   5055
   ClientLeft      =   1965
   ClientTop       =   1815
   ClientWidth     =   7155
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "Wizard.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   ScaleHeight     =   5055
   ScaleWidth      =   7155
   StartUpPosition =   2  'CenterScreen
   Tag             =   "10"
   Begin VB.Frame fraStep 
      BorderStyle     =   0  'None
      Caption         =   "Finish with an error"
      Enabled         =   0   'False
      Height          =   4425
      Index           =   4
      Left            =   0
      TabIndex        =   24
      Top             =   0
      Width           =   7245
      Begin VB.Label lblStep 
         Caption         =   "Finish with an error"
         Height          =   3930
         Index           =   4
         Left            =   2355
         TabIndex        =   25
         Top             =   210
         Width           =   4320
      End
      Begin VB.Image imgStep 
         BorderStyle     =   1  'Fixed Single
         Height          =   1830
         Index           =   3
         Left            =   210
         Picture         =   "Wizard.frx":0442
         Top             =   210
         Width           =   1815
      End
   End
   Begin VB.Frame fraStep 
      BorderStyle     =   0  'None
      Caption         =   "Introduction Screen"
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   178
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   4425
      Index           =   0
      Left            =   -10000
      TabIndex        =   12
      Tag             =   "Welcome..."
      Top             =   0
      Width           =   7155
      Begin VB.CheckBox chkShowIntro 
         Caption         =   "&Skip this screen in the future."
         Height          =   315
         Left            =   2535
         MaskColor       =   &H00000000&
         TabIndex        =   0
         Tag             =   "1002"
         Top             =   3660
         Width           =   3810
      End
      Begin VB.Label lblStep 
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         Caption         =   "Introduction"
         ForeColor       =   &H80000008&
         Height          =   2745
         Index           =   0
         Left            =   2535
         TabIndex        =   13
         Tag             =   "1001"
         Top             =   210
         Width           =   3960
      End
      Begin VB.Image imgStep 
         BorderStyle     =   1  'Fixed Single
         Height          =   4155
         Index           =   0
         Left            =   210
         Picture         =   "Wizard.frx":A6C4
         Top             =   210
         Width           =   2190
      End
   End
   Begin VB.Frame fraStep 
      BorderStyle     =   0  'None
      Caption         =   "Login"
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   178
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   4425
      Index           =   1
      Left            =   -10000
      TabIndex        =   14
      Tag             =   "Login to Aria Server"
      Top             =   0
      Width           =   7155
      Begin VB.CheckBox chkRemPassword 
         Caption         =   "Remember password"
         Height          =   225
         Left            =   2535
         TabIndex        =   1
         Top             =   2430
         Width           =   1800
      End
      Begin VB.TextBox txtUserID 
         Height          =   315
         Left            =   5175
         MaxLength       =   5
         TabIndex        =   2
         Top             =   3615
         Width           =   1665
      End
      Begin VB.TextBox txtPassword 
         Height          =   315
         IMEMode         =   3  'DISABLE
         Left            =   5175
         MaxLength       =   10
         PasswordChar    =   "*"
         TabIndex        =   3
         Top             =   4020
         Width           =   1665
      End
      Begin VB.Label lblLoginLabels 
         AutoSize        =   -1  'True
         Caption         =   "Password"
         Height          =   210
         Index           =   1
         Left            =   4320
         TabIndex        =   10
         Top             =   4072
         Width           =   690
      End
      Begin VB.Label lblLoginLabels 
         AutoSize        =   -1  'True
         Caption         =   "User ID"
         Height          =   210
         Index           =   0
         Left            =   4320
         TabIndex        =   9
         Top             =   3667
         Width           =   540
      End
      Begin VB.Label lblStep 
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         Caption         =   "Login"
         ForeColor       =   &H80000008&
         Height          =   1470
         Index           =   1
         Left            =   2535
         TabIndex        =   15
         Tag             =   "2001"
         Top             =   210
         Width           =   3960
      End
      Begin VB.Image imgStep 
         BorderStyle     =   1  'Fixed Single
         Height          =   4155
         Index           =   1
         Left            =   210
         Picture         =   "Wizard.frx":26F72
         Top             =   210
         Width           =   2190
      End
   End
   Begin VB.Frame fraStep 
      BorderStyle     =   0  'None
      Caption         =   "Progress"
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   178
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   4425
      Index           =   2
      Left            =   -10000
      TabIndex        =   16
      Tag             =   "Progress"
      Top             =   0
      Width           =   7155
      Begin InetCtlsObjects.Inet netConnection 
         Left            =   6390
         Top             =   2580
         _ExtentX        =   1005
         _ExtentY        =   1005
         _Version        =   393216
      End
      Begin VB.Frame fraDownProgress 
         BorderStyle     =   0  'None
         Height          =   1035
         Left            =   2535
         TabIndex        =   26
         Top             =   3135
         Visible         =   0   'False
         Width           =   4395
         Begin MSComctlLib.ProgressBar prgDownTherm 
            Height          =   210
            Left            =   15
            TabIndex        =   27
            Top             =   825
            Width           =   4380
            _ExtentX        =   7726
            _ExtentY        =   370
            _Version        =   393216
            Appearance      =   1
         End
         Begin VB.Label lblSoFar 
            AutoSize        =   -1  'True
            Caption         =   "Bytes Transfered so Far:"
            Height          =   195
            Left            =   0
            TabIndex        =   38
            Top             =   495
            Width           =   1800
         End
         Begin VB.Label lblTotal 
            AutoSize        =   -1  'True
            Caption         =   "File Size:"
            Height          =   195
            Left            =   0
            TabIndex        =   37
            Top             =   240
            Width           =   630
         End
         Begin VB.Label lblFileName 
            AutoSize        =   -1  'True
            Caption         =   "File Name:"
            Height          =   195
            Left            =   0
            TabIndex        =   36
            Top             =   0
            Width           =   750
         End
         Begin VB.Label lblSoFarNum 
            Alignment       =   1  'Right Justify
            AutoSize        =   -1  'True
            Caption         =   "0000"
            Height          =   195
            Left            =   4035
            TabIndex        =   29
            Top             =   495
            Width           =   360
         End
         Begin VB.Label lblTotalNum 
            Alignment       =   1  'Right Justify
            AutoSize        =   -1  'True
            Caption         =   "0000"
            Height          =   195
            Left            =   4035
            TabIndex        =   28
            Top             =   247
            Width           =   360
         End
      End
      Begin MSComctlLib.ImageList imlStatus 
         Left            =   6525
         Top             =   1275
         _ExtentX        =   1005
         _ExtentY        =   1005
         BackColor       =   -2147483643
         ImageWidth      =   32
         ImageHeight     =   32
         MaskColor       =   12632256
         _Version        =   393216
         BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
            NumListImages   =   2
            BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "Wizard.frx":43820
               Key             =   "Active"
            EndProperty
            BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
               Picture         =   "Wizard.frx":43B3C
               Key             =   "Done"
            EndProperty
         EndProperty
      End
      Begin VB.Frame fraStatus 
         BorderStyle     =   0  'None
         Height          =   1920
         Left            =   2535
         TabIndex        =   20
         Top             =   1065
         Width           =   3795
         Begin VB.Image imgStatus 
            Height          =   480
            Index           =   0
            Left            =   0
            Top             =   -45
            Width           =   480
         End
         Begin VB.Label lblStatus 
            AutoSize        =   -1  'True
            Caption         =   "Verifying User ID and Password"
            Height          =   195
            Index           =   0
            Left            =   450
            TabIndex        =   39
            Top             =   75
            Width           =   2265
         End
         Begin VB.Image imgStatus 
            Height          =   480
            Index           =   4
            Left            =   0
            Top             =   1440
            Width           =   480
         End
         Begin VB.Label lblStatus 
            AutoSize        =   -1  'True
            Caption         =   "Updating Service Packs"
            Height          =   195
            Index           =   4
            Left            =   450
            TabIndex        =   34
            Top             =   1575
            Width           =   1665
         End
         Begin VB.Label lblStatus 
            AutoSize        =   -1  'True
            Caption         =   "Connecting to Aria server"
            Height          =   195
            Index           =   1
            Left            =   450
            TabIndex        =   23
            Top             =   450
            Width           =   1785
         End
         Begin VB.Image imgStatus 
            Height          =   480
            Index           =   1
            Left            =   0
            Top             =   315
            Width           =   480
         End
         Begin VB.Image imgStatus 
            Height          =   480
            Index           =   3
            Left            =   0
            Top             =   1065
            Width           =   480
         End
         Begin VB.Label lblStatus 
            AutoSize        =   -1  'True
            Caption         =   "Retrieving Service Packs"
            Height          =   195
            Index           =   3
            Left            =   450
            TabIndex        =   22
            Top             =   1200
            Width           =   1800
         End
         Begin VB.Label lblStatus 
            AutoSize        =   -1  'True
            Caption         =   "Getting Service Packs Information"
            Height          =   195
            Index           =   2
            Left            =   450
            TabIndex        =   21
            Top             =   825
            Width           =   2430
         End
         Begin VB.Image imgStatus 
            Height          =   480
            Index           =   2
            Left            =   0
            Top             =   690
            Width           =   480
         End
      End
      Begin XFTPPROLib.XFTPPro ftpConnector 
         Left            =   6585
         Top             =   1890
         _Version        =   65536
         _ExtentX        =   847
         _ExtentY        =   847
         _StockProps     =   0
      End
      Begin VB.Frame fraInfoProgress 
         BorderStyle     =   0  'None
         Height          =   1035
         Left            =   2535
         TabIndex        =   30
         Top             =   3135
         Visible         =   0   'False
         Width           =   4395
         Begin MSComctlLib.ProgressBar prgInfoTherm 
            Height          =   210
            Left            =   15
            TabIndex        =   31
            Top             =   825
            Width           =   4380
            _ExtentX        =   7726
            _ExtentY        =   370
            _Version        =   393216
            Appearance      =   1
         End
         Begin VB.Label lblIfoStatus 
            AutoSize        =   -1  'True
            Caption         =   "Module description"
            Height          =   195
            Index           =   1
            Left            =   15
            TabIndex        =   33
            Top             =   465
            Width           =   1335
         End
         Begin VB.Label lblIfoStatus 
            AutoSize        =   -1  'True
            Caption         =   "Getting Service Packs information for..."
            Height          =   195
            Index           =   0
            Left            =   15
            TabIndex        =   32
            Top             =   240
            Width           =   2835
         End
      End
      Begin VB.Label lblStep 
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         Caption         =   "Progress"
         ForeColor       =   &H80000008&
         Height          =   810
         Index           =   2
         Left            =   2535
         TabIndex        =   17
         Tag             =   "2003"
         Top             =   210
         Width           =   3960
      End
      Begin VB.Image imgStep 
         BorderStyle     =   1  'Fixed Single
         Height          =   4155
         Index           =   2
         Left            =   210
         Picture         =   "Wizard.frx":43E58
         Top             =   210
         Width           =   2190
      End
   End
   Begin VB.Frame fraStep 
      BorderStyle     =   0  'None
      Caption         =   "Finished!"
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   178
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   4425
      Index           =   3
      Left            =   -10000
      TabIndex        =   18
      Tag             =   "Finished!"
      Top             =   0
      Width           =   7155
      Begin VB.Label lblSign 
         Alignment       =   1  'Right Justify
         Caption         =   "The Team"
         Height          =   480
         Left            =   2535
         TabIndex        =   41
         Top             =   3810
         Width           =   4335
      End
      Begin VB.Label lblPleaseUpdate 
         Caption         =   "Label1"
         Height          =   1425
         Left            =   2535
         TabIndex        =   40
         Top             =   975
         Width           =   4500
      End
      Begin VB.Label lblStep 
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         Caption         =   "Finished!"
         ForeColor       =   &H80000008&
         Height          =   930
         Index           =   3
         Left            =   2535
         TabIndex        =   19
         Tag             =   "2003"
         Top             =   195
         Width           =   4500
      End
      Begin VB.Image imgStep 
         BorderStyle     =   1  'Fixed Single
         Height          =   4155
         Index           =   5
         Left            =   210
         Picture         =   "Wizard.frx":60706
         Top             =   210
         Width           =   2190
      End
   End
   Begin VB.PictureBox picNav 
      Align           =   2  'Align Bottom
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   178
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   570
      Left            =   0
      ScaleHeight     =   570
      ScaleWidth      =   7155
      TabIndex        =   11
      TabStop         =   0   'False
      Top             =   4485
      Width           =   7155
      Begin VB.CommandButton cmdShowLog 
         Caption         =   "Log..."
         Height          =   312
         Left            =   105
         TabIndex        =   4
         Top             =   150
         Visible         =   0   'False
         Width           =   1092
      End
      Begin VB.CommandButton cmdNav 
         Cancel          =   -1  'True
         Caption         =   "&Cancel"
         Height          =   312
         Index           =   4
         Left            =   5910
         MaskColor       =   &H00000000&
         TabIndex        =   8
         Tag             =   "104"
         Top             =   150
         Width           =   1092
      End
      Begin VB.CommandButton cmdNav 
         Caption         =   "&Next >"
         Default         =   -1  'True
         Height          =   312
         Index           =   3
         Left            =   4620
         MaskColor       =   &H00000000&
         TabIndex        =   7
         Tag             =   "103"
         Top             =   150
         Width           =   1092
      End
      Begin VB.CommandButton cmdNav 
         Caption         =   "< &Back"
         Height          =   312
         Index           =   2
         Left            =   3510
         MaskColor       =   &H00000000&
         TabIndex        =   6
         Tag             =   "102"
         Top             =   150
         Width           =   1092
      End
      Begin VB.CommandButton cmdNav 
         Caption         =   "&Options..."
         Height          =   312
         Index           =   1
         Left            =   2250
         MaskColor       =   &H00000000&
         TabIndex        =   5
         Tag             =   "101"
         Top             =   150
         Width           =   1092
      End
      Begin VB.Line Line1 
         BorderColor     =   &H00808080&
         Index           =   1
         X1              =   108
         X2              =   7012
         Y1              =   0
         Y2              =   0
      End
      Begin VB.Line Line1 
         BorderColor     =   &H00FFFFFF&
         Index           =   0
         X1              =   108
         X2              =   7012
         Y1              =   24
         Y2              =   24
      End
   End
   Begin MSComctlLib.Toolbar Toolbar1 
      Align           =   1  'Align Top
      Height          =   660
      Left            =   0
      TabIndex        =   35
      Top             =   0
      Width           =   7155
      _ExtentX        =   12621
      _ExtentY        =   1164
      ButtonWidth     =   609
      ButtonHeight    =   1005
      Appearance      =   1
      _Version        =   393216
   End
End
Attribute VB_Name = "frmWizard"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Const STEP_INTRO              As Integer = 0
Const STEP_LOGIN              As Integer = 1
Const STEP_PROGRESS           As Integer = 2
Const STEP_FINISH             As Integer = 3
Const STEP_ERROR              As Integer = 4

Const BTN_OPTIONS             As Integer = 1
Const BTN_BACK                As Integer = 2
Const BTN_NEXT                As Integer = 3
Const BTN_CANCEL              As Integer = 4
Const BTN_CANCEL_WITHOUT_ASK  As Integer = 100

Const PROGRESS_NUM_STEPS      As Integer = 5
Const PROGRESS_VERIFY         As Integer = 0
Const PROGRESS_LOGIN          As Integer = 1
Const PROGRESS_GET_INFO       As Integer = 2
Const PROGRESS_DOWNLOAD       As Integer = 3
Const PROGRESS_UPDATE         As Integer = 4

Const DOWNLOAD_DIR            As String = "SP"
Const LOG_FILE_NAME           As String = "DSP-LOG.TXT"
Const ASP_VERIFY_FILE_NAME    As String = "DNLOGIN.ASP"
Const UPDATE_MYSELF_FILE_NAME As String = "SMSP"
Const MAX_DIGIT_FOR_VERSION   As Integer = 4
Const CANCEL_BUTT_MSG         As String = "Are you sure you want to exit ?"

Private mstrUserID            As String
Private mstrPassword          As String
Private mintNumOfSteps        As Integer
Private mintCurStep           As Integer
Private mintConnectorResponse As Integer
Private mstrFilesList         As String
Private mstrFailedFiles       As String
Private mobjModulesRecSet     As ADODB.Recordset
Private mbolReadingStandard   As Boolean
Private mstrCurFileName       As String
Private mbolError             As Boolean
Private mobjText              As AriaTextStream
Private mintTrialNumber       As Integer
Private mbolShouldUpdate      As Boolean
Private mstrMySPFileName      As String

Dim WithEvents mobjLogForm    As frmLog
Attribute mobjLogForm.VB_VarHelpID = -1
Private Sub SetFormInformation()
  fraStep(STEP_INTRO).Tag = "CAPTION=Introduction,BTN_OPTIONS=1,BTN_BACK=0,BTN_NEXT=1,BTN_CANCEL=1"
  fraStep(STEP_LOGIN).Tag = "CAPTION=Login to Aria server,BTN_OPTIONS=1,BTN_BACK=1,BTN_NEXT=1,BTN_CANCEL=1"
  fraStep(STEP_PROGRESS).Tag = "CAPTION=Progress,BTN_OPTIONS=0,BTN_BACK=0,BTN_NEXT=0,BTN_CANCEL=1"
  fraStep(STEP_FINISH).Tag = "CAPTION=Finish!,BTN_OPTIONS=0,BTN_BACK=0,BTN_NEXT=1,BTN_NEXT_CAPTION=&Finish,BTN_CANCEL=0"
  fraStep(STEP_ERROR).Tag = "CAPTION=Error,BTN_OPTIONS=0,BTN_BACK=0,BTN_NEXT=1,BTN_NEXT_CAPTION=&Finish,BTN_CANCEL=0"

  lblStep(STEP_INTRO) = "Welcome to the '" & App.Title & "' program." & _
                        vbCrLf & vbCrLf & _
                        "The '" & App.Title & "' program ensures that " & _
                        "you have the most recent standard and special updates." & _
                        vbCrLf & vbCrLf & _
                        "To keep your installed 'Aria Advantage Series' system " & _
                        "up-to-date, run the '" & App.Title & "' program " & _
                        "once a week." & _
                        vbCrLf & vbCrLf & _
                        "Click 'Next' to update your system with the latest " & _
                        "service packs availabe."
  
  lblStep(STEP_LOGIN) = "The '" & App.Title & "' program should login " & _
                        "to Aria server to be able to download the latest " & _
                        "service packs." & _
                        vbCrLf & vbCrLf & _
                        "Please enter your login ID and password."

  lblStep(STEP_PROGRESS) = "The '" & App.Title & "' program is finding out " & _
                           "which updates are available for your " & _
                           "'Aria Advantage Series' system."
                           
  lblStep(STEP_FINISH) = "Downloading service packs has completed successfully." & _
                         vbCrLf & _
                         "Thank you for using this program."
  
  lblPleaseUpdate = "Important Note: " & vbCrLf & _
                    "A new service pack for the '" & App.Title & "' program is downloaded, " & _
                    "it is highly recommended that you update the program with this service pack." & vbCrLf & vbCrLf & _
                    "To update the program please run..." & vbCrLf & _
                    UCase(App.Path & "\" & mstrMySPFileName)
  
  lblSign = "The '" & App.Title & "' Team" & vbCrLf & _
            "Aria Systems, Inc."

  lblStep(STEP_ERROR) = "The '" & App.Title & "' program failed " & _
                        "to download the service packs because of an error…" & vbCrLf & vbCrLf & _
                        "Please retry again later..." & vbCrLf & vbCrLf & _
                        "Aria Systems, Inc." & vbCrLf & _
                        "16 East 34th Street" & vbCrLf & _
                        "2nd Floor" & vbCrLf & vbCrLf & _
                        "New York, NY 10016" & vbCrLf & vbCrLf & _
                        "Technical Support: 212-714-1334" & vbCrLf & _
                        "Fax: 212-714-1378"
End Sub
Private Sub chkShowIntro_Click()
  Call SaveSetting(APP_CATEGORY, WIZARD_NAME, SKIP_INTRO, chkShowIntro)
End Sub
Private Sub cmdNav_Click(Index As Integer)
  Select Case Index
    Case BTN_OPTIONS
      Dim objOptionsForm As frmOptions
      Set objOptionsForm = New frmOptions
      objOptionsForm.Show vbModal
    
    Case BTN_CANCEL_WITHOUT_ASK
      cmdNav(BTN_CANCEL).Enabled = False
      Call Terminate
    
    Case BTN_CANCEL
      If MsgBox(CANCEL_BUTT_MSG, vbQuestion + vbYesNo) = vbYes Then
        cmdNav(BTN_CANCEL).Enabled = False
        Call Terminate
      End If
      
    Case BTN_BACK
      SetStep mintCurStep - 1
      
    Case BTN_NEXT
      If mintCurStep = STEP_FINISH Or mintCurStep = STEP_ERROR Then
        cmdNav(BTN_CANCEL).Enabled = False
        Call Terminate
      Else
        SetStep mintCurStep + 1
      End If
  End Select
End Sub

Private Sub Terminate()
  Hide
  DoEvents
  
  Dim objQuiting As frmQuiting
  Set objQuiting = New frmQuiting
  objQuiting.Show
  objQuiting.Refresh
  
  If Not mobjModulesRecSet Is Nothing Then
    If mobjModulesRecSet.State <> adStateClosed Then
      mobjModulesRecSet.Close
    End If
    Set mobjModulesRecSet = Nothing
  End If
  
  Call EndLogSession
  
  If ftpConnector.Connected Then
    Call WaitUntilFTPFinishsExecuting
    ftpConnector.ABOR
    Call WaitUntilFTPFinishsExecuting
    ftpConnector.Quit
    Call WaitUntilFTPFinishsExecuting
  End If
  
  Unload objQuiting
  Unload Me
  If mbolShouldUpdate Then
    MsgBox lblPleaseUpdate, vbInformation + vbOKOnly
  End If
  
  End
End Sub

Private Sub Form_Load()
  mintNumOfSteps = 5
  
  Call SetFormInformation
  
  Dim intStep As Integer
  For intStep = 0 To mintNumOfSteps - 1
    fraStep(intStep).Left = -10000
  Next intStep
  
  chkShowIntro = LoadSetting(SKIP_INTRO)
  If LoadSetting(SAVE_ID) Then
    txtUserID = LoadSetting(USER_ID)
  End If
  chkRemPassword = LoadSetting(SAVE_PASSWORD)
  If chkRemPassword = vbChecked Then
    txtPassword = LoadSetting(USER_PASSWORD)
  End If
  Call SetStep(IIf(chkShowIntro = vbChecked, STEP_LOGIN, STEP_INTRO))
End Sub
Private Sub StepLostFocus()
  Select Case mintCurStep
    Case STEP_INTRO
    Case STEP_LOGIN
      Dim strPass As String
      Dim strUser As String
      strUser = IIf(GetSetting(APP_CATEGORY, WIZARD_NAME, SAVE_ID, GetDefault(SAVE_ID)), Trim(txtPassword), vbNullString)
      SaveSetting APP_CATEGORY, WIZARD_NAME, USER_ID, Trim(txtUserID)
      
      strPass = IIf(chkRemPassword, Trim(txtPassword), vbNullString)
      SaveSetting APP_CATEGORY, WIZARD_NAME, USER_PASSWORD, strPass
    
    Case STEP_PROGRESS
    Case STEP_FINISH
    Case STEP_ERROR
  End Select
End Sub
Private Sub StepGotFocus()
  Select Case mintCurStep
    Case STEP_INTRO
    Case STEP_LOGIN
    Case STEP_PROGRESS
      Call ProgressStart
      
    Case STEP_FINISH
    Case STEP_ERROR
  End Select
End Sub
Private Sub SetStep(ByVal intStep As Integer)
  Call StepLostFocus
  
  fraStep(mintCurStep).Enabled = False
  fraStep(mintCurStep).Left = -10000
  fraStep(intStep).Left = 0
  fraStep(intStep).Enabled = True
  lblStatus(PROGRESS_UPDATE).Visible = LoadSetting(AUTO_UPDATE)
  lblPleaseUpdate.Visible = intStep = STEP_FINISH And mbolShouldUpdate = True
  
  Call SetCaption(intStep)
  Call SetNavBtns(intStep)
  Refresh
  
  Call StepGotFocus
End Sub
Private Sub SetNavBtns(ByVal intStep As Integer)
  mintCurStep = intStep
  
  Dim strCaption As String
  strCaption = GetStepInfo(intStep, "BTN_NEXT_CAPTION")
  strCaption = IIf(strCaption = Space(0), cmdNav(BTN_NEXT).Caption, strCaption)
  
  cmdNav(BTN_OPTIONS).Enabled = GetStepInfo(intStep, "BTN_OPTIONS")
  cmdNav(BTN_BACK).Enabled = GetStepInfo(intStep, "BTN_BACK")
  cmdNav(BTN_NEXT).Enabled = GetStepInfo(intStep, "BTN_NEXT")
  cmdNav(BTN_NEXT).Caption = strCaption
  cmdNav(BTN_CANCEL).Enabled = GetStepInfo(intStep, "BTN_CANCEL")
End Sub
Private Sub SetCaption(ByVal intStep As Integer, Optional ByVal strPercent As String = "")
  strPercent = IIf(Len(Trim$(strPercent)), " - " & strPercent & "%", Space(0))
  Caption = App.Title & " - " & GetStepInfo(intStep, "Caption") & strPercent
End Sub
Private Function GetStepInfo(ByVal intStep As Integer, ByVal strOption As String) As String
  Dim astrInfo() As String
  Dim astrFilter() As String
  
  strOption = Trim(UCase(strOption))
  astrInfo = Split(fraStep(intStep).Tag, ",", -1, vbTextCompare)
  astrFilter = Filter(astrInfo, strOption, True, vbTextCompare)
  GetStepInfo = Space(0)
  If UBound(astrFilter) <> -1 Then
    GetStepInfo = Replace(astrFilter(0), strOption & "=", Space(0), 1, -1, vbTextCompare)
  End If
End Function
Private Sub GoToProgressStepNumber(ByVal intStep As Integer)
  Dim intStepNum As Integer
  For intStepNum = 0 To PROGRESS_NUM_STEPS - 1
    Set imgStatus(intStepNum) = IIf(intStepNum = intStep, Me.imlStatus.ListImages.Item(1).Picture, Nothing)
    Select Case intStep
      Case Is < intStepNum
        Set imgStatus(intStepNum) = Nothing
        lblStatus(intStepNum).FontBold = False
      Case Is = intStepNum
        Set imgStatus(intStepNum) = imlStatus.ListImages.Item(1).Picture
        lblStatus(intStepNum).FontBold = True
      Case Is > intStepNum
        Set imgStatus(intStepNum) = imlStatus.ListImages.Item(2).Picture
        lblStatus(intStepNum).FontBold = False
    End Select
  Next intStepNum
  fraInfoProgress.Visible = intStep = PROGRESS_GET_INFO
  fraDownProgress.Visible = intStep = PROGRESS_DOWNLOAD
  Refresh
End Sub
Private Function ProgressVerifyLogin() As Integer
  Dim strErrorMessage As String
  GoToProgressStepNumber PROGRESS_VERIFY
  
  Dim strURL As String
  strURL = Trim(LoadSetting(HOST_SERVER)) & "/" & LCase(ASP_VERIFY_FILE_NAME) & _
           "?" & _
           "CustID=" & txtUserID & _
           "&" & _
           "CustPass=" & txtPassword
  
  Screen.MousePointer = vbHourglass
  On Error GoTo GetResponse
  Dim varRetHTML As String
  netConnection.RequestTimeout = 120
  
  varRetHTML = netConnection.OpenURL(strURL, icString)
  Screen.MousePointer = vbDefault
  
  If Len(Trim(varRetHTML)) = 0 Then
    strErrorMessage = "Invalid return value."
    GoTo GetResponse
  Else
    Dim intStart As Integer
    intStart = InStr(1, varRetHTML, "<title>", vbTextCompare)
    If intStart = 0 Then
      Dim intEqualPos As Integer
      Dim intSemiiPos As Integer
      
      intEqualPos = InStr(1, varRetHTML, "=", vbTextCompare)
      intSemiiPos = InStr(1, varRetHTML, ";", vbTextCompare)
      mstrUserID = Mid$(varRetHTML, intEqualPos + 1, intSemiiPos - (intEqualPos + 1))
      
      intEqualPos = InStr(intSemiiPos, varRetHTML, "=", vbTextCompare)
      intSemiiPos = InStr(intSemiiPos + 1, varRetHTML, ";", vbTextCompare)
      mstrPassword = Mid$(varRetHTML, intEqualPos + 1, intSemiiPos - (intEqualPos + 1))
    
      If Len(Trim(mstrUserID)) > 0 And Len(Trim(mstrPassword)) > 0 Then
        ProgressVerifyLogin = LOGIN_PROCEED
      Else
        MsgBox "Invalid login information. Please try again.", vbCritical
        netConnection.Cancel
        ProgressVerifyLogin = LOGIN_GOTO_LOGIN_INFO
      End If
    Else
      Dim intEnd As Integer
      intEnd = InStr(1, varRetHTML, "</title>", vbTextCompare)
      strErrorMessage = Mid$(varRetHTML, intStart + 7, intEnd - (intStart + 7))
      GoTo GetResponse
    End If
  End If
  Exit Function

GetResponse:
  netConnection.Cancel
  strErrorMessage = IIf(Len(Trim(strErrorMessage)) = 0, Err.Description, strErrorMessage)
  ProgressVerifyLogin = GetUserResponse(strErrorMessage)
  Exit Function

End Function
Private Sub ProgressStart()
  Call InitLogSession
  Dim intLoginResult As Integer
  
  intLoginResult = LOGIN_RETRY
  mintTrialNumber = 0
  Do While intLoginResult = LOGIN_RETRY
    intLoginResult = ProgressVerifyLogin()
  Loop
  Select Case intLoginResult
    Case LOGIN_GOTO_LOGIN_INFO
      SetStep STEP_LOGIN
      
    Case LOGIN_CANCEL
      Call cmdNav_Click(BTN_CANCEL_WITHOUT_ASK)
      
    Case LOGIN_ERROR
      SetStep STEP_ERROR
    
    Case LOGIN_PROCEED
      Call GoOnTheProgress
  End Select
End Sub
Private Sub GoOnTheProgress()
  Dim intLoginResult As Integer
  
  intLoginResult = LOGIN_RETRY
  mintTrialNumber = 0
  Do While intLoginResult = LOGIN_RETRY
    intLoginResult = ProgressLogin()
  Loop
  
  Select Case intLoginResult
    Case LOGIN_GOTO_LOGIN_INFO
      SetStep STEP_LOGIN
    
    Case LOGIN_CANCEL
      Call cmdNav_Click(BTN_CANCEL_WITHOUT_ASK)
    
    Case LOGIN_ERROR
      SetStep STEP_ERROR
      
    Case LOGIN_PROCEED
      If ProgressGetInfo() Then
        If ProgressDownload() Then
          If LoadSetting(AUTO_UPDATE) Then
             If ProgressUpdate() Then
               SetStep STEP_FINISH
             Else
               ' I could not run the service packs update program.
               SetStep STEP_ERROR
             End If
          Else
            ' I'm done downloading.
            SetStep STEP_FINISH
          End If
        Else
          ' Could not download the service packs.
          ' this will happen only if I have no service packs to update, otherwise
          ' the error handler will take care of it.
          SetStep STEP_FINISH
        End If
      Else
        ' Could not get the service packs information.
        SetStep STEP_ERROR
      End If
  End Select   ' For the process
End Sub
Private Function ProgressLogin() As Integer
  GoToProgressStepNumber PROGRESS_LOGIN
  mintConnectorResponse = 0
  Screen.MousePointer = vbHourglass
  
  With ftpConnector
    cmdNav(BTN_CANCEL).Enabled = False
    .RemoteHost = LoadSetting(HOST_SERVER)
    Call WaitUntilFTPFinishsExecuting
    .LongDirs = False
    Call WaitUntilFTPFinishsExecuting
'   .UseProxy = LoadSetting(USE_PROXY)
'   .ProxyHost = IIf(.UseProxy, LoadSetting(FTP_ADDRESS), vbNullString)
'   .ProxyPort = IIf(.UseProxy, LoadSetting(PORT), vbNullString)
'   .ProxyUserID = IIf(.UseProxy, mstrUserID, vbNullString)
'   .ProxyPassWd = IIf(.UseProxy, mstrPassword, vbNullString)
'   .ProxyType = xFTP_PROXY_USER_SITEPORT2
    .UserName = mstrUserID
    Call WaitUntilFTPFinishsExecuting
    .Password = mstrPassword
    Call WaitUntilFTPFinishsExecuting
    .TimeOut = 120
    
    .Connect
    Call WaitUntilFTPFinishsExecuting
    cmdNav(BTN_CANCEL).Enabled = True
  End With
  Screen.MousePointer = vbDefault
  ProgressLogin = LOGIN_PROCEED
  If mintConnectorResponse <> xFTPERR_OK Then
    ProgressLogin = GetUserResponse(GetFTPErrorDescription(mintConnectorResponse))
  End If
  mintConnectorResponse = 0
End Function
Private Function GetUserResponse(ByVal strErrDesc As String) As Integer
  Dim bolShowScreen As Boolean
  Dim intTotalTrials As Integer
  Dim bolAutoReConnect As Boolean
  
  bolAutoReConnect = LoadSetting(RETRY_CONNECT)
  mintTrialNumber = IIf(mintTrialNumber = 0, 1, mintTrialNumber + 1)
  If bolAutoReConnect Then
    intTotalTrials = LoadSetting(TIMES)
    bolShowScreen = intTotalTrials >= mintTrialNumber
  Else
    bolShowScreen = True
  End If
  
  If bolShowScreen Then
    Dim objErrForm As frmErrBox
    Set objErrForm = New frmErrBox
    Load objErrForm
    objErrForm.ErrorDesc = strErrDesc
    objErrForm.TrialNumber = mintTrialNumber
    objErrForm.LastTry = IIf(bolAutoReConnect, mintTrialNumber = intTotalTrials, False)
    objErrForm.Show vbModal
    GetUserResponse = objErrForm.ReturnValue
    Unload objErrForm
  Else
    GetUserResponse = LOGIN_ERROR
  End If
End Function
Private Function GetFTPErrorDescription(ByVal intErr As Integer) As String
  Select Case intErr
    Case xFTPERR_SERVERCANTEXECUTE
      GetFTPErrorDescription = "Directory does not exist."
    Case xFTPERR_NOTINITIALIZED
      GetFTPErrorDescription = "Session has not been initialized by FtpInit."
    Case xFTPERR_NOTCONNECTED
      GetFTPErrorDescription = "User is not connected to a remote host."
    Case xFTPERR_SENDREFUSED
      GetFTPErrorDescription = "Network is down."
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
Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
  If cmdNav(BTN_CANCEL).Enabled Then
    If MsgBox(CANCEL_BUTT_MSG, vbQuestion + vbYesNo) = vbYes Then
      Call cmdNav_Click(BTN_CANCEL_WITHOUT_ASK)
    End If
  End If
  Cancel = True
End Sub
Private Sub ftpConnector_OnAsyncEvent(ByVal Filesize As Long, ByVal BytesSoFar As Long, ByVal Command As String, ByVal Complete As Boolean)
  Dim strText As String
  
  If Complete Then
    If Filesize = BytesSoFar Then
      strText = mstrCurFileName & " is successfully transferred."
    Else
      strText = "Unable to download " & mstrCurFileName & _
                " due to an unknown error."
      mstrFailedFiles = mstrFailedFiles & UCase(mstrCurFileName)
    End If
  Else
    strText = ConvertToText(BytesSoFar) & " out of " & _
              ConvertToText(Filesize) & _
              " have been successfully transferred so far."
  End If
  
  Call UpdateLog(1, "RECEIVE", strText)
  Call ShowDownloadStatus(Filesize, BytesSoFar)
End Sub
Private Sub ftpConnector_OnDirList(ByVal DirLine As String, ByVal Complete As Boolean, ByVal retCode As Long)
  Dim strMdule As String
  If Len(Trim(DirLine)) Then
    If mbolReadingStandard Then
      If mobjModulesRecSet.EOF Then
        strMdule = Space(0)
      Else
        strMdule = mobjModulesRecSet!cApp_ID
      End If
    Else
      strMdule = Trim(txtUserID)
    End If
    
    Dim strFileName  As String
    strFileName = Replace(DirLine, "/" & DOWNLOAD_DIR & "/" & strMdule & "/", "")
    If Len(Trim(strFileName)) = 12 Then
      mstrFilesList = mstrFilesList & strFileName
    End If
  End If
End Sub
Private Sub ftpConnector_OnFTPError(ByVal ErrorCode As XFTPPROLib.cnstxFtpErrors, ByVal ErrorText As String)
  If Not mbolError Then
    mbolError = True
    
    Dim strMsg As String
    strMsg = "The following FTP Error has occurred: " & vbCrLf & vbCrLf & _
             "Number: " & ErrorCode & vbCrLf & _
             ErrorText & vbCrLf & vbCrLf & _
             "Click 'Ok' to abort the application."
    
    Call UpdateLog(0, "FTP Error", ErrorText)
    Call EndLogSession
    MsgBox strMsg, vbCritical + vbOKOnly, App.Title & " - FTP Error.."
    Call cmdNav_Click(BTN_CANCEL_WITHOUT_ASK)
  End If
End Sub
Private Sub ftpConnector_OnHostResponse(ByVal Code As Integer, ByVal Command As String, ByVal Response As String)
  Call UpdateLog(Code, Command, Mid$(Response, 5))
End Sub
Private Sub ftpConnector_OnLogin(ByVal ResultCode As XFTPPROLib.cnstxFtpErrors)
  mintConnectorResponse = ResultCode
End Sub
Private Function ProgressGetInfo() As Boolean
  GoToProgressStepNumber PROGRESS_GET_INFO
  Screen.MousePointer = vbHourglass
  
  Dim strSource As String
  strSource = LoadSetting(ARIA_SYS_PATH)
  strSource = strSource & IIf(Right(strSource, 1) = "\", "", "\")
  
  Dim strConn As String
  strConn = "DSN=DSSysFiles;SourceDB=" & strSource & ";"

  On Error GoTo ErrHand
  Set mobjModulesRecSet = New ADODB.Recordset
  
  Dim objCon As ADODB.Connection
  Set objCon = New ADODB.Connection
  objCon.ConnectionString = strConn
  Call objCon.Open
  
  With mobjModulesRecSet
    .CursorLocation = adUseClient
    .CursorType = adOpenStatic
    .LockType = adLockOptimistic
    .Open "SELECT * FROM SYDAPPL", objCon
  End With
  
  prgInfoTherm.Min = 0
  prgInfoTherm.Max = mobjModulesRecSet.RecordCount + 1
  prgInfoTherm.Value = prgInfoTherm.Min
  Call SetCaption(mintCurStep, ConvertPrecent(prgInfoTherm))
  
  mstrFilesList = Space(0)
  mbolReadingStandard = True
  Dim strDir As String
  Do While Not mobjModulesRecSet.EOF()
    Call WaitUntilFTPFinishsExecuting
    lblIfoStatus(1) = "The '" & Trim(mobjModulesRecSet!cApp_Name) & "' module."
    Call DownloadList(mobjModulesRecSet!cApp_ID)
    prgInfoTherm.Value = prgInfoTherm.Value + 1
    Call SetCaption(mintCurStep, ConvertPrecent(prgInfoTherm))
    mobjModulesRecSet.MoveNext
  Loop
  mbolReadingStandard = False
  lblIfoStatus(1) = "The custom module(s)"
  Call DownloadList(Trim(txtUserID))
  prgInfoTherm.Value = prgInfoTherm.Max
  Call SetCaption(mintCurStep)
  
  ProgressGetInfo = True
  Screen.MousePointer = vbDefault
  Exit Function

ErrHand:
  MsgBox "The following Error occurred while reading the " & _
         "service packs information: " & vbCrLf & vbCrLf & _
         "Error: " & Err.Description & vbCrLf & vbCrLf & _
         "Cannot proceed.", vbCritical + vbOKOnly
  Exit Function
End Function
Private Function ConvertPrecent(ByVal objTherm As MSComctlLib.ProgressBar)
  Dim lngPercent As Long
  
  lngPercent = (objTherm.Value / objTherm.Max) * 100
  ConvertPrecent = Format$(lngPercent, "##0.00")
End Function
Private Sub DownloadList(ByVal strDownModule As String)
  Dim strDir As String
  
  strDir = "/" & DOWNLOAD_DIR & "/" & strDownModule & "/"
  ftpConnector.List strDir
  Call WaitUntilFTPFinishsExecuting
End Sub
Private Sub GetMyServicePackFileName(ByRef intCount As Integer, ByRef strNames As String)
  Dim strFileName As String
  Dim intVersion As Integer
  Dim intDownloadVer As Integer
  Dim strTempFileName As String
  
  Do While InStr(1, mstrFilesList, UPDATE_MYSELF_FILE_NAME, vbTextCompare) > 0
    strFileName = Mid$(mstrFilesList, InStr(1, mstrFilesList, UPDATE_MYSELF_FILE_NAME, vbTextCompare), 12)
    mstrFilesList = Replace(mstrFilesList, strFileName, "", 1, 1, vbTextCompare)
    intVersion = Val(Mid$(strFileName, Len(UPDATE_MYSELF_FILE_NAME) + 1, InStr(1, strFileName, ".", vbTextCompare) - Len(UPDATE_MYSELF_FILE_NAME) - 1))
    If intVersion > App.Major And intVersion > intDownloadVer Then
      strTempFileName = strFileName
      intDownloadVer = intVersion
      mstrMySPFileName = UCase(strTempFileName)
      Call SetFormInformation
    End If
  Loop

  If Len(Trim(strTempFileName)) Then
    intCount = intCount + 1
    strNames = strNames & strTempFileName
  End If
End Sub
Private Sub ShowList(ByRef strFilesToUpdate As String, ByVal bolForFail As Boolean)
  Dim bolShow As Boolean
  If bolForFail Then
    bolShow = Len(Trim(strFilesToUpdate)) > 0
  Else
    bolShow = Len(Trim(strFilesToUpdate)) > 0 And LoadSetting(SELECT_TO_DOWN)
  End If
  
  If bolShow Then
    Dim objSelectForm As frmShowFiles
    Set objSelectForm = New frmShowFiles
    Load objSelectForm
    objSelectForm.Mode = IIf(bolForFail, Failed, ToSelect)
    objSelectForm.ListToShow = strFilesToUpdate
    objSelectForm.Show vbModal
    strFilesToUpdate = objSelectForm.SelectedList
  End If
End Sub
Private Function ProgressDownload() As Boolean
  Dim intNoOfFiles As Integer
  Dim strFilesToUpdate As String
  
  ProgressDownload = True
  GoToProgressStepNumber PROGRESS_DOWNLOAD
  If Len(Trim(mstrFilesList)) Then
    Screen.MousePointer = vbHourglass
    If InStr(1, mstrFilesList, UPDATE_MYSELF_FILE_NAME, vbTextCompare) > 0 Then
      Call GetMyServicePackFileName(intNoOfFiles, strFilesToUpdate)
    Else
      strFilesToUpdate = Space(0)
      intNoOfFiles = 0
    End If
    
    mobjModulesRecSet.MoveFirst
    Do While Not mobjModulesRecSet.BOF And Not mobjModulesRecSet.EOF
      intNoOfFiles = intNoOfFiles + GetTheFilesForThisModule(strFilesToUpdate)
      mobjModulesRecSet.MoveNext
    Loop
    Screen.MousePointer = vbDefault
    
    Call ShowList(strFilesToUpdate, False)
    intNoOfFiles = Int(Len(strFilesToUpdate) / 12)
    If intNoOfFiles > 0 Then
      Call DownLoad(intNoOfFiles, strFilesToUpdate)
      Call ShowList(mstrFailedFiles, True)
    Else
      ProgressDownload = False
      MsgBox "No service packs to update.", vbInformation
    End If
  Else
    ProgressDownload = False
    MsgBox "No service packs to update.", vbInformation
  End If
End Function
Private Function IsThisMyServicePackFile(ByVal strFileName As String) As Boolean
  IsThisMyServicePackFile = UCase(Mid$(strFileName, 1, Len(UPDATE_MYSELF_FILE_NAME))) = UCase(UPDATE_MYSELF_FILE_NAME)
End Function
Private Sub DownLoad(ByVal intNoOfFiles As Integer, ByVal strFilesToUpdate As String)
  Dim intCount As Integer
  Dim strFileName As String
  Dim strFileLoc As String
  Dim strDownTo As String
  
  prgDownTherm.Min = 0
  prgDownTherm.Max = intNoOfFiles
  prgDownTherm.Value = prgDownTherm.Min
  strDownTo = LoadSetting(DOWNLOAD_TO)
  Call SetCaption(mintCurStep, ConvertPrecent(prgDownTherm))
  
  For intCount = 0 To intNoOfFiles - 1
    strFileName = Mid(strFilesToUpdate, (intCount * 12) + 1, 12)
    
    ftpConnector.RemoteFile = "/" & DOWNLOAD_DIR & "/" & _
                              IIf(UCase(Right(strFileName, 3)) = "EXE", Left(strFileName, 2), Trim(txtUserID)) & _
                              "/" & strFileName
    Call WaitUntilFTPFinishsExecuting
    ftpConnector.LocalFile = IIf(IsThisMyServicePackFile(strFileName), App.Path, strDownTo) & "\" & strFileName
    Call WaitUntilFTPFinishsExecuting
    
    mstrCurFileName = strFileName
    Call ShowDownloadStatus(0, 0)
    If ftpConnector.Connected Then
      Call WaitUntilFTPFinishsExecuting
      ftpConnector.RETR
      Call WaitUntilFTPFinishsExecuting
    End If
    Call WaitUntilFTPFinishsExecuting
    prgDownTherm.Value = prgDownTherm.Value + 1
    Call SetCaption(mintCurStep, ConvertPrecent(prgDownTherm))
    
    If IsThisMyServicePackFile(strFileName) Then
      mbolShouldUpdate = True
    End If
  Next
  ftpConnector.Quit
  Call WaitUntilFTPFinishsExecuting
  prgDownTherm.Value = prgDownTherm.Max
  Call SetCaption(mintCurStep)
End Sub
Private Sub ShowDownloadStatus(ByVal lngTotal As Long, ByVal lngSoFar As Long)
  lblFileName = "Downloading '" & mstrCurFileName & "'"
  lblTotalNum = ConvertToText(lngTotal)
  lblSoFarNum = ConvertToText(lngSoFar)
  Refresh
End Sub
Private Function ConvertToText(ByVal lngValue As Long)
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
Private Function GetTheFilesForThisModule(ByRef strFilesToUpdate As String)
  Dim intNoOfFiles As Integer
  Dim intCount As Integer
  Dim intCustomCount As Integer
  Dim strFileName As String
  Dim strCustomExt As String
  Dim strCustomCount As String
  
  intCount = 1
  Do While InStr(1, UCase(mstrFilesList), GetFileName(intCount, True), vbTextCompare)
    strFileName = GetFileName(intCount, False)
    intNoOfFiles = intNoOfFiles + 1
    strFilesToUpdate = strFilesToUpdate & strFileName & ".EXE"
    
    intCustomCount = 1
    strCustomExt = ".001"
    Do While InStr(1, UCase(mstrFilesList), strFileName + strCustomExt)
      intNoOfFiles = intNoOfFiles + 1
      strFilesToUpdate = strFilesToUpdate + strFileName + strCustomExt
      intCustomCount = intCustomCount + 1
      strCustomCount = Trim(CStr(intCustomCount))
      strCustomExt = "." + String(3 - Len(strCustomCount), "0") + strCustomCount
    Loop
    
    intCount = intCount + 1
  Loop
  GetTheFilesForThisModule = intNoOfFiles
End Function
Private Function GetFileName(ByVal intCount As Integer, ByVal bolAddExe As Boolean)
  GetFileName = mobjModulesRecSet!cApp_ID & _
                Right(Trim(mobjModulesRecSet!cMdlBuild), 3) & _
                String(3 - Len(Trim(CStr((mobjModulesRecSet!nSrvcPak + intCount)))), "0") & _
                Trim(CStr((mobjModulesRecSet!nSrvcPak + intCount))) & _
                IIf(bolAddExe, ".EXE", "")
End Function
Private Sub cmdShowLog_Click()
  cmdShowLog.Visible = False
  mobjLogForm.Show
End Sub
Private Sub mobjLogForm_Quiting()
  cmdShowLog.Visible = True
End Sub
Private Sub InitLogSession()
  If LoadSetting(INTERACTIVE) Then
    If mobjLogForm Is Nothing Then
      Set mobjLogForm = New frmLog
      Load mobjLogForm
    End If
    mobjLogForm.Move Left, Top + Height, Width
    mobjLogForm.Show
    mobjLogForm.Refresh
    cmdShowLog.Visible = False
  End If
  
  Set mobjText = New AriaTextStream
  If mobjText.OpenTextFile(LoadSetting(DOWNLOAD_TO) & "\" & LOG_FILE_NAME, ForAppending, TristateUseDefault) Then
    mobjText.WriteLine _
    vbCrLf & _
    Space(2) & "New '" & App.Title & "' Session..." & vbCrLf & _
    Space(2) & "Started on: " & Now & vbCrLf & _
    Space(2) & String(76, "-") & vbCrLf & _
    Space(2) & "Code" & vbTab & "Command" & vbTab & vbTab & "Response" & vbCrLf & _
    Space(2) & String(76, "-")
  Else
    Set mobjText = Nothing
  End If
  
End Sub
Private Sub EndLogSession()
  If Not mobjLogForm Is Nothing Then
    Unload mobjLogForm
    Set mobjLogForm = Nothing
  End If
  
  If Not mobjText Is Nothing Then
    mobjText.WriteLine _
    Space(2) & String(76, "-") & vbCrLf & _
    Space(2) & "End of Session." & vbCrLf & _
    Space(2) & "Finished on: " & Now & vbCrLf & vbCrLf & vbCrLf
    mobjText.CloseFile
    Set mobjText = Nothing
  End If
End Sub
Private Sub UpdateLog(ByVal intCode As Integer, ByVal strCommand As String, ByVal strResponse As String)
  If Not mobjLogForm Is Nothing Then
    Call mobjLogForm.AddEntry(intCode, strCommand, strResponse)
  End If
  If Not mobjText Is Nothing Then
    mobjText.WriteLine Space(2) & intCode & vbTab & strCommand & vbTab & vbTab & strResponse
  End If
End Sub
Private Sub netConnection_StateChanged(ByVal State As Integer)
  Dim strText As String
  Select Case State
    Case icHostResolved
      strText = "Successfully found the IP address of the specified host computer."
    Case icConnecting
      strText = "Connecting to the host computer."
    Case icConnected
      strText = "Successfully connected to the host computer."
    Case icRequesting
      strText = "Sending a request to the host computer."
    Case icRequestSent
      strText = "Successfully sent the request."
    Case icReceivingResponse
      strText = "Receiving a response from the host computer."
    Case icResponseReceived
      strText = "Successfully received a response from the host computer."
    Case icDisconnecting
      strText = "Disconnecting from the host computer."
    Case icDisconnected
      strText = "Successfully disconnected from the host computer."
    Case icError
      strText = "An error occurred in communicating with the host computer."
    Case icResponseCompleted
      strText = "The request has completed and all data has been received."
  End Select
  Call UpdateLog(2, "VERIFY", strText)
End Sub
Private Function ProgressUpdate() As Boolean
  GoToProgressStepNumber PROGRESS_UPDATE
  Screen.MousePointer = vbHourglass
  
  On Error GoTo ErrHand
  Dim strFileName As String
  strFileName = App.Path & "\SERVPCK.EXE"
  
  Dim dblRetValue As Double
  dblRetValue = Shell(strFileName)
  
GoOut:
  Screen.MousePointer = vbDefault
  ProgressUpdate = dblRetValue <> 0
  Exit Function
  
ErrHand:
  MsgBox "The following Error has occurred while trying " & _
         "to update service packs: " & vbCrLf & vbCrLf & _
         "Error: " & Err.Description & vbCrLf & vbCrLf & _
         "Cannot proceed.", vbCritical + vbOKOnly
  GoTo GoOut
End Function

