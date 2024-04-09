VERSION 5.00
Begin VB.Form frmErrBox 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Connection Failed"
   ClientHeight    =   3045
   ClientLeft      =   2760
   ClientTop       =   3750
   ClientWidth     =   5940
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   178
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3045
   ScaleWidth      =   5940
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox picNav 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   570
      Left            =   -45
      ScaleHeight     =   570
      ScaleWidth      =   6030
      TabIndex        =   6
      TabStop         =   0   'False
      Top             =   2475
      Width           =   6030
      Begin VB.CommandButton cmdAction 
         Caption         =   "&Retry"
         Default         =   -1  'True
         Height          =   312
         Index           =   1
         Left            =   2370
         MaskColor       =   &H00000000&
         TabIndex        =   0
         Tag             =   "102"
         Top             =   150
         Width           =   1092
      End
      Begin VB.CommandButton cmdAction 
         Caption         =   "Chan&ge"
         Height          =   312
         Index           =   2
         Left            =   3570
         MaskColor       =   &H00000000&
         TabIndex        =   1
         Tag             =   "103"
         Top             =   150
         Width           =   1092
      End
      Begin VB.CommandButton cmdAction 
         Cancel          =   -1  'True
         Caption         =   "&Cancel"
         Height          =   312
         Index           =   3
         Left            =   4770
         MaskColor       =   &H00000000&
         TabIndex        =   2
         Tag             =   "104"
         Top             =   150
         Width           =   1092
      End
      Begin VB.Line Line1 
         BorderColor     =   &H00FFFFFF&
         Index           =   0
         X1              =   120
         X2              =   5850
         Y1              =   15
         Y2              =   15
      End
      Begin VB.Line Line1 
         BorderColor     =   &H00808080&
         Index           =   1
         X1              =   120
         X2              =   5865
         Y1              =   0
         Y2              =   0
      End
   End
   Begin VB.Timer timRetry 
      Left            =   360
      Top             =   1185
   End
   Begin VB.Label lblButDesc 
      Caption         =   $"frmErrBox.frx":0000
      Height          =   615
      Left            =   270
      TabIndex        =   8
      Top             =   1725
      Width           =   5490
   End
   Begin VB.Label lblSec 
      AutoSize        =   -1  'True
      Caption         =   "Label1"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   1140
      TabIndex        =   7
      Top             =   1395
      Width           =   555
   End
   Begin VB.Label lblRetry 
      AutoSize        =   -1  'True
      Caption         =   "Label1"
      Height          =   195
      Left            =   1140
      TabIndex        =   5
      Top             =   1170
      Width           =   465
   End
   Begin VB.Label lblDueTo 
      Caption         =   "Label1"
      Height          =   375
      Left            =   1140
      TabIndex        =   4
      Top             =   150
      Width           =   4530
   End
   Begin VB.Label lblError 
      Caption         =   "Label1"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   540
      Left            =   1140
      TabIndex        =   3
      Top             =   660
      Width           =   4530
   End
   Begin VB.Image imgCritical 
      Height          =   720
      Left            =   225
      Picture         =   "frmErrBox.frx":009F
      Top             =   300
      Width           =   720
   End
   Begin VB.Shape Shape1 
      BackColor       =   &H00FFFFFF&
      BorderColor     =   &H00FFFFFF&
      FillColor       =   &H00FFFFFF&
      FillStyle       =   0  'Solid
      Height          =   285
      Left            =   435
      Top             =   510
      Width           =   255
   End
End
Attribute VB_Name = "frmErrBox"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Const BTN_NOW    As Integer = 1
Const BTN_CHANGE As Integer = 2
Const BTN_CANCEL As Integer = 3

Private mintReturnValue As Integer
Private mintSecSoFar    As Integer
Private mintTotalSec    As Integer
Private mbolLastTry     As Boolean
Private mintTrail       As Integer
Public Property Let LastTry(ByVal bolVal As Boolean)
  mbolLastTry = bolVal
  Call SetCaption
End Property
Public Property Let TrialNumber(ByVal intTrail As Integer)
  mintTrail = intTrail
End Property
Public Property Get ReturnValue() As Integer
  ReturnValue = mintReturnValue
End Property
Public Property Let ErrorDesc(ByVal strError As String)
  lblError = strError
End Property
Private Sub cmdAction_Click(Index As Integer)
  timRetry.Interval = 0
  timRetry.Enabled = False
  
  Select Case Index
    Case BTN_NOW
      mintReturnValue = LOGIN_RETRY
      
    Case BTN_CHANGE
      mintReturnValue = LOGIN_GOTO_LOGIN_INFO
    
    Case BTN_CANCEL
      mintReturnValue = LOGIN_CANCEL
  End Select
  
  Hide
End Sub
Private Sub Form_Load()
  Call SetCaption
  lblDueTo = App.Title & " failed to connect to '" & _
             LoadSetting(HOST_SERVER) & _
             "' due to the following reported error:"
  lblRetry = App.Title & " will retry to connect in..."
  
  If LoadSetting(RETRY_CONNECT) Then
    mintTotalSec = LoadSetting(NUM_SECONDS)
    Call UpdateSec
    timRetry.Interval = 1000
    timRetry.Enabled = True
  Else
    Dim intDiff As Integer
    intDiff = picNav.Top - lblButDesc.Top
    lblButDesc.Top = lblRetry.Top
    Height = Height - (picNav.Top - (lblButDesc.Top + intDiff))
    picNav.Top = lblButDesc.Top + intDiff
    
    lblRetry.Visible = False
    lblSec.Visible = False
    timRetry.Interval = 0
    timRetry.Enabled = False
  End If
End Sub
Private Sub UpdateSec()
  lblSec = mintTotalSec - mintSecSoFar & " second(s)."
End Sub
Private Sub timRetry_Timer()
  timRetry.Interval = 0
  mintSecSoFar = mintSecSoFar + 1
  If mintTotalSec - mintSecSoFar = 0 Then
    mintReturnValue = LOGIN_RETRY
    Hide
  Else
    Call UpdateSec
  End If
  timRetry.Interval = 1000
End Sub
Private Sub SetCaption()
  Caption = App.Title & " - Connection Failed [" & _
            IIf(mbolLastTry, "Last Trial]", "Trail # " & mintTrail & "]")
End Sub
