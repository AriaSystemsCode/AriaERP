VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "Mscomctl.ocx"
Object = "*\A..\..\Presentation\AriaButton.vbp"
Begin VB.Form frmMain 
   Caption         =   "Test the button"
   ClientHeight    =   5640
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   7770
   LinkTopic       =   "Form1"
   ScaleHeight     =   376
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   518
   StartUpPosition =   2  'CenterScreen
   Begin AriaButton.Button butTest 
      Height          =   330
      Left            =   1320
      TabIndex        =   7
      Top             =   735
      Width           =   3975
      _ExtentX        =   7011
      _ExtentY        =   582
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      FontName        =   "Tahoma"
      TextAlign       =   0
      MaskColor       =   -2147483633
      Style           =   1
      DropDown        =   -1  'True
      AutoSize        =   0   'False
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Command1"
      Height          =   495
      Left            =   5550
      TabIndex        =   6
      Top             =   1665
      Width           =   1215
   End
   Begin AriaButton.Button Button1 
      Height          =   960
      Left            =   1395
      TabIndex        =   5
      Top             =   2670
      Width           =   4155
      _ExtentX        =   7329
      _ExtentY        =   1693
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   14.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      FontName        =   "Tahoma"
      FontSize        =   14.25
      PictureAlign    =   0
      TextAlign       =   0
      MaskColor       =   -2147483633
      Style           =   1
      DropDown        =   -1  'True
      AutoSize        =   0   'False
      Spacing         =   6
   End
   Begin VB.CheckBox Check3 
      Caption         =   "Show Focus"
      Height          =   495
      Left            =   5730
      TabIndex        =   4
      Top             =   4140
      Width           =   1650
   End
   Begin VB.CheckBox Check2 
      Caption         =   "Show Flat Gray"
      Height          =   495
      Left            =   5730
      TabIndex        =   3
      Top             =   3660
      Value           =   1  'Checked
      Width           =   1650
   End
   Begin VB.CheckBox Check1 
      Caption         =   "Enabled"
      Height          =   495
      Left            =   5730
      TabIndex        =   2
      Top             =   3180
      Value           =   1  'Checked
      Width           =   1215
   End
   Begin VB.CommandButton cmdFill 
      Caption         =   "Fill"
      Height          =   495
      Left            =   3300
      TabIndex        =   1
      Top             =   4800
      Width           =   1215
   End
   Begin MSComctlLib.ImageList ImageList1 
      Left            =   1965
      Top             =   4455
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   3
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":0000
            Key             =   "MAN"
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":08DA
            Key             =   "SHR"
         EndProperty
         BeginProperty ListImage3 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":11B4
            Key             =   "YMA"
         EndProperty
      EndProperty
   End
   Begin VB.CommandButton cmdEmpty 
      Caption         =   "Empty"
      Height          =   495
      Left            =   4740
      TabIndex        =   0
      Top             =   4725
      Width           =   1215
   End
   Begin VB.Menu mnuMain 
      Caption         =   "Test"
      Visible         =   0   'False
      Begin VB.Menu mnuOne 
         Caption         =   "Menu Option 1"
      End
      Begin VB.Menu mnuTwo 
         Caption         =   "Menu Option 2"
      End
      Begin VB.Menu mnuThree 
         Caption         =   "Menu Option 3"
      End
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub butTest_DropDownClick()
  PopupMenu mnuMain, , butTest.Left, butTest.Top + butTest.Height
End Sub

Private Sub Button1_Click()
  MsgBox "This is a click"
End Sub

Private Sub Button1_MenuBarClicked(objBar As AriaButton.Bar)
  If objBar.Key = "MNM" Then
    objBar.Checked = Not objBar.Checked
  End If
  MsgBox objBar.Caption, vbInformation
End Sub

Private Sub Button1_MenuBarOver(objBar As AriaButton.Bar)
  Set Picture = objBar.Icon
End Sub

Private Sub Button1_MenuBarsInitialized()
  Button1.Locked = False
  Button1.DropDown = True
End Sub

Private Sub Button1_MenuBarsRemoved()
  Button1.Locked = True
  Button1.DropDown = False
End Sub

Private Sub Check1_Click()
  Button1.Enabled = Check1.Value = vbChecked
End Sub

Private Sub Check2_Click()
  Button1.ShowFlatGrey = Check2.Value = vbChecked
End Sub

Private Sub Check3_Click()
  Button1.ShowFocusRect = Check3.Value = vbChecked
End Sub

Private Sub cmdFill_Click()
  
  Button1.Menu.Add "YMA", "Yasser Mohammed Aly", , , , ImageList1.ListImages("YMA").Picture
  Button1.Menu.Add "MAN", "Mohammed Abdel-Salam", , , , ImageList1.ListImages("MAN").Picture
  Button1.Menu.Add "SHR", "Sherein Himdan", , , , ImageList1.ListImages("SHR").Picture
  Button1.Menu.Add "SP1", "- Yasser", , , , ImageList1.ListImages("YMA").Picture
  Button1.Menu.Add "MNM", "Mohammed Nael"

  Button1.Menu("MNM").Checked = True
End Sub

Private Sub cmdEmpty_Click()
  Button1.Menu.Remove "YMA"
  Button1.Menu.Remove "MAN"
  Button1.Menu.Remove "SHR"
  Button1.Menu.Remove "SP1"
  Button1.Menu.Remove "MNM"
  
  Button1.Menu.Clear
End Sub

Private Sub Command1_Click()
  Button1.Style = [Cool Button]
  Button1.DropDown = True
  Button1.DropDownAsButton = True
  Button1.Enabled = True
  Button1.Locked = True
End Sub

Private Sub Form_Load()
  Check1.Value = IIf(Button1.Enabled, vbChecked, vbUnchecked)
  Check2.Value = IIf(Button1.ShowFlatGrey, vbChecked, vbUnchecked)
  Check3.Value = IIf(Button1.ShowFocusRect, vbChecked, vbUnchecked)
  
  Call cmdFill_Click
End Sub
