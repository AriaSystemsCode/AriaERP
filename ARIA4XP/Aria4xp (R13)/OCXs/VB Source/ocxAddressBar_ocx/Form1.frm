VERSION 5.00
Object = "{11139D0F-6579-4BB6-BCE5-84D0C88F557C}#12.0#0"; "ocxExpToolbar.ocx"
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   3090
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   9810
   LinkTopic       =   "Form1"
   ScaleHeight     =   3090
   ScaleWidth      =   9810
   StartUpPosition =   3  'Windows Default
   Begin vbpExpToolbar.ctlExpToolbar ctlExpToolbar1 
      Height          =   855
      Left            =   720
      TabIndex        =   7
      Top             =   720
      Width           =   7215
      _extentx        =   12726
      _extenty        =   1508
   End
   Begin VB.CommandButton Command7 
      Caption         =   "Command7"
      Height          =   735
      Left            =   600
      TabIndex        =   6
      Top             =   360
      Width           =   2055
   End
   Begin VB.CommandButton Command6 
      Caption         =   "Command6"
      Height          =   495
      Left            =   240
      TabIndex        =   5
      Top             =   1800
      Width           =   1815
   End
   Begin VB.CommandButton Command5 
      Caption         =   "Command5"
      Height          =   615
      Left            =   8160
      TabIndex        =   4
      Top             =   2040
      Width           =   975
   End
   Begin VB.CommandButton Command4 
      Caption         =   "Command4"
      Height          =   855
      Left            =   5160
      TabIndex        =   3
      Top             =   2040
      Width           =   1815
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Command3"
      Height          =   495
      Left            =   2760
      TabIndex        =   2
      Top             =   1680
      Width           =   1335
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Command2"
      Height          =   375
      Left            =   3600
      TabIndex        =   1
      Top             =   2280
      Width           =   1335
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Command1"
      Height          =   375
      Left            =   1440
      TabIndex        =   0
      Top             =   2400
      Width           =   1695
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
  ctlExpToolbar1.PushForwardMenu InputBox("Key"), InputBox("desc")
End Sub

Private Sub Command2_Click()
ctlExpToolbar1.PopForwardMenu
End Sub

Private Sub Command3_Click()
  ctlExpToolbar1.SetUp "DDDD"
End Sub

Private Sub Command4_Click()
ctlExpToolbar1.ClearUp
End Sub

Private Sub Command5_Click()
  MsgBox ctlExpToolbar1.GetViewMode
End Sub

Private Sub Command6_Click()
  ctlExpToolbar1.SetLinks "A>B>C", "a>b>c", True
End Sub

Private Sub Command7_Click()
 'WebBrowser1.Navigate "C:\DOCUME~1\mah\LOCALS~1\Temp\radBC43D.tmp"
 WebBrowser1.Navigate2 "C:\DOCUME~1\mah\LOCALS~1\Temp\radBC43D.tmp"
 
End Sub



Private Sub ctlExpToolbar1_ShowInFolders()
MsgBox "Folder"
End Sub

Private Sub ctlExpToolbar1_ShowInGroups()
MsgBox "groupd"
End Sub

Private Sub ctlExpToolbar1_Up(ByVal strKey As String)
MsgBox strKey
End Sub


