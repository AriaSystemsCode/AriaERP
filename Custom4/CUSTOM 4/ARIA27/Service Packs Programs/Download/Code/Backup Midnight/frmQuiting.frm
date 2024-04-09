VERSION 5.00
Begin VB.Form frmQuiting 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Download Service Packs"
   ClientHeight    =   705
   ClientLeft      =   2760
   ClientTop       =   3750
   ClientWidth     =   4635
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   705
   ScaleWidth      =   4635
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Image imgLogo 
      Height          =   480
      Left            =   165
      Picture         =   "frmQuiting.frx":0000
      Top             =   75
      Width           =   480
   End
   Begin VB.Label lblWait 
      AutoSize        =   -1  'True
      Caption         =   "Please wait while Download Service Packs exits."
      Height          =   195
      Left            =   885
      TabIndex        =   0
      Top             =   225
      Width           =   3435
   End
End
Attribute VB_Name = "frmQuiting"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit

Private Sub Form_Load()
  Caption = App.Title
  lblWait = "Please wait while " & App.Title & " exits."
End Sub
