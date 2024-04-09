VERSION 5.00
Begin VB.Form frmShowFiles 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Select what to download"
   ClientHeight    =   3540
   ClientLeft      =   2760
   ClientTop       =   3750
   ClientWidth     =   4695
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3540
   ScaleWidth      =   4695
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdPrint 
      Caption         =   "&Show Report..."
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   312
      Left            =   2910
      TabIndex        =   5
      Top             =   2700
      Width           =   1680
   End
   Begin VB.Frame fraTag 
      BorderStyle     =   0  'None
      Height          =   1185
      Left            =   2910
      TabIndex        =   8
      Top             =   825
      Width           =   1680
      Begin VB.CommandButton cmdInvert 
         Caption         =   "&Invert"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   312
         Left            =   0
         TabIndex        =   4
         Top             =   750
         Width           =   1680
      End
      Begin VB.CommandButton cmdTagNone 
         Caption         =   "Tag &None"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   312
         Left            =   0
         TabIndex        =   3
         Top             =   375
         Width           =   1680
      End
      Begin VB.CommandButton cmdTagAll 
         Caption         =   "&Tag All"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   312
         Left            =   0
         TabIndex        =   2
         Top             =   0
         Width           =   1680
      End
   End
   Begin VB.ListBox lstCheck 
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2535
      Left            =   105
      Style           =   1  'Checkbox
      TabIndex        =   1
      Top             =   825
      Width           =   2655
   End
   Begin VB.ListBox lstStandard 
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2595
      Left            =   105
      TabIndex        =   0
      Top             =   825
      Width           =   2655
   End
   Begin VB.CommandButton cmdOk 
      Caption         =   "&OK"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   312
      Left            =   2910
      TabIndex        =   6
      Top             =   3075
      Width           =   1680
   End
   Begin VB.Label lblText 
      Caption         =   "Following is a list of the files that could not be downloaded, write them down and try to download them later..."
      Height          =   615
      Left            =   105
      TabIndex        =   7
      Top             =   105
      Width           =   3885
   End
End
Attribute VB_Name = "frmShowFiles"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Public Enum Modes
  Failed = 1
  ToSelect = 2
End Enum

Private mstrListToShow As String
Private mintMode As Modes
Private mstrRepTitle As String
Public Property Let Mode(ByVal intMode As Modes)
  mintMode = intMode
  Select Case mintMode
    Case Is = Failed
      fraTag.Visible = False
      lstCheck.Visible = False
      lstStandard.Visible = True
      Caption = "Failed to download..."
      lblText = "Following is a list of the service packs that could not be downloaded. " & _
                "Please, print them to try to download them later..."
      mstrRepTitle = "Failed Service Packs"
      
    Case Is = ToSelect
      fraTag.Visible = True
      lstCheck.Visible = True
      lstStandard.Visible = False
      Caption = "Select what to download..."
      lblText = "Following is a list of all the available service packs to download. " & _
                "Please, select what to download from this list..."
      mstrRepTitle = "Available Service Packs"
  End Select
End Property
Public Property Let ListToShow(ByVal strList As String)
  mstrListToShow = strList
  
  Dim strFileName As String
  Do While Len(Trim(strList)) > 0
    strFileName = Mid$(strList, 1, 12)
    strList = Replace(strList, strFileName, "", 1, 1, vbTextCompare)
    lstCheck.AddItem UCase(strFileName)
    lstCheck.Selected(lstCheck.ListCount - 1) = True
    lstStandard.AddItem UCase(strFileName)
  Loop
  lstCheck.ListIndex = 0
  lstStandard.ListIndex = 0
  cmdPrint.Enabled = lstStandard.ListCount > 0
End Property
Public Property Get SelectedList() As String
  Dim intItem As Integer
  For intItem = 0 To lstCheck.ListCount - 1
    If lstCheck.Selected(intItem) Then
      SelectedList = SelectedList & lstCheck.List(intItem)
    End If
  Next
End Property
Private Sub cmdOk_Click()
  Hide
End Sub
Private Sub DoTags(ByVal bolTag As Integer)
  Dim intItem As Integer
  Dim intOldListIdex As Integer
  
  intOldListIdex = lstCheck.ListIndex
  For intItem = 0 To lstCheck.ListCount - 1
    lstCheck.Selected(intItem) = IIf(bolTag = 3, Not lstCheck.Selected(intItem), bolTag)
  Next
  lstCheck.ListIndex = intOldListIdex
End Sub
Private Sub cmdTagAll_Click()
  Call DoTags(True)
End Sub
Private Sub cmdTagNone_Click()
  Call DoTags(False)
End Sub
Private Sub cmdInvert_Click()
  Call DoTags(3)
End Sub
Private Sub cmdPrint_Click()
  Dim objRS As ADODB.Recordset
  Dim intItem As Integer
  Set objRS = New ADODB.Recordset
  objRS.Fields.Append "FileName", adChar, 12
  objRS.Fields.Append "Selected", adChar, 3
  objRS.Open
  For intItem = 0 To lstCheck.ListCount - 1
    objRS.AddNew
    objRS!FileName = lstCheck.List(intItem)
    objRS!Selected = IIf(mintMode = Failed, "No", IIf(lstCheck.Selected(intItem), "Yes", "No"))
  Next
  objRS.MoveFirst
  
  Dim objRepForm As frmShowRep
  Set objRepForm = New frmShowRep
  Load objRepForm
  Set objRepForm.SourceRecordSet = objRS
  objRepForm.ShowSelected = mintMode = ToSelect
  objRepForm.RepTitle = mstrRepTitle
  objRepForm.Show vbModal
  
  objRS.Close
  Set objRS = Nothing
End Sub
Private Sub Form_Load()
  lstCheck.Move 105, 825
  lstStandard.Move 105, 825
  Mode = ToSelect
End Sub
