VERSION 5.00
Begin VB.Form frmActSetting 
   Caption         =   "Define Activities"
   ClientHeight    =   3690
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   5625
   LinkTopic       =   "Form1"
   ScaleHeight     =   3690
   ScaleWidth      =   5625
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdClose 
      Caption         =   "&Close"
      Height          =   405
      Left            =   2190
      TabIndex        =   5
      Top             =   3240
      Width           =   1500
   End
   Begin VB.Frame Frame1 
      Height          =   1095
      Left            =   60
      TabIndex        =   6
      Top             =   2085
      Width           =   5505
      Begin VB.ComboBox cboTypes 
         Enabled         =   0   'False
         Height          =   315
         ItemData        =   "TypeSet.frx":0000
         Left            =   750
         List            =   "TypeSet.frx":0010
         Style           =   2  'Dropdown List
         TabIndex        =   1
         Top             =   225
         Width           =   2910
      End
      Begin VB.CommandButton cmdRemove 
         Caption         =   "&Remove"
         Height          =   390
         Left            =   4395
         TabIndex        =   4
         Top             =   645
         Width           =   990
      End
      Begin VB.CommandButton cmdNew 
         Caption         =   "&New"
         Height          =   390
         Left            =   4395
         TabIndex        =   3
         Top             =   195
         Width           =   990
      End
      Begin VB.TextBox txtActivity 
         Enabled         =   0   'False
         Height          =   360
         Left            =   750
         TabIndex        =   2
         Top             =   600
         Width           =   3585
      End
      Begin VB.Label Label2 
         Caption         =   "Type:"
         Height          =   345
         Left            =   165
         TabIndex        =   8
         Top             =   255
         Width           =   555
      End
      Begin VB.Label Label1 
         Caption         =   "Activity:"
         Height          =   330
         Left            =   165
         TabIndex        =   7
         Top             =   660
         Width           =   720
      End
   End
   Begin VB.ListBox lstActivities 
      Height          =   2010
      Left            =   60
      TabIndex        =   0
      Top             =   75
      Width           =   5535
   End
End
Attribute VB_Name = "frmActSetting"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private colSelTypes As New Collection
Private objdata As New GetData
Private objActivityRs As New ADODB.Recordset
Private intQSSequence As Integer
Private intDGSequence As Integer
Private intQRSequence As Integer
Private intQUSequence As Integer

Private Sub cmdClose_Click()
objActivityRs.Close
Set objActivityRs = Nothing
Set colSelTypes = Nothing
Set objdata = Nothing
Unload Me
End Sub

Private Sub cmdNew_Click()
Me.cboTypes.ListIndex = 0
Me.cboTypes.Enabled = True
Me.txtActivity.Enabled = True
Me.txtActivity = ""
Me.cboTypes.SetFocus
End Sub

Private Sub cmdRemove_Click()

Dim pos As Integer
pos = Me.lstActivities.ListIndex
objActivityRs.MoveFirst
Do While Not objActivityRs.EOF
  If objActivityRs!cType = colSelTypes.item(pos * 3 + 3) Then
    Exit Do
  End If
  objActivityRs.MoveNext
Loop
If objActivityRs!nUsedLine > 0 Then
  MsgBox "Activity type is already used for some items. Cannot Remove.", vbCritical + vbOKOnly, "POS"
  Exit Sub
End If
If MsgBox("Are you sure you want to remove this activity type?", vbCritical + vbOKCancel, "POS") = vbCancel Then
  Exit Sub
End If

objActivityRs.Delete
colSelTypes.Remove (pos * 3 + 1)
colSelTypes.Remove (pos * 3 + 1)
colSelTypes.Remove (pos * 3 + 1)
Me.lstActivities.RemoveItem (Me.lstActivities.ListIndex)
If Me.lstActivities.ListCount = 0 Then
  Me.txtActivity = ""
  Me.cboTypes.ListIndex = -1
Else
  If pos = Me.lstActivities.ListCount Then
    pos = Me.lstActivities.ListCount - 1
  End If
  Me.lstActivities.ListIndex = pos
  Me.lstActivities.Selected(pos) = True
  Me.txtActivity = colSelTypes.item(Me.lstActivities.ListIndex * 3 + 1)
  Me.cboTypes.ListIndex = Val(colSelTypes.item(Me.lstActivities.ListIndex * 3 + 2))
End If
End Sub
Private Sub Form_Load()

Dim intActType As Integer
Dim DataPath As String
intQSSequence = 0
intDGSequence = 0
intQRSequence = 0
intQUSequence = 0

DataPath = Space(100)
GetPrivateProfileString "Send Activity", "DataDir", "NONE", DataPath, 160, App.Path & "\SendActi.ini"
DataPath = Mid$(Trim$(DataPath), 1, Len(Trim$(DataPath)) - 1) & "\"
objdata.filename = "TMACTTYP"
objdata.sqlSelect.add "cTYpe,cTypeDesc,cEdiType,nUsedLine"
objdata.SqlWhere.add "NOT DELETED()"
Set objActivityRs = objdata.retreivedata(DataPath, adOpenKeyset, adLockOptimistic)
Do While Not objActivityRs.EOF
  Select Case objActivityRs!cEdiType
    Case "QS"
      intActType = 0
      intQSSequence = IIf(Val(Mid$(objActivityRs!cType, 3, 3)) > intQSSequence, Val(Mid$(objActivityRs!cType, 3, 3)), intQSSequence)
    Case "DG"
      intActType = 1
      intDGSequence = IIf(Val(Mid$(objActivityRs!cType, 3, 3)) > intDGSequence, Val(Mid$(objActivityRs!cType, 3, 3)), intDGSequence)
    Case "QR"
      intActType = 2
      intQRSequence = IIf(Val(Mid$(objActivityRs!cType, 3, 3)) > intQRSequence, Val(Mid$(objActivityRs!cType, 3, 3)), intQRSequence)
    Case "QU"
      intActType = 3
      intQUSequence = IIf(Val(Mid$(objActivityRs!cType, 3, 3)) > intQUSequence, Val(Mid$(objActivityRs!cType, 3, 3)), intQUSequence)
  End Select
  Me.lstActivities.AddItem Trim$(objActivityRs!cTypeDesc) & " [" & Me.cboTypes.List(intActType) & "]"
  colSelTypes.add CStr(objActivityRs!cTypeDesc)
  colSelTypes.add Trim$(Str$(intActType))
  colSelTypes.add CStr(objActivityRs!cType)
  objActivityRs.MoveNext
Loop
Me.lstActivities.ListIndex = 0
If Me.lstActivities.ListCount > 0 Then
  Me.txtActivity = colSelTypes.item(Me.lstActivities.ListIndex * 3 + 1)
  Me.cboTypes.ListIndex = Val(colSelTypes.item(Me.lstActivities.ListIndex * 3 + 2))
End If
End Sub

Private Sub lstActivities_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
If Me.lstActivities.ListCount > 0 Then
  Me.txtActivity = colSelTypes.item(Me.lstActivities.ListIndex * 3 + 1)
  Me.cboTypes.ListIndex = Val(colSelTypes.item(Me.lstActivities.ListIndex * 3 + 2))
End If
End Sub

Private Sub txtActivity_LostFocus()
Me.cboTypes.Enabled = False
Me.txtActivity.Enabled = False
End Sub

Private Sub txtActivity_Validate(Cancel As Boolean)
If Trim$(Me.txtActivity) <> "" Then
  Dim strnewtype As String
  Select Case Me.cboTypes.ListIndex
    Case 0
      intQSSequence = intQSSequence + 1
      strnewtype = "QS" & Format$(intQSSequence, "000")
    Case 1
      intDGSequence = intDGSequence + 1
      strnewtype = "DG" & Format$(intDGSequence, "000")
    Case 2
      intQRSequence = intQRSequence + 1
      strnewtype = "QR" & Format$(intQRSequence, "000")
    Case 3
      intQUSequence = intQUSequence + 1
      strnewtype = "QU" & Format$(intQUSequence, "000")
  End Select
  
  Me.lstActivities.AddItem Trim$(Me.txtActivity) & " [" & Me.cboTypes.Text & "]"
  colSelTypes.add Trim$(Me.txtActivity)
  colSelTypes.add CStr(Me.cboTypes.ListIndex)
  colSelTypes.add strnewtype
  objActivityRs.AddNew
  objActivityRs!cType = strnewtype
  objActivityRs!cEdiType = Left$(strnewtype, 2)
  objActivityRs!cTypeDesc = Trim$(Me.txtActivity)
  objActivityRs.Save
 End If
End Sub
