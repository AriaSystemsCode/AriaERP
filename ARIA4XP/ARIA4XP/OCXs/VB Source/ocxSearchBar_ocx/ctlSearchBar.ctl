VERSION 5.00
Object = "{396F7AC0-A0DD-11D3-93EC-00C0DFE7442A}#1.0#0"; "vbalIml6.ocx"
Object = "{128C9411-33F6-402A-9B30-806CD450E360}#1.2#0"; "vbalExpBar61.ocx"
Begin VB.UserControl ctlSearchBar 
   AutoRedraw      =   -1  'True
   ClientHeight    =   6015
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   7260
   ControlContainer=   -1  'True
   ScaleHeight     =   401
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   484
   Begin vbpSearchBar.ctlTitleBar ctlTitleBar1 
      Height          =   255
      Left            =   3720
      TabIndex        =   2
      Top             =   960
      Width           =   2175
      _ExtentX        =   3836
      _ExtentY        =   450
   End
   Begin vbalExplorerBarLib61.vbalExplorerBarCtl vbalExplorerBarCtl1 
      Height          =   3615
      Left            =   3600
      TabIndex        =   1
      Top             =   1320
      Width           =   2535
      _ExtentX        =   4471
      _ExtentY        =   6376
      BackColorEnd    =   0
      BackColorStart  =   0
   End
   Begin VB.TextBox txtNotes 
      Height          =   1335
      Left            =   1560
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Top             =   480
      Visible         =   0   'False
      Width           =   1455
   End
   Begin vbalIml6.vbalImageList vbalImageList2 
      Left            =   720
      Top             =   2040
      _ExtentX        =   953
      _ExtentY        =   953
   End
   Begin vbalIml6.vbalImageList vbalImageList1 
      Left            =   480
      Top             =   1200
      _ExtentX        =   953
      _ExtentY        =   953
   End
End
Attribute VB_Name = "ctlSearchBar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Event ItemClick(itm As vbalExplorerBarLib61.cExplorerBarItem)
Public Event BarClick(bar As vbalExplorerBarLib61.cExplorerBar)
Public Event BeforeBarClick(bar As vbalExplorerBarLib61.cExplorerBar)
Public Event Scroll()
Public Event CloseClicked()
Public Event SettingChange()
Public Event NotesDClick()
Public TextBoxBackColor As Long

Public Property Let Caption(ByVal strValue As String)
On Error Resume Next
  ctlTitleBar1.Caption = strValue
  Call PropertyChanged("Caption")
End Property

Public Property Get Caption() As String
  On Error Resume Next
  Caption = ctlTitleBar1.Caption
End Property

Public Sub BindImageLists()
  On Error Resume Next
  vbalExplorerBarCtl1.BarTitleImageList = vbalImageList1.hIml
  vbalExplorerBarCtl1.ImageList = vbalImageList2.hIml
End Sub

Public Property Get ImageListBarTitle() As Object
  On Error Resume Next
  Set ImageListBarTitle = vbalImageList1
End Property

Public Property Get ImageList() As Object
  On Error Resume Next
  Set ImageList = vbalImageList2
End Property

Private Sub ctlTitleBar1_CloseClicked()
  On Error Resume Next
  RaiseEvent CloseClicked
End Sub

Private Sub Image1_Click()
On Error Resume Next
End Sub

Private Sub txtNotes_DblClick()
  On Error Resume Next
  RaiseEvent NotesDClick
End Sub

Private Sub txtNotes_GotFocus()
 On Error Resume Next
 txtNotes.BackColor = TextBoxBackColor
End Sub

Private Sub txtNotes_LostFocus()
 On Error Resume Next
 txtNotes.BackColor = &H80000005
End Sub

Private Sub txtNotes_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  On Error Resume Next
  UserControl.MousePointer = 0
End Sub

Private Sub UserControl_Resize()
  On Error Resume Next
  ctlTitleBar1.Move 0, 0, ScaleWidth, 24
  vbalExplorerBarCtl1.Move 0, 24, ScaleWidth, ScaleHeight - 24
  Call ctlTitleBar1.Refresh
End Sub

Public Function LoadControlPicture(ByVal strControlName As String, ByVal strPictureName As String) As Boolean
  On Error Resume Next
  UserControl.Controls(strControlName).Picture = LoadPicture(strPictureName)
  If Err.Number = 0 Then
    LoadControlPicture = True
  End If
End Function

Public Function AddControl(ByVal strControlType As String, ByVal strControlName As String) As Object
  On Error Resume Next
  
  Set AddControl = UserControl.Controls.Add(strControlType, strControlName)
  
  Dim objControl As Object
  For Each objControl In UserControl.Controls
    If UCase(Trim(objControl.Name)) = UCase(Trim(strControlName)) Then
      Set AddControl = objControl
    End If
  Next objControl
  
  Call ctlTitleBar1.ZOrder(0)
End Function

Public Function GetControl(ByVal strControlName As String) As Object
  On Error Resume Next
  
  Dim objControl As Object
  For Each objControl In UserControl.Controls
    If UCase(Trim(objControl.Name)) = UCase(Trim(strControlName)) Then
      Set GetControl = objControl
    End If
  Next objControl
End Function

Public Property Get hWnd() As Variant
  On Error Resume Next
  hWnd = UserControl.hWnd
End Property

Public Property Let hWnd(ByVal vNewValue As Variant)
On Error Resume Next
End Property

Public Property Get ScaleMode() As Variant
  On Error Resume Next
  ScaleMode = UserControl.ScaleMode
End Property

Public Property Let ScaleMode(ByVal vNewValue As Variant)
On Error Resume Next
End Property

Public Function ScaleX(a, b, c) As Single
  On Error Resume Next
  ScaleX = UserControl.ScaleX(a, b, c)
End Function

Public Function ScaleY(a, b, c) As Single
  On Error Resume Next
  ScaleY = UserControl.ScaleY(a, b, c)
End Function


Public Property Get Control() As Object
  On Error Resume Next
  Set Control = vbalExplorerBarCtl1
End Property

Private Sub UserControl_Show()
  On Error Resume Next
  If Not (vbalExplorerBarCtl1.GetIsXP() Or vbalExplorerBarCtl1.GetIs2000With256) Then
    vbalExplorerBarCtl1.Redraw = False
    vbalExplorerBarCtl1.BackColorStart = RGB(139, 169, 229)
    vbalExplorerBarCtl1.BackColorEnd = RGB(100, 135, 220)
    vbalExplorerBarCtl1.UseExplorerStyle = False
    vbalExplorerBarCtl1.Redraw = True
  End If
  
  Call ctlTitleBar1.Refresh
End Sub

Private Sub vbalExplorerBarCtl1_BarClick(bar As vbalExplorerBarLib61.cExplorerBar)
  On Error Resume Next
  RaiseEvent BarClick(bar)
End Sub

Private Sub vbalExplorerBarCtl1_BeforeBarClick(bar As vbalExplorerBarLib61.cExplorerBar)
  On Error Resume Next
  RaiseEvent BeforeBarClick(bar)
End Sub

Private Sub vbalExplorerBarCtl1_ItemClick(itm As vbalExplorerBarLib61.cExplorerBarItem)
  On Error Resume Next
  RaiseEvent ItemClick(itm)
End Sub

Private Sub vbalExplorerBarCtl1_Scroll()
    On Error Resume Next
    RaiseEvent Scroll
End Sub

Private Sub vbalExplorerBarCtl1_SettingChange()
  On Error Resume Next
  RaiseEvent SettingChange
  Call ctlTitleBar1.Refresh
End Sub


Private Sub UserControl_InitProperties()
  On Error Resume Next
  ctlTitleBar1.Caption = ""
End Sub

Private Sub UserControl_ReadProperties(PropBag As PropertyBag)
  On Error Resume Next
  ctlTitleBar1.Caption = PropBag.ReadProperty("Caption", "")
End Sub

Private Sub UserControl_WriteProperties(PropBag As PropertyBag)
  On Error Resume Next
  Call PropBag.WriteProperty("Caption", ctlTitleBar1.Caption, "")
End Sub

