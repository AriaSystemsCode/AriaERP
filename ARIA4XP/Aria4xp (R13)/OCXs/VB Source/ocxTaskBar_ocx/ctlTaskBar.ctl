VERSION 5.00
Object = "{396F7AC0-A0DD-11D3-93EC-00C0DFE7442A}#1.0#0"; "vbalIml6.ocx"
Object = "{77EBD0B1-871A-4AD1-951A-26AEFE783111}#2.6#0"; "vbalExpBar6.ocx"
Begin VB.UserControl ctlTaskBar 
   ClientHeight    =   6015
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   7260
   ClipControls    =   0   'False
   ScaleHeight     =   401
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   484
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
   Begin vbalExplorerBarLib6.vbalExplorerBarCtl vbalExplorerBarCtl1 
      Height          =   4335
      Left            =   2880
      TabIndex        =   0
      Top             =   720
      Width           =   2535
      _ExtentX        =   4471
      _ExtentY        =   7646
      BackColorEnd    =   0
      BackColorStart  =   0
   End
End
Attribute VB_Name = "ctlTaskBar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Event ItemClick(itm As vbalExplorerBarLib6.cExplorerBarItem)
Public Event BarClick(bar As vbalExplorerBarLib6.cExplorerBar)
Public Event BeforeBarClick(bar As vbalExplorerBarLib6.cExplorerBar)

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

Private Sub UserControl_Resize()
  On Error Resume Next
  vbalExplorerBarCtl1.Move 0, 0, ScaleWidth, ScaleHeight
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
End Function

Private Sub UserControl_Show()
  On Error Resume Next
  If Not (vbalExplorerBarCtl1.GetIsXP Or vbalExplorerBarCtl1.GetIs2000With256) Then
    vbalExplorerBarCtl1.Redraw = False
    vbalExplorerBarCtl1.UseExplorerStyle = False
    vbalExplorerBarCtl1.BackColorStart = RGB(122, 161, 230)
    vbalExplorerBarCtl1.BackColorEnd = RGB(99, 117, 214)
    vbalExplorerBarCtl1.Redraw = True
  End If
End Sub

Public Property Get hWnd() As Variant
  On Error Resume Next
  hWnd = UserControl.hWnd
End Property

Public Property Let hWnd(ByVal vNewValue As Variant)
End Property


Public Property Get ScaleMode() As Variant
  On Error Resume Next
  ScaleMode = UserControl.ScaleMode
End Property

Public Property Let ScaleMode(ByVal vNewValue As Variant)
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

Private Sub vbalExplorerBarCtl1_BarClick(bar As vbalExplorerBarLib6.cExplorerBar)
  On Error Resume Next
  RaiseEvent BarClick(bar)
End Sub

Private Sub vbalExplorerBarCtl1_BeforeBarClick(bar As vbalExplorerBarLib6.cExplorerBar)
  On Error Resume Next
  RaiseEvent BeforeBarClick(bar)
End Sub

Private Sub vbalExplorerBarCtl1_ItemClick(itm As vbalExplorerBarLib6.cExplorerBarItem)
  On Error Resume Next
  RaiseEvent ItemClick(itm)
End Sub

