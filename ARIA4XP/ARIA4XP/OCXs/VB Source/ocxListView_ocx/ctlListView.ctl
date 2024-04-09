VERSION 5.00
Object = "{E910F8E1-8996-4EE9-90F1-3E7C64FA9829}#1.3#0"; "vbaListView6.ocx"
Object = "{396F7AC0-A0DD-11D3-93EC-00C0DFE7442A}#1.0#0"; "vbalIml6.ocx"
Begin VB.UserControl ctlListView 
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   ScaleHeight     =   3600
   ScaleWidth      =   4800
   Begin vbalIml6.vbalImageList vbalImageList5 
      Left            =   4080
      Top             =   2520
      _ExtentX        =   953
      _ExtentY        =   953
   End
   Begin vbalIml6.vbalImageList vbalImageList4 
      Left            =   3720
      Top             =   1680
      _ExtentX        =   953
      _ExtentY        =   953
   End
   Begin vbalIml6.vbalImageList vbalImageList3 
      Left            =   3600
      Top             =   840
      _ExtentX        =   953
      _ExtentY        =   953
      IconSizeX       =   48
      IconSizeY       =   48
   End
   Begin vbalIml6.vbalImageList vbalImageList2 
      Left            =   3480
      Top             =   2760
      _ExtentX        =   953
      _ExtentY        =   953
   End
   Begin vbalIml6.vbalImageList vbalImageList1 
      Left            =   4080
      Top             =   120
      _ExtentX        =   953
      _ExtentY        =   953
   End
   Begin vbalListViewLib6.vbalListViewCtl vbalListViewCtl1 
      Height          =   2805
      Left            =   480
      TabIndex        =   0
      Top             =   300
      Width           =   2895
      _ExtentX        =   5106
      _ExtentY        =   4948
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      MultiSelect     =   -1  'True
      LabelEdit       =   0   'False
      AutoArrange     =   0   'False
      Appearance      =   0
      BorderStyle     =   0
      HeaderButtons   =   0   'False
      HeaderTrackSelect=   0   'False
      HideSelection   =   0   'False
      InfoTips        =   0   'False
   End
End
Attribute VB_Name = "ctlListView"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Event ItemDblClick(ByVal Item As vbalListViewLib6.cListItem)
Public Event ItemClick(ByVal Item As vbalListViewLib6.cListItem)
Public Event ItemSelected(ByVal strKey As String)

Public Event KeyDown(KeyCode As Integer, Shift As Integer)
Public Event KeyPress(KeyAscii As Integer)
Public Event KeyUp(KeyCode As Integer, Shift As Integer)
Public Event MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Public Event MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Public Event MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)

Public Sub BindImageLists()
  vbalListViewCtl1.ImageList(eLVHeaderImages) = vbalImageList1
  vbalListViewCtl1.ImageList(eLVLargeIcon) = vbalImageList2
  vbalListViewCtl1.ImageList(eLVSmallIcon) = vbalImageList3
  vbalListViewCtl1.ImageList(eLVStateImages) = vbalImageList4
  vbalListViewCtl1.ImageList(eLVTileImages) = vbalImageList5
  
  
End Sub

Private Sub UserControl_Resize()
  vbalListViewCtl1.Move 0, 0, ScaleWidth, ScaleHeight
End Sub

Public Property Get hWnd() As Variant
  hWnd = UserControl.hWnd
End Property

Public Property Let hWnd(ByVal vNewValue As Variant)
End Property


Public Property Get ScaleMode() As Variant
  ScaleMode = UserControl.ScaleMode
End Property

Public Property Let ScaleMode(ByVal vNewValue As Variant)
End Property

Public Function ScaleX(a, b, c) As Single
  ScaleX = UserControl.ScaleX(a, b, c)
End Function

Public Function ScaleY(a, b, c) As Single
  ScaleY = UserControl.ScaleY(a, b, c)
End Function


Public Property Get Control() As Object
  Set Control = vbalListViewCtl1
End Property

Public Property Get ImageListHeaderImages() As Object
  Set ImageListHeaderImages = vbalImageList1
End Property

Public Property Get ImageListLargeIcon() As Object
  Set ImageListLargeIcon = vbalImageList2
End Property

Public Property Get ImageListSmallIcon() As Object
  Set ImageListSmallIcon = vbalImageList3
End Property

Public Property Get ImageListStateImages() As Object
  Set ImageListStateImages = vbalImageList4
End Property

Public Property Get ImageListTileImages() As Object
  Set ImageListTileImages = vbalImageList5
End Property

Private Sub vbalListViewCtl1_ItemClick(Item As vbalListViewLib6.cListItem)
  RaiseEvent ItemClick(ByVal Item)
End Sub

Private Sub vbalListViewCtl1_ItemDblClick(Item As vbalListViewLib6.cListItem)
  RaiseEvent ItemDblClick(ByVal Item)
End Sub

Private Sub vbalListViewCtl1_ItemSelected(ByVal strKey As String)
  RaiseEvent ItemSelected(ByVal strKey)
End Sub

Private Sub vbalListViewCtl1_KeyDown(KeyCode As Integer, Shift As Integer)
  RaiseEvent KeyDown(KeyCode, Shift)
End Sub

Private Sub vbalListViewCtl1_KeyPress(KeyAscii As Integer)
  RaiseEvent KeyPress(KeyAscii)
End Sub

Private Sub vbalListViewCtl1_KeyUp(KeyCode As Integer, Shift As Integer)
    RaiseEvent KeyUp(KeyCode, Shift)
End Sub

Private Sub vbalListViewCtl1_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  RaiseEvent MouseDown(Button, Shift, X, Y)
End Sub

Private Sub vbalListViewCtl1_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  RaiseEvent MouseMove(Button, Shift, X, Y)
End Sub

Private Sub vbalListViewCtl1_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
  RaiseEvent MouseUp(Button, Shift, X, Y)
End Sub
