VERSION 5.00
Begin VB.UserControl ctlAddressBar 
   BackStyle       =   0  'Transparent
   ClientHeight    =   3465
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4290
   ScaleHeight     =   3465
   ScaleWidth      =   4290
   Begin vbpExpToolbar.ctlWebBrowser WebBrowser1 
      Height          =   1215
      Left            =   720
      TabIndex        =   1
      Top             =   1440
      Width           =   2895
      _ExtentX        =   5106
      _ExtentY        =   2143
   End
   Begin VB.TextBox Text1 
      Height          =   855
      Left            =   0
      TabIndex        =   0
      Text            =   "Text1"
      Top             =   0
      Width           =   4215
   End
End
Attribute VB_Name = "ctlAddressBar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Event ClickLink(ByVal strKey As String)

Public Property Get hWnd() As Long
  On Error Resume Next
  hWnd = UserControl.hWnd
End Property

Public Property Let hWnd(ByVal vNewValue As Long)
  On Error Resume Next
End Property

Private Sub UserControl_Resize()
  On Error Resume Next
  Text1.Move ScaleLeft + 4 * Screen.TwipsPerPixelX, ScaleTop, ScaleWidth - 4 * Screen.TwipsPerPixelX, ScaleHeight
  WebBrowser1.Move ScaleLeft + Screen.TwipsPerPixelX + 4 * Screen.TwipsPerPixelX, _
                   ScaleTop + Screen.TwipsPerPixelY, _
                   ScaleWidth - 2 * Screen.TwipsPerPixelX - 4 * Screen.TwipsPerPixelX, _
                   ScaleHeight - 2 * Screen.TwipsPerPixelY
End Sub


Public Property Get strNavTo() As String
  On Error Resume Next
  Set Control = WebBrowser1.Control
End Property

Public Property Let strNavTo(ByVal vNewValue As String)
  On Error Resume Next
  WebBrowser1.strNavTo = vNewValue
End Property

Public Property Get Control() As Object
  On Error Resume Next
  Set Control = WebBrowser1.Control
End Property

Public Property Set Control(ByVal vNewValue As Object)
  On Error Resume Next
End Property


Private Sub WebBrowser1_ClickLink(ByVal strKey As String)
  On Error Resume Next
  RaiseEvent ClickLink(strKey)
End Sub
