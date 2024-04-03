VERSION 5.00
Object = "{EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}#1.1#0"; "shdocvw.dll"
Begin VB.UserControl ctlWebBrowser 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   ScaleHeight     =   3600
   ScaleWidth      =   4800
   Begin SHDocVwCtl.WebBrowser WebBrowser1 
      Height          =   855
      Left            =   120
      TabIndex        =   0
      Top             =   840
      Width           =   3495
      ExtentX         =   6165
      ExtentY         =   1508
      ViewMode        =   0
      Offline         =   0
      Silent          =   0
      RegisterAsBrowser=   0
      RegisterAsDropTarget=   0
      AutoArrange     =   0   'False
      NoClientEdge    =   0   'False
      AlignLeft       =   0   'False
      NoWebView       =   0   'False
      HideFileNames   =   0   'False
      SingleClick     =   0   'False
      SingleSelection =   0   'False
      NoFolders       =   0   'False
      Transparent     =   0   'False
      ViewID          =   "{0057D0E0-3573-11CF-AE69-08002B2E1262}"
      Location        =   ""
   End
End
Attribute VB_Name = "ctlWebBrowser"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Event ClickLink(ByVal strKey As String)
Public strNavTo As String

Private Sub UserControl_Resize()
  On Error Resume Next
  WebBrowser1.Move ScaleLeft - 2 * Screen.TwipsPerPixelX, _
                   ScaleTop - 2 * Screen.TwipsPerPixelY, _
                   ScaleWidth + 4 * Screen.TwipsPerPixelX, _
                   ScaleHeight + 4 * Screen.TwipsPerPixelY
End Sub


Public Property Get Control() As Object
  On Error Resume Next
  Set Control = WebBrowser1
End Property

Public Property Set Control(ByVal vNewValue As Object)
  On Error Resume Next
End Property

Private Sub WebBrowser1_BeforeNavigate2(ByVal pDisp As Object, URL As Variant, Flags As Variant, TargetFrameName As Variant, PostData As Variant, Headers As Variant, Cancel As Boolean)

  On Error Resume Next
  Dim objSysFile As New FileSystemObject
  Dim strPath As String
  strPath = objSysFile.GetFile(URL).ShortPath
  If UCase$(strNavTo) <> UCase$(strPath) Then
    Cancel = True
  End If
  
  RaiseEvent ClickLink(URL)
End Sub
