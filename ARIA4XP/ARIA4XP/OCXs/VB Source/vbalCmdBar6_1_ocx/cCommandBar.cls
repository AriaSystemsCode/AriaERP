VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cCommandBar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

' cCommandBar is a proxy to a real cCommandBarInt object,
' via a control.

Private m_hWnd As Long
Private m_sKey As String

Friend Sub fInit(ByVal hWnd As Long, ByVal Key As String)
On Error Resume Next
   m_hWnd = hWnd
   m_sKey = Key
End Sub

Public Property Get Key() As String
Attribute Key.VB_Description = "Gets the key of this CommandBar."
On Error Resume Next
   Key = m_sKey
End Property
Public Property Get Title() As String
Attribute Title.VB_Description = "Gets/sets the title of this CommandBar."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Title = ctl.BarTitle(m_sKey)
   End If
End Property
Public Property Let Title(ByVal sTitle As String)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.BarTitle(m_sKey) = sTitle
   End If
End Property
Public Property Get Buttons() As cCommandBarButtons
Attribute Buttons.VB_Description = "Gets the collection of buttons associated with this CommandBar."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Set Buttons = ctl.BarButtonCollection(m_sKey)
   End If
End Property
