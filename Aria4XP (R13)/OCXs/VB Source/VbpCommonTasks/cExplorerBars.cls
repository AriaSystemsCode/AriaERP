VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "cExplorerBars"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Private m_hWnd As Long

Friend Sub fInit(ByVal hWnd As Long)
   m_hWnd = hWnd
End Sub

Public Property Get Count() As Long
Attribute Count.VB_Description = "Gets the number of bars in the control."
Dim ctl As vbalExplorerBarCtl
   If Verify(ctl, m_hWnd, 0, 0) Then
      Count = ctl.fBarCount
   End If
End Property
Public Sub Clear()
Attribute Clear.VB_Description = "Removes all of the bars currently associated with the control."
Dim ctl As vbalExplorerBarCtl
   If Verify(ctl, m_hWnd, 0, 0) Then
      ctl.fClearBars
   End If
End Sub
Public Sub Remove(key As Variant)
Attribute Remove.VB_Description = "Removes the specified bar by index or key."
Dim ctl As vbalExplorerBarCtl
   If Verify(ctl, m_hWnd, 0, 0) Then
      ctl.fRemoveBar key
   End If
End Sub
Public Function Add( _
      Optional Index As Variant, _
      Optional key As Variant, _
      Optional Title As Variant _
   ) As cExplorerBar
Attribute Add.VB_Description = "Adds or inserts a new bar to the control."
Dim ctl As vbalExplorerBarCtl
   If (Verify(ctl, m_hWnd, 0, 0)) Then
      Set Add = ctl.fAddBar(Index, key, Title)
   End If
End Function
Public Property Get Item( _
      Index As Variant _
   ) As cExplorerBar
Attribute Item.VB_Description = "Gets a reference to the specified bar by index or key."
Attribute Item.VB_UserMemId = 0
Dim ctl As vbalExplorerBarCtl
   If (Verify(ctl, m_hWnd, 0, 0)) Then
      Set Item = ctl.fGetBar(Index)
   End If
End Property
Public Property Get Exists( _
      Index As Variant _
   ) As Boolean
Dim ctl As vbalExplorerBarCtl
   If (Verify(ctl, m_hWnd, 0, 0)) Then
      Exists = Not (ctl.fGetBar(Index) Is Nothing)
   End If
End Property

