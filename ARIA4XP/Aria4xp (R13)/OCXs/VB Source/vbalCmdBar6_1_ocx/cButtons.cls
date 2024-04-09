VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cButtons"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit


' cCommandBars is not a real object.  It is a proxy
' onto the mCommandBars.m_colCommandBarItems collection,
' given that the owning control exists.

Private m_hWnd As Long

Friend Sub fInit(ByVal hWnd As Long)
  On Error Resume Next
   m_hWnd = hWnd
End Sub

Public Property Get Count() As Long
Attribute Count.VB_Description = "Gets the number of buttons which have been created for all control instances in your project."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Count = ctl.ButtonCount
   End If
End Property

Public Property Get Item(ByVal index As Variant)
Attribute Item.VB_Description = "Gets the button with the specified index or key."
Attribute Item.VB_UserMemId = 0
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Set Item = ctl.ButtonItem(index)
   End If
End Property

Public Sub Remove(ByVal sKey As String)
Attribute Remove.VB_Description = "Removes the button with the specified index or key."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonRemove sKey
   End If
End Sub

Public Function Add( _
      ByVal sKey As String, _
      Optional ByVal iIcon As Long = -1, _
      Optional ByVal sCaption As String = "", _
      Optional ByVal eStyle As EButtonStyle = eNormal, _
      Optional ByVal sToolTip As String = "", _
      Optional ByVal vShortcutKey As Integer = 0, _
      Optional ByVal eShortcutModifier As ShiftConstants = vbCtrlMask _
   ) As cButton
Attribute Add.VB_Description = "Adds a new button to the collection of buttons.  The mandatory Key argument must be a globally unique string across all buttons in your project."

On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Set Add = ctl.ButtonAdd(sKey, iIcon, sCaption, eStyle, sToolTip, vShortcutKey, eShortcutModifier)
   End If
End Function
