VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cCommandBarButtons"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

' This class is a proxy via the control onto a cCommandBar class
' to access the button collection related methods

Private m_hWnd As Long
Private m_sKey As String

Friend Function fInit(ByVal hWnd As Long, ByVal sKey As String)
On Error Resume Next
   m_hWnd = hWnd
   m_sKey = sKey
End Function

Public Property Get Count() As Long
Attribute Count.VB_Description = "Gets the number of Buttons associated with this CommandBar."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Count = ctl.BarButtonCount(m_sKey)
   End If
End Property
Public Property Get Item(index As Variant) As cButton
Attribute Item.VB_Description = "Gets the Button with the specified index or key in the CommandBar."
Attribute Item.VB_UserMemId = 0
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Set Item = ctl.BarButton(m_sKey, index)
   End If
End Property
Public Sub Clear()
Attribute Clear.VB_Description = "Clears all Button objects associated with this CommandBar."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.BarButtonClear m_sKey
   End If
End Sub
Public Sub Remove(ByVal sKey As String)
Attribute Remove.VB_Description = "Removes the Button with the specified index or key from the command bar."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.BarButtonRemove m_sKey, sKey
   End If
End Sub
Public Sub Add(btn As cButton)
Attribute Add.VB_Description = "Adds a new Button to this CommandBar."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (btn Is Nothing) Then
      gErr 8
   Else
      If (ControlFromhWnd(m_hWnd, ctl)) Then
         ctl.BarButtonAdd m_sKey, btn
      End If
   End If
End Sub
Public Sub InsertBefore(btn As cButton, btnBefore As cButton)
Attribute InsertBefore.VB_Description = "Inserts a Button before the specified button in the CommandBar."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (btn Is Nothing) Or (btnBefore Is Nothing) Then
      gErr 8
   Else
      If (ControlFromhWnd(m_hWnd, ctl)) Then
         ctl.BarButtonInsertBefore m_sKey, btn, btnBefore
      End If
   End If
End Sub
Public Sub InsertAfter(btn As cButton, btnAfter As cButton)
Attribute InsertAfter.VB_Description = "Inserts a Button after the specified button in the CommandBar."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (btn Is Nothing) Or (btnAfter Is Nothing) Then
      gErr 8
   Else
      If (ControlFromhWnd(m_hWnd, ctl)) Then
         ctl.BarButtonInsertAfter m_sKey, btn, btnAfter
      End If
   End If
End Sub

