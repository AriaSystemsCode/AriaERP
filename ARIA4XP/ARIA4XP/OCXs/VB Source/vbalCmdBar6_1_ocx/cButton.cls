VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cButton"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Private m_hWnd As Long
Private m_sKey As String

Friend Sub fInit(ByVal hWnd As Long, ByVal sKey As String)
On Error Resume Next
   m_hWnd = hWnd
   m_sKey = sKey
End Sub

Public Property Get Key() As String
Attribute Key.VB_Description = "Gets/sets the Key for this button.  All buttons must have globally unique keys."
   On Error Resume Next
   Key = m_sKey
End Property


Public Property Get Tag() As String
Attribute Tag.VB_Description = "Gets/sets a string value associated with this button."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Tag = mCommandBars.ButtonItem(m_sKey).Tag
   End If
End Property
Public Property Let Tag(ByVal sTag As String)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      mCommandBars.ButtonItem(m_sKey).Tag = sTag
   End If
End Property

Public Property Get itemData() As Long
Attribute itemData.VB_Description = "Gets/sets a long value associated with this button."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      itemData = mCommandBars.ButtonItem(m_sKey).itemData
   End If
End Property
Public Property Let itemData(ByVal lItemData As Long)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      mCommandBars.ButtonItem(m_sKey).itemData = lItemData
   End If
End Property


Public Property Get Caption() As String
Attribute Caption.VB_Description = "Gets/sets the caption for this button."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Caption = ctl.ButtonCaption(m_sKey)
   End If
End Property
Public Property Let Caption(ByVal sCaption As String)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonCaption(m_sKey) = sCaption
   End If
End Property

Public Property Get IconIndex() As Long
Attribute IconIndex.VB_Description = "Gets/sets the 0-based index of an icon to use from the ImageList associated with the control.  Set to -1 for no icon."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Caption = ctl.ButtonIconIndex(m_sKey)
   End If
End Property
Public Property Let IconIndex(ByVal lIconIndex As Long)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonIconIndex(m_sKey) = lIconIndex
   End If
End Property

Public Property Get ColorBox() As OLE_COLOR
Attribute ColorBox.VB_Description = "Gets/sets a colour which is drawn in the icon box when the item has no icon.  Set to -1 for no colour box."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Caption = ctl.ButtonColourBox(m_sKey)
   End If
End Property
Public Property Let ColorBox(ByVal oColor As OLE_COLOR)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonColourBox(m_sKey) = oColor
   End If
End Property



Public Property Get ToolTip() As String
Attribute ToolTip.VB_Description = "Gets/sets the tooltip text to display for this button."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ToolTip = ctl.ButtonToolTip(m_sKey)
   End If
End Property
Public Property Let ToolTip(ByVal sToolTip As String)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonToolTip(m_sKey) = sToolTip
   End If
End Property

Public Property Get ShortcutKey() As Integer
Attribute ShortcutKey.VB_Description = "Gets/sets the shortcut key for the accelerator associated with this control.  Set to 0 for no accelerator."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ShortcutKey = ctl.ButtonShortcutKey(m_sKey)
   End If
End Property
Public Property Let ShortcutKey(ByVal vShortcutKey As Integer)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonShortcutKey(m_sKey) = vShortcutKey
   End If
End Property

Public Property Get ShortcutModifiers() As ShiftConstants
Attribute ShortcutModifiers.VB_Description = "Gets/sets the shift modifiers for the accelerator associated with this control.  Set to 0 for no shift modifiers."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ShortcutModifiers = ctl.ButtonShortcutModifiers(m_sKey)
   End If
End Property
Public Property Let ShortcutModifiers(ByVal eShortcutModifiers As ShiftConstants)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonShortcutModifiers(m_sKey) = eShortcutModifiers
   End If
End Property

Public Property Get Bar() As cCommandBar
Attribute Bar.VB_Description = "Gets/sets the CommandBar to be shown when this button is dropped, if any."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Set Bar = ctl.ButtonBar(m_sKey)
   End If
End Property

Public Property Let Bar(cmdBar As cCommandBar)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonSetBar m_sKey, cmdBar
   End If
End Property
Public Property Set Bar(cmdBar As cCommandBar)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonSetBar m_sKey, cmdBar
   End If
End Property

Public Property Get Checked() As Boolean
Attribute Checked.VB_Description = "Gets/sets whether this button is checked."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Checked = ctl.ButtonChecked(m_sKey)
   End If
End Property
Public Property Let Checked(ByVal bChecked As Boolean)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonChecked(m_sKey) = bChecked
   End If
End Property

Public Property Get Enabled() As Boolean
Attribute Enabled.VB_Description = "Gets/sets whether this button is enabled or not."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Enabled = ctl.ButtonEnabled(m_sKey)
   End If
End Property
Public Property Let Enabled(ByVal bEnabled As Boolean)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonEnabled(m_sKey) = bEnabled
   End If
End Property

Public Property Get Locked() As Boolean
Attribute Locked.VB_Description = "Gets/sets whether this button is locked."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Locked = ctl.ButtonLocked(m_sKey)
   End If
End Property
Public Property Let Locked(ByVal bLocked As Boolean)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonLocked(m_sKey) = bLocked
   End If
End Property


Public Property Get ShowCaptionInToolbar() As Boolean
Attribute ShowCaptionInToolbar.VB_Description = "Gets/sets whether this button's caption is shown when it's owning CommandBar is associated with a toolbar."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ShowCaptionInToolbar = ctl.ButtonShowCaptionInToolbar(m_sKey)
   End If
End Property
Public Property Let ShowCaptionInToolbar(ByVal bShowCaptionInToolbar As Boolean)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonShowCaptionInToolbar(m_sKey) = bShowCaptionInToolbar
   End If
End Property

Public Property Get ShowDropDownInToolbar() As Boolean
Attribute ShowDropDownInToolbar.VB_Description = "Gets/sets whether the drop-down glyph is shown for this item when it's owning CommandBar is associated with a Toolbar."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ShowCaptionInToolbar = ctl.ButtonShowDropDownInToolbar(m_sKey)
   End If
End Property
Public Property Let ShowDropDownInToolbar(ByVal bShowDropDownInToolbar As Boolean)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonShowDropDownInToolbar(m_sKey) = bShowDropDownInToolbar
   End If
End Property

Public Property Get Visible() As Boolean
Attribute Visible.VB_Description = "Gets/sets whether this button is visible or not."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Visible = ctl.ButtonVisible(m_sKey)
   End If
End Property
Public Property Let Visible(ByVal bVisible As Boolean)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonVisible(m_sKey) = bVisible
   End If
End Property

Public Property Get PanelWidth() As Long
Attribute PanelWidth.VB_Description = "Gets the width of this button when it has the ePanel style."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      PanelWidth = ctl.ButtonPanelWidth(m_sKey)
   End If
End Property
Public Property Let PanelWidth(ByVal lPanelWidth As Long)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      ctl.ButtonPanelWidth(m_sKey) = lPanelWidth
   End If
End Property

Public Property Get PanelControl() As Object
Attribute PanelControl.VB_Description = "Gets/sets a control to display over this button."
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Set PanelControl = ctl.ButtonPanelControl(m_sKey)
   End If
End Property
Public Property Let PanelControl(ctlPanel As Object)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Set ctl.ButtonPanelControl(m_sKey) = ctlPanel
   End If
End Property
Public Property Set PanelControl(ctlPanel As Object)
On Error Resume Next
Dim ctl As vbalCommandBar
   If (ControlFromhWnd(m_hWnd, ctl)) Then
      Set ctl.ButtonPanelControl(m_sKey) = ctlPanel
   End If
End Property

