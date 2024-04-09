VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "Mscomctl.ocx"
Begin VB.UserControl Button 
   AutoRedraw      =   -1  'True
   ClientHeight    =   360
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   1125
   DefaultCancel   =   -1  'True
   ScaleHeight     =   24
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   75
   ToolboxBitmap   =   "Button.ctx":0000
   Begin MSComctlLib.ImageList imlMenu 
      Left            =   2250
      Top             =   255
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      MaskColor       =   12632256
      _Version        =   393216
   End
End
Attribute VB_Name = "Button"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "PropPageWizardRun" ,"Yes"
Option Explicit

Private Declare Function GetWindowRect& Lib "user32" (ByVal hWnd As Long, lpRect As RECT)

Const cnsDropDownArrowWidth    As Integer = 10
Const cnsVerticalBorderWidth   As Integer = 3
Const cnsHorizontalBorderWidth As Integer = 3

' Needed API functions:
Private Declare Function DrawFocusRect Lib "user32" _
        (ByVal hDC As Long, lpRect As RECT) As Long

Private Declare Function DrawText Lib "user32" Alias "DrawTextA" _
        (ByVal hDC As Long, _
         ByVal lpStr As String, _
         ByVal nCount As Long, _
         lpRect As RECT, _
         ByVal wFormat As Long) As Long
         
Private Declare Function GetCursorPos Lib "user32" _
        (lpPoint As POINTAPI) As Long

Private Declare Function WindowFromPoint Lib "user32" _
        (ByVal xPoint As Long, _
         ByVal yPoint As Long) As Long

Private Declare Function FillRect Lib "user32" _
        (ByVal hDC As Long, _
         lpRect As RECT, _
         ByVal hBrush As Long) As Long

Private Declare Function CreatePatternBrush Lib "gdi32" _
       (ByVal hBitmap As Long) As Long

' API Constants...
Private Const DT_CENTER = &H1
Private Const DT_RIGHT = &H2
Private Const DT_LEFT = &H0
Private Const DT_SINGLELINE = &H20
Private Const DT_VCENTER = &H4

' API Types
Private Type RECT
  Left As Long
  Top As Long
  Right As Long
  Bottom As Long
End Type

Public Type POINTAPI
  x As Long
  y As Long
End Type

' Enums
Enum envbuPictureAlign
  vbPicLeft = 0
  vbPicRight = 1
  vbPicTop = 2
  vbPicBottom = 3
End Enum

Enum envbuTextAlign
  vbTextLeft = 0
  vbTextRight = 1
  vbTextCenter = 2
End Enum

Enum vbuStyle
  [Cool Button] = 0
  [Toolbar Button] = 1
  [Seperator] = 2
  [SeperatorH] = 3
  [Toolbar Handle] = 4
  [Toolbar HandleH] = 5
  [Standard Button] = 6
  [Up-Down Button] = 7
End Enum

' Event Declarations:
Public Event MouseEnter()
Public Event MouseExit()
Public Event DropDownClick()
Public Event Click()
Public Event DblClick()
Public Event KeyDown(KeyCode As Integer, Shift As Integer)
Public Event KeyPress(KeyAscii As Integer)
Public Event KeyUp(KeyCode As Integer, Shift As Integer)
Public Event MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
Public Event MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
Public Event MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
Public Event MenuBarClicked(ByRef objBar As Bar)
Public Event MenuBarOver(ByRef objBar As Bar)
Public Event MenuBarsInitialized()
Public Event MenuBarsRemoved()

Private WithEvents ExitTimer As objTimer
Attribute ExitTimer.VB_VarHelpID = -1
Private WithEvents mobjMenu  As PopUp
Attribute mobjMenu.VB_VarHelpID = -1

' Property Variables:
Private menvStyle                       As vbuStyle
Private menvPictureAlign                As envbuPictureAlign
Private menvTextAlign                   As envbuTextAlign
Private mstrCaption                     As String
Private mbolButtonGroup                 As String
Private mbytSpacing                     As Byte
Private mbolAutoSize                    As Boolean
Private mbolDropDown                    As Boolean
Private mbolValue                       As Boolean
Private mbolButtonGroupDefault          As Boolean
Private mbolButtonGroupDefault2         As Boolean
Private mbolDropDownAsButton            As Boolean
Private mbolDropDownUpdPic              As Boolean
Private mbolDropDownUpdCap              As Boolean
Private mbolShowFlatGrey                As Boolean
Private mbolShowFocusRect               As Boolean
Private mclrMaskColor                   As OLE_COLOR
Private mclrButtonFace                  As OLE_COLOR
Private mclrButtonLightShadow           As OLE_COLOR
Private mclrButtonDarkShadow            As OLE_COLOR
Private mclrButtonHighlight             As OLE_COLOR
Private mpicPicture                     As StdPicture
Private mstrPictureName                 As String
Private mpicDownPicture                 As StdPicture
Private mpicDisabledPicture             As StdPicture
Private mpicFlatPicture                 As StdPicture
Private mpicUpPicture                   As StdPicture
Private mpicHoverPicture                As StdPicture

' Default Property Values:
Private Const m_def_Style               As Integer = 0
Private Const m_def_DropDown            As Boolean = False
Private Const m_def_MaskColor           As Long = vbButtonFace
Private Const m_def_PictureAlign        As Integer = 2
Private Const m_def_TextAlign           As Integer = 2
Private Const m_def_Caption             As String = "Caption"
Private Const m_def_ButtonGroup         As String = ""
Private Const m_def_ButtonGroupDefault  As Boolean = False
Private Const m_def_ButtonGroupDefault2 As Boolean = False
Private Const m_def_ShowFocusRect       As Boolean = False
Private Const m_def_AutoSize            As Boolean = True
Private Const m_def_Spacing             As Byte = 2
Private Const m_def_Locked              As Boolean = False
Private Const m_def_DropDownAsButton    As Boolean = True
Private Const m_def_DropDownUpdPic      As Boolean = True
Private Const m_def_DropDownUpdCap      As Boolean = True

' Private module level variables
Private hUpDownDitherBrush    As Long
Private mbolFromCode          As Boolean
Private mbolButtonDown As Boolean
Private mbMouseDown As Boolean
Private miXOffset As Integer
Private miYOffset As Integer
Private mbolHasFocus As Boolean
Private mbolMouseOver As Boolean
Private mbDropDownPressed As Boolean
Private mintCurrentButtonPressed As Integer
Private mbolLocked As Boolean
Private mintClientWidth As Integer
Private mintClientHeight As Integer
Private mintClientTop As Integer
Private mintClientLeft As Integer

Public Property Let DropDownUpdatePicture(ByVal bolValue As Boolean)
  mbolDropDownUpdPic = bolValue
  PropertyChanged "DropDownUpdatePicture"
End Property

Public Property Get DropDownUpdatePicture() As Boolean
  DropDownUpdatePicture = mbolDropDownUpdPic
End Property

Public Property Let DropDownUpdateCaption(ByVal bolValue As Boolean)
  mbolDropDownUpdCap = bolValue
  PropertyChanged "DropDownUpdateCaption"
End Property

Public Property Get DropDownUpdateCaption() As Boolean
  DropDownUpdateCaption = mbolDropDownUpdCap
End Property

Public Property Let DropDownAsButton(ByVal bolValue As Boolean)
  If mbolDropDown Then
    mbolDropDownAsButton = bolValue
    PropertyChanged "DropDownAsButton"
  End If
End Property

Public Property Get DropDownAsButton() As Boolean
  DropDownAsButton = mbolDropDownAsButton
End Property

Public Property Let Locked(ByVal bolValue As Boolean)
  mbolLocked = bolValue
  PropertyChanged "Locked"
End Property

Public Property Get Locked() As Boolean
Attribute Locked.VB_ProcData.VB_Invoke_Property = "General"
  Locked = mbolLocked
End Property

Public Property Get Menu() As PopUp
  If mobjMenu Is Nothing Then
    Set mobjMenu = New PopUp
    Set mobjMenu.ImageList = imlMenu
    mobjMenu.ParenthWnd = hWnd
  End If
  Set Menu = mobjMenu
End Property

Public Property Get Spacing() As Byte
Attribute Spacing.VB_ProcData.VB_Invoke_Property = "General"
  Spacing = mbytSpacing
End Property

Public Property Let Spacing(ByVal bytSpacing As Byte)
  If bytSpacing >= 0 And bytSpacing <= 20 Then
    mbytSpacing = bytSpacing
    PropertyChanged "Spacing"
    DrawButton
  End If
End Property

Public Property Get AutoSize() As Boolean
Attribute AutoSize.VB_ProcData.VB_Invoke_Property = "General"
  AutoSize = mbolAutoSize
End Property

Public Property Let AutoSize(ByVal New_AutoSize As Boolean)
  mbolAutoSize = New_AutoSize
  PropertyChanged "AutoSize"
  DrawButton
End Property

Public Property Get Picture() As StdPicture
  Set Picture = mpicPicture
End Property

Public Property Set Picture(ByVal New_Picture As StdPicture)
  Set mpicPicture = New_Picture
  PropertyChanged "Picture"
  DrawButton
End Property

Public Property Get DownPicture() As StdPicture
  Set DownPicture = mpicDownPicture
End Property

Public Property Set DownPicture(ByVal New_DownPicture As StdPicture)
  Set mpicDownPicture = New_DownPicture
  PropertyChanged "DownPicture"
End Property

Public Property Get DisabledPicture() As StdPicture
  Set DisabledPicture = mpicDisabledPicture
End Property

Public Property Set DisabledPicture(ByVal New_Picture As StdPicture)
  Set mpicDisabledPicture = New_Picture
  PropertyChanged "DisabledPicture"
End Property

Public Property Get FlatPicture() As StdPicture
  Set FlatPicture = mpicFlatPicture
End Property

Public Property Set FlatPicture(ByVal New_FlatPicture As StdPicture)
  Set mpicFlatPicture = New_FlatPicture
  PropertyChanged "FlatPicture"
  DrawButton
End Property

Public Property Get UpPicture() As StdPicture
  Set UpPicture = mpicUpPicture
End Property

Public Property Set UpPicture(ByVal New_Picture As StdPicture)
  Set mpicUpPicture = New_Picture
  PropertyChanged "UpPicture"
  DrawButton
End Property

Public Property Get HoverPicture() As StdPicture
  Set HoverPicture = mpicHoverPicture
End Property

Public Property Set HoverPicture(ByVal New_Picture As StdPicture)
  Set mpicHoverPicture = New_Picture
  PropertyChanged "HoverPicture"
  DrawButton
End Property

Private Sub Leave()
  mbolMouseOver = False
  Set ExitTimer = Nothing
  DrawButton
  RaiseEvent MouseExit
End Sub

Private Function UnderMouse() As Boolean
  Dim ptMouse As POINTAPI
  Call GetCursorPos(ptMouse)
  UnderMouse = WindowFromPoint(ptMouse.x, ptMouse.y) = UserControl.hWnd
End Function

Private Function GetPicToDisplay() As StdPicture
  Dim picButton As StdPicture
  If Enabled Then
    Select Case menvStyle
      Case [Cool Button]
        Set picButton = IIf(mbolMouseOver And mbMouseDown, mpicHoverPicture, mpicFlatPicture)
      
      Case [Up-Down Button]
        Set picButton = IIf(mbolValue, mpicDownPicture, mpicUpPicture)
        
      Case Else
        Set picButton = mpicPicture
        
    End Select
  Else
    Set picButton = mpicDisabledPicture
  End If
  If picButton Is Nothing Then Set picButton = mpicPicture
  
  Set GetPicToDisplay = picButton
End Function

Private Sub GetDrawTextInfo(ByVal objPic As StdPicture, _
                            ByRef lngFormat As Long)
  'If objPic Is Nothing Then
    lngFormat = DT_CENTER
  'Else
    Select Case menvTextAlign
      Case vbTextCenter
        lngFormat = DT_CENTER
      Case vbTextLeft
        lngFormat = DT_LEFT
      Case vbTextRight
        lngFormat = DT_RIGHT
    End Select
  'End If
End Sub

Public Property Get BackColor() As OLE_COLOR
Attribute BackColor.VB_Description = "Returns/sets the background color used to display text and graphics in an object."
Attribute BackColor.VB_UserMemId = -501
  BackColor = UserControl.BackColor
End Property

Public Property Let BackColor(ByVal New_BackColor As OLE_COLOR)
  UserControl.BackColor() = New_BackColor
  PropertyChanged "BackColor"
  DrawButton
End Property

Public Property Get ForeColor() As OLE_COLOR
Attribute ForeColor.VB_Description = "Returns/sets the foreground color used to display text and graphics in an object."
  ForeColor = UserControl.ForeColor
End Property

Public Property Let ForeColor(ByVal New_ForeColor As OLE_COLOR)
  UserControl.ForeColor() = New_ForeColor
  PropertyChanged "ForeColor"
  DrawButton
End Property

Public Property Get Enabled() As Boolean
Attribute Enabled.VB_Description = "Returns/sets a value that determines whether an object can respond to user-generated events."
Attribute Enabled.VB_ProcData.VB_Invoke_Property = "General"
  Enabled = UserControl.Enabled
End Property

Public Property Let Enabled(ByVal New_Enabled As Boolean)
  UserControl.Enabled() = New_Enabled
  PropertyChanged "Enabled"
  DrawButton
End Property

Public Property Get Font() As Font
Attribute Font.VB_Description = "Returns a Font object."
Attribute Font.VB_UserMemId = -512
  Set Font = UserControl.Font
End Property

Public Property Set Font(ByVal New_Font As Font)
  Set UserControl.Font = New_Font
  PropertyChanged "Font"
  DrawButton
End Property

Public Property Get FontBold() As Boolean
  FontBold = UserControl.FontBold
End Property

Public Property Let FontBold(ByVal bolNewValue As Boolean)
  UserControl.FontBold() = bolNewValue
  UserControl.Font.Bold = UserControl.FontBold
  DrawButton
  PropertyChanged "FontBold"
End Property

Public Property Get FontItalic() As Boolean
  FontItalic = UserControl.FontItalic
End Property

Public Property Let FontItalic(ByVal bolNewValue As Boolean)
  UserControl.FontItalic() = bolNewValue
  UserControl.Font.Italic = UserControl.FontItalic
  DrawButton
  PropertyChanged "FontItalic"
End Property

Public Property Get FontName() As String
  FontName = UserControl.FontName
End Property

Public Property Let FontName(ByVal strNewValue As String)
  UserControl.FontName() = strNewValue
  UserControl.Font.Name = UserControl.FontName
  DrawButton
  PropertyChanged "FontName"
End Property

Public Property Get FontSize() As Single
  FontSize = UserControl.FontSize
End Property

Public Property Let FontSize(ByVal sinNewValue As Single)
  UserControl.FontSize() = sinNewValue
  UserControl.Font.Size = UserControl.FontSize
  DrawButton
  PropertyChanged "FontSize"
End Property

Public Property Get FontStrikethru() As Boolean
  FontStrikethru = UserControl.FontStrikethru
End Property

Public Property Let FontStrikethru(ByVal bolNewValue As Boolean)
  UserControl.FontStrikethru() = bolNewValue
  UserControl.Font.Strikethrough = UserControl.FontStrikethru
  DrawButton
  PropertyChanged "FontStrikethru"
End Property

Public Property Get FontUnderline() As Boolean
  FontUnderline = UserControl.FontUnderline
End Property

Public Property Let FontUnderline(ByVal bolNewValue As Boolean)
  UserControl.FontUnderline() = bolNewValue
  UserControl.Font.Underline = UserControl.FontUnderline
  DrawButton
  PropertyChanged "FontUnderline"
End Property

Public Sub Refresh()
Attribute Refresh.VB_Description = "Forces a complete repaint of a object."
  UserControl.Refresh
End Sub

Private Sub ExitTimer_Timer()
  If Not UnderMouse Then Leave
End Sub

Private Sub mobjMenu_Click(objBar As Bar)
  RaiseEvent MenuBarClicked(objBar)
End Sub

Private Sub mobjMenu_MenuBarsInitialized()
  RaiseEvent MenuBarsInitialized
End Sub

Private Sub mobjMenu_MenuBarsRemoved()
  RaiseEvent MenuBarsRemoved
End Sub

Private Sub mobjMenu_MouseOver(objBar As Bar)
  RaiseEvent MenuBarOver(objBar)
End Sub

Private Sub UserControl_AccessKeyPress(KeyAscii As Integer)
  If menvStyle <> [Seperator] And menvStyle <> SeperatorH And menvStyle <> [Toolbar Handle] And menvStyle <> [Toolbar HandleH] And Not mbolLocked Then
    RaiseEvent Click
  End If
End Sub

Private Sub UserControl_AmbientChanged(PropertyName As String)
  If PropertyName = "DisplayAsDefault" Then
    DrawButton
  End If
End Sub

Private Sub UserControl_DblClick()
  RaiseEvent DblClick
End Sub

Private Sub UserControl_EnterFocus()
  mbolHasFocus = True
  DrawButton
End Sub

Private Sub UserControl_ExitFocus()
  mbolHasFocus = False
  DrawButton
  Refresh
End Sub

Private Sub UserControl_Initialize()
  InitializeUpDownDither
End Sub

Private Sub UserControl_Terminate()
  Set mobjMenu = Nothing
End Sub

Private Sub UserControl_KeyDown(KeyCode As Integer, Shift As Integer)
  If KeyCode = 32 Then
    mintCurrentButtonPressed = 0
    mbolButtonDown = True
    DrawButton
  End If
  RaiseEvent KeyDown(KeyCode, Shift)
End Sub

Private Sub UserControl_KeyPress(KeyAscii As Integer)
  RaiseEvent KeyPress(KeyAscii)
End Sub

Private Sub UserControl_KeyUp(KeyCode As Integer, Shift As Integer)
  If KeyCode = 32 Then
    mintCurrentButtonPressed = -1
    mbolButtonDown = False
    DrawButton
    If menvStyle <> [Seperator] And menvStyle <> SeperatorH And menvStyle <> [Toolbar Handle] And menvStyle <> [Toolbar HandleH] Then
      RaiseEvent Click
    End If
  End If
  RaiseEvent KeyUp(KeyCode, Shift)
End Sub

Private Sub UserControl_LostFocus()
  mbolHasFocus = False
  DrawButton
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
  If mbolLocked Then Exit Sub
  
  If mbolDropDown Then
    If x > (UserControl.ScaleWidth - 11) Then
      mbDropDownPressed = True
      mintCurrentButtonPressed = 1
    Else
      If mbolDropDownAsButton Then
        mbolButtonDown = True
        mintCurrentButtonPressed = 0
      End If
    End If
  Else
    mbolButtonDown = True
    mintCurrentButtonPressed = 0
  End If
  mbMouseDown = True
  DrawButton
  RaiseEvent MouseDown(Button, Shift, x, y)
End Sub

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
  
  If mbolLocked Then Exit Sub
  
  If Button = 1 Then
    If (x < 0 Or y < 0 Or x >= ScaleWidth Or y >= ScaleHeight) Then
      If mintCurrentButtonPressed = 0 Then
        mbolButtonDown = False
      Else
        mbDropDownPressed = False
      End If
      DrawButton
    Else
      If mintCurrentButtonPressed = 0 Then
        mbolButtonDown = True
      Else
        If mbolDropDownAsButton Then
          mbDropDownPressed = True
        End If
      End If
      DrawButton
    End If
  End If

  If mbolMouseOver Then
    If Not UnderMouse Then
      Leave
    End If
  Else
    If UnderMouse Then
      mbolMouseOver = True
      RaiseEvent MouseEnter
      DrawButton

      ' Set up the ExitTimer
      Set ExitTimer = New objTimer
      ExitTimer.Interval = 50
      ExitTimer.Enabled = True
    End If
  End If
  RaiseEvent MouseMove(Button, Shift, x, y)
End Sub

Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
  RaiseEvent MouseUp(Button, Shift, x, y)
  
  Dim bOverButton As Boolean
  bOverButton = (x >= 0 And x <= ScaleWidth) And (y >= 0 And y <= ScaleHeight)
  
  Dim bOverDropDown As Boolean
  bOverDropDown = bOverButton And (x > ScaleWidth - 10)
  
  If mintCurrentButtonPressed = 1 And bOverDropDown Then
    RaiseEvent DropDownClick
    
    If Not mobjMenu Is Nothing Then
      If mobjMenu.Count > 0 Then
        Dim udtRect As RECT
        Call GetWindowRect(hWnd, udtRect)
        
        Dim strRes As String
        strRes = mobjMenu.Show(udtRect.Left, udtRect.Bottom)
        If Len(Trim(strRes)) > 0 Then
          If mbolDropDownUpdPic Then
            Set mpicPicture = mobjMenu(strRes).Icon
          End If
          
          If mbolDropDownUpdCap Then
            mstrCaption = mobjMenu(strRes).Caption
          End If
        End If
        
      End If
    End If
  End If
  
  mbolButtonDown = False
  mbDropDownPressed = False
  mbMouseDown = False

  If menvStyle = [Up-Down Button] Then
    mbolValue = Not mbolValue
    CheckButtonGroup
  End If

  DrawButton
  
  Dim bolIsButton As Boolean
  bolIsButton = menvStyle = [Cool Button] Or _
                menvStyle = [Standard Button] Or _
                menvStyle = [Toolbar Button] Or _
                menvStyle = [Up-Down Button]
  

  If bolIsButton And mintCurrentButtonPressed = 0 And bOverButton Then
    Dim bolRaise As Boolean
    bolRaise = True
    If mbolDropDownAsButton Then
      bolRaise = IIf(mbolDropDown, x < ScaleWidth - 10, True)
    End If
    
    If bolRaise Then
      RaiseEvent Click
    End If
  End If
  mintCurrentButtonPressed = -1
  DrawButton
End Sub

Public Property Get TextAlign() As envbuTextAlign
  TextAlign = menvTextAlign
End Property

Public Property Let TextAlign(ByVal New_TextAlign As envbuTextAlign)
  menvTextAlign = New_TextAlign
  PropertyChanged "TextAlign"
  DrawButton
End Property

Public Property Get PictureAlign() As envbuPictureAlign
Attribute PictureAlign.VB_Description = "Specifies alignment of the picture property."
  PictureAlign = menvPictureAlign
End Property

Public Property Let PictureAlign(ByVal New_PictureAlign As envbuPictureAlign)
  menvPictureAlign = New_PictureAlign
  PropertyChanged "PictureAlign"
  DrawButton
End Property

Public Property Get Caption() As String
Attribute Caption.VB_Description = "Text displayed on the face of the button."
Attribute Caption.VB_ProcData.VB_Invoke_Property = "General"
Attribute Caption.VB_UserMemId = -518
  Caption = mstrCaption
End Property

Public Property Let Caption(ByVal New_Caption As String)
  mstrCaption = New_Caption
  PropertyChanged "Caption"
  SetAccessKey
  DrawButton
End Property

Private Sub SetAccessKey()
  Dim iPos As Integer
  Dim sChar As String
  
  iPos = InStr(1, mstrCaption, "&")
  If iPos > 0 Then
    sChar = Mid$(mstrCaption, iPos + 1, 1)
    If sChar <> "&" Then
      UserControl.AccessKeys = LCase(sChar)
    End If
  End If
End Sub

Private Sub UserControl_Paint()
  DrawButton
End Sub

Private Sub UserControl_InitProperties()
  Dim fntAmbent As Font
  Set fntAmbent = Ambient.Font
  
  Set UserControl.Font = fntAmbent
  UserControl.FontBold = fntAmbent.Bold
  UserControl.FontItalic = fntAmbent.Italic
  UserControl.FontName = fntAmbent.Name
  UserControl.FontSize = fntAmbent.Size
  UserControl.FontStrikethru = fntAmbent.Strikethrough
  UserControl.FontUnderline = fntAmbent.Underline
  
  'mstrPictureName = ""
  Set mpicPicture = Nothing
  
  Set mpicFlatPicture = Nothing
  Set mpicDownPicture = Nothing
  Set mpicDisabledPicture = Nothing
  menvPictureAlign = m_def_PictureAlign
  menvTextAlign = m_def_TextAlign
  mstrCaption = m_def_Caption
  mclrMaskColor = m_def_MaskColor
  menvStyle = m_def_Style
  mbolValue = False
  mbolDropDown = m_def_DropDown
  mclrButtonFace = vbButtonFace
  mclrButtonLightShadow = vbButtonShadow
  mclrButtonDarkShadow = vb3DDKShadow
  mclrButtonHighlight = vb3DHighlight
  mbolShowFlatGrey = False
  mbolButtonGroup = m_def_ButtonGroup
  mbolButtonGroupDefault = m_def_ButtonGroupDefault
  mbolButtonGroupDefault2 = m_def_ButtonGroupDefault2
  mbolShowFocusRect = m_def_ShowFocusRect
  mintCurrentButtonPressed = -1
  mbolMouseOver = False
  mbolButtonDown = False
  mbMouseDown = False
  mbolHasFocus = False
  mbDropDownPressed = False
  mbolAutoSize = m_def_AutoSize
  mbytSpacing = m_def_Spacing
  mbolLocked = m_def_Locked
  mbolDropDownAsButton = m_def_DropDownAsButton
  mbolDropDownUpdPic = m_def_DropDownUpdPic
  mbolDropDownUpdCap = m_def_DropDownUpdCap
End Sub

Private Sub UserControl_ReadProperties(PropBag As PropertyBag)
  Dim fntAmbent As Font
  Set fntAmbent = Ambient.Font

  Set UserControl.Font = PropBag.ReadProperty("Font", fntAmbent)
  UserControl.FontBold = PropBag.ReadProperty("FontBold", fntAmbent.Bold)
  UserControl.FontItalic = PropBag.ReadProperty("FontItalic", fntAmbent.Italic)
  UserControl.FontName = PropBag.ReadProperty("FontName", fntAmbent.Name)
  UserControl.FontSize = PropBag.ReadProperty("FontSize", fntAmbent.Size)
  UserControl.FontStrikethru = PropBag.ReadProperty("FontStrikethru", fntAmbent.Strikethrough)
  UserControl.FontUnderline = PropBag.ReadProperty("FontUnderline", fntAmbent.Underline)
  
  Set mpicPicture = PropBag.ReadProperty("Picture", Nothing)
  Set mpicDownPicture = PropBag.ReadProperty("DownPicture", Nothing)
  Set mpicFlatPicture = PropBag.ReadProperty("FlatPicture", Nothing)
  Set mpicDisabledPicture = PropBag.ReadProperty("DisabledPicture", Nothing)
  Set mpicUpPicture = PropBag.ReadProperty("UpPicture", Nothing)
  Set mpicHoverPicture = PropBag.ReadProperty("HoverPicture", Nothing)
  UserControl.BackColor = PropBag.ReadProperty("BackColor", &H8000000F)
  UserControl.ForeColor = PropBag.ReadProperty("ForeColor", &H80000012)
  UserControl.Enabled = PropBag.ReadProperty("Enabled", True)
  menvPictureAlign = PropBag.ReadProperty("PictureAlign", m_def_PictureAlign)
  menvTextAlign = PropBag.ReadProperty("TextAlign", m_def_TextAlign)
  mstrCaption = PropBag.ReadProperty("Caption", m_def_Caption)
  mclrMaskColor = PropBag.ReadProperty("MaskColor", &HC0C0C0)
  menvStyle = PropBag.ReadProperty("Style", m_def_Style)
  mbolDropDown = PropBag.ReadProperty("DropDown", m_def_DropDown)
  mclrButtonDarkShadow = PropBag.ReadProperty("ColorDarkShadow", vb3DDKShadow)
  mclrButtonLightShadow = PropBag.ReadProperty("ColorLightShadow", vbButtonShadow)
  mclrButtonHighlight = PropBag.ReadProperty("ColorHighlight", vb3DHighlight)
  mbolShowFocusRect = PropBag.ReadProperty("ShowFocusRect", m_def_ShowFocusRect) 'LJC
  mbolShowFlatGrey = PropBag.ReadProperty("ShowFlatGrey", False)
  mbolButtonGroup = PropBag.ReadProperty("ButtonGroup", m_def_ButtonGroup)
  mbolButtonGroupDefault = PropBag.ReadProperty("ButtonGroupDefault", m_def_ButtonGroupDefault)
  mbolButtonGroupDefault2 = PropBag.ReadProperty("ButtonGroupDefault2", m_def_ButtonGroupDefault2)
  mbolValue = PropBag.ReadProperty("Value", False)
  mbolAutoSize = PropBag.ReadProperty("AutoSize", m_def_AutoSize)
  mbytSpacing = PropBag.ReadProperty("Spacing", m_def_Spacing)
  mbolLocked = PropBag.ReadProperty("Locked", m_def_Locked)
  mbolDropDownAsButton = PropBag.ReadProperty("DropDownAsButton", m_def_DropDownAsButton)
  mbolDropDownUpdPic = PropBag.ReadProperty("DropDownUpdatePicture", m_def_DropDownUpdPic)
  mbolDropDownUpdCap = PropBag.ReadProperty("DropDownUpdateCaption", m_def_DropDownUpdCap)
  
  SetAccessKey
  mintCurrentButtonPressed = -1
  
  DrawButton
End Sub

Private Sub UserControl_WriteProperties(PropBag As PropertyBag)
  Dim fntAmbent As Font
  Set fntAmbent = Ambient.Font

  Call PropBag.WriteProperty("Font", UserControl.Font, fntAmbent)
  Call PropBag.WriteProperty("FontBold", UserControl.FontBold, fntAmbent.Bold)
  Call PropBag.WriteProperty("FontItalic", UserControl.FontItalic, fntAmbent.Italic)
  Call PropBag.WriteProperty("FontName", UserControl.FontName, fntAmbent.Name)
  Call PropBag.WriteProperty("FontSize", UserControl.FontSize, fntAmbent.Size)
  Call PropBag.WriteProperty("FontStrikethru", UserControl.FontStrikethru, fntAmbent.Strikethrough)
  Call PropBag.WriteProperty("FontUnderline", UserControl.FontUnderline, fntAmbent.Underline)
  Call PropBag.WriteProperty("Picture", Picture, Nothing)
  
  Call PropBag.WriteProperty("Picture", mpicPicture, Nothing)
  Call PropBag.WriteProperty("DownPicture", mpicDownPicture, Nothing)
  Call PropBag.WriteProperty("FlatPicture", mpicFlatPicture, Nothing)
  Call PropBag.WriteProperty("DisabledPicture", mpicDisabledPicture, Nothing)
  Call PropBag.WriteProperty("UpPicture", mpicUpPicture, Nothing)
  Call PropBag.WriteProperty("HoverPicture", mpicHoverPicture, Nothing)
  Call PropBag.WriteProperty("BackColor", UserControl.BackColor, &H8000000F)
  Call PropBag.WriteProperty("ForeColor", UserControl.ForeColor, &H80000012)
  Call PropBag.WriteProperty("Enabled", UserControl.Enabled, True)
  Call PropBag.WriteProperty("PictureAlign", menvPictureAlign, m_def_PictureAlign)
  Call PropBag.WriteProperty("TextAlign", menvTextAlign, m_def_TextAlign)
  Call PropBag.WriteProperty("Caption", mstrCaption, m_def_Caption)
  Call PropBag.WriteProperty("MaskColor", mclrMaskColor, &HC0C0C0)
  Call PropBag.WriteProperty("Style", menvStyle, m_def_Style)
  Call PropBag.WriteProperty("DropDown", mbolDropDown, m_def_DropDown)
  Call PropBag.WriteProperty("ColorDarkShadow", mclrButtonDarkShadow, vb3DDKShadow)
  Call PropBag.WriteProperty("ColorLightShadow", mclrButtonLightShadow, vbButtonShadow)
  Call PropBag.WriteProperty("ColorHighlight", mclrButtonHighlight, vb3DHighlight)
  Call PropBag.WriteProperty("ShowFocusRect", mbolShowFocusRect, m_def_ShowFocusRect) 'LJC
  Call PropBag.WriteProperty("ShowFlatGrey", mbolShowFlatGrey, False)
  Call PropBag.WriteProperty("ButtonGroup", mbolButtonGroup, m_def_ButtonGroup)
  Call PropBag.WriteProperty("ButtonGroupDefault", mbolButtonGroupDefault, m_def_ButtonGroupDefault)
  Call PropBag.WriteProperty("ButtonGroupDefault2", mbolButtonGroupDefault2, m_def_ButtonGroupDefault2)
  Call PropBag.WriteProperty("Value", mbolValue, False)
  Call PropBag.WriteProperty("AutoSize", mbolAutoSize, m_def_AutoSize)
  Call PropBag.WriteProperty("Spacing", mbytSpacing, m_def_Spacing)
  Call PropBag.WriteProperty("Locked", mbolLocked, m_def_Locked)
  Call PropBag.WriteProperty("DropDownAsButton", mbolDropDownAsButton, m_def_DropDownAsButton)
  Call PropBag.WriteProperty("DropDownUpdatePicture", mbolDropDownUpdPic, m_def_DropDownUpdPic)
  Call PropBag.WriteProperty("DropDownUpdateCaption", mbolDropDownUpdCap, m_def_DropDownUpdCap)
End Sub

Private Sub UserControl_Resize()
  If Not mbolFromCode Then DrawButton
End Sub

Public Property Get MaskColor() As OLE_COLOR
Attribute MaskColor.VB_Description = "Sets/gets mask color to use when drawing picture"
  MaskColor = mclrMaskColor
End Property

Public Property Let MaskColor(ByVal New_MaskColor As OLE_COLOR)
  mclrMaskColor = New_MaskColor
  PropertyChanged "MaskColor"
  DrawButton
End Property

Public Property Get Style() As vbuStyle
Attribute Style.VB_Description = "Gets/Sets the style of the button"
  Style = menvStyle
End Property

Public Property Let Style(ByVal New_Style As vbuStyle)
  menvStyle = New_Style
  PropertyChanged "Style"
  DrawButton
End Property

Public Property Get DropDown() As Boolean
Attribute DropDown.VB_Description = "Determines whether or not to display the Drop Down Button."
  DropDown = mbolDropDown
End Property

Public Property Let DropDown(ByVal New_DropDown As Boolean)
  mbolDropDown = New_DropDown
  PropertyChanged "DropDown"
  
  If mbolDropDown Then
    If menvStyle = [Up-Down Button] Then
      menvStyle = [Cool Button]
      PropertyChanged "Style"
    End If
  Else
    mbolDropDownAsButton = m_def_DropDownAsButton
    PropertyChanged "DropDownAsButton"
  End If
  
  DrawButton
End Property

Private Sub DrawVLine(ByVal x As Single, ByVal y As Single, ByVal cx As Single, ByVal cy As Single)
  Line (x + 1, y)-(x + 1, y + cy), mclrButtonHighlight
  Line (x, y)-(x, y + cy), mclrButtonLightShadow
End Sub

Private Sub DrawHLine(ByVal x As Single, ByVal y As Single, ByVal cx As Single, ByVal cy As Single)
  Line (x, y + 1)-(x + cx, y + 1), mclrButtonHighlight
  Line (x, y)-(x + cx, y), mclrButtonLightShadow
End Sub

Private Sub DrawRaisedVLine(ByVal x As Single, ByVal y As Single, ByVal cx As Single, ByVal cy As Single)
  Line (x, y)-(x, y + cy), mclrButtonHighlight
  Line (x + 1, y)-(x + 1, y + cy), mclrButtonHighlight
  Line (x + 2, y)-(x + 2, y + cy), mclrButtonHighlight
  Line (x, y + 1)-(x, y + cy), mclrButtonLightShadow
  Line (x + 1, y + 1)-(x + 1, y + cy), mclrButtonLightShadow
  Line (x + 2, y + 1)-(x + 2, y + cy), mclrButtonLightShadow
  Line (x, y)-(x, y + cy - 1), mclrButtonHighlight
  Line (x + 1, y + 1)-(x + 1, y + cy - 1), mclrButtonFace
End Sub

Private Sub DrawRaisedHLine(ByVal x As Single, ByVal y As Single, ByVal cx As Single, ByVal cy As Single)
  Line (x, y)-(x + cx, y), mclrButtonHighlight
  Line (x, y + 1)-(x + cx, y + 1), mclrButtonHighlight
  Line (x, y + 2)-(x + cx, y + 2), mclrButtonHighlight
  Line (x + 1, y)-(x + cx, y), mclrButtonLightShadow
  Line (x + 1, y + 1)-(x + cx, y + 1), mclrButtonLightShadow
  Line (x + 1, y + 2)-(x + cx, y + 2), mclrButtonLightShadow
  Line (x, y)-(x + cx - 1, y), mclrButtonHighlight
  Line (x + 1, y + 1)-(x + cx - 1, y + 1), mclrButtonFace
End Sub

Private Sub DrawShadowBox(RectSize As RECT, ByVal Pressed As Boolean, ByVal DKShadow As Boolean)
  Dim x As Integer, y As Integer, cx As Integer, cy As Integer
  x = RectSize.Left
  y = RectSize.Top
  cx = RectSize.Right
  cy = RectSize.Bottom
  
  If DKShadow Then
    If Pressed Then
      Line (x, y)-(x + cx - 1, y), mclrButtonDarkShadow
      Line (x, y)-(x, y + cy - 1), mclrButtonDarkShadow
      Line (x + 1, y + 1)-(x + cx - 2, y + 1), mclrButtonLightShadow
      Line (x + 1, y + 1)-(x + 1, y + cy - 2), mclrButtonLightShadow
      Line (x + cx - 1, y)-(x + cx - 1, y + cy), mclrButtonHighlight
      Line (x, y + cy - 1)-(x + cx, y + cy - 1), mclrButtonHighlight
    Else
      Line (x, y)-(x + cx - 1, y), mclrButtonHighlight
      Line (x, y)-(x, y + cy - 1), mclrButtonHighlight
      Line (x + cx - 2, y + 1)-(x + cx - 2, y + cy - 1), mclrButtonLightShadow
      Line (x + 1, y + cy - 2)-(x + cx - 1, y + cy - 2), mclrButtonLightShadow
      Line (x + cx - 1, y)-(x + cx - 1, y + cy), mclrButtonDarkShadow
      Line (x, y + cy - 1)-(x + cx, y + cy - 1), mclrButtonDarkShadow
    End If
  Else
    Dim Color1 As Long
    Dim Color2 As Long
    If Pressed Then
      Color1 = mclrButtonLightShadow
      Color2 = mclrButtonHighlight
    Else
      Color1 = mclrButtonHighlight
      Color2 = mclrButtonLightShadow
    End If
    
    Line (x, y)-(x + cx - 1, y), Color1
    Line (x, y)-(x, y + cy - 1), Color1
    Line (x + cx - 1, y)-(x + cx - 1, y + cy), Color2
    Line (x, y + cy - 1)-(x + cx, y + cy - 1), Color2
  End If
End Sub

Public Property Get ColorLightShadow() As OLE_COLOR
Attribute ColorLightShadow.VB_Description = "Sets/gets color of border light shadow"
  ColorLightShadow = mclrButtonLightShadow
End Property

Public Property Let ColorLightShadow(ByVal New_Value As OLE_COLOR)
  If Not (mclrButtonLightShadow = New_Value) Then
    mclrButtonLightShadow = New_Value
    DrawButton
  End If
  PropertyChanged "ColorLightShadow"
End Property

Public Property Get ColorDarkShadow() As OLE_COLOR
Attribute ColorDarkShadow.VB_Description = "Sets/gets color of border 3D dark shadow"
  ColorDarkShadow = mclrButtonDarkShadow
End Property

Public Property Let ColorDarkShadow(ByVal New_Value As OLE_COLOR)
  If Not (mclrButtonDarkShadow = New_Value) Then
    mclrButtonDarkShadow = New_Value
    DrawButton
  End If
  PropertyChanged "ColorDarkShadow"
End Property

Public Property Get ColorHighlight() As OLE_COLOR
Attribute ColorHighlight.VB_Description = "Sets/gets color of border 3D highlight"
  ColorHighlight = mclrButtonHighlight
End Property

Public Property Let ColorHighlight(ByVal New_Value As OLE_COLOR)
  If Not (mclrButtonHighlight = New_Value) Then
    mclrButtonHighlight = New_Value
    DrawButton
  End If
  PropertyChanged "ColorHighlight"
End Property

Public Property Get ShowFlatGrey() As Boolean
Attribute ShowFlatGrey.VB_Description = "Sets/gets a value to determine if picture is drawn in greyscale when mouse is not over button"
Attribute ShowFlatGrey.VB_ProcData.VB_Invoke_Property = "General"
  ShowFlatGrey = mbolShowFlatGrey
End Property

Public Property Let ShowFlatGrey(ByVal New_Value As Boolean)
  mbolShowFlatGrey = New_Value
  PropertyChanged "DropDown"
  DrawButton
End Property

Public Property Get ShowFocusRect() As Boolean
Attribute ShowFocusRect.VB_ProcData.VB_Invoke_Property = "General"
  ShowFocusRect = mbolShowFocusRect
End Property

Public Property Let ShowFocusRect(ByVal New_Value As Boolean)
  mbolShowFocusRect = New_Value
  PropertyChanged "ShowFocusRect"
  DrawButton
End Property

Public Property Get ButtonGroup() As String
  ButtonGroup = mbolButtonGroup
End Property

Public Property Let ButtonGroup(ByVal New_ButtonGroup As String)
  If Not (mbolButtonGroup = New_ButtonGroup) Then
    mbolButtonGroup = New_ButtonGroup
    If menvStyle = [Up-Down Button] Then
      CheckButtonGroup
      Cls
      UserControl_Paint
    End If
  End If
  PropertyChanged "ButtonGroup"
End Property

Public Property Get ButtonGroupDefault() As Boolean
  ButtonGroupDefault = mbolButtonGroupDefault
End Property

Public Property Let ButtonGroupDefault(ByVal New_ButtonGroupDefault As Boolean)
  ' The following line of code ensures that the integer
  ' value of the boolean parameter is either
  ' 0 or -1.  It is known that Access 97 will
  ' set the boolean's value to 255 for true.
  ' In this case a P-Code compiled VB5 built
  ' OCX will return True for the expression
  ' (Not [boolean variable that ='s 255]).  This
  ' line ensures the reliability of boolean operations
  New_ButtonGroupDefault = CBool(New_ButtonGroupDefault)
  
  If Not (mbolButtonGroupDefault = New_ButtonGroupDefault) Then
    mbolButtonGroupDefault = New_ButtonGroupDefault
    If menvStyle = [Up-Down Button] Then
      CheckButtonGroupDefault
      CheckButtonGroup
      Cls
      UserControl_Paint
    End If
  End If
  PropertyChanged "ButtonGroupDefault"
End Property

Private Sub CheckButtonGroupDefault()
  If (Len(mbolButtonGroup) > 0) Then
    
    ' make all others in group not default
    If mbolButtonGroupDefault Then
      Dim ctl As Control
      Dim i As Long
      For i = 0 To UserControl.ParentControls.Count - 1
        If TypeOf UserControl.ParentControls(i) Is Control Then
          Set ctl = UserControl.ParentControls(i)
          If TypeOf ctl Is Button Then
            If ctl.ButtonGroup = mbolButtonGroup Then
              If Not ((ctl Is UserControl.Extender) Or (ctl Is UserControl)) Then
                ctl.ButtonGroupDefault = False
              End If
            End If
          End If
        End If
      Next
    End If
    
  End If
End Sub

Public Property Get ButtonGroupDefault2() As Boolean
  ButtonGroupDefault2 = mbolButtonGroupDefault2
End Property

Public Property Let ButtonGroupDefault2(ByVal New_ButtonGroupDefault2 As Boolean)
  ' The following line of code ensures that the integer
  ' value of the boolean parameter is either
  ' 0 or -1.  It is known that Access 97 will
  ' set the boolean's value to 255 for true.
  ' In this case a P-Code compiled VB5 built
  ' OCX will return True for the expression
  ' (Not [boolean variable that ='s 255]).  This
  ' line ensures the reliability of boolean operations
  New_ButtonGroupDefault2 = CBool(New_ButtonGroupDefault2)
  
  If Not (mbolButtonGroupDefault2 = New_ButtonGroupDefault2) Then
    mbolButtonGroupDefault2 = New_ButtonGroupDefault2
    If menvStyle = [Up-Down Button] Then
      CheckButtonGroupDefault2
      CheckButtonGroup
      Cls
      UserControl_Paint
    End If
  End If
  
  PropertyChanged "ButtonGroupDefault2"
End Property

Private Sub CheckButtonGroupDefault2()
  If (Len(mbolButtonGroup) > 0) Then
    
    ' make all others in group not default
    If mbolButtonGroupDefault2 Then
      Dim ctl As Control
      Dim i As Long
      For i = 0 To UserControl.ParentControls.Count - 1
        If TypeOf UserControl.ParentControls(i) Is Control Then
          Set ctl = UserControl.ParentControls(i)
          If TypeOf ctl Is Button Then
            If ctl.ButtonGroup = mbolButtonGroup Then
              If Not ((ctl Is UserControl.Extender) Or (ctl Is UserControl)) Then
                ctl.ButtonGroupDefault2 = False
              End If
            End If
          End If
        End If
      Next
    End If
    
  End If
End Sub

Private Sub CheckButtonGroup()
  If (Len(mbolButtonGroup) > 0) Then
    Dim ctl As Control
    Dim i As Long
    
    ' Clear all others in group
    If mbolValue Then
      For i = 0 To UserControl.ParentControls.Count - 1
        If TypeOf UserControl.ParentControls(i) Is Control Then
          Set ctl = UserControl.ParentControls(i)
          If TypeOf ctl Is Button Then
            If ctl.ButtonGroup = mbolButtonGroup Then
              If Not ((ctl Is UserControl.Extender) Or (ctl Is UserControl)) Then
                ctl.Value = False
              End If
            End If
          End If
        End If
      Next
    Else
      ' Set group default if necessary
      Dim GroupValueSet As Boolean
      Dim ctlDefault As Button
      Dim ctlDefault2 As Button
      Set ctlDefault = Nothing
      Set ctlDefault2 = Nothing
      GroupValueSet = False
        
      For i = 0 To UserControl.ParentControls.Count - 1
        If TypeOf UserControl.ParentControls(i) Is Control Then
          Set ctl = UserControl.ParentControls(i)
          If TypeOf ctl Is Button Then
            If ctl.ButtonGroup = mbolButtonGroup Then
              If ctl.Value Then
                GroupValueSet = True
                Exit For
              ElseIf ctl.ButtonGroupDefault Then
                Set ctlDefault = ctl
              ElseIf ctl.ButtonGroupDefault2 Then
                Set ctlDefault2 = ctl
              End If
            End If
          End If
        End If
      Next
        
      If Not (GroupValueSet Or (ctlDefault Is Nothing)) Then
        If (Not mbolButtonGroupDefault) Or (ctlDefault2 Is Nothing) Then
          ctlDefault.Value = True
        Else
          ctlDefault2.Value = True
        End If
      End If
    
    End If
  End If
End Sub

Public Property Get Value() As Boolean
Attribute Value.VB_ProcData.VB_Invoke_Property = "General"
  Value = mbolValue
End Property

Public Property Let Value(ByVal New_Value As Boolean)
  'The following line of code ensures that the integer
  'value of the boolean parameter is either
  '0 or -1.  It is known that Access 97 will
  'set the boolean's value to 255 for true.
  'In this case a P-Code compiled VB5 built
  'OCX will return True for the expression
  '(Not [boolean variable that ='s 255]).  This
  'line ensures the reliability of boolean operations
  New_Value = CBool(New_Value)
  
  If Not (mbolValue = New_Value) Then
    mbolValue = New_Value
    If menvStyle = [Up-Down Button] Then
      CheckButtonGroup
      Cls
      UserControl_Paint
    End If
  End If
  PropertyChanged "Value"
  
  DrawButton
End Property

Private Sub PaintUpDownDither(x As Long, y As Long, Width As Long, Height As Long)
  Dim ret As Long
  Dim MyRect As RECT
  
  ' Draw on the form with that brush
  MyRect.Left = x
  MyRect.Top = y
  MyRect.Right = x + Width
  MyRect.Bottom = y + Height
  
  ret = FillRect(UserControl.hDC, MyRect, hUpDownDitherBrush)
End Sub

Private Sub InitializeUpDownDither()
  Dim i As Long
  Dim j As Long
  
  ' One-Time Setup : put this in it's own routine
  ' set (invisible) picturebox properties for creating a brush
  ' Draw the dither in it
  For i = 0 To UserControl.ScaleWidth - 1
    For j = 0 To UserControl.ScaleHeight - 1
      If (i + j) Mod 2 Then
        UserControl.PSet (i, j), vb3DHighlight
      Else
        UserControl.PSet (i, j), vbButtonFace
      End If
    Next j
  Next i
  
  'create the brush from it
  hUpDownDitherBrush = CreatePatternBrush(UserControl.Image.handle)
End Sub

Private Sub Draw3dBorder(ByRef udtRect As RECT, ByRef intDownOffset As Integer)
  Select Case menvStyle
    Case [Cool Button]
      If mbolMouseOver Or mintCurrentButtonPressed > -1 Then
        Call DrawShadowBox(udtRect, mbolButtonDown, False)
        intDownOffset = IIf(mbolButtonDown, 1, 0)
      End If
  
    Case [Toolbar Button]
      Call DrawShadowBox(udtRect, mbolButtonDown, False)
      intDownOffset = IIf(mbolButtonDown, 1, 0)
    
    Case [Standard Button]
      Call DrawShadowBox(udtRect, mbolButtonDown, True)
      intDownOffset = IIf(mbolButtonDown, 1, 0)
      
    Case [Seperator]
      Line (0, 0)-(ScaleWidth, ScaleHeight), BackColor, BF
      Call DrawVLine(ScaleWidth \ 2 - 1, 0, 2, ScaleHeight)
    
    Case [SeperatorH]
      Line (0, 0)-(ScaleWidth, ScaleHeight), BackColor, BF
      Call DrawHLine(0, ScaleHeight \ 2 - 1, ScaleWidth, 2)
    
    Case [Toolbar Handle]
      Line (0, 0)-(ScaleWidth, ScaleHeight), BackColor, BF
      Call DrawRaisedVLine(ScaleWidth \ 2 - 4, 0, 3, ScaleHeight)
      Call DrawRaisedVLine(ScaleWidth \ 2, 0, 3, ScaleHeight)
    
    Case [Toolbar HandleH]
      Line (0, 0)-(ScaleWidth, ScaleHeight), BackColor, BF
      Call DrawRaisedHLine(0, ScaleHeight \ 2 - 4, ScaleWidth, 3)
      Call DrawRaisedHLine(0, ScaleHeight \ 2, ScaleWidth, 3)
    
    Case [Up-Down Button]
      If mbolValue Then
        If mbolMouseOver Then
          Call PaintUpDownDither(1, 1, ScaleWidth - 2, ScaleHeight - 2)
          Call DrawShadowBox(udtRect, True, False)
        Else
          Call DrawShadowBox(udtRect, True, False)
        End If
      Else
        If mbolMouseOver Or mintCurrentButtonPressed > -1 Then
          Call DrawShadowBox(udtRect, mbolButtonDown, False)
          intDownOffset = IIf(mbolButtonDown, 1, 0)
        End If
      End If
  End Select
End Sub

Private Sub AdjustControlSize(ByRef bolHasPicture As Boolean, _
                              ByVal intPicWidth As Integer, _
                              ByVal intPicHeight As Integer, _
                              ByVal bolHasCaption As Boolean, _
                              ByVal intTextWidth As Integer, _
                              ByVal intTextHeight As Integer)

  Dim intWidthInPixels As Integer
  Dim intHeightInPixels As Integer
    
  If bolHasPicture And bolHasCaption Then
    Select Case menvPictureAlign
      Case vbPicLeft, vbPicRight
        intWidthInPixels = (2 * cnsVerticalBorderWidth) + (3 * mbytSpacing) + intPicWidth + intTextWidth + 4
        intHeightInPixels = (2 * cnsHorizontalBorderWidth) + (2 * mbytSpacing) + intPicHeight + 2
      
      Case vbPicTop, vbPicBottom
        Dim intMax As Integer
        intMax = IIf(intPicWidth > intTextWidth, intPicWidth, intTextWidth)
        intWidthInPixels = (2 * cnsVerticalBorderWidth) + (3 * mbytSpacing) + intMax + 4
        intHeightInPixels = (2 * cnsHorizontalBorderWidth) + (3 * mbytSpacing) + _
                            intPicHeight + intTextHeight + 2
    End Select
  End If
  
  If bolHasPicture And Not bolHasCaption Then
    intWidthInPixels = (2 * cnsVerticalBorderWidth) + (2 * mbytSpacing) + intPicWidth - 1
    intHeightInPixels = (2 * cnsHorizontalBorderWidth) + (2 * mbytSpacing) + intPicHeight - 1
  End If
  
  If Not bolHasPicture And bolHasCaption Then
    intWidthInPixels = (2 * cnsVerticalBorderWidth) + (2 * mbytSpacing) + intTextWidth + 2
    intHeightInPixels = (2 * cnsHorizontalBorderWidth) + (2 * mbytSpacing) + intTextHeight
  End If
  
  If mbolDropDown Then
    intWidthInPixels = intWidthInPixels + cnsDropDownArrowWidth
  End If
  
  UserControl.Width = intWidthInPixels * Screen.TwipsPerPixelX
  UserControl.Height = intHeightInPixels * Screen.TwipsPerPixelY
End Sub

Private Function GetAmbientScaleMode() As VBRUN.ScaleModeConstants
  Select Case UCase(UserControl.Ambient.ScaleUnits)
    Case UCase("User")
      GetAmbientScaleMode = vbUser
    Case UCase("Twip")
      GetAmbientScaleMode = vbTwips
    Case UCase("Point")
      GetAmbientScaleMode = vbPoints
    Case UCase("Pixel")
      GetAmbientScaleMode = vbPixels
    Case UCase("Character")
      GetAmbientScaleMode = vbCharacters
    Case UCase("Inche")
      GetAmbientScaleMode = vbInches
    Case UCase("Millimeter")
      GetAmbientScaleMode = vbMillimeters
    Case UCase("Centimeter")
      GetAmbientScaleMode = vbCentimeters
    Case UCase("Himetric")
      GetAmbientScaleMode = vbHimetric
    Case UCase("ContainerPosition")
      GetAmbientScaleMode = vbContainerPosition
    Case UCase("ContainerSize")
      GetAmbientScaleMode = vbContainerSize
  End Select
End Function


Private Sub SetFaceInfo(ByRef bolHasPicture As Boolean, _
                        ByVal intPicWidth As Integer, _
                        ByVal intPicHeight As Integer, _
                        ByRef intPicLeft As Integer, _
                        ByRef intPicTop As Integer, _
                        ByVal bolHasCaption As Boolean, _
                        ByRef udtTextRect As RECT)

  If bolHasPicture And bolHasCaption Then
    Select Case menvPictureAlign
      Case vbPicLeft
        intPicTop = mintClientTop + ((mintClientHeight - intPicHeight) / 2) - 1
        intPicLeft = mintClientLeft + (mbytSpacing / 2)
        udtTextRect.Left = intPicLeft + intPicWidth + mbytSpacing
        udtTextRect.Top = mintClientTop
        udtTextRect.Right = mintClientLeft + mintClientWidth - mbytSpacing - 4
        udtTextRect.Bottom = mintClientTop + mintClientHeight - 4
      
      Case vbPicRight
        intPicTop = mintClientTop + ((mintClientHeight - intPicHeight) / 2) - 1
        intPicLeft = mintClientLeft + mintClientWidth - (intPicWidth + mbytSpacing) - 3
        udtTextRect.Top = mintClientTop
        udtTextRect.Left = mintClientLeft + mbytSpacing
        udtTextRect.Bottom = mintClientTop + mintClientHeight - 4
        udtTextRect.Right = intPicLeft - mbytSpacing - 2
      
      Case vbPicTop
        intPicTop = mintClientTop + mbytSpacing
        intPicLeft = mintClientLeft + ((mintClientWidth - intPicWidth) / 2) - 1
        udtTextRect.Top = intPicTop + intPicHeight + mbytSpacing
        udtTextRect.Bottom = mintClientTop + mintClientHeight - (1 * mbytSpacing) - 4
        udtTextRect.Left = mintClientLeft + mbytSpacing
        udtTextRect.Right = mintClientLeft + mintClientWidth - (1 * mbytSpacing) - 4
      
      Case vbPicBottom
        intPicTop = mintClientTop + mintClientHeight - intPicHeight - mbytSpacing - 3
        intPicLeft = mintClientLeft + ((mintClientWidth - intPicWidth) / 2) - 1
        udtTextRect.Top = mintClientTop + mbytSpacing
        udtTextRect.Bottom = intPicTop - mbytSpacing - 1
        udtTextRect.Left = mintClientLeft + mbytSpacing
        udtTextRect.Right = mintClientLeft + mintClientWidth - (1 * mbytSpacing) - 4
    End Select
  End If
  
  If bolHasPicture And Not bolHasCaption Then
    intPicLeft = mintClientLeft + ((mintClientWidth - intPicWidth) / 2) - 2
    intPicTop = mintClientTop + ((mintClientHeight - intPicHeight) / 2) - 2
    udtTextRect.Top = mintClientTop
    udtTextRect.Bottom = mintClientTop + mintClientHeight - 4
    udtTextRect.Left = mintClientLeft
    udtTextRect.Right = mintClientLeft + mintClientWidth - 4
  End If
  
  If Not bolHasPicture And bolHasCaption Then
    udtTextRect.Top = mintClientTop
    udtTextRect.Bottom = mintClientTop + mintClientHeight - 4
    udtTextRect.Left = mintClientLeft
    udtTextRect.Right = mintClientLeft + mintClientWidth - 4
  End If

End Sub

Private Sub DrawButton()
  ' -------------------------------------------------------------------------------------
  ' Step 1 is to get all the information required to draw the button --------------------
  ' -------------------------------------------------------------------------------------
  ' Figure out which picture to display
  Dim picButton As StdPicture
  Set picButton = GetPicToDisplay()
  
  ' Does it have a picture ?
  Dim bolHasPicture As Boolean
  bolHasPicture = Not picButton Is Nothing
  
  ' Get the Pictures Width and Height
  Dim intPicWidth As Integer
  Dim intPicHeight As Integer
  If bolHasPicture Then
    intPicWidth = ScaleX(picButton.Width, vbHimetric, vbPixels)
    intPicHeight = ScaleY(picButton.Height, vbHimetric, vbPixels)
  End If
  
  ' Does it have a caption ?
  Dim bolHasCaption As Boolean
  bolHasCaption = Len(Trim(mstrCaption)) > 0
  
  ' Get the Caption Width and Height
  Dim intTextWidth As Integer
  Dim intTextHeight As Integer
  If bolHasCaption Then
    intTextWidth = TextWidth(mstrCaption)
    intTextHeight = TextHeight(mstrCaption)
  End If

  ' Get button the text format.
  Dim lngFormat As Long
  Call GetDrawTextInfo(picButton, lngFormat)

  ' -------------------------------------------------------------------------------------
  ' Step 2 Adjust the size of the control if the user wants to..
  ' -------------------------------------------------------------------------------------
  ' Adjust the size of the control
  If mbolAutoSize Then
    Call AdjustControlSize(bolHasPicture, intPicWidth, intPicHeight, _
                           bolHasCaption, intTextWidth, intTextHeight)
  End If
    
  ' Get the current coordinates of the user control.
  Dim intWidth As Integer
  Dim intHeight As Integer
  intWidth = ScaleWidth - IIf(mbolDropDown, cnsDropDownArrowWidth, 0)
  intHeight = ScaleHeight

  ' Set the client aria variables...
  ' These client variables describe the area inside the button to draw the picture.
  ' You can think of these like page margins in a word processor
  mintClientTop = cnsHorizontalBorderWidth
  mintClientLeft = cnsVerticalBorderWidth
  mintClientWidth = intWidth - (2 * cnsVerticalBorderWidth) + 2
  mintClientHeight = intHeight - (2 * cnsHorizontalBorderWidth) + 2
  
  ' -------------------------------------------------------------------------------------
  ' Step 3 Drow the button focus rectangle and the 3D border...
  ' -------------------------------------------------------------------------------------
  ' Clear the control.
  UserControl.Cls

  ' Draw the Focus Rectangle
  ' Only the standard button shows a focus rectangel...
  Dim intFocusOffset As Integer
  If menvStyle = [Standard Button] And mbolHasFocus Then
    intFocusOffset = 1
    Line (0, 0)-(ScaleWidth - 1, ScaleHeight - 1), vb3DDKShadow, B
  End If

  ' Setup the coordinates for the shadow box
  Dim udtRect As RECT
  udtRect.Top = intFocusOffset
  udtRect.Left = intFocusOffset
  udtRect.Right = intWidth - (2 * intFocusOffset)
  udtRect.Bottom = intHeight - (2 * intFocusOffset)
  
  ' Draw the 3d Border around the button, and the inside effect of the button (For the Up-Down).
  Dim intDownOffset As Integer
  Call Draw3dBorder(udtRect, intDownOffset)
  
  ' Drow the 3d border for the part of the button that represent the drop down.
  If mbolDropDown Then
    ' Reset the corrdinates to draw the dropdown button.
    udtRect.Top = intFocusOffset
    udtRect.Left = intWidth
    udtRect.Right = cnsDropDownArrowWidth - intFocusOffset
    udtRect.Bottom = intHeight - (2 * intFocusOffset)
    
    ' Draw the 3d Border around the button, and the inside effect of the button (For the Up-Down).
    Select Case menvStyle
      Case [Cool Button]
        If mbolMouseOver Or mintCurrentButtonPressed > -1 Then
          Call DrawShadowBox(udtRect, mbDropDownPressed, False)
          intDownOffset = 0
        End If
      
      Case [Toolbar Button], [Standard Button]
        Call DrawShadowBox(udtRect, mbDropDownPressed, False)
        intDownOffset = 0
    End Select
  
    ' Draw the Dropdown arrow.
    If menvStyle = [Cool Button] Or menvStyle = [Toolbar Button] Or menvStyle = [Standard Button] Then
      Dim lngArrowTop  As Long
      lngArrowTop = (ScaleHeight - 3) / 2
      
      Dim lngArrowLeft As Long
      lngArrowLeft = intWidth + 1 - intFocusOffset
      
      Dim lngColor As Long
      lngColor = IIf(Enabled, ForeColor, vbGrayText)
      
      Line ((lngArrowLeft) + 1, lngArrowTop)-((lngArrowLeft) + 6, lngArrowTop), lngColor
      Line ((lngArrowLeft) + 2, lngArrowTop + 1)-((lngArrowLeft) + 5, lngArrowTop + 1), lngColor
      Line ((lngArrowLeft) + 3, lngArrowTop + 2)-((lngArrowLeft) + 4, lngArrowTop + 2), lngColor
    End If
  End If

  ' -------------------------------------------------------------------------------------
  ' Step 3 Drow the button face (text and picture)...
  ' -------------------------------------------------------------------------------------
  Dim udtTextRect As RECT
  Dim intPicTop As Integer
  Dim intPicLeft As Integer
  Call SetFaceInfo(bolHasPicture, intPicWidth, intPicHeight, intPicLeft, _
                   intPicTop, bolHasCaption, udtTextRect)
  
  ' Draw the Picture
  Dim bolIsButton As Boolean
  bolIsButton = menvStyle = [Cool Button] Or _
                menvStyle = [Toolbar Button] Or _
                menvStyle = [Standard Button] Or _
                menvStyle = [Up-Down Button]
  
  If bolIsButton And bolHasPicture Then
    Dim objPaint As PaintEffects
    Set objPaint = New PaintEffects
    With objPaint
      If Enabled Then
        If menvStyle = [Cool Button] And Not mbolMouseOver And mbolShowFlatGrey Then
          Call .PaintGreyScaleCornerStdPic(hDC, intPicLeft + intDownOffset, _
                                           intPicTop + intDownOffset, intPicWidth, _
                                           intPicHeight, picButton, 0, 0)
        Else
          Call .PaintTransparentStdPic(hDC, intPicLeft + intDownOffset, _
                                       intPicTop + intDownOffset, intPicWidth, _
                                       intPicHeight, picButton, 0, 0, mclrMaskColor)
        End If
      Else
        If Not mpicDisabledPicture Is Nothing Then
          Call .PaintTransparentStdPic(hDC, intPicLeft + intDownOffset, _
                                       intPicTop + intDownOffset, intPicWidth, _
                                       intPicHeight, picButton, 0, 0, mclrMaskColor)
        Else
          Call .PaintDisabledStdPic(hDC, intPicLeft + intDownOffset, _
                                    intPicTop + intDownOffset, intPicWidth, _
                                    intPicHeight, picButton, 0, 0, mclrMaskColor)
        End If
      End If
    End With
    Set objPaint = Nothing
  End If
  Set picButton = Nothing
  
  ' Print the caption
  If menvStyle = [Cool Button] Or menvStyle = [Toolbar Button] Or menvStyle = [Standard Button] Or menvStyle = [Up-Down Button] Then
    udtTextRect.Top = udtTextRect.Top + intDownOffset
    udtTextRect.Left = udtTextRect.Left + intDownOffset
    udtTextRect.Bottom = udtTextRect.Bottom + intDownOffset
    udtTextRect.Right = udtTextRect.Right + intDownOffset
      
    If Enabled Then
      Call DrawText(hDC, mstrCaption, Len(mstrCaption), udtTextRect, _
                    lngFormat Or DT_SINGLELINE Or DT_VCENTER)
    Else
      Dim lngPrvColor As Long
      lngPrvColor = UserControl.ForeColor
      UserControl.ForeColor = vbGrayText
      Call DrawText(hDC, mstrCaption, Len(mstrCaption), udtTextRect, _
                    lngFormat Or DT_SINGLELINE Or DT_VCENTER)
      UserControl.ForeColor = lngPrvColor
    End If
  End If
    
  ' Draw the focus rectangle.
  If (menvStyle = [Standard Button] Or menvStyle = [Cool Button]) And mbolShowFocusRect Then
    udtRect.Top = mintClientTop
    udtRect.Left = mintClientLeft
    udtRect.Right = mintClientWidth
    udtRect.Bottom = mintClientHeight + IIf(menvStyle = [Standard Button], 0, 1)
    If mbolHasFocus Then
      Call DrawFocusRect(hDC, udtRect)
    Else
      Line (udtRect.Left - 1, udtRect.Top - 1)-(udtRect.Right, udtRect.Bottom), vb3DFace, B
    End If
  End If
  
  Refresh
End Sub

