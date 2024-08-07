VERSION 5.00
Begin VB.UserControl vbalCommandBar 
   Alignable       =   -1  'True
   AutoRedraw      =   -1  'True
   CanGetFocus     =   0   'False
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   ControlContainer=   -1  'True
   ScaleHeight     =   240
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   320
   ToolboxBitmap   =   "vbalCommandBar.ctx":0000
   Begin VB.Timer tmrMenuPopup 
      Enabled         =   0   'False
      Interval        =   250
      Left            =   1080
      Top             =   1680
   End
   Begin VB.Timer tmrLostMouse 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   480
      Top             =   1680
   End
End
Attribute VB_Name = "vbalCommandBar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

' API calls
Private Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" ( _
    ByVal lpLibFileName As String) As Long
Private Declare Function FreeLibrary Lib "kernel32" ( _
   ByVal hLibModule As Long) As Long
Private Declare Sub InitCommonControls Lib "comctl32.dll" ()

Private Declare Function GetParent Lib "user32" (ByVal hWnd As Long) As Long
Private Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long
Private Declare Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long) As Long
   Private Const GWL_STYLE = (-16)
   Private Const WS_BORDER = &H800000
   Private Const WS_CHILD = &H40000000
   Private Const WS_DISABLED = &H8000000
   Private Const WS_VISIBLE = &H10000000
   Private Const WS_TABSTOP = &H100000
   Private Const WS_HSCROLL = &H100000
   Private Const GWL_EXSTYLE = (-20)
   Private Const WS_EX_TOPMOST = &H8&
   Private Const WS_EX_CLIENTEDGE = &H200&
   Private Const WS_EX_STATICEDGE = &H20000
   Private Const WS_EX_WINDOWEDGE = &H100&
   Private Const WS_EX_APPWINDOW = &H40000
   Private Const WS_EX_TOOLWINDOW = &H80&
   Private Const WS_EX_LAYERED As Long = &H80000

Private Declare Function SetWindowPos Lib "user32" (ByVal hWnd As Long, ByVal hWndInsertAfter As Long, ByVal x As Long, ByVal y As Long, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long
   Private Const SWP_FRAMECHANGED = &H20        '  The frame changed: send WM_NCCALCSIZE
   Private Const SWP_NOACTIVATE = &H10
   Private Const SWP_NOMOVE = &H2
   Private Const SWP_NOOWNERZORDER = &H200      '  Don't do owner Z ordering
   Private Const SWP_NOREDRAW = &H8
   Private Const SWP_NOSIZE = &H1
   Private Const SWP_NOZORDER = &H4
   Private Const SWP_SHOWWINDOW = &H40
   Private Const HWND_DESKTOP = 0
   Private Const HWND_NOTOPMOST = -2
   Private Const HWND_TOP = 0
   Private Const HWND_TOPMOST = -1
   Private Const HWND_BOTTOM = 1

Private Declare Function SetParent Lib "user32" (ByVal hWndChild As Long, ByVal hWndNewParent As Long) As Long

Private Const WM_ACTIVATEAPP = &H1C
Private Const WM_SETTINGCHANGE = &H1A&

Public Enum EToolBarStyle
   ' Render using Office XP style
   eOfficeXP
   ' Render using Office 2003 style (the default)
   eOffice2003
   ' Render using MS Money style
   eMoney
   ' Render using same style as System's ComCtl32.DLL
   eComCtl32
End Enum

Public Enum ECustomColors
   [_eccCustomColorFirst]
   eccButtonTextColor
   eccButtonTextHotColor
   eccButtonTextDisabledColor
   eccButtonBackgroundColorStart
   eccButtonBackgroundColorEnd
   eccButtonHotBackgroundColorStart
   eccButtonHotBackgroundColorEnd
   eccButtonCheckedBackgroundColorStart
   eccButtonCheckedBackgroundColorEnd
   eccButtonCheckedHotBackgroundColorStart
   eccButtonCheckedHotBackgroundColorEnd
   eccMenuShadowColor
   eccMenuBorderColor
   eccMenuTextColor
   eccMenuTextHotColor
   eccMenuTextDisabledColor
   eccMenuBackgroundColorStart
   eccMenuBackgroundColorEnd
   eccMenuHotBackgroundColorStart
   eccMenuHotBackgroundColorEnd
   eccMenuHotBorderColor
   eccMenuCheckedBackgroundColorStart
   eccMenuCheckedBackgroundColorEnd
   eccMenuCheckedHotBackgroundColorStart
   eccMenuCheckedHotBackgroundColorEnd
   eccIconDisabledColor
   eccLightColor
   eccDarkColor
   eccGradientColorStart
   eccGradientColorEnd
   [_eccCustomColorLast]
End Enum

Public Enum EButtonStyle
   
   ' A normal push button
   eNormal
   
   ' A group separator
   eSeparator
   
   ' A button which is split and should have a drop down
   eSplit
   
   ' A panel which holds a control. When the toolbar is shown
   ' in a vertical orientation, the panel is either hidden or,
   ' if the object has a valid icon index, it is displayed
   ' as a push button.
   ePanel
      
   ' A checkable button.
   eCheck
   
   ' A checkable button that toggles off any other buttons in
   ' the group when checked. In a radio group at least one
   ' button must be checked.
   eRadio
   
   ' A checkable button that toggles off any other button
   ' in the group when checked.  In a nullable radio group
   ' a checked radio button can be unchecked.
   eRadioNullable
   
End Enum

Public Enum ECommandBarOrientation
   eTop
   eLeft
   eRight
   eBottom
End Enum

Public Enum ECommandBarButtonTextPosition
   eButtonTextSide
   eButtonTextBottom
End Enum

Private m_hWnd As Long
Private m_hWndParent As Long
Private m_bDesignTime As Boolean
Private m_hMod As Long

Private m_bInUse As Boolean
Private m_hWndShownFrom As Long
Private m_hWndShownFromParent As Long
Private m_eMenuPopoutDirection As ECommandBarOrientation
Private m_lMenuPopoutStart As Long
Private m_lMenuPopoutExtent As Long
Private m_hMonitorOn As Long
Private m_eMenuTrackMode As Long
Private m_tLastMousePos As POINTAPI

Private m_bEnabled As Boolean
Private m_bVisible As Boolean
Private m_bRedraw As Boolean
Private m_bMainMenu As Boolean
Private m_bResizeInterlock As Boolean
Private m_eButtonPosition As ECommandBarButtonTextPosition

Private m_bPopup As Boolean
Private m_bPopupVisibleChecks As Boolean
Private m_bWrappable As Boolean

Private m_eOrientation As ECommandBarOrientation
Private m_sToolBarKey As String
Private m_bInDragMode As Boolean

Private m_item() As cDisplayButtonInfo
Private m_sLastToolTip As String

Private m_fntCache As New cFontCache

Private m_cRightShadow As cMenuDropShadow
Private m_cBottomShadow As cMenuDropShadow

Private m_cToolbarImageList As cCommandBarImageList
Private m_cMenuImageList As cCommandBarImageList

Private m_cBack As cAlphaDIBSection

Public Event ButtonDropDown(btn As cButton, cancel As Boolean)
Attribute ButtonDropDown.VB_Description = "Raised when a button is about to show a drop-down object."
Public Event ButtonClick(btn As cButton)
Attribute ButtonClick.VB_Description = "Raised when a button is clicked."
Public Event BeforeShowMenu(Bar As cCommandBar)
Attribute BeforeShowMenu.VB_Description = "Raised before a menu is about to be shown."
Public Event AfterShowMenu(Bar As cCommandBar)
Attribute AfterShowMenu.VB_Description = "Raised when a menu has just been closed."
Public Event RequestNewInstance(ctl As Object)
Attribute RequestNewInstance.VB_Description = "Raised when the control requires a new instance to display a menu."
Public Event Resize()
Attribute Resize.VB_Description = "Raised when the size of the toolbar changes."
Public Event RightClick(btn As cButton, ByVal x As Long, ByVal y As Long)
Attribute RightClick.VB_Description = "Raised when the user right clicks on the toolbar or menu, or an item within it."

Implements ISubclass

Public Property Get CustomColor(ByVal eColor As ECustomColors) As OLE_COLOR
Attribute CustomColor.VB_Description = "Gets/sets one of the colours used to draw all toolbars in the project."
  On Error Resume Next
   CustomColor = mCommandBarColours.CustomColor(eColor)
End Property
Public Property Let CustomColor(ByVal eColor As ECustomColors, ByVal oColor As OLE_COLOR)
  On Error Resume Next
   If Not (mCommandBarColours.CustomColor(eColor) = oColor) Then
      mCommandBarColours.CustomColor(eColor) = oColor
      fPaint
   End If
End Property
Public Property Get StyleColor(ByVal eColor As ECustomColors) As OLE_COLOR
Attribute StyleColor.VB_Description = "Gets the colour used for a particular element for the selected Style."
  On Error Resume Next
   StyleColor = mCommandBarColours.StyleColor(eColor)
End Property
Public Property Get UseStyleColor(ByVal eColor As ECustomColors) As Boolean
Attribute UseStyleColor.VB_Description = "Gets/sets whether to use the standard style colour for an element."
 On Error Resume Next
   UseStyleColor = mCommandBarColours.UseStyleColor(eColor)
End Property
Public Property Let UseStyleColor(ByVal eColor As ECustomColors, ByVal bState As Boolean)
  On Error Resume Next
   If Not (UseStyleColor(eColor) = bState) Then
      UseStyleColor(eColor) = bState
      fPaint
   End If
End Property

Public Property Get BackgroundImage() As IPicture
Attribute BackgroundImage.VB_Description = "Gets/sets a picture to tile behind this control's toolbar."
  On Error Resume Next
   If Not (m_cBack Is Nothing) Then
      Set BackgroundImage = m_cBack.Picture
   End If
End Property
Public Property Let BackgroundImage(pic As IPicture)
  On Error Resume Next
   pSetBackgroundImage pic
End Property
Public Property Set BackgroundImage(pic As IPicture)
  On Error Resume Next
   pSetBackgroundImage pic
End Property
Private Sub pSetBackgroundImage(pic As IPicture)
  On Error Resume Next
   If (pic Is Nothing) Then
      Set m_cBack = Nothing
   Else
      Set m_cBack = New cAlphaDIBSection
      m_cBack.CreateFromPicture pic
   End If
   fPaint
   
End Sub
Public Sub AdjustBackgroundImage( _
      Optional ByVal newHue As Long = -1, _
      Optional ByVal newSaturation As Long = -1, _
      Optional ByVal fLuminanceAdjustPercent As Single = 0 _
   )
Attribute AdjustBackgroundImage.VB_Description = "Adjusts the hue, luminance or saturation of the background image assigned to this toolbar."
   On Error Resume Next
   If Not (m_cBack Is Nothing) Then
      m_cBack.AdjustHLS newHue, newSaturation, fLuminanceAdjustPercent
      fPaint
   End If
End Sub

Public Function AdjustImage( _
      ByVal picIn As IPicture, _
      Optional ByVal newHue As Long = -1, _
      Optional ByVal newSaturation As Long = -1, _
      Optional ByVal fLuminanceAdjustPercent As Single = 0 _
   ) As IPicture
Attribute AdjustImage.VB_Description = "Adjusts the hue, luminance or saturation of a StdPicture object."
   On Error Resume Next
   Dim c As New cAlphaDIBSection
   c.CreateFromPicture picIn
   If (newHue > -1) And (newSaturation > -1) Then
      c.AdjustHLS newHue, newSaturation, fLuminanceAdjustPercent
   End If
   Set AdjustImage = c.Picture
End Function

Public Property Get MainMenu() As Boolean
Attribute MainMenu.VB_Description = "Gets/sets whether this control should be regarded as the main menu for its owner form."
  On Error Resume Next
   MainMenu = m_bMainMenu
End Property
Public Property Let MainMenu(ByVal bState As Boolean)
  On Error Resume Next
   If Not (m_bMainMenu = bState) Then
      m_bMainMenu = bState
      PropertyChanged "MainMenu"
   End If
End Property

Public Property Get ButtonTextPosition() As ECommandBarButtonTextPosition
Attribute ButtonTextPosition.VB_Description = "Gets/sets the position of the text in buttons shown in this toolbar."
  On Error Resume Next
   ButtonTextPosition = m_eButtonPosition
End Property
Public Property Let ButtonTextPosition(ByVal ePosition As ECommandBarButtonTextPosition)
  On Error Resume Next
   If Not (m_eButtonPosition = ePosition) Then
      m_eButtonPosition = ePosition
      fResize
      PropertyChanged "ButtonTextPosition"
   End If
End Property

Friend Property Get hWndParent() As Long
  On Error Resume Next
   Debug.Print "Parent:", Hex(m_hWndParent)
   hWndParent = m_hWndParent
End Property

Friend Function NewInstance() As vbalCommandBar
  On Error Resume Next
   '
   Dim ctl As vbalCommandBar
   RaiseEvent RequestNewInstance(ctl)
   Set NewInstance = ctl
   '
End Function

Friend Sub fSetAsMenu()
  On Error Resume Next
   m_bPopup = True
   If Not (m_cMenuImageList Is Nothing) Then
      m_cMenuImageList.DisabledColor = IconDisabledColor
   End If
   m_eOrientation = eTop
   Enabled = True
End Sub

Friend Property Get fIsSetAsMenu() As Boolean
  On Error Resume Next
   fIsSetAsMenu = m_bPopup
End Property

Friend Sub fShowMenuShadow()
  On Error Resume Next
Dim tR As RECT
   
   GetWindowRect m_hWnd, tR
   Set m_cRightShadow = New cMenuDropShadow
   With m_cRightShadow
      .ShadowType = ERightShadow
      .Initialise tR.right, tR.top, .ShadowSize, tR.bottom - tR.top, m_hWnd
      .ShadowColor = MenuShadowColor
      .Create
   End With
   
   Set m_cBottomShadow = New cMenuDropShadow
   With m_cBottomShadow
      .ShadowType = EBottomShadow
      .Initialise tR.left, tR.bottom, tR.right - tR.left, .ShadowSize, m_hWnd
      .ShadowColor = MenuShadowColor
      .Create
   End With
   
End Sub

Public Property Get hWnd() As Long
Attribute hWnd.VB_Description = "Gets the Window handle for this control."
On Error Resume Next
   hWnd = m_hWnd
End Property

Public Sub ClientCoordinatesToScreen( _
      ByRef xPixels As Long, _
      ByRef yPixels As Long, _
      Optional ByVal hWndClient As Long = 0 _
   )
Attribute ClientCoordinatesToScreen.VB_Description = "Converts x,y coordinates in pixels relative to a particular window to screen coordinates.  If no window is specified the control's window is used."
   On Error Resume Next
Dim tP As POINTAPI
   tP.x = xPixels
   tP.y = yPixels
   If (hWndClient = 0) Then
      hWndClient = m_hWnd
   End If
   ClientToScreen hWndClient, tP
   xPixels = tP.x
   yPixels = tP.y
End Sub

Public Sub ScreenCoordinatesToClient( _
      ByRef xPixels As Long, _
      ByRef yPixels As Long, _
      Optional ByVal hWndClient As Long = 0 _
   )
Attribute ScreenCoordinatesToClient.VB_Description = "Converts x,y coordinates in pixels relative to the screen to client coordinates in pixels.  If no window is specified the control's window is used."
   On Error Resume Next
Dim tP As POINTAPI
   tP.x = xPixels
   tP.y = yPixels
   If (hWndClient = 0) Then
      hWndClient = m_hWnd
   End If
   ScreenToClient hWndClient, tP
   xPixels = tP.x
   yPixels = tP.y
End Sub

Public Sub ShowPopupMenu( _
      ByVal x As Long, _
      ByVal y As Long, _
      ByVal Bar As cCommandBar _
   )
Attribute ShowPopupMenu.VB_Description = "Shows the specified CommandBar at the specified position. The position is in pixels, relative to the screen."
   On Error Resume Next
Dim barDropDownInt As cCommandBarInt
   
   Set barDropDownInt = mCommandBars.BarItem(Bar.Key)
   pShowDropDownBar barDropDownInt, 0, x, y, , True
   
End Sub

Public Sub ShowChevronMenu( _
      ByVal x As Long, _
      ByVal y As Long _
   )
Attribute ShowChevronMenu.VB_Description = "Shows the chevron menu for this toolbar at the specified location."
   On Error Resume Next
Dim myBar As cCommandBarInt
Dim iBtn As Long
Dim barWork As cCommandBarInt
Dim btnWork As cButtonInt
Dim btnEquiv As cButtonInt
Dim barChevron As cCommandBarInt
Dim barAddOrRemove As cCommandBarInt
Dim btnAddOrRemoveSep As cButtonInt

   If (Len(m_sToolBarKey) > 0) Then
      Set myBar = mCommandBars.BarItem(m_sToolBarKey)
      If Not (myBar Is Nothing) Then
   
         Set btnWork = mCommandBars.ButtonItem("CHEVRON:ADDORREMOVEBAR")
         btnWork.Caption = myBar.Title
         
         Set barChevron = mCommandBars.BarItem("CHEVRON")

         Set barAddOrRemove = mCommandBars.BarItem("CHEVRON:ADDORREMOVE")
         Set btnAddOrRemoveSep = mCommandBars.ButtonItem("CHEVRON:ADDORREMOVE:SEPARATOR")
         
         Do While barAddOrRemove.Count > 0
            If (barAddOrRemove.Item(1).Key = btnAddOrRemoveSep.Key) Then
               Exit Do
            Else
               barAddOrRemove.Remove barAddOrRemove.Item(1)
            End If
         Loop
         
         For iBtn = 1 To myBar.Count
            Set btnWork = myBar.Item(iBtn)
            If Not (btnWork.Style = eSeparator) Then
               btnWork.VisibleCheck = IIf(btnWork.Visible, vbChecked, vbUnchecked)
               barAddOrRemove.InsertBefore btnWork, btnAddOrRemoveSep
               If (m_item(iBtn).Hidden) Then
                  
               End If
            End If
         Next iBtn
               
         Dim ctlChevron As vbalCommandBar
         pShowDropDownBar barChevron, 0, x, y
         
      End If
   End If
End Sub

Public Property Get Orientation() As ECommandBarOrientation
Attribute Orientation.VB_Description = "Gets/sets the orientation to display this toolbar in."
On Error Resume Next
   Orientation = m_eOrientation
End Property
Public Property Let Orientation(ByVal eOrientation As ECommandBarOrientation)
On Error Resume Next
   If Not (eOrientation = m_eOrientation) Then
      If (m_bPopup) Then
         ' TODO possibly should raise an error here?
      Else
         m_eOrientation = eOrientation
      End If
      fResize
      fPaint
      PropertyChanged "Orientation"
   End If
End Property

Public Property Let ToolbarImageList( _
        ByRef vImageList As Variant _
    )
Attribute ToolbarImageList.VB_Description = "Associates an ImageList with the control.  The ImageList may either be a Microsoft Common Controls object or a handle to a ComCtl32 ImageList."
    On Error Resume Next
    m_cToolbarImageList.InitialiseFromVariant vImageList
    fResize
End Property
Public Property Let MenuImageList( _
      ByRef vImageList As Variant _
   )
Attribute MenuImageList.VB_Description = "Gets/sets the ImageList to be used for icons in drop-down menus shown from this toolbar."
   On Error Resume Next
   m_cMenuImageList.InitialiseFromVariant vImageList
   fResize
End Property

Friend Sub fSetImageListAndFont( _
      cToolbarImageList As cCommandBarImageList, _
      cMenuImageList As cCommandBarImageList, _
      fnt As IFont _
   )
   On Error Resume Next
   Font = fnt
   m_cToolbarImageList.InitialiseFromInstance cToolbarImageList
   m_cMenuImageList.InitialiseFromInstance cMenuImageList
   
End Sub
Public Property Get Redraw() As Boolean
Attribute Redraw.VB_Description = "Gets/sets whether the control redraws or not."
On Error Resume Next
   Redraw = m_bRedraw
End Property
Public Property Let Redraw(ByVal bState As Boolean)
On Error Resume Next
   If Not (m_bRedraw = bState) Then
      m_bRedraw = bState
      If (m_bRedraw) Then
         fPaint
      End If
      PropertyChanged "Redraw"
   End If
End Property
Public Property Get Font() As IFont
Attribute Font.VB_Description = "Gets/sets the font used to draw the control.  The font should be a True-Type font such as Tahoma or Arial."
On Error Resume Next
   Set Font = UserControl.Font
End Property
Public Property Let Font(ifnt As IFont)
On Error Resume Next
   Set UserControl.Font = ifnt
   fResize
   PropertyChanged "Font"
End Property
Public Property Set Font(ifnt As IFont)
On Error Resume Next
   Set UserControl.Font = ifnt
   fResize
   PropertyChanged "Font"
End Property

Public Property Get Enabled() As Boolean
Attribute Enabled.VB_Description = "Gets/sets whether the control is enabled."
On Error Resume Next
   Enabled = m_bEnabled
End Property
Public Property Let Enabled(ByVal bEnabled As Boolean)
On Error Resume Next
   m_bEnabled = bEnabled
   UserControl.Enabled = m_bEnabled
   PropertyChanged "Enabled"
End Property

Public Property Get HideInfrequentlyUsed() As Boolean
Attribute HideInfrequentlyUsed.VB_Description = "Gets/sets whether infrequently used menu items are hidden or not."
On Error Resume Next
   HideInfrequentlyUsed = mCommandBars.HideInfrequentlyUsed
End Property
Public Property Let HideInfrequentlyUsed(ByVal bState As Boolean)
On Error Resume Next
   mCommandBars.HideInfrequentlyUsed = bState
   PropertyChanged "HideInfrequentlyUsed"
End Property

Public Property Get Style() As EToolBarStyle
Attribute Style.VB_Description = "Gets/sets the global style used to render all toolbars and menus in this project."
On Error Resume Next
   Style = mCommandBarColours.Style
End Property
Public Property Let Style(ByVal eStyle As EToolBarStyle)
On Error Resume Next
   If Not (mCommandBarColours.Style = eStyle) Then
      mCommandBarColours.Style = eStyle
      PropertyChanged "Style"
   End If
End Property

Public Property Get IdealSize() As Long
Attribute IdealSize.VB_Description = "Gets the ideal size in pixels of this toolbar."
On Error Resume Next
   '
   '
End Property

Public Property Get Toolbar() As cCommandBar
Attribute Toolbar.VB_Description = "Gets/sets the CommandBar object to be displayed in this control instance."
On Error Resume Next
   If Len(m_sToolBarKey) > 0 Then
      Dim c As New cCommandBar
      c.fInit m_hWnd, m_sToolBarKey
      Set Toolbar = c
   End If
End Property
Public Property Let Toolbar(Bar As cCommandBar)
On Error Resume Next
   pSetToolbar Bar
End Property
Public Property Set Toolbar(Bar As cCommandBar)
On Error Resume Next
   pSetToolbar Bar
   
End Property
Private Sub pSetToolbar(Bar As cCommandBar)
On Error Resume Next
   fInUse = True
   
   Dim barInt As cCommandBarInt
   If Len(m_sToolBarKey) > 0 Then
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      If Not barInt Is Nothing Then
         barInt.ReleaseRef m_hWnd
      End If
   End If

   If (Bar Is Nothing) Then
      pUnSubclass
      m_sToolBarKey = ""
      fResize
      fPaint
   Else
      pSubClass
      m_sToolBarKey = Bar.Key
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      barInt.AddRefhWnd m_hWnd, m_hWndParent
      If Not (m_bPopup) Then
         Dim i As Long
         Dim ctl As Object
         For i = 1 To barInt.Count
            Set ctl = barInt.Item(i).PanelControl
            If Not ctl Is Nothing Then
               On Error Resume Next
               Set ctl.Container = UserControl.Extender
               On Error GoTo 0
            End If
         Next i
      End If
      m_bPopupVisibleChecks = (m_sToolBarKey = "CHEVRON:ADDORREMOVE")
      fResize
      fPaint
   End If
End Sub

Public Property Get CommandBars() As cCommandBars
Attribute CommandBars.VB_Description = "Gets the collection of all CommandBar objects.  CommandBar objects are global to the project, not a specific control."
On Error Resume Next
   Dim c As New cCommandBars
   c.fInit m_hWnd
   Set CommandBars = c
End Property
Public Property Get Buttons() As cButtons
Attribute Buttons.VB_Description = "Gets the collection of all Button objects.  Button objects are global to the project, not a specific control."
On Error Resume Next
   Dim c As New cButtons
   c.fInit m_hWnd
   Set Buttons = c
End Property


Friend Property Get Popup() As Boolean
On Error Resume Next
   Popup = m_bPopup
End Property

Friend Function BarCount() As Long
On Error Resume Next
   If Not (m_hWnd = 0) Then
      BarCount = mCommandBars.BarCount
   End If
End Function
Friend Function BarItem(ByVal index As Variant) As cCommandBar
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim barInt As cCommandBarInt
      Set barInt = mCommandBars.BarItem(index)
      If Not (barInt Is Nothing) Then
         Dim c As New cCommandBar
         c.fInit m_hWnd, barInt.Key
         Set BarItem = c
      End If
   End If
End Function
Friend Property Get BarTitle(ByVal sKey As String) As String
   On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim intBar As cCommandBarInt
      Set intBar = mCommandBars.BarItem(sKey)
      If Not (intBar Is Nothing) Then
         BarTitle = intBar.Title
      End If
   End If
End Property
Friend Property Let BarTitle(ByVal sKey As String, ByVal sTitle As String)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim intBar As cCommandBarInt
      Set intBar = mCommandBars.BarItem(sKey)
      If Not (intBar Is Nothing) Then
         intBar.Title = sTitle
      End If
   End If
End Property
Friend Sub BarRemove(ByVal sKey As String)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      mCommandBars.BarRemove sKey
   End If
End Sub
Friend Function BarAdd(ByVal sKey As String, ByVal sTitle As String) As cCommandBar
On Error Resume Next
Dim barInt As cCommandBarInt
   Set barInt = mCommandBars.BarAdd(sKey)
   If Not (barInt Is Nothing) Then
      If Len(sTitle) > 0 Then
         barInt.Title = sTitle
      End If
      Dim c As New cCommandBar
      c.fInit m_hWnd, sKey
      Set BarAdd = c
   End If
End Function

Friend Property Get BarButtonCount(ByVal sKey As String) As Long
On Error Resume Next
Dim barInt As cCommandBarInt
   Set barInt = mCommandBars.BarItem(sKey)
   If Not (barInt Is Nothing) Then
      BarButtonCount = barInt.Count
   End If
End Property
Friend Sub BarButtonClear(ByVal sKey As String)
On Error Resume Next
Dim barInt As cCommandBarInt
   Set barInt = mCommandBars.BarItem(sKey)
   If Not (barInt Is Nothing) Then
      barInt.Clear
   End If
End Sub
Friend Function BarButtonCollection(ByVal sKey As String) As cCommandBarButtons
On Error Resume Next
Dim barInt As cCommandBarInt
   Set barInt = mCommandBars.BarItem(sKey)
   If Not (barInt Is Nothing) Then
      Dim c As New cCommandBarButtons
      c.fInit m_hWnd, sKey
      Set BarButtonCollection = c
   End If
End Function
Friend Sub BarButtonRemove(ByVal sBarKey As String, ByVal sButtonKey As String)
On Error Resume Next
Dim barInt As cCommandBarInt
Dim btnInt As cButtonInt
   Set barInt = mCommandBars.BarItem(sBarKey)
   If Not (barInt Is Nothing) Then
      Set btnInt = mCommandBars.ButtonItem(sButtonKey)
      barInt.Remove btnInt
   End If
End Sub
Friend Sub BarButtonAdd(ByVal sBarKey As String, btn As cButton)
On Error Resume Next
Dim btnInt As cButtonInt
Dim barInt As cCommandBarInt
   Set barInt = mCommandBars.BarItem(sBarKey)
   If Not (barInt Is Nothing) Then
      Set btnInt = mCommandBars.ButtonItem(btn.Key)
      If Not (btnInt Is Nothing) Then
         barInt.Add btnInt
      End If
   End If
End Sub
Friend Sub BarButtonInsertAfter(ByVal sBarKey As String, btn As cButton, btnAfter As cButton)
On Error Resume Next
Dim btnInt As cButtonInt
Dim btnAfterInt As cButtonInt
Dim barInt As cCommandBarInt
   Set barInt = mCommandBars.BarItem(sBarKey)
   If Not (barInt Is Nothing) Then
      Set btnInt = mCommandBars.ButtonItem(btn.Key)
      If Not (btnInt Is Nothing) Then
         Set btnAfterInt = mCommandBars.ButtonItem(btnAfter.Key)
         If Not (btnAfterInt Is Nothing) Then
            barInt.InsertAfter btnInt, btnAfterInt
         End If
      End If
   End If
End Sub
Friend Sub BarButtonInsertBefore(ByVal sBarKey As String, btn As cButton, btnBefore As cButton)
On Error Resume Next
Dim btnInt As cButtonInt
Dim btnBeforeInt As cButtonInt
Dim barInt As cCommandBarInt
   Set barInt = mCommandBars.BarItem(sBarKey)
   If Not (barInt Is Nothing) Then
      Set btnInt = mCommandBars.ButtonItem(btn.Key)
      If Not (btnInt Is Nothing) Then
         Set btnBeforeInt = mCommandBars.ButtonItem(btnBefore.Key)
         If Not (btnBeforeInt Is Nothing) Then
            barInt.InsertBefore btnInt, btnBeforeInt
         End If
      End If
   End If
End Sub
Friend Property Get BarButton(ByVal sBarKey As String, ByVal index As Variant) As cButton
On Error Resume Next
Dim btnInt As cButtonInt
Dim barInt As cCommandBarInt
   Set barInt = mCommandBars.BarItem(sBarKey)
   If Not (barInt Is Nothing) Then
      Set btnInt = barInt.Item(index)
      If Not (btnInt Is Nothing) Then
         Dim c As New cButton
         c.fInit m_hWnd, btnInt.Key
         Set BarButton = c
      End If
   End If
End Property

Friend Function ButtonCount() As Long
On Error Resume Next
   If Not (m_hWnd = 0) Then
      ButtonCount = mCommandBars.ButtonCount
   End If
End Function
Friend Function ButtonIndex(btn As cButtonInt) As Long
On Error Resume Next
   If Not (m_hWnd = 0) Then
      If Len(m_sToolBarKey) > 0 Then
         Dim Bar As cCommandBarInt
         Set Bar = mCommandBars.BarItem(m_sToolBarKey)
         If Not (Bar Is Nothing) Then
            Dim i As Long
            For i = 1 To Bar.Count
               If (Bar.Item(i) Is btn) Then
                  ButtonIndex = i
                  Exit For
               End If
            Next i
         End If
      End If
   End If
End Function
Friend Function ButtonItem(ByVal index As Variant) As cButton
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(index)
      If Not (btnInt Is Nothing) Then
         Dim c As New cButton
         c.fInit m_hWnd, btnInt.Key
         Set ButtonItem = c
      End If
   End If
End Function
Friend Sub ButtonRemove(ByVal sKey As String)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      mCommandBars.ButtonRemove sKey
   End If
End Sub
Friend Function ButtonAdd( _
      ByVal sKey As String, _
      Optional ByVal iIcon As Long = -1, _
      Optional ByVal sCaption As String = "", _
      Optional ByVal eStyle As EButtonStyle = eNormal, _
      Optional ByVal sToolTip As String = "", _
      Optional ByVal vShortcutKey As Integer = 0, _
      Optional ByVal eShortcutModifier As ShiftConstants = vbCtrlMask _
   ) As cButton
   On Error Resume Next
Dim btnInt As cButtonInt
   Set btnInt = mCommandBars.ButtonAdd(sKey)
   If Not (btnInt Is Nothing) Then
      If (iIcon <> -1) Then
         btnInt.IconIndex = iIcon
      End If
      If Len(sCaption) > 0 Then
         btnInt.Caption = sCaption
      End If
      If (eStyle <> eNormal) Then
         btnInt.Style = eStyle
      End If
      btnInt.ShortcutKey = vShortcutKey
      btnInt.ShortcutModifiers = eShortcutModifier
      btnInt.ToolTip = sToolTip
      Dim c As New cButton
      c.fInit m_hWnd, sKey
      Set ButtonAdd = c
   End If
End Function
Friend Property Get ButtonCaption(ByVal sKey As String) As String
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonCaption = btnInt.Caption
      End If
   End If
End Property
Friend Property Let ButtonCaption(ByVal sKey As String, ByVal sCaption As String)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.Caption = sCaption
      End If
   End If
End Property
Friend Property Get ButtonShortcutKey(ByVal sKey As String) As Integer
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonShortcutKey = btnInt.ShortcutKey
      End If
   End If
End Property
Friend Property Let ButtonShortcutKey(ByVal sKey As String, ByVal vShortcutKey As Integer)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.ShortcutKey = vShortcutKey
      End If
   End If
End Property
Friend Property Get ButtonShortcutModifiers(ByVal sKey As String) As ShiftConstants
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonShortcutModifiers = btnInt.ShortcutModifiers
      End If
   End If
End Property
Friend Property Let ButtonShortcutModifiers(ByVal sKey As String, ByVal eShortcutModifiers As ShiftConstants)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.ShortcutModifiers = eShortcutModifiers
      End If
   End If
End Property
Friend Property Get ButtonToolTip(ByVal sKey As String) As String
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonToolTip = btnInt.ToolTip
      End If
   End If
End Property
Friend Property Let ButtonToolTip(ByVal sKey As String, ByVal sToolTip As String)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.ToolTip = sToolTip
      End If
   End If
End Property
Friend Property Get ButtonColourBox(ByVal sKey As String) As OLE_COLOR
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonColourBox = btnInt.colourBox
      End If
   End If
End Property
Friend Property Let ButtonColourBox(ByVal sKey As String, ByVal oColor As OLE_COLOR)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.colourBox = oColor
      End If
   End If
End Property
Friend Property Get ButtonIconIndex(ByVal sKey As String) As Long
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonIconIndex = btnInt.IconIndex
      End If
   End If
End Property
Friend Property Let ButtonIconIndex(ByVal sKey As String, ByVal lIconIndex As Long)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.IconIndex = lIconIndex
      End If
   End If
End Property
Friend Property Get ButtonPanelWidth(ByVal sKey As String) As Long
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonPanelWidth = btnInt.PanelWidth
      End If
   End If
End Property
Friend Property Let ButtonPanelWidth(ByVal sKey As String, ByVal lPanelWidth As Long)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.PanelWidth = lPanelWidth
      End If
   End If
End Property
Friend Property Get ButtonPanelControl(ByVal sKey As String) As Object
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         Set ButtonPanelControl = btnInt.PanelControl
      End If
   End If
End Property
Friend Property Let ButtonPanelControl(ByVal sKey As String, ctl As Object)
On Error Resume Next
   pSetButtonPanelControl sKey, ctl
End Property
Friend Property Set ButtonPanelControl(ByVal sKey As String, ctl As Object)
On Error Resume Next
   pSetButtonPanelControl sKey, ctl
End Property
Private Sub pSetButtonPanelControl(ByVal sKey As String, ctl As Object)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      If Not (m_bPopup) Then
         Dim btnInt As cButtonInt
         Set btnInt = mCommandBars.ButtonItem(sKey)
         If Not (btnInt Is Nothing) Then
            Dim ctlPrev As Object
            Set ctlPrev = btnInt.PanelControl
            If Not (ctlPrev Is Nothing) Then
               On Error Resume Next
               ctlPrev.Visible = False
               On Error GoTo 0
            End If
            If (ctl Is Nothing) Then
               Set btnInt.PanelControl = Nothing
            Else
               On Error Resume Next
               Set ctl.Container = UserControl.Extender
               ctl.Visible = False
               On Error GoTo 0
               Set btnInt.PanelControl = ctl
            End If
         End If
      End If
   End If

End Sub
Friend Property Get ButtonEnabled(ByVal sKey As String) As Boolean
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonEnabled = btnInt.Enabled
      End If
   End If
End Property
Friend Property Let ButtonEnabled(ByVal sKey As String, ByVal bEnabled As Boolean)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.Enabled = bEnabled
      End If
   End If
End Property
Friend Property Get ButtonLocked(ByVal sKey As String) As Boolean
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonLocked = btnInt.Locked
      End If
   End If
End Property
Friend Property Let ButtonLocked(ByVal sKey As String, ByVal bLocked As Boolean)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.Locked = bLocked
      End If
   End If
End Property
Friend Property Get ButtonVisible(ByVal sKey As String) As Boolean
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonVisible = btnInt.Visible
      End If
   End If
End Property
Friend Property Let ButtonVisible(ByVal sKey As String, ByVal bVisible As Boolean)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.Visible = bVisible
      End If
   End If
End Property
Friend Property Get ButtonChecked(ByVal sKey As String) As Boolean
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonChecked = btnInt.Checked
      End If
   End If
End Property
Friend Property Let ButtonChecked(ByVal sKey As String, ByVal bChecked As Boolean)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.Checked = bChecked
      End If
   End If
End Property
Friend Property Get ButtonShowCaptionInToolbar(ByVal sKey As String) As Boolean
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonShowCaptionInToolbar = btnInt.ShowCaptionInToolbar
      End If
   End If
End Property
Friend Property Let ButtonShowCaptionInToolbar(ByVal sKey As String, ByVal bShowCaptionInToolbar As Boolean)
   On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.ShowCaptionInToolbar = bShowCaptionInToolbar
      End If
   End If
End Property
Friend Property Get ButtonShowDropDownInToolbar(ByVal sKey As String) As Boolean
   On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonShowDropDownInToolbar = btnInt.ShowDropDownInToolbar
      End If
   End If
End Property
Friend Property Let ButtonShowDropDownInToolbar(ByVal sKey As String, ByVal bShowDropDownInToolbar As Boolean)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.ShowDropDownInToolbar = bShowDropDownInToolbar
      End If
   End If
End Property
Friend Property Get ButtonStyle(ByVal sKey As String) As EButtonStyle
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         ButtonStyle = btnInt.Style
      End If
   End If
End Property
Friend Property Let ButtonStyle(ByVal sKey As String, ByVal eStyle As EButtonStyle)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         btnInt.Style = eStyle
      End If
   End If
End Property
Friend Property Get ButtonBar(ByVal sKey As String) As cCommandBar
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         Dim barInt As cCommandBarInt
         Set barInt = btnInt.Bar
         If Not (barInt Is Nothing) Then
            Dim c As New cCommandBar
            c.fInit m_hWnd, barInt.Key
            Set ButtonBar = c
         End If
      End If
   End If
End Property
Friend Sub ButtonSetBar(ByVal sKey As String, cmdBar As cCommandBar)
On Error Resume Next
   If Not (m_hWnd = 0) Then
      Dim btnInt As cButtonInt
      Dim barInt As cCommandBarInt
      Set btnInt = mCommandBars.ButtonItem(sKey)
      If Not (btnInt Is Nothing) Then
         Set barInt = mCommandBars.BarItem(cmdBar.Key)
         If Not (barInt Is Nothing) Then
            btnInt.SetBar barInt
         End If
      End If
   End If
End Sub

Friend Sub ChangeNotification(Bar As cCommandBarInt, ByVal eventType As Long, itm As cButtonInt)
   '
   On Error Resume Next
   If StrComp(Bar.Key, m_sToolBarKey) = 0 Then
      If (eventType = CHANGENOTIFICATIONBARCONTENTCHANGE) Or _
         (eventType = CHANGENOTIFICATIONBUTTONSIZECHANGE) Then
         fResize
         If (m_bPopup) Then
            fShowMenuShadow
         End If
         
      End If
      If Not (itm Is Nothing) And _
         (eventType = CHANGENOTIFICATIONBUTTONREDRAW) Or _
         (eventType = CHANGENOTIFICATIONBUTTONCHECKCHANGE) Then
         If (m_bWrappable Or m_bMainMenu) Then
            fPaint
         Else
            fPaintOneButton Bar.IndexOf(itm.Key)
         End If
      Else
         fPaint
      End If
   End If
   '
End Sub

Private Sub prepareDisplayItemArray(barInt As cCommandBarInt)
On Error Resume Next
   If (barInt.Count > 0) Then
      Dim i As Long
      ReDim Preserve m_item(1 To barInt.Count) As cDisplayButtonInfo
      For i = 1 To barInt.Count
         Set m_item(i) = New cDisplayButtonInfo
      Next i
   End If
End Sub

Friend Sub fResize()
   '
   On Error Resume Next
   
   Erase m_item
   
   If Len(m_sToolBarKey) > 0 Then
      
      Dim barInt As cCommandBarInt
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      
      If Not (barInt Is Nothing) Then
         
         prepareDisplayItemArray barInt
         
         Dim tR As RECT
         GetClientRect m_hWnd, tR
   
         Dim cMP As New cMeasureButtonParams
         With cMP
            .FontFace = Me.Font.Name
            .FontSize = Me.Font.Size
            .hdc = UserControl.hdc
            .Height = tR.bottom - tR.top
            .hFont = plGetHFont()
            .hWnd = m_hWnd
            .RightToLeft = pbRightToLeft()
            .Size = tR.right - tR.left
         End With
   
         If (m_bPopup) Then
            '
            cMP.Orientation = eTop
            cMP.ButtonPosition = m_eButtonPosition
            If m_bPopupVisibleChecks Then
               cMP.SizeStyle = COMMANDBARSIZESTYLEMENUVISIBLECHECK
            Else
               cMP.SizeStyle = COMMANDBARSIZESTYLEMENU
            End If            '
            cMP.Size = cMP.Size - 4
            cMP.IconWidth = m_cMenuImageList.IconWidth
            If (cMP.IconWidth = 0) Then cMP.IconWidth = 8
            cMP.IconHeight = m_cMenuImageList.IconHeight
            If (cMP.IconHeight = 0) Then cMP.IconHeight = 8
            
            ' we need to calculate the required width & height
            ' of the control:
            Dim menuWidth As Long
            Dim menuHeight As Long
            If (barInt.Count > 0) Then
               barInt.CalculateMenuSize cMP, menuWidth, menuHeight, m_item, (cMP.SizeStyle = COMMANDBARSIZESTYLEMENUVISIBLECHECK)
               GetWindowRect m_hWnd, tR
               If (tR.right - tR.left = menuWidth) And (tR.bottom - tR.top = menuHeight) Then
               Else
                  On Error Resume Next
                  UserControl.Extender.Width = UserControl.Extender.Container.ScaleX(menuWidth, vbPixels, UserControl.Extender.Container.ScaleMode)
                  UserControl.Extender.Height = UserControl.Extender.Container.ScaleX(menuHeight + 2, vbPixels, UserControl.Extender.Container.ScaleMode)
               End If
            End If
            
            '
         Else
            '
            Dim toolbarWidth As Long
            Dim toolbarHeight As Long
            
            toolbarWidth = tR.right - tR.left
            toolbarHeight = tR.bottom - tR.top
            cMP.Orientation = m_eOrientation
            cMP.ButtonPosition = m_eButtonPosition
            cMP.IconHeight = m_cToolbarImageList.IconHeight
            cMP.IconWidth = m_cToolbarImageList.IconWidth
            
            ' the calculation result depends on whether we're wrappable or not
            If (m_bWrappable) Then
               '
               
               cMP.SizeStyle = COMMANDBARSIZESTYLETOOLBARWRAPPABLE
               
               ' Given the current width, what height do we need
               ' to be?
            
            Else
               '
               If (m_bMainMenu) Then
                  cMP.SizeStyle = COMMANDBARSIZESTYLETOOLBARMENU
               Else
                  cMP.SizeStyle = COMMANDBARSIZESTYLETOOLBAR
               End If
               
               If (barInt.Count > 0) Then
               
                  barInt.CalculateToolbarSize cMP, toolbarWidth, toolbarHeight, m_item
                  
                  GetWindowRect m_hWnd, tR
                  If (m_eOrientation = eLeft) Or (m_eOrientation = eRight) Then
                     If Not ((tR.right - tR.left) = toolbarHeight) Then
                        On Error Resume Next
                        UserControl.Extender.Width = UserControl.Extender.Container.ScaleX(toolbarHeight, vbPixels, UserControl.Extender.Container.ScaleMode)
                     End If
                  Else
                     If Not ((tR.bottom - tR.top) = toolbarHeight) Then
                        On Error Resume Next
                        UserControl.Extender.Height = UserControl.Extender.Container.ScaleY(toolbarHeight, vbPixels, UserControl.Extender.Container.ScaleMode)
                     End If
                  End If
               End If
            
            End If
            
            '
         End If
         
         fPaint
         
      End If
   End If
   '
End Sub
Friend Sub fPaintStyleChanged()
   On Error Resume Next
   If (m_bPopup) Then
      m_cMenuImageList.DisabledColor = IconDisabledColor
   Else
      m_cToolbarImageList.DisabledColor = ButtonTextDisabledColor
      m_cToolbarImageList.HighlightColor = ButtonTextHotColor
   End If
   fPaint
   
End Sub

Friend Sub fPaint()
   '
   On Error Resume Next
   If (m_bRedraw And m_bVisible) Then
   
      Dim lHDC As Long
      Dim tR As RECT
      
      lHDC = UserControl.hdc
      GetClientRect m_hWnd, tR
      
      If (m_bPopup) Then
      
         pPaintMenuBackground lHDC, tR.left, tR.top, tR.right, tR.bottom, False
         
      Else
         ' paint the background to the bar:
         If Not (m_cBack Is Nothing) Then
            TileArea lHDC, tR.left, tR.top, tR.right - tR.left, tR.bottom - tR.top, m_cBack.hdc, m_cBack.Width, m_cBack.Height, 0
         Else
            If (m_bMainMenu) Or (GradientColorStart = CLR_NONE) Then
               UtilDrawBackgroundPortion m_hWnd, m_hWndParent, lHDC, _
                  GradientColorEnd, GradientColorStart, _
                  tR.left, tR.top, tR.right - tR.left, tR.bottom - tR.top, _
                  True, (Style = eComCtl32)
            Else
               UtilDrawBackground lHDC, _
                  GradientColorStart, GradientColorEnd, _
                  tR.left, tR.top, tR.right - tR.left, tR.bottom - tR.top, _
                  ((m_eOrientation = eRight) Or (m_eOrientation = eLeft))
            End If
         End If
      End If
            
      ' ask the bar to render itself, if any:
      If Len(m_sToolBarKey) > 0 Then
         
         Dim barInt As cCommandBarInt
         Set barInt = mCommandBars.BarItem(m_sToolBarKey)
         If Not (barInt Is Nothing) Then
            
            Dim cDP As New cDrawButtonParams
            
            cDP.hWnd = m_hWnd
            cDP.hdc = lHDC
            cDP.FontFace = UserControl.Font.Name
            cDP.FontSize = UserControl.Font.Size
            cDP.hFont = plGetHFont()
            cDP.Enabled = m_bEnabled
            cDP.RightToLeft = pbRightToLeft()
            cDP.ButtonPosition = m_eButtonPosition
            
            If (m_bPopup) Then
               Set cDP.ImageList = m_cMenuImageList
               cDP.Orientation = eTop
               If (m_bPopupVisibleChecks) Then
                  cDP.SizeStyle = COMMANDBARSIZESTYLEMENUVISIBLECHECK
               Else
                  cDP.SizeStyle = COMMANDBARSIZESTYLEMENU
               End If
            Else
               Set cDP.ImageList = m_cToolbarImageList
               cDP.Orientation = m_eOrientation
               If (cDP.Orientation = eLeft) Or (cDP.Orientation = eRight) Then
                  cDP.ToolbarSize = tR.right - tR.left
               Else
                  cDP.ToolbarSize = tR.bottom - tR.top
               End If
               If (m_bWrappable) Then
                  cDP.SizeStyle = COMMANDBARSIZESTYLETOOLBARWRAPPABLE
               ElseIf (m_bMainMenu) Then
                  cDP.SizeStyle = COMMANDBARSIZESTYLETOOLBARMENU
               Else
                  cDP.SizeStyle = COMMANDBARSIZESTYLETOOLBAR
               End If
            End If
            barInt.Draw cDP, m_item
            
         End If
      End If
      
      UserControl.Refresh
   
   End If
   '
End Sub

Private Sub pPaintMenuBackground( _
      ByVal lHDC As Long, _
      ByVal lLeft As Long, ByVal lTop As Long, _
      ByVal lRight As Long, ByVal lBottom As Long, _
      ByVal bForOneItem As Boolean _
   )
   On Error Resume Next
   Dim lSideBarWidth As Long
   lSideBarWidth = m_cMenuImageList.IconWidth + 8
   
   If (m_bPopupVisibleChecks) Then
      ' Add the extra side bar width
      lSideBarWidth = lSideBarWidth + m_cMenuImageList.IconWidth + 2
   End If
   
   UtilDrawBackground lHDC, _
      MenuBackgroundColorStart, MenuBackgroundColorEnd, _
      lLeft, lTop, lRight - lLeft, lBottom - lTop
      
   Dim tR As RECT
   GetClientRect m_hWnd, tR
   UtilDrawBorderRectangle lHDC, MenuBorderColor, _
      tR.left, tR.top, tR.right - tR.left, tR.bottom - tR.top, False
   
   Dim tPStart As POINTAPI
   Dim tPEnd As POINTAPI
   Dim hPen As Long
   Select Case m_eMenuPopoutDirection
   Case eLeft, eRight
      tPStart.y = m_lMenuPopoutStart + 1
      ScreenToClient m_hWnd, tPStart
      tPEnd.y = m_lMenuPopoutStart + m_lMenuPopoutExtent - 1
      ScreenToClient m_hWnd, tPEnd
      If (m_eMenuPopoutDirection = eLeft) Then
         tPStart.x = tR.left
         tPEnd.x = tR.left
      Else
         tPStart.x = tR.right - 1
         tPEnd.x = tR.right - 1
      End If
      hPen = CreatePen(PS_SOLID, 1, MenuBackgroundColorStart)
   Case eTop, eBottom
      tPStart.x = m_lMenuPopoutStart + 1
      ScreenToClient m_hWnd, tPStart
      tPEnd.x = m_lMenuPopoutStart + m_lMenuPopoutExtent - 1
      ScreenToClient m_hWnd, tPEnd
      If (m_eMenuPopoutDirection = eTop) Then
         tPStart.y = tR.top
         tPEnd.y = tR.top
      Else
         tPStart.y = tR.bottom - 1
         tPEnd.y = tR.bottom - 1
      End If
      hPen = CreatePen(PS_SOLID, 1, MenuBackgroundColorStart)
   End Select
   
   If Not (hPen = 0) Then
      Dim hPenOld As Long
      Dim tJunk As POINTAPI
      hPenOld = SelectObject(lHDC, hPen)
      MoveToEx lHDC, tPStart.x, tPStart.y, tJunk
      LineTo lHDC, tPEnd.x, tPEnd.y
      SelectObject lHDC, hPenOld
      DeleteObject hPen
   End If
   
   lLeft = lLeft + 1
   lRight = lRight - 2
   If Not (bForOneItem) Then
      lTop = lTop + 2
      lBottom = lBottom - 2
   ElseIf lTop = 1 Then
      lTop = lTop + 1
   End If
      
   ' paint the side bar:
   If (pbRightToLeft()) Then
      UtilDrawBackground lHDC, _
         GradientColorStart, GradientColorEnd, _
         lRight - lSideBarWidth, lTop, lSideBarWidth, lBottom - lTop, _
         True
   Else
      UtilDrawBackground lHDC, _
         GradientColorStart, GradientColorEnd, _
         lLeft, lTop, lSideBarWidth, lBottom - lTop, _
         True
   End If

End Sub

Friend Sub fPaintOneButton(ByVal lIndex As Long)
   '
   On Error Resume Next
   If (m_bRedraw And m_bVisible) Then
         
      ' the bar:
      If Len(m_sToolBarKey) > 0 Then
         
         Dim barInt As cCommandBarInt
         Set barInt = mCommandBars.BarItem(m_sToolBarKey)
         If Not (barInt Is Nothing) Then
      
            Dim lHDC As Long
            lHDC = UserControl.hdc
            Dim tR As RECT
            GetClientRect m_hWnd, tR
                     
            ' Set up to draw the button:
            Dim cDP As New cDrawButtonParams
               
            cDP.hWnd = m_hWnd
            cDP.hdc = lHDC
            cDP.FontFace = UserControl.Font.Name
            cDP.FontSize = UserControl.Font.Size
            cDP.hFont = plGetHFont()
            cDP.Enabled = m_bEnabled
            cDP.RightToLeft = pbRightToLeft()
            If (m_bPopup) Then
               Set cDP.ImageList = m_cMenuImageList
               cDP.Orientation = eTop
               If (m_bPopupVisibleChecks) Then
                  cDP.SizeStyle = COMMANDBARSIZESTYLEMENUVISIBLECHECK
               Else
                  cDP.SizeStyle = COMMANDBARSIZESTYLEMENU
               End If
            Else
               Set cDP.ImageList = m_cToolbarImageList
               cDP.Orientation = m_eOrientation
               If (m_bWrappable) Then
                  cDP.SizeStyle = COMMANDBARSIZESTYLETOOLBARWRAPPABLE
               Else
                  cDP.SizeStyle = COMMANDBARSIZESTYLETOOLBAR
               End If
            End If
            cDP.left = m_item(lIndex).left
            cDP.top = m_item(lIndex).top
            cDP.Size = m_item(lIndex).right - m_item(lIndex).left
            cDP.Height = m_item(lIndex).bottom - m_item(lIndex).top
            cDP.MouseDownButton = m_item(lIndex).mouseDown
            cDP.MouseOverButton = m_item(lIndex).mouseOver
            cDP.MouseDownSplit = m_item(lIndex).MouseDownSplit
            cDP.MouseOverSplit = m_item(lIndex).MouseOverSplit
            cDP.ShowingMenu = m_item(lIndex).ShowingMenu
            cDP.Hidden = m_item(lIndex).Hidden
            cDP.ButtonPosition = m_eButtonPosition
            If (m_eOrientation = eLeft) Or (m_eOrientation = eRight) Then
               cDP.ToolbarSize = tR.right - tR.left
            Else
               cDP.ToolbarSize = tR.bottom - tR.top
            End If
   
            ' paint the background to the item:
            If (cDP.SizeStyle = COMMANDBARSIZESTYLETOOLBARWRAPPABLE) Or _
               (cDP.SizeStyle = COMMANDBARSIZESTYLETOOLBAR) Then
               Dim lBackWidth As Long
               Dim lBackHeight As Long
               Dim lBackLeft As Long
               Dim lBackTop As Long
               lBackWidth = m_item(lIndex).right - m_item(lIndex).left
               lBackHeight = m_item(lIndex).bottom - m_item(lIndex).top
               lBackLeft = m_item(lIndex).left
               lBackTop = m_item(lIndex).top
               GetClientRect m_hWnd, tR
               If (m_eOrientation = eLeft) Or (m_eOrientation = eRight) Then
                  lBackLeft = 0
                  lBackWidth = tR.right - tR.left
               Else
                  lBackHeight = tR.bottom - tR.top
               End If
               If Not (m_cBack Is Nothing) Then
                  TileArea lHDC, lBackLeft, lBackTop, lBackWidth, lBackHeight, _
                     m_cBack.hdc, m_cBack.Width, m_cBack.Height, 0
               Else
                  If (m_bMainMenu) Or (GradientColorStart = CLR_NONE) Then
                     UtilDrawBackgroundPortion m_hWnd, m_hWndParent, lHDC, _
                        GradientColorStart, GradientColorEnd, _
                        lBackLeft, lBackTop, _
                        lBackWidth, lBackHeight, _
                        True, (Style = eComCtl32)
                  Else
                     UtilDrawBackground lHDC, GradientColorStart, GradientColorEnd, _
                        lBackLeft, lBackTop, _
                        lBackWidth, lBackHeight, _
                        ((m_eOrientation = eLeft) Or (m_eOrientation = eRight))
                  End If
               End If
            Else
               Dim lTop As Long
               lTop = m_item(lIndex).top
               If (lIndex = 1) Then lTop = lTop + 1 ' comment-on-dit 'hax0r'?
               If m_item(lIndex).left <= 2 Then
                  pPaintMenuBackground lHDC, _
                     m_item(lIndex).left, lTop, _
                     m_item(lIndex).right, m_item(lIndex).bottom, _
                     True
               Else
                  UtilDrawBackground lHDC, MenuBackgroundColorStart, MenuBackgroundColorEnd, _
                     m_item(lIndex).left, lTop, _
                     m_item(lIndex).right - m_item(lIndex).left, m_item(lIndex).bottom - m_item(lIndex).top, _
                     True
               End If
            End If
   
            barInt.DrawOneButton cDP, lIndex
      
         End If
         
      End If
   
      UserControl.Refresh
   
   End If
   '

End Sub

Friend Function fHitTest(ByVal x As Long, ByVal y As Long) As Long
On Error Resume Next
Dim i As Long
   If Len(m_sToolBarKey) > 0 Then
      Dim barInt As cCommandBarInt
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      If Not (barInt Is Nothing) Then
         For i = 1 To barInt.Count
            If (x >= m_item(i).left) And (x <= m_item(i).right) Then
               If (y >= m_item(i).top And y <= m_item(i).bottom) Then
                  fHitTest = i
                  Exit For
               End If
            End If
         Next i
      End If
   End If
End Function

Friend Function fTrack( _
      ByVal button As MouseButtonConstants, _
      ByVal iIndex As Long, _
      Optional ByVal mouseDown As Boolean = False, _
      Optional ByVal fromKey As Boolean = False _
   ) As Long
   On Error Resume Next
   Dim sToolTip As String
   
   If Len(m_sToolBarKey) > 0 Then
   
      Dim barInt As cCommandBarInt
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      
      If Not (barInt Is Nothing) Then
         
         Dim i As Long
         Dim j As Long
         Dim changeCount As Long
         Dim changeIndex() As Long
         Dim track As Boolean
         Dim found As Long
         Dim tP As POINTAPI
         Dim xOffset As Long
         Dim yOffset As Long
         Dim addChange As Boolean
         Dim indexMouseOver As Long

         For i = 1 To barInt.Count
            addChange = False
            
            If (i = iIndex) Then
               
               sToolTip = barInt.Item(i).TooltipText(True)
               
               'If (barInt.Item(i).CanAction(m_eOrientation,m_bPopup, m_bPopupVisibleChecks)) Then
                                    
                  If (barInt.Item(i).Style = eSplit) Then
                     ' check if we're over split or not:
                     GetCursorPos tP
                     LSet m_tLastMousePos = tP
                     ScreenToClient m_hWnd, tP
                     xOffset = tP.x - m_item(i).left
                     yOffset = tP.y - m_item(i).top
                     Dim OverSplit As Boolean
                     OverSplit = barInt.Item(i).OverSplit( _
                        xOffset, yOffset, _
                        m_item(i).right - m_item(i).left, m_item(i).bottom - m_item(i).top, _
                        pbRightToLeft(), m_eOrientation)
                     If (OverSplit) Then
                        If Not (m_item(i).MouseOverSplit) Then
                           If (button = vbLeftButton) Then
                              m_item(i).MouseOverSplit = m_item(i).MouseDownSplit
                           Else
                              m_item(i).MouseOverSplit = True
                           End If
                           m_item(i).mouseOver = False
                           addChange = True
                        End If
                     Else
                        If Not (m_item(i).mouseOver) Then
                           If (button = vbLeftButton) Then
                              m_item(i).mouseOver = m_item(i).mouseDown
                           Else
                              m_item(i).mouseOver = True
                           End If
                           m_item(i).MouseOverSplit = False
                           addChange = True
                        End If
                     End If
                  Else
                     If Not (m_item(i).mouseOver) Then
                        If (button = vbLeftButton) Then
                           m_item(i).mouseOver = m_item(i).mouseDown
                        Else
                           m_item(i).mouseOver = True
                        End If
                        addChange = True
                     End If
                  End If
                  
                  If (addChange) Then
                     track = True
                  End If
                                 
                  If mouseDown Then
                     track = False
                     If (barInt.Item(i).Style = eSplit) Then
                        If (OverSplit) Then
                           If Not (m_item(i).MouseDownSplit) Then
                              m_item(i).MouseDownSplit = True
                              m_item(i).mouseDown = False
                              m_item(i).MouseOverSplit = True
                              m_item(i).mouseOver = False
                              addChange = True
                           End If
                        Else
                           If Not (m_item(i).mouseDown) Then
                              m_item(i).mouseDown = True
                              m_item(i).MouseDownSplit = False
                              m_item(i).mouseOver = True
                              m_item(i).MouseOverSplit = False
                              addChange = True
                           End If
                        End If
                     Else
                        If Not (m_item(i).mouseDown) Then
                           m_item(i).mouseDown = True
                           addChange = True
                        End If
                     End If
                  End If
               
                  If (addChange) Then
                     changeCount = changeCount + 1
                     ReDim Preserve changeIndex(1 To changeCount) As Long
                     changeIndex(changeCount) = iIndex
                  End If
                  
               'End If
            
            Else
            
               If (m_item(i).mouseOver) Or (m_item(i).MouseOverSplit) Then
                  m_item(i).mouseOver = False
                  m_item(i).MouseOverSplit = False
                  changeCount = changeCount + 1
                  ReDim Preserve changeIndex(1 To changeCount) As Long
                  changeIndex(changeCount) = i
               End If
               
            End If
            
         Next i
         
         If (changeCount > 0) Then
            For i = 1 To changeCount
               If Not (m_bWrappable Or m_bMainMenu) Then
                  fPaintOneButton changeIndex(i)
               End If
               If (m_item(changeIndex(i)).mouseOver) Or (m_item(changeIndex(i)).MouseOverSplit) Then
                  indexMouseOver = changeIndex(i)
               End If
            Next i
            If (m_bWrappable Or m_bMainMenu) Then
               fPaint
            End If
            If InMenuLoop Then
               If (indexMouseOver > 0) Then
                  If Not fromKey Then
                     processMenuMouseOver indexMouseOver
                     If (m_bPopup) Then
                        ActiveMenu = m_hWnd
                     End If
                  End If
               End If
            End If
         End If
         
         If (track And Not (fromKey)) Then
            tmrLostMouse.Enabled = True
         End If
      End If
            
   End If
   
   If Not (StrComp(sToolTip, m_sLastToolTip) = 0) Then
      On Error Resume Next
      UserControl.Extender.TooltipText = sToolTip
      m_sLastToolTip = sToolTip
   End If
   
End Function

Friend Sub fKeyDown(ByVal vKey As Long, ByVal shift As Long)
On Error GoTo ErrorHandler

Dim ctl As vbalCommandBar
Dim barInt As cCommandBarInt
Dim i As Long
Dim iSelected As Long

   '
   If Len(m_sToolBarKey) > 0 Then

      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      If Not (barInt Is Nothing) Then

         For i = 1 To barInt.Count
            If (m_item(i).mouseOver) Or (m_item(i).ShowingMenu) Then
               iSelected = i
               Exit For
            End If
         Next i

         If (InMenuLoop) Then
            If (m_bPopup) Then
               
               ActiveMenu = m_hWnd
               
               Select Case vKey
               Case vbKeyLeft
                  ' next menu to the left
                  pSelectNextMenu -1
                  
               Case vbKeyRight
                  ' next menu to the right
                  pSelectNextMenu 1
                  
               Case vbKeyUp
                  ' select next item upwards, wrapping at the end
                  pSelectMenuItem -1
                  
               Case vbKeyDown
                  ' select next item downwards, wrapping at the end
                  pSelectMenuItem 1
                  
               Case vbKeyHome
                  ' select the first item
                  pSelectMenuItem 0, True, False
               
               Case vbKeyEnd
                  ' select the last item
                  pSelectMenuItem 0, False, True
               
               Case vbKeyEscape
                  ' Cancel menu
                  If (ControlFromhWnd(m_hWndShownFrom, ctl)) Then
                     ctl.fCloseMenus False
                     If (m_bPopup) Then
                        m_eMenuTrackMode = 0
                     End If
                  End If
                  
               Case vbKeyReturn
                  ' Select item
                  If (iSelected > 0) Then
                     If (barInt.Item(iSelected).Bar Is Nothing) Then
                        fClickButton iSelected
                     Else
                        pShowDropDown iSelected, True
                     End If
                  End If
               
               Case Else
                  ' See if the key matches any of the
                  ' accelerators in this control, if so
                  ' select it
                  For i = 1 To barInt.Count
                     If (barInt.Item(i).AltKeyMatches(vKey)) Then
                        If (barInt.Item(i).Bar Is Nothing) Then
                           fClickButton i
                        Else
                           pShowDropDown i, True
                        End If
                        Exit For
                     End If
                  Next i
               
               
               End Select
               
            Else
               If (m_eOrientation = eLeft) Or (m_eOrientation = eRight) Then
                  Select Case vKey
                  Case vbKeyLeft
                     vKey = vbKeyUp
                  Case vbKeyRight
                     vKey = vbKeyDown
                  Case vbKeyUp
                     vKey = vbKeyLeft
                  Case vbKeyDown
                     vKey = vbKeyRight
                  End Select
               End If
            
               Select Case vKey
               Case vbKeyLeft
                  pSelectMenuItem -1
                  
               Case vbKeyRight
                  pSelectMenuItem 1
                  
               Case vbKeyDown, vbKeyUp
                  pShowDropDown iSelected, True
               
               Case vbKeyEscape
                  SetInMenuLoop False, 0
                  pMouseMove 0, 0
               
               Case Else
                  ' See if the key matches any of the
                  ' accelerators in this control, if so
                  ' call pShowDropDown
                  For i = 1 To barInt.Count
                     If (barInt.Item(i).AltKeyMatches(vKey)) Then
                        pShowDropDown i, True
                        Exit For
                     End If
                  Next i
                  
               End Select
            
            End If
         End If
         
      End If
   End If
   '
   Exit Sub
   
ErrorHandler:
   Debug.Print "Error in fKeyDown!!"
   Exit Sub
   
End Sub

Private Sub pSelectNextMenu( _
      ByVal iDir As Long _
   )
   On Error Resume Next
Dim iCurrentSelection As Long
Dim bCurrentSelectionHasPopup As Boolean
Dim bCurrentSelectionShowingPopup As Boolean
Dim iIndex As Long
Dim iNextSelection As Long
Dim ctl As vbalCommandBar
   
   If Len(m_sToolBarKey) > 0 Then
      Dim barInt As cCommandBarInt
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      If Not (barInt Is Nothing) Then
         If (barInt.Count > 0) Then

            For iIndex = 1 To barInt.Count
               If (m_item(iIndex).mouseOver Or m_item(iIndex).ShowingMenu) Then
                  iCurrentSelection = iIndex
                  If Not (barInt.Item(iCurrentSelection).Bar Is Nothing) Then
                     bCurrentSelectionHasPopup = True
                     bCurrentSelectionShowingPopup = m_item(iCurrentSelection).ShowingMenu
                  End If
                  Exit For
               End If
            Next iIndex

            
            If (iDir = 1) Then
               ' check if there is a submenu for the currently selected item:
               If (bCurrentSelectionHasPopup) Then
                  ' Show that item
                  pShowDropDown iCurrentSelection, True
                  ' & exit
                  Exit Sub
               End If
            End If
            
            iNextSelection = iCurrentSelection + iDir
            If (iNextSelection < 1) Then
               iNextSelection = barInt.Count
            ElseIf (iNextSelection > barInt.Count) Then
               iNextSelection = 1
            End If
                        
            fKeyDown vbKeyEscape, 0
                        
            If (ControlFromhWnd(ActiveMenu, ctl)) Then
               If Not (ctl.fIsSetAsMenu) Then
                  ctl.fSelectNextMenu iDir
               ElseIf (iDir = 1) Then
                  If (ControlFromhWnd(menuInitiator, ctl)) Then
                     ctl.fSelectNextMenu iDir
                  End If
               End If
         '      If ctl.fIsSetAsMenu Then
         '         ctl.fKeyDown IIf(iDir = -1, vbKeyLeft, vbKeyRight), 0
            End If
            
         End If
      End If
   End If
End Sub

Friend Sub fSelectNextMenu(ByVal iDir As Long)
On Error Resume Next
Dim barInt As cCommandBarInt
Dim i As Long
Dim iSelected As Long
Dim iStartIndex As Long
Dim bActiveSelection As Boolean
Dim ctl As vbalCommandBar

   If Len(m_sToolBarKey) > 0 Then
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      If Not (barInt Is Nothing) Then
         For i = 1 To barInt.Count
            If (m_item(i).mouseOver Or m_item(i).ShowingMenu) Then
               iSelected = i
               Exit For
            End If
         Next i
         
         If (iSelected = 0) Then
            iSelected = 1
         End If
         iStartIndex = iSelected

         Do
            iSelected = iSelected + iDir
            If (iSelected <= 0) Then
               iSelected = barInt.Count
            ElseIf (iSelected > barInt.Count) Then
               iSelected = 1
            End If
            If (iSelected = iStartIndex) Then
               Exit Do
            End If
            bActiveSelection = Not (m_item(iSelected).Hidden)
            If (bActiveSelection) Then
               With barInt.Item(iSelected)
                  bActiveSelection = .Enabled And .Visible And Not (.Style = eSeparator) Or (.Style = ePanel)
               End With
            End If
         Loop While Not bActiveSelection
         
         fTrack 0, iSelected
         If (ControlFromhWnd(ActiveMenu, ctl)) Then
            ctl.fKeyDown vbKeyDown, 0
         End If
      End If
   End If
End Sub

Private Sub pSelectMenuItem( _
      ByVal iDir As Long, _
      Optional ByVal bFirst As Boolean = False, _
      Optional ByVal bLast As Boolean = False _
   )
   On Error Resume Next
Dim iCurrentSelection As Long
Dim iNewSelection As Long
Dim iIndex As Long
Dim iStartSelection As Long

   If Len(m_sToolBarKey) > 0 Then
      Dim barInt As cCommandBarInt
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      If Not (barInt Is Nothing) Then
         If (barInt.Count > 0) Then

            For iIndex = 1 To barInt.Count
               If (m_item(iIndex).mouseOver Or m_item(iIndex).ShowingMenu) Then
                  iCurrentSelection = iIndex
                  Exit For
               End If
            Next iIndex

            If (bFirst) Then
               iNewSelection = 1
               iDir = 1
            ElseIf (bLast) Then
               iNewSelection = barInt.Count
               iDir = -1
            Else
               iNewSelection = iCurrentSelection + iDir
               If (iNewSelection < 1) Then
                  iNewSelection = barInt.Count
               ElseIf (iNewSelection > barInt.Count) Then
                  iNewSelection = 1
               End If
            End If
            
            iStartSelection = iNewSelection
            Do While (barInt.Item(iNewSelection).Style = eSeparator) Or Not (barInt.Item(iNewSelection).Visible)
               iNewSelection = iNewSelection + iDir
               If (iNewSelection = iStartSelection) Then
                  Exit Sub
               End If
               If (iNewSelection < 1) Then
                  iNewSelection = barInt.Count
               ElseIf (iNewSelection > barInt.Count) Then
                  iNewSelection = 1
               End If
            Loop
            
            If Not (iNewSelection = iCurrentSelection) Then
               ' select the new item:
               fTrack 0, iNewSelection, , True
               tmrLostMouse.Enabled = False
            End If
            
         End If
      End If
   End If
   
End Sub

Private Sub processMenuMouseOver(ByVal indexMouseOver As Long)
   On Error Resume Next
   If Not (m_item(indexMouseOver).ShowingMenu) Then
      Dim barInt As cCommandBarInt
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      If (barInt.Item(indexMouseOver).CanAction(m_eOrientation, m_bPopup, m_bPopupVisibleChecks)) Then
         If Not (m_bPopup) Then
            fCloseMenus True
            pShowDropDown indexMouseOver
         Else
            tmrMenuPopup.Tag = indexMouseOver
            tmrMenuPopup.Enabled = True
         End If
      End If
   End If
   
End Sub
Friend Sub fSetShownFrom( _
      ctl As vbalCommandBar, _
      ByVal hWndShownFromParent As Long, _
      ByVal ePopoutDirection As ECommandBarOrientation, _
      ByVal lPopoutStart As Long, _
      ByVal lPopoutExtent As Long, _
      ByVal hMonitorOn As Long _
   )
   On Error Resume Next
   m_hWndShownFrom = ctl.hWnd
   m_hWndShownFromParent = hWndShownFromParent
   m_eMenuPopoutDirection = ePopoutDirection
   m_lMenuPopoutStart = lPopoutStart
   m_lMenuPopoutExtent = lPopoutExtent
   m_hMonitorOn = hMonitorOn
   m_bMainMenu = False
   GetCursorPos m_tLastMousePos
   fInUse = True
End Sub

Friend Property Get fInUse() As Boolean
On Error Resume Next
   fInUse = m_bInUse
End Property
Friend Property Let fInUse(ByVal bState As Boolean)
On Error Resume Next
   m_bInUse = bState
   If (m_bInUse) Then
      Enabled = True
   End If
End Property
Friend Sub fRaiseHiddenMenuClickEvent(cBtn As cButtonInt)
   On Error Resume Next
   '
   Dim btn As New cButton
   btn.fInit m_hWnd, cBtn.Key
   RaiseEvent ButtonClick(btn)
   '
   
End Sub
Friend Sub fClickButton(ByVal index As Long)
   On Error Resume Next
   If Len(m_sToolBarKey) > 0 Then
      Dim barInt As cCommandBarInt
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      If Not (barInt Is Nothing) Then
         
         If (InMenuLoop) Then
            If (barInt.Item(index).Bar Is Nothing) Then
               If m_bPopupVisibleChecks Then
                  If Not (barInt.Item(index).VisibleCheck = vbGrayed) Then
                     fPaintOneButton index
                     barInt.Item(index).Visible = (barInt.Item(index).VisibleCheck = vbChecked)
                  Else
                     SetInMenuLoop False, 0
                  End If
               Else
                  SetInMenuLoop False, 0
               End If
            End If
         End If
                  
         barInt.ClickButton index
         If Not (m_item(index).ShowingMenu) Then
            Dim c As New cButton
            c.fInit m_hWnd, barInt.Item(index).Key
            RaiseEvent ButtonClick(c)
         End If
         
      End If
   End If
   
End Sub
Friend Sub fDropDownButton(ByVal index As Long)
   On Error Resume Next
   If Len(m_sToolBarKey) > 0 Then
      
      Dim barInt As cCommandBarInt
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      If Not (barInt Is Nothing) Then
         If Not (barInt.Item(index).Bar Is Nothing) Then
            Dim c As New cButton
            c.fInit m_hWnd, barInt.Item(index).Key
            
            Dim bCancel As Boolean
            RaiseEvent ButtonDropDown(c, bCancel)
            If Not (bCancel) Then
            '
               ' Time to show a drop-down:
               pShowDropDown index
               '
            End If
         End If
      End If
   End If
End Sub

Public Sub pShowDropDown(ByVal index As Long, Optional ByVal selectFirst As Boolean = False)
   On Error Resume Next
   If Len(m_sToolBarKey) > 0 Then
      
      Dim barInt As cCommandBarInt
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      If Not (barInt Is Nothing) Then
         
         Dim barDropDownInt As cCommandBarInt
         Set barDropDownInt = barInt.Item(index).Bar
   
         If Not (barDropDownInt Is Nothing) Then
            pShowDropDownBar barDropDownInt, index, , , selectFirst
         End If
         
      End If
   End If
   
End Sub
Private Function pShowDropDownBar( _
      barDropDownInt As cCommandBarInt, _
      Optional ByVal index As Long = 0, _
      Optional ByVal showAtX As Long = 0, _
      Optional ByVal showAtY As Long = 0, _
      Optional ByVal selectFirst As Boolean = False, _
      Optional ByVal fromShowPopupMethod As Boolean = False _
   ) As vbalCommandBar
   On Error Resume Next
   
   ' Get a new instance:
   Dim ctl As vbalCommandBar
   Set ctl = mCommandBars.NewInstance()
   
   ' Assuming we received one:
   If Not (ctl Is Nothing) Then
      
      Dim Bar As New cCommandBar
      Bar.fInit ctl.hWnd, barDropDownInt.Key
      RaiseEvent BeforeShowMenu(Bar)
      
      ' turn it into a menu:
      ctl.fSetAsMenu
      ' set the image list:
      ctl.fSetImageListAndFont m_cToolbarImageList, m_cMenuImageList, Font
      
      ' set the key:
      Dim barDropDown As cCommandBar
      Set barDropDown = New cCommandBar
      barDropDown.fInit m_hWnd, barDropDownInt.Key
      Set ctl.Toolbar = barDropDown
      
      ' Now show it at the appropriate position:
      pShowMenu ctl, index, showAtX, showAtY, selectFirst, fromShowPopupMethod
      
      ' Debug.Print "Would add " & Hex(ctl.hWnd) & " to trail here"
      AddPopupToTrail ctl.hWnd, m_hWnd, fromShowPopupMethod, (fromShowPopupMethod And m_bPopup)
      
      Set pShowDropDownBar = ctl
      
   End If
            

End Function

Private Sub pShowMenu( _
      ByRef ctlPopup As vbalCommandBar, _
      ByVal index As Long, _
      ByVal showAtX As Long, _
      ByVal showAtY As Long, _
      ByVal selectFirst As Boolean, _
      ByVal fromShowPopupMethod As Boolean _
   )
   On Error Resume Next
Dim tR As RECT
Dim tROrig As RECT
Dim tP As POINTAPI
Dim tPCalc As POINTAPI
Dim cM As New cMonitor
Dim lMinX As Long
Dim lMaxX As Long
Dim lMinY As Long
Dim lMaxY As Long
Dim lhWnd As Long
Dim ePopoutDirection As ECommandBarOrientation
Dim lPopoutStart As Long
Dim lPopoutExtent As Long
Dim hWndShownFromParent As Long
Dim i As Long
      
   ' Get the size of the menu item:
   lhWnd = ctlPopup.hWnd
   GetWindowRect lhWnd, tR
   OffsetRect tR, -tR.left, -tR.top
   LSet tROrig = tR
   
   If (index > 0) Then
      ' Calculate menu position for drop-down from an item
      If (m_bPopup) Then
         If (m_hWndShownFrom = 0) Or (m_eMenuPopoutDirection = eTop) Or (m_eMenuPopoutDirection = eBottom) Then
            If (pbRightToLeft()) Then
               ePopoutDirection = eRight
            Else
               ePopoutDirection = eLeft
            End If
         Else
            ePopoutDirection = m_eMenuPopoutDirection
         End If
         If (ePopoutDirection = eLeft) Then
            tP.x = m_item(index).right
         Else
            tP.x = m_item(index).left
         End If
         tP.y = m_item(index).top
         ClientToScreen m_hWnd, tP
         If (ePopoutDirection = eLeft) Then
            OffsetRect tR, tP.x - 1, tP.y - 1
         Else
            OffsetRect tR, tP.x - (tR.right - tR.left) + 1, tP.y - 1
         End If
      Else
         
         Select Case m_eOrientation
         Case eLeft, eRight
            ePopoutDirection = m_eOrientation
            If (m_eOrientation = eLeft) Then
               tP.x = m_item(index).right - IIf(Style = eComCtl32, 0, 2)
            Else
               tP.x = m_item(index).left + IIf(Style = eComCtl32, 0, 2)
            End If
            tP.y = m_item(index).top + IIf(Style = eComCtl32, 0, 1)
            ClientToScreen m_hWnd, tP
            If (m_eOrientation = eLeft) Then
               OffsetRect tR, tP.x, tP.y
            Else
               OffsetRect tR, tP.x - (tR.right - tR.left), tP.y
            End If
            lPopoutStart = tP.y
            tPCalc.y = m_item(index).bottom - 1
            ClientToScreen m_hWnd, tPCalc
            lPopoutExtent = tPCalc.y - lPopoutStart
            
         Case eTop, eBottom
            ePopoutDirection = eTop
            If (pbRightToLeft()) Then
               tP.x = m_item(index).right + IIf(Style = eComCtl32, 0, -1)
            Else
               tP.x = m_item(index).left + IIf(Style = eComCtl32, 0, 1)
            End If
            'If (m_eOrientation = eTop) Then
               tP.y = m_item(index).bottom - IIf(Style = eComCtl32, 0, 2)
            'Else
            '   tP.y = m_item(index).Top + 2
            'End If
            ClientToScreen m_hWnd, tP
            If (pbRightToLeft()) Then
               OffsetRect tR, tP.x - (tR.right - tR.left), 0
            Else
               OffsetRect tR, tP.x, 0
            End If
            'If (m_eOrientation = eTop) Then
               OffsetRect tR, 0, tP.y
            'Else
            '   OffsetRect tR, 0, tP.y - (tR.bottom - tR.Top)
            'End If
            If (pbRightToLeft()) Then
               lPopoutExtent = tP.x
               tPCalc.x = m_item(index).left + 1
               ClientToScreen m_hWnd, tPCalc
               lPopoutExtent = lPopoutExtent - tPCalc.x
               lPopoutStart = tPCalc.x
            Else
               lPopoutStart = tP.x
               tPCalc.x = m_item(index).right - 1
               ClientToScreen m_hWnd, tPCalc
               lPopoutExtent = tPCalc.x - lPopoutStart
            End If
            
            
         End Select
      End If
            
      If (m_hMonitorOn = 0) Then
         cM.CreateFromPoint tP.x, tP.y
      Else
         cM.fInit m_hMonitorOn
      End If
      If (cM.hMonitor = 0) Then
         lMinX = 0
         lMaxX = Screen.Width \ Screen.TwipsPerPixelX
         lMinY = 0
         lMaxY = Screen.Height \ Screen.TwipsPerPixelY
      Else
         lMinX = cM.WorkLeft
         lMaxX = cM.WorkLeft + cM.WorkWidth
         lMinY = cM.WorkTop
         lMaxY = cM.WorkTop + cM.WorkHeight
      End If
      
      If (tR.top >= lMinY) And (tR.bottom <= lMaxY) Then
         ' It fits, we can display it
      Else
         ' Good in x, bad in y
         
         If (tR.bottom > lMaxY) Then
            ' The bottom is off the bottom of the screen
            If (m_bPopup) Then
               ' shift vertically until we can see the menu
               OffsetRect tR, 0, -(tR.bottom - lMaxY)
            Else
               Select Case ePopoutDirection
               Case eTop
                  ' show as if it was from a bottom aligned menu
                  ScreenToClient m_hWnd, tP
                  tP.y = m_item(index).top + 2
                  ClientToScreen m_hWnd, tP
                  LSet tR = tROrig
                  If (pbRightToLeft()) Then
                     OffsetRect tR, tP.x - (tR.right - tR.left), 0
                  Else
                     OffsetRect tR, tP.x, 0
                  End If
                  OffsetRect tR, 0, tP.y - (tR.bottom - tR.top)
                  ePopoutDirection = eBottom
               
               Case eBottom
                  ' This should not occur, as we're popping up from the bottom.
                  ' If it did, the implication is that the button is offscreen
                  ' and if we moved it, then menu would be disconnected from the
                  ' toolbar button that was used to show it.
                  
               Case eLeft, eRight
                  ' shift vertically until we can see the menu
                  OffsetRect tR, 0, -(tR.bottom - lMaxY)
                  
               End Select
            End If
            
         Else
            ' The top is off the top of the screen
            If (m_bPopup) Then
               ' This will get sorted by the check later
               
            Else
               Select Case ePopoutDirection
               Case eTop
                  ' This should not occur, as we're popping up from the bottom.
                  ' If it did, the implication is that the button is offscreen
                  ' and if we moved it, then menu would be disconnected from the
                  ' toolbar button that was used to show it.
               
               
               Case eBottom
                  ' show as if it was from a bottom aligned menu
                  ScreenToClient m_hWnd, tP
                  tP.y = m_item(index).bottom - 2
                  ClientToScreen m_hWnd, tP
                  LSet tR = tROrig
                  If (pbRightToLeft()) Then
                     OffsetRect tR, tP.x - (tR.right - tR.left), 0
                  Else
                     OffsetRect tR, tP.x, 0
                  End If
                  OffsetRect tR, 0, tP.y
                  ePopoutDirection = eTop
                  
               Case eLeft, eRight
                  ' This will get sorted by the check later
                  
               End Select
            End If
            
            
         End If
            
         ' We always keep the top is on screen:
         If (tR.top < lMinY) Then
            OffsetRect tR, 0, lMinY - tR.top
         End If
         
      End If
         
      If (tR.left >= lMinX) And (tR.right <= lMaxX) Then
         ' It fits, we can display it
      Else
         ' Good in y, bad in x
         If (tR.right > lMaxX) Then
            ' Off the screen to the right
            If (m_bPopup) Then
               ' show on the opposite side of the menu from the one
               ' currently selected:
               If (ePopoutDirection = eLeft) Then
                  tP.x = m_item(index).left
                  ePopoutDirection = eRight
               Else
                  tP.x = m_item(index).right
                  ePopoutDirection = eLeft
               End If
               tP.y = m_item(index).top
               ClientToScreen m_hWnd, tP
               tR.left = tROrig.left
               tR.right = tROrig.right
               If (ePopoutDirection = eRight) Then
                  OffsetRect tR, tP.x - (tR.right - tR.left) + 1, 0
               Else
                  OffsetRect tR, tP.x - 1, 0
               End If
                        
            Else
               Select Case ePopoutDirection
               Case eTop, eBottom
                  ' shift left until it is displayed
                  OffsetRect tR, -(tR.right - lMaxX), 0
               
               Case eLeft
                  ' Show as if from a right menu
                  ScreenToClient m_hWnd, tP
                  tP.x = m_item(index).left + 2
                  ClientToScreen m_hWnd, tP
                  tR.left = tROrig.left
                  tR.right = tROrig.right
                  OffsetRect tR, tP.x - (tR.right - tR.left), 0
                  ePopoutDirection = eRight
                  
               Case eRight
                  '
                  
               End Select
            
            End If
         Else
            ' Off the screen to the left
            If (m_bPopup) Then
               ' show on the opposite side of the menu from the one
               ' currently selected:
               If (ePopoutDirection = eLeft) Then
                  tP.x = m_item(index).left
                  ePopoutDirection = eRight
               Else
                  tP.x = m_item(index).right
                  ePopoutDirection = eLeft
               End If
               tP.y = m_item(index).top
               ClientToScreen m_hWnd, tP
               tR.left = tROrig.left
               tR.right = tROrig.right
               If (ePopoutDirection = eRight) Then
                  OffsetRect tR, tP.x - (tR.right - tR.left) + 1, 0
               Else
                  OffsetRect tR, tP.x - 1, 0
               End If
            
            Else
               Select Case ePopoutDirection
               Case eTop, eBottom
                  ' Shift right until displayed
                  
               Case eLeft
                  '
                  
               Case eRight
                  ' Show as if from a left menu
                  ScreenToClient m_hWnd, tP
                  tP.x = m_item(index).right - 2
                  ClientToScreen m_hWnd, tP
                  tR.left = tROrig.left
                  tR.right = tROrig.right
                  OffsetRect tR, tP.x, 0
                  ePopoutDirection = eLeft
                  
               End Select
               
            End If
         End If
      End If
   
   Else
      ' We're showing a popup menu
      OffsetRect tR, showAtX, showAtY
         
   End If
   
   ' Hide any menus that we're showing at the moment:
   fCloseMenus False
   
   ' Tell the item we're showing whom it is showing from, and
   ' where the button rectangle that was used to show it is
   ' located on the screen:
   SetInMenuLoop True, IIf(fromShowPopupMethod, 0, m_hWnd)
   If (m_bPopup) Then
      hWndShownFromParent = m_hWndShownFromParent
   Else
      hWndShownFromParent = m_hWnd
   End If
   If (Style = eComCtl32) Then
      lPopoutStart = -3000
      lPopoutExtent = 0
   End If
   ctlPopup.fSetShownFrom Me, hWndShownFromParent, ePopoutDirection, lPopoutStart, lPopoutExtent, cM.hMonitor
   If (index > 0) Then
      m_item(index).mouseOver = True
      m_item(index).ShowingMenu = True
      m_item(index).hWndMenu = lhWnd
      If Not (m_bPopup) And (m_bWrappable Or m_bMainMenu) Then
         fPaint
      Else
         fPaintOneButton index
      End If
         
      
      Dim barInt As cCommandBarInt
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      For i = 1 To barInt.Count
         If Not (i = index) Then
            If (m_item(i).mouseOver) Then
               m_item(i).mouseOver = False
               If (m_bWrappable Or m_bMainMenu) Then
                  fPaint
               Else
                  fPaintOneButton i
               End If
            End If
         End If
      Next i
   End If
   
   ' Set the style of the object so it works as a popup:
   Dim lStyle As Long
   lStyle = GetWindowLong(lhWnd, GWL_EXSTYLE)
   lStyle = lStyle Or WS_EX_TOOLWINDOW
   lStyle = lStyle And Not (WS_EX_APPWINDOW)
   SetWindowLong lhWnd, GWL_EXSTYLE, lStyle
   SetParent lhWnd, HWND_DESKTOP
   
   SetWindowPos lhWnd, HWND_TOPMOST, tR.left, tR.top, tR.right - tR.left, tR.bottom - tR.top, SWP_SHOWWINDOW
   ctlPopup.fPaint
   ctlPopup.fShowMenuShadow
   If (selectFirst) Then
      ctlPopup.fKeyDown vbKeyDown, 0
   End If
   
   If Not (m_bPopup) Then
      HidePopupsFromOtherControls m_hWnd
   End If
   ActiveMenu = lhWnd
   m_eMenuTrackMode = 1
   
End Sub

Friend Sub fCloseMenusInternal(ByVal bHide As Boolean, ByVal lhWndExclude As Long)
   On Error Resume Next
   Dim barInt As cCommandBarInt
   Set barInt = mCommandBars.BarItem(m_sToolBarKey)
   If Not (barInt Is Nothing) Then
      Dim i As Long
      Dim ctl As vbalCommandBar
      For i = 1 To barInt.Count
         If (m_item(i).ShowingMenu) Then
            If ControlFromhWnd(m_item(i).hWndMenu, ctl) Then
               If Not (ctl Is Me) Then
                  ctl.fCloseMenusInternal True, lhWndExclude
               End If
            End If
            m_item(i).ShowingMenu = False
            m_item(i).hWndMenu = 0
            If Not (m_bWrappable Or m_bMainMenu) Then
               fPaintOneButton i
            End If
         End If
      Next i
      If (m_bWrappable Or m_bMainMenu) Then
         fPaint
      End If
      
   End If
   
   If m_bPopup Then
      If bHide Then
         fInUse = False
         If Not (m_cRightShadow Is Nothing) Then
            Dim Bar As New cCommandBar
            Bar.fInit m_hWnd, barInt.Key
            RaiseEvent AfterShowMenu(Bar)
            
            'Debug.Print "Would remove " & Hex(m_hWnd) & " from trail here"
            RemovePopupFromTrail m_hWnd
            
         End If
         ShowWindow m_hWnd, SW_HIDE
         If Not (m_cRightShadow Is Nothing) Then
            m_cRightShadow.Destroy
            Set m_cRightShadow = Nothing
         End If
         If Not (m_cBottomShadow Is Nothing) Then
            m_cBottomShadow.Destroy
            Set m_cBottomShadow = Nothing
         End If
      Else
         ActiveMenu = m_hWnd
      End If
   Else
      ActiveMenu = m_hWnd
   End If

   
End Sub

Friend Sub fCloseMenus(ByVal bHide As Boolean)
      On Error Resume Next
   fCloseMenusInternal bHide, m_hWnd
   
End Sub

Private Sub pMouseMove(ByVal button As MouseButtonConstants, ByVal shift As ShiftConstants)
   
   '
   On Error Resume Next
   Dim tP As POINTAPI
   Dim iIndex As Long
   
   GetCursorPos tP
   If Not ((tP.x = m_tLastMousePos.x) And (tP.y = m_tLastMousePos.y)) Then
      LSet m_tLastMousePos = tP
      ScreenToClient m_hWnd, tP
      iIndex = fHitTest(tP.x, tP.y)
      If (menuInitiator = m_hWnd) Then
         If (iIndex = 0) Then
            Exit Sub
         Else
            fTrack button, iIndex
         End If
      Else
         fTrack button, iIndex
      End If
   End If
   
End Sub
Private Sub pMouseDown(ByVal button As MouseButtonConstants, ByVal shift As ShiftConstants)
   '
   On Error Resume Next
   Dim tP As POINTAPI
   Dim iIndex As Long
   Dim barInt As cCommandBarInt
   
   GetCursorPos tP
   LSet m_tLastMousePos = tP

   ScreenToClient m_hWnd, tP
   iIndex = fHitTest(tP.x, tP.y)
   
   If (button = vbLeftButton) Then
      fTrack button, iIndex, True
      If (iIndex > 0) Then
         Set barInt = mCommandBars.BarItem(m_sToolBarKey)
         If (barInt.Item(iIndex).Enabled) Then
            If (barInt.Item(iIndex).Style = eSplit) Then
               If (m_item(iIndex).MouseOverSplit) Then
                  fDropDownButton iIndex
               End If
            Else
               fDropDownButton iIndex
            End If
         End If
      End If
   Else
      fTrack button, iIndex, False
      If (button = vbRightButton) Then
         Dim cBtn As cButton
         If (iIndex > 0) Then
            Set barInt = mCommandBars.BarItem(m_sToolBarKey)
            Set cBtn = New cButton
            cBtn.fInit m_hWnd, barInt.Item(iIndex).Key
         End If
         RaiseEvent RightClick(cBtn, tP.x, tP.y)
      End If
   End If
   '
End Sub
Private Sub pMouseUp(ByVal button As MouseButtonConstants, ByVal shift As ShiftConstants)
   '
   On Error Resume Next
   Dim tP As POINTAPI
   
   GetCursorPos tP
   LSet m_tLastMousePos = tP
   ScreenToClient m_hWnd, tP
   
   pMouseUpInternal button, shift, tP
   
   '
End Sub

Private Sub pMouseUpInternal( _
      ByVal button As MouseButtonConstants, _
      ByVal shift As ShiftConstants, _
      tP As POINTAPI _
   )
   On Error Resume Next
Dim iIndex As Long
Dim i As Long
   
   iIndex = fHitTest(tP.x, tP.y)
   
   If (button = vbLeftButton) Then
      If Len(m_sToolBarKey) > 0 Then
         Dim barInt As cCommandBarInt
         Set barInt = mCommandBars.BarItem(m_sToolBarKey)
         If Not (barInt Is Nothing) Then
            If (iIndex > 0) Then
               If (barInt.Item(iIndex).CanAction(m_eOrientation, m_bPopup, m_bPopupVisibleChecks)) Then
                  If (m_item(iIndex).mouseOver) And (m_item(iIndex).mouseDown) Then
                     fClickButton iIndex
                  End If
               End If
            End If
            For i = 1 To barInt.Count
               If (m_item(i).mouseDown) Or (m_item(i).MouseDownSplit) Then
                  m_item(i).mouseDown = False
                  m_item(i).MouseDownSplit = False
                  If Not (m_bWrappable Or m_bMainMenu) Then
                     fPaintOneButton i
                  End If
               End If
            Next i
            If (m_bWrappable Or m_bMainMenu) Then
               fPaint
            End If
         End If
      End If
      fTrack 0, iIndex
      If (iIndex > 0) Then
         If (m_bWrappable Or m_bMainMenu) Then
            fPaint
         Else
            fPaintOneButton iIndex
         End If
      End If
   Else
      fTrack 0, iIndex
   End If

End Sub

Private Function pbRightToLeft() As Boolean
On Error Resume Next
   pbRightToLeft = UserControl.RightToLeft
End Function
Private Function plGetHFont() As Long
On Error Resume Next
Dim lHDC As Long
Dim f As StdFont
   Set f = UserControl.Font
   lHDC = UserControl.hdc
   If (m_eOrientation = eTop) Or (m_eOrientation = eBottom) Then
      plGetHFont = m_fntCache.hFont(f, 0, lHDC)
   Else
      plGetHFont = m_fntCache.hFont(f, 2700, lHDC)
   End If
End Function
Private Function getFormParenthWnd(ByVal hWndControl As Long) As Long
On Error Resume Next
Dim lhWnd As Long
Dim lhWndTest As Long
Dim lErr As Long
   On Error Resume Next
   lhWnd = UserControl.Parent.hWnd
   If Not (Err.Number = 0) Then
      On Error GoTo 0
      lhWndTest = GetParent(hWndControl)
      Do
         lhWnd = lhWndTest
         lhWndTest = GetParent(lhWnd)
      Loop While Not (lhWndTest = 0)
      getFormParenthWnd = lhWnd
   Else
      On Error GoTo 0
      getFormParenthWnd = lhWnd
   End If
End Function

Private Sub pUnSubclass()
On Error Resume Next
   If Not (m_hWndParent = 0) Then
      DetachMessage Me, m_hWndParent, WM_ACTIVATEAPP
      DetachMessage Me, m_hWndParent, WM_SETTINGCHANGE
      m_hWndParent = 0
   End If
End Sub
Private Sub pSubClass()
   On Error Resume Next
   pUnSubclass
   
   m_hWndParent = getFormParenthWnd(m_hWnd)
   If Not (m_hWndParent = 0) Then
      AttachMessage Me, m_hWndParent, WM_ACTIVATEAPP
      AttachMessage Me, m_hWndParent, WM_SETTINGCHANGE
   End If
   
End Sub

Private Sub pInitialise()

   On Error Resume Next
   m_bDesignTime = Not (UserControl.Ambient.UserMode)
   If (Err.Number <> 0) Then m_bDesignTime = False
   
   On Error GoTo 0
   If Not (m_bDesignTime) Then
      m_hWnd = UserControl.hWnd
      mCommandBars.AddRef hWnd, Me
      Set m_cToolbarImageList = New cCommandBarImageList
      Set m_cMenuImageList = New cCommandBarImageList
      m_cToolbarImageList.DisabledColor = ButtonTextDisabledColor
      m_cToolbarImageList.HighlightColor = ButtonTextHotColor
      m_cMenuImageList.DisabledColor = ButtonTextDisabledColor
   End If
   
End Sub

Private Sub pTerminate()
On Error Resume Next
   pUnSubclass

   If Not (m_hWnd = 0) Then
   
      m_cToolbarImageList.Destroy
      Set m_cToolbarImageList = Nothing
      m_cMenuImageList.Destroy
      Set m_cMenuImageList = Nothing

      Dim barInt As cCommandBarInt
      If Len(m_sToolBarKey) > 0 Then
         Set barInt = mCommandBars.BarItem(m_sToolBarKey)
         If Not barInt Is Nothing Then
            barInt.ReleaseRef m_hWnd
         End If
      End If
   
      mCommandBars.ReleaseRef hWnd
   End If
   m_hWnd = 0
   
End Sub

Private Property Let ISubclass_MsgResponse(ByVal RHS As EMsgResponse)
On Error Resume Next
   '
   '
End Property

Private Property Get ISubclass_MsgResponse() As EMsgResponse
On Error Resume Next
   '
   ISubclass_MsgResponse = emrPostProcess
   '
End Property

Private Function ISubclass_WindowProc(ByVal hWnd As Long, ByVal iMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
On Error Resume Next
   '
   Select Case iMsg
   Case WM_ACTIVATEAPP
      If (wParam = 0) Then
         If (InMenuLoop) Then
            SetInMenuLoop False, m_hWnd
         End If
      End If
      
   Case WM_SETTINGCHANGE
      UserControl_Resize
      
   End Select
   '
End Function

Private Sub tmrLostMouse_Timer()
On Error Resume Next
   '
   Dim tP As POINTAPI
   Dim tR As RECT
   Dim i As Long
   
   If (Me.Enabled) Then
      GetCursorPos tP
      If Not ((tP.x = m_tLastMousePos.x) And (tP.y = m_tLastMousePos.y)) Then
         LSet m_tLastMousePos = tP
   
         GetWindowRect m_hWnd, tR
         If (PtInRect(tR, tP.x, tP.y) = 0) Then
            If Not (menuInitiator = m_hWnd) Then
               fTrack 0, 0
            Else
               ' unhighlight the item if it is not showing a menu
               Dim barInt As cCommandBarInt
               Set barInt = mCommandBars.BarItem(m_sToolBarKey)
               If Not (barInt Is Nothing) Then
                  For i = 1 To barInt.Count
                     If (m_item(i).mouseOver Or m_item(i).MouseOverSplit) Then
                        If Not (m_item(i).ShowingMenu) Then
                           fTrack 0, 0
                           Exit For
                        End If
                     End If
                  Next i
               End If
            End If
            tmrLostMouse.Enabled = False
         End If
      End If
   End If
   '
End Sub

Private Sub tmrMenuPopup_Timer()
On Error Resume Next
   If IsNumeric(tmrMenuPopup.Tag) Then
      Dim index As Long
      index = CLng(tmrMenuPopup.Tag)
      Dim barInt As cCommandBarInt
      Set barInt = mCommandBars.BarItem(m_sToolBarKey)
      If (index > 0) And (index <= barInt.Count) Then
      If (m_item(index).mouseOver) Then
         If Not (barInt.Item(index).Bar Is Nothing) Then
            If Not (m_item(index).ShowingMenu) Then
               pShowDropDown index
            End If
         Else
            Dim ctl As vbalCommandBar
            For index = 1 To barInt.Count
               If (m_item(index).ShowingMenu) And Not (m_item(index).hWndMenu = 0) Then
                  If (ControlFromhWnd(m_item(index).hWndMenu, ctl)) Then
                     ctl.fCloseMenus True
                  End If
                  m_item(index).ShowingMenu = False
                  m_item(index).hWndMenu = 0
                  If Not (m_bWrappable Or m_bMainMenu) Then
                     fPaintOneButton index
                  End If
               End If
            Next index
            If (m_bWrappable Or m_bMainMenu) Then
               fPaint
            End If
         End If
      End If
      End If
   End If
   tmrMenuPopup.Enabled = False
End Sub

Private Sub UserControl_AmbientChanged(PropertyName As String)
On Error Resume Next
   '
   'Debug.Print PropertyName
   '
End Sub

Private Sub UserControl_Initialize()
On Error Resume Next
   '
   'Debug.Print ">> PREPARE FOR WAVE " + UserControl.Name
   
   ' Hack for XP Crash under VB6
   m_hMod = LoadLibrary("shell32.dll")
   InitCommonControls
   
   m_bEnabled = True
   m_bVisible = True
   m_bRedraw = True
   '
End Sub

Private Sub UserControl_InitProperties()
   On Error Resume Next
   '
   pInitialise
   '
End Sub

Private Sub UserControl_MouseDown(button As Integer, shift As Integer, x As Single, y As Single)
   On Error Resume Next
   '
   If m_bInDragMode Then
      ' TODO
   Else
      pMouseDown button, shift
   End If
   '
End Sub

Private Sub UserControl_MouseMove(button As Integer, shift As Integer, x As Single, y As Single)
   On Error Resume Next
   '
   If m_bInDragMode Then
      ' TODO
   Else
      pMouseMove button, shift
   End If
   '
End Sub

Private Sub UserControl_MouseUp(button As Integer, shift As Integer, x As Single, y As Single)
   On Error Resume Next
   '
   If m_bInDragMode Then
      ' TODO
   Else
      pMouseUp button, shift
   End If
   '
End Sub

Private Sub UserControl_Paint()
   On Error Resume Next
   '
   'fPaint
   '
End Sub

Private Sub UserControl_ReadProperties(PropBag As PropertyBag)
   On Error Resume Next
   '
   pInitialise
   
   Dim defFont As New StdFont
   defFont.Name = "Tahoma"
   defFont.Size = 8.25
   Set Font = PropBag.ReadProperty("Font", defFont)
   m_bEnabled = PropBag.ReadProperty("Enabled", True)
   Orientation = PropBag.ReadProperty("Orientation", eTop)
   MainMenu = PropBag.ReadProperty("MainMenu", False)
   Style = PropBag.ReadProperty("Style", eOffice2003)
   '
End Sub

Private Sub UserControl_Resize()
   On Error Resume Next
   '
   If Not (m_bResizeInterlock) Then
      m_bResizeInterlock = True
      If Not (m_bPopup) Then
         fResize
      End If
      RaiseEvent Resize
      m_bResizeInterlock = False
   End If
   '
End Sub

Private Sub UserControl_Terminate()
   On Error Resume Next
   '
   pTerminate
   
   If Not (m_hMod = 0) Then
      FreeLibrary m_hMod
      m_hMod = 0
   End If
   
   'Debug.Print ">> WAVE DEFEATED " + UserControl.Name
   '
End Sub

Private Sub UserControl_WriteProperties(PropBag As PropertyBag)
   On Error Resume Next
   '
   PropBag.WriteProperty "Font", Font
   PropBag.WriteProperty "Enabled", m_bEnabled, True
   PropBag.WriteProperty "Orientation", m_eOrientation, eTop
   PropBag.WriteProperty "MainMenu", MainMenu, False
   PropBag.WriteProperty "Style", Style, eOffice2003
   '
End Sub


'
'
' Noisy Playlist:
'
'  The Rapture - The Coming of Spring
'  Audio Bullys - I Go To Your House
'  Free Form Five - Perspex Sex (Ewan Pearson Mix)
'  Dead Prez - Hip Hop
'  Yeah Yeah Yeahs - Rich
'  Akufen - New Process
'  Dizzee Rascal - I Luv U
'  Dr Octagon - Bear Witness
'  New Flesh featuring Robotic EBU - Stick & Move
'  The Bug vs The Rootsman ft He-Man - Killer
'  Grandmaster Flash and The Furious Five - Scorpio (Plaid Remix)
'
'
' Fun Playlist:
'
'  Kid Koala - Drunk Trumpet
'  Barry Adamson - Something Wicked This Way Comes
'  Prince - Baby I'm A Star
'  Skee Lo - I Wish
'  Stevie Wonder - Sir Duke
'  The Jackson 5 - It's Great to be Here
'  Gladys Knight and the Pips - Bourgie Bourgie
'  The Chi-Lites - My First Mistake
'  Wade Marcus - Spinning Wheel
'
