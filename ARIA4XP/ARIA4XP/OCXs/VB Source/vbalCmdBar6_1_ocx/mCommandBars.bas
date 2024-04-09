Attribute VB_Name = "mCommandBars"
Option Explicit

Public Type POINTAPI
   x As Long
   y As Long
End Type

Public Type RECT
   left As Long
   top As Long
   right As Long
   bottom As Long
End Type

Public Declare Function IsWindow Lib "user32" (ByVal hWnd As Long) As Long
Public Declare Function ShowWindow Lib "user32" (ByVal hWnd As Long, ByVal nCmdShow As Long) As Long
   Public Const SW_HIDE = 0
Public Declare Function GetClientRect Lib "user32" (ByVal hWnd As Long, lpRect As RECT) As Long
Public Declare Function GetWindowRect Lib "user32" (ByVal hWnd As Long, lpRect As RECT) As Long
Public Declare Function CreateDCAsNull Lib "gdi32" Alias "CreateDCA" (ByVal lpDriverName As String, _
           lpDeviceName As Any, lpOutput As Any, lpInitData As Any) As Long
Public Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
Public Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Public Declare Function GetObjectAPI Lib "gdi32" Alias "GetObjectA" (ByVal hObject As Long, ByVal nCount As Long, lpObject As Any) As Long
Public Declare Function GetDC Lib "user32" (ByVal hWnd As Long) As Long
Public Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function ReleaseDC Lib "user32" (ByVal hWnd As Long, ByVal hdc As Long) As Long
Public Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function CreateCompatibleBitmap Lib "gdi32" (ByVal hdc As Long, ByVal nWidth As Long, ByVal nHeight As Long) As Long
Public Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long
Public Declare Function CreateSolidBrush Lib "gdi32" (ByVal crColor As Long) As Long
Public Declare Function GetSysColorBrush Lib "user32" (ByVal nIndex As Long) As Long
Public Declare Function SetPixel Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal crColor As Long) As Long
Public Declare Function CreatePen Lib "gdi32" (ByVal nPenStyle As Long, ByVal nWidth As Long, ByVal crColor As Long) As Long
Public Const PS_SOLID = 0
Public Declare Function SelectClipRgn Lib "gdi32" (ByVal hdc As Long, ByVal hRgn As Long) As Long
Public Declare Function SetTextColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long
Public Declare Function SetBkColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long
Public Declare Function SetBkMode Lib "gdi32" (ByVal hdc As Long, ByVal nBkMode As Long) As Long
    Public Const OPAQUE = 2
    Public Const TRANSPARENT = 1
Public Declare Function SetTextAlign Lib "gdi32" (ByVal hdc As Long, ByVal wFlags As Long) As Long
   Public Const TA_BASELINE = 24
Public Declare Function SetViewportOrgEx Lib "gdi32" (ByVal hdc As Long, ByVal nX As Long, ByVal nY As Long, lpPoint As Any) As Long
Public Declare Function GetSysColor Lib "user32" (ByVal nIndex As Long) As Long

Public Declare Function DrawTextA Lib "user32" (ByVal hdc As Long, ByVal lpStr As String, ByVal nCount As Long, lpRect As RECT, ByVal wFormat As Long) As Long
Public Declare Function DrawTextW Lib "user32" (ByVal hdc As Long, ByVal lpStr As Long, ByVal nCount As Long, lpRect As RECT, ByVal wFormat As Long) As Long
    Public Const DT_LEFT = &H0&
    Public Const DT_TOP = &H0&
    Public Const DT_CENTER = &H1&
    Public Const DT_RIGHT = &H2&
    Public Const DT_VCENTER = &H4&
    Public Const DT_BOTTOM = &H8&
    Public Const DT_WORDBREAK = &H10&
    Public Const DT_SINGLELINE = &H20&
    Public Const DT_EXPANDTABS = &H40&
    Public Const DT_TABSTOP = &H80&
    Public Const DT_NOCLIP = &H100&
    Public Const DT_EXTERNALLEADING = &H200&
    Public Const DT_CALCRECT = &H400&
    Public Const DT_NOPREFIX = &H800
    Public Const DT_INTERNAL = &H1000&
    Public Const DT_WORD_ELLIPSIS = &H40000

' Rectangle functions:
Public Declare Function Rectangle Lib "gdi32" (ByVal hdc As Long, ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long) As Long
Public Declare Function EqualRect Lib "user32" (lpRect1 As RECT, lpRect2 As RECT) As Long
Public Declare Function InflateRect Lib "user32" (lpRect As RECT, ByVal x As Long, ByVal y As Long) As Long
Public Declare Function OffsetRect Lib "user32" (lpRect As RECT, ByVal x As Long, ByVal y As Long) As Long
Public Declare Function PtInRect Lib "user32" (lpRect As RECT, ByVal ptX As Long, ByVal ptY As Long) As Long
Public Declare Function FillRect Lib "user32" (ByVal hdc As Long, lpRect As RECT, ByVal hBrush As Long) As Long

Private Declare Function GetForegroundWindow Lib "user32" () As Long
Private Declare Function IsIconic Lib "user32" (ByVal hWnd As Long) As Long
Private Declare Function IsWindowEnabled Lib "user32" (ByVal hWnd As Long) As Long

' All controls which are connected to the command bar data
Private m_colhWnd As Collection

' The command bars & the respective buttons
Private m_colCommandBars As Collection
Private m_colButtons As Collection

' A collection of controls which we created ourselves
Private m_colPopups As Collection

Private m_showingInfrequentlyUsed As Boolean
Private m_hideInfrequentlyUsed As Boolean
Private m_inMenuLoop As Boolean
Private m_colDisabled As Collection
Private m_hWndActiveMenu As Long
Private m_hWndMenuLoopInitControl As Long
Private m_bHighlightDisabledItems As Boolean

Private m_colPopupTrail As Collection
Private m_iRecurseLevel As Long

Public Sub AddPopupToTrail(ByVal hWnd As Long, ByVal hWndSource As Long, ByVal bShownAsPopup As Boolean, ByVal bPoppedOverPopup As Boolean)
   
   If (m_colPopupTrail Is Nothing) Then
      Set m_colPopupTrail = New Collection
   End If
   Dim iRecursionLevel As Long
   If (m_colPopupTrail.Count > 0) Then
      iRecursionLevel = m_colPopupTrail(m_colPopupTrail.Count).RecursionLevel
   End If
   If (bPoppedOverPopup) Then
      Debug.Print "Recursive popup", iRecursionLevel
      iRecursionLevel = iRecursionLevel + 1
   End If
   Dim c As New cMenuPopupStack
   c.Initialise hWnd, hWndSource, bShownAsPopup, iRecursionLevel
   m_colPopupTrail.Add c, "H" & hWnd
   
   ' Disable all menus at lower recursion levels
   Dim cTrailItem As cMenuPopupStack
   Dim ctl As vbalCommandBar
   For Each cTrailItem In m_colPopupTrail
      If (cTrailItem.RecursionLevel < iRecursionLevel) Then
         If ControlFromhWnd(cTrailItem.hWnd, ctl) Then
            ctl.Enabled = False
         End If
         If ControlFromhWnd(cTrailItem.hWndSource, ctl) Then
            ctl.Enabled = False
         End If
      End If
   Next
      
   m_iRecurseLevel = iRecursionLevel
End Sub
Public Sub RemovePopupFromTrail(ByVal hWnd As Long)
   
   On Error Resume Next
      Dim cThisTrailItem As cMenuPopupStack
   Set cThisTrailItem = m_colPopupTrail.Item("H" & hWnd)
   If (Err.Number = 0) Then
      On Error GoTo 0
      
      Dim cTrailItem As cMenuPopupStack
      Dim iCount As Long
      ' Check if there are any other items at this recursion level
      For Each cTrailItem In m_colPopupTrail
         If Not (cTrailItem Is cThisTrailItem) Then
            If (cTrailItem.RecursionLevel = cThisTrailItem.RecursionLevel) Then
               iCount = iCount + 1
            End If
         End If
      Next
      
      If (iCount = 0) Then
         ' Re-enable all items with recursion level -1
         Dim ctl As vbalCommandBar
         For Each cTrailItem In m_colPopupTrail
            If (cTrailItem.RecursionLevel = cThisTrailItem.RecursionLevel - 1) Then
               If ControlFromhWnd(cTrailItem.hWnd, ctl) Then
                  ctl.Enabled = True
               End If
               If ControlFromhWnd(cTrailItem.hWndSource, ctl) Then
                  ctl.Enabled = True
               End If
            End If
         Next
      End If
      
      ' Remove item from trail
      m_colPopupTrail.Remove "H" & hWnd
      
   End If
   On Error GoTo 0
   
   If (m_colPopupTrail.Count = 0) Then
      Set m_colPopupTrail = Nothing
   End If
   
End Sub

Public Property Get HighlightDisabledItems() As Boolean
   HighlightDisabledItems = m_bHighlightDisabledItems
End Property
Public Property Let HighlightDisabledItems(ByVal bState As Boolean)
   m_bHighlightDisabledItems = bState
End Property

Private Function getCachedControlInstance(ctl As vbalCommandBar) As Boolean
Dim ctlCache As vbalCommandBar
Dim bSucceeded As Boolean

   If Not m_colPopups Is Nothing Then
      
      Dim vlhWnd As Variant
      Dim lhWnd As Long
      Dim ctlCheck As vbalCommandBar
            
      For Each vlhWnd In m_colPopups
         lhWnd = vlhWnd
         If (ControlFromhWnd(lhWnd, ctlCheck)) Then
            If Not (ctlCheck.fInUse) Then
               Set ctl = ctlCheck
               ctl.fInUse = True
               bSucceeded = True
               Exit For
            End If
         End If
      Next
      
   End If
   
   getCachedControlInstance = bSucceeded
   
End Function

Private Sub cacheControlInstance(ctl As vbalCommandBar)
   If m_colPopups Is Nothing Then
      Set m_colPopups = New Collection
   End If
   TagControl ctl.hWnd, ctl, True
   m_colPopups.Add ctl.hWnd, "H" & ctl.hWnd
End Sub

Private Sub releaseCachedControlInstances()
   If Not m_colPopups Is Nothing Then
      Dim vlhWnd As Variant
      Dim lhWnd As Long
      For Each vlhWnd In m_colPopups
         lhWnd = vlhWnd
         If Not (IsWindow(lhWnd) = 0) Then
            TagControl lhWnd, Nothing, False
         End If
      Next vlhWnd
      Set m_colPopups = Nothing
   End If
End Sub

Private Sub markCachedControlsUnused()
   If Not m_colPopups Is Nothing Then
      Dim vlhWnd As Variant
      Dim lhWnd As Long
      Dim ctl As vbalCommandBar
      For Each vlhWnd In m_colPopups
         lhWnd = vlhWnd
         If (ControlFromhWnd(lhWnd, ctl)) Then
            ctl.fInUse = False
         End If
      Next vlhWnd
   End If
End Sub

Public Sub HidePopupsFromOtherControls(ByVal hWndExclude As Long)
   If Not (m_colPopups Is Nothing) Then
      Dim vlhWnd As Variant
      Dim lhWnd As Long
      Dim ctl As vbalCommandBar
      For Each vlhWnd In m_colhWnd
         lhWnd = vlhWnd
         If Not (lhWnd = hWndExclude) Then
            If (ControlFromhWnd(lhWnd, ctl)) Then
               If Not (ctl.fIsSetAsMenu) Then
                  ctl.fCloseMenus True
               End If
            End If
         End If
      Next vlhWnd
   End If
End Sub

Public Sub RepaintControls()
   If Not (m_colhWnd Is Nothing) Then
      Dim vlhWnd As Variant
      Dim ctl As vbalCommandBar
      For Each vlhWnd In m_colhWnd
         If (ControlFromhWnd(vlhWnd, ctl)) Then
            ctl.fPaintStyleChanged
         End If
      Next
   End If
End Sub

Private Sub hidePopupsAtRecurseLevel(ByVal iRecurseLevel As Long)

   If Not (m_colPopupTrail Is Nothing) Then
      
      Dim cTrailItem As cMenuPopupStack
      Dim ctl As vbalCommandBar
      Dim i As Long
      
      For i = m_colPopupTrail.Count To 1 Step -1
         Set cTrailItem = m_colPopupTrail(i)
         If (cTrailItem.RecursionLevel >= iRecurseLevel) Then
            If (ControlFromhWnd(cTrailItem.hWnd, ctl)) Then
               ctl.fCloseMenus True
            End If
         End If
      Next i
      
   End If

End Sub

Private Sub hidePopups()

   If Not (m_colPopups Is Nothing) Then
      Dim vlhWnd As Variant
      Dim lhWnd As Long
      Dim ctl As vbalCommandBar
      For Each vlhWnd In m_colhWnd
         lhWnd = vlhWnd
         If (ControlFromhWnd(lhWnd, ctl)) Then
            ctl.fCloseMenus True
         End If
      Next vlhWnd
   End If
   
End Sub

Public Property Get NewInstance() As vbalCommandBar
   
   ' Either use an existing cached control instance, or
   ' request a new control instance from one of the
   ' controls that's connected to me.
   If (m_colhWnd.Count > 0) Then
      
      Dim ctlNew As vbalCommandBar
      If Not getCachedControlInstance(ctlNew) Then
         Dim lhWnd As Long
         Dim ctl As vbalCommandBar
         lhWnd = m_colhWnd(1)
         If (ControlFromhWnd(lhWnd, ctl)) Then
            
            Set ctlNew = ctl.NewInstance()
            
            If Not (ctlNew Is Nothing) Then
               cacheControlInstance ctlNew
            End If
            
         End If
      End If
      Set NewInstance = ctlNew

   End If
End Property

Public Property Get HideInfrequentlyUsed() As Boolean
   HideInfrequentlyUsed = m_hideInfrequentlyUsed
End Property
Public Property Let HideInfrequentlyUsed(ByVal bState As Boolean)
   m_hideInfrequentlyUsed = bState
End Property
Public Property Get ShowingInfrequentlyUsed() As Boolean
   If (m_hideInfrequentlyUsed) Then
      ShowingInfrequentlyUsed = m_showingInfrequentlyUsed
   Else
      ShowingInfrequentlyUsed = True
   End If
End Property
Public Sub ShowInfrequentlyUsed()
   m_showingInfrequentlyUsed = True
End Sub
Public Property Get ActiveMenu() As Long
   ActiveMenu = m_hWndActiveMenu
End Property
Public Property Let ActiveMenu(ByVal hWnd As Long)
   m_hWndActiveMenu = hWnd
End Property
Public Property Get menuInitiator() As Long
   menuInitiator = m_hWndMenuLoopInitControl
End Property
Public Property Get InMenuLoop() As Boolean
   InMenuLoop = m_inMenuLoop
End Property
Public Sub SetInMenuLoop(ByVal bState As Boolean, ByVal hWndControl As Long)
Dim vlhWnd As Variant
Dim ctl As vbalCommandBar

   If Not (m_inMenuLoop = bState) Then
      m_showingInfrequentlyUsed = False
      
      If (bState) Then
      
         m_inMenuLoop = True
         
         ' disable all non-popup controls until we have
         ' completed the menu loop
         Set m_colDisabled = New Collection
         m_hWndMenuLoopInitControl = hWndControl
         For Each vlhWnd In m_colhWnd
            If (ControlFromhWnd(vlhWnd, ctl)) Then
               If Not (ctl.fIsSetAsMenu) And Not (hWndControl = vlhWnd) Then
                  ctl.Enabled = False
                  m_colDisabled.Add vlhWnd
               End If
            End If
         Next
         AttachMouseHook 0
      Else
         If (m_iRecurseLevel > 0) Then
            hidePopupsAtRecurseLevel m_iRecurseLevel
            m_iRecurseLevel = m_iRecurseLevel - 1
            
         Else
            
            m_inMenuLoop = False
            
            HighlightDisabledItems = False
            DetachMouseHook 0
            markCachedControlsUnused
            hidePopups
            ActiveMenu = 0
            
            If Not m_colDisabled Is Nothing Then
               For Each vlhWnd In m_colDisabled
                  If (ControlFromhWnd(vlhWnd, ctl)) Then
                     ctl.Enabled = True
                  End If
               Next
               Set m_colDisabled = Nothing
            End If
         
            If Not (m_hWndMenuLoopInitControl = 0) Then
               If (ControlFromhWnd(m_hWndMenuLoopInitControl, ctl)) Then
                  ctl.fTrack 0, 0
               End If
            End If
            m_hWndMenuLoopInitControl = 0
            
         End If
      End If
      
   End If
End Sub

Public Function ProcessAccelerators(ByVal vKey As Long, ByVal shiftState As Long) As Boolean
Dim lhWnd As Long
   ' Find active form
   lhWnd = getActiveEnabledForegroundWindow()
   If Not (lhWnd = 0) Then
      Dim iBtn As Long
      Dim cBtn As cButtonInt
      Dim cMatch As cButtonInt
      Dim ctl As vbalCommandBar
      Dim lIndex As Long
      For iBtn = 1 To ButtonCount
         Set cBtn = ButtonItem(iBtn)
         Set cMatch = cBtn.AcceleratorMatches(lhWnd, vKey, shiftState, False, ctl)
         If Not (cMatch Is Nothing) Then
            ' We have one, send a click event
            If Not (ctl Is Nothing) Then
               lIndex = ctl.ButtonIndex(cMatch)
               If (lIndex > 0) Then
                  ctl.fClickButton lIndex
               Else
                  ctl.fRaiseHiddenMenuClickEvent cMatch
               End If
            End If
            ProcessAccelerators = True
            Exit For
         End If
      Next iBtn
   End If
End Function

Private Function getActiveEnabledForegroundWindow() As Long
Dim lhWnd As Long
   lhWnd = GetForegroundWindow()
   If Not (IsWindowEnabled(lhWnd) = 0) Then
      If (IsIconic(lhWnd) = 0) Then
         getActiveEnabledForegroundWindow = lhWnd
      End If
   End If
End Function

Public Function FindActiveMenuControl() As Long
Dim lhWnd As Long
   ' Find active form
   lhWnd = getActiveEnabledForegroundWindow()
   If Not (lhWnd = 0) Then
      ' Check all toolbars for one with a main menu
      ' that is owned by this form:
      Dim vlhWnd As Variant
      Dim ctl As vbalCommandBar
      For Each vlhWnd In m_colhWnd
         If ControlFromhWnd(vlhWnd, ctl) Then
            If (ctl.hWndParent = lhWnd) Then
               If (ctl.MainMenu) Then
                  If (ctl.Enabled) Then
                     FindActiveMenuControl = ctl.hWnd
                     Exit For
                  End If
               End If
            End If
         End If
      Next vlhWnd
   End If
End Function

Private Sub CreateChevronBars()
Dim barAddOrRemoveBar As cCommandBarInt
Dim btnAddOrRemoveBar As cButtonInt
Dim barAddOrRemove As cCommandBarInt
Dim btnAddOrRemove As cButtonInt
Dim btnInt As cButtonInt
Dim barChevron As cCommandBarInt
Dim btnChevron As cButtonInt
   
   Set barAddOrRemove = BarAdd("CHEVRON:ADDORREMOVE")
   Set btnInt = ButtonAdd("CHEVRON:ADDORREMOVE:SEPARATOR")
   btnInt.Style = eSeparator
   barAddOrRemove.Add btnInt
   Set btnInt = ButtonAdd("CHEVRON:ADDORREMOVE:RESET")
   btnInt.Caption = "&Reset Toolbar"
   btnInt.Enabled = False
   btnInt.VisibleCheck = Gray
   barAddOrRemove.Add btnInt
   
   Set barAddOrRemoveBar = BarAdd("CHEVRON:ADDORREMOVEBAR")
   Set btnAddOrRemoveBar = ButtonAdd("CHEVRON:ADDORREMOVEBAR")
   btnAddOrRemoveBar.Caption = "CommandBar Name"
   btnAddOrRemoveBar.SetBar barAddOrRemove
   barAddOrRemoveBar.Add btnAddOrRemoveBar
   Set btnInt = ButtonAdd("CHEVRON:SEPARATOR")
   btnInt.Style = eSeparator
   barAddOrRemoveBar.Add btnInt
   Set btnInt = ButtonAdd("CHEVRON:CUSTOMISE")
   btnInt.Caption = "&Customise..."
   btnInt.Enabled = False
   barAddOrRemoveBar.Add btnInt

   Set barChevron = BarAdd("CHEVRON")
   Set btnChevron = ButtonAdd("CHEVRON:ADDORREMOVE")
   btnChevron.Caption = "&Add or Remove Buttons"
   btnChevron.SetBar barAddOrRemoveBar
   barChevron.Add btnChevron
      
End Sub

Public Sub AddRef(ByVal hWnd As Long, ctlCmdBar As vbalCommandBar)
   If (m_colhWnd Is Nothing) Then
      ColourInitialise
      Debug.Print "PREPARE FOR INVASION"
      VerInitialise
      Set m_colhWnd = New Collection
      Set m_colCommandBars = New Collection
      Set m_colButtons = New Collection
      AttachKeyboardHook 0
      InitTheme hWnd
      CreateChevronBars
   End If
   m_colhWnd.Add hWnd, "H" & hWnd
   ' tag control with object pointer:
   TagControl hWnd, ctlCmdBar, True
End Sub

Public Sub ReleaseRef(ByVal hWnd As Long)
   m_colhWnd.Remove "H" & hWnd
   ' untag control
   TagControl hWnd, Nothing, False
   If (m_colhWnd.Count = 0) Then
      
      On Error Resume Next
      SetInMenuLoop False, 0
      
      ' JIC
      DetachMouseHook 0
      
      DetachKeyboardHook 0

      Set m_colhWnd = Nothing
      
      Dim barInt As cCommandBarInt
      For Each barInt In m_colCommandBars
         barInt.Dispose
      Next
      Set m_colCommandBars = Nothing
      Dim btnInt As cButtonInt
      For Each btnInt In m_colButtons
         btnInt.Dispose
      Next
      Set m_colButtons = Nothing
      releaseCachedControlInstances
            
      Debug.Print "GAME OVER"
   End If
End Sub

Public Function BarCount() As Long
   BarCount = m_colCommandBars.Count
End Function

Public Sub BarRemove(ByVal sKey As String)
   If CollectionContains(m_colCommandBars, sKey) Then
      Dim barInt As cCommandBarInt
      Set barInt = m_colCommandBars(sKey)
      barInt.Clear
      m_colCommandBars.Remove sKey
   Else
      gErr 3
   End If
End Sub
Public Property Get BarItem(index As Variant) As cCommandBarInt
   Set BarItem = m_colCommandBars.Item(index)
End Property
Public Function BarAdd(ByVal sKey As String) As cCommandBarInt
   If CollectionContains(m_colCommandBars, sKey) Then
      gErr 5
   ElseIf (IsNumeric(sKey)) Then
      gErr 4
   Else
      Dim barInt As New cCommandBarInt
      barInt.fInit sKey
      m_colCommandBars.Add barInt, sKey
      Set BarAdd = barInt
   End If
End Function

Public Function ButtonCount() As Long
   ButtonCount = m_colButtons.Count
End Function
Public Sub ButtonRemove(ByVal sKey As String)
   If CollectionContains(m_colButtons, sKey) Then
      Dim btn As cButtonInt
      Set btn = m_colButtons(sKey)
      btn.Deleted
      m_colButtons.Remove sKey
   Else
      gErr 3
   End If
End Sub
Public Property Get ButtonItem(index As Variant) As cButtonInt
   Set ButtonItem = m_colButtons.Item(index)
End Property
Public Function ButtonAdd(ByVal sKey As String) As cButtonInt
   If CollectionContains(m_colButtons, sKey) Then
      gErr 5
   ElseIf (IsNumeric(sKey)) Then
      gErr 4
   Else
      Dim btnInt As New cButtonInt
      btnInt.fInit sKey
      m_colButtons.Add btnInt, sKey
      Set ButtonAdd = btnInt
   End If
End Function

