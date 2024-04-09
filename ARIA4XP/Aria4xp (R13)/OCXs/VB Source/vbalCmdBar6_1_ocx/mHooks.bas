Attribute VB_Name = "mHooks"
Option Explicit

Private Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Long) As Integer
Private Declare Function SetWindowsHookEx Lib "user32" Alias "SetWindowsHookExA" (ByVal idHook As Long, ByVal lpfn As Long, ByVal hmod As Long, ByVal dwThreadId As Long) As Long
Private Declare Function UnhookWindowsHookEx Lib "user32" (ByVal hHook As Long) As Long
Private Declare Function CallNextHookEx Lib "user32" (ByVal hHook As Long, ByVal nCode As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Private Declare Function GetCurrentThreadId Lib "kernel32" () As Long
Private Const WH_KEYBOARD As Long = 2
Private Const MSGF_MENU = 2
Private Const HC_ACTION = 0
Private Const WH_MOUSE As Long = 7
Private Const WM_MOUSEMOVE = &H200
Private Const WM_NCMOUSEMOVE = &HA0
Private Declare Function WindowFromPoint Lib "user32" (ByVal xPoint As Long, ByVal yPoint As Long) As Long
Private Const WM_LBUTTONDOWN = &H201
Private Const WM_MBUTTONDOWN = &H207
Private Const WM_RBUTTONDOWN = &H204
Private Const WM_NCLBUTTONDOWN = &HA1
Private Const WM_NCMBUTTONDOWN = &HA7
Private Const WM_NCRBUTTONDOWN = &HA4

Private m_iKeyHookCount As Long
Private m_lKeyHookhWnd() As Long
Private m_hKeyHook As Long

Private m_iMouseHookCount As Long
Private m_lMouseHookhWnd() As Long
Private m_hMouseHook As Long

Public Sub AttachMouseHook(ByVal hWnd As Long)
   
   Dim lpfn As Long
   If (m_iMouseHookCount = 0) Then
      lpfn = HookAddress(AddressOf MouseFilter)
      m_hMouseHook = SetWindowsHookEx(WH_MOUSE, lpfn, 0&, GetCurrentThreadId())
      Debug.Assert (m_hMouseHook <> 0)
   End If
   
   Dim i As Long
   For i = 1 To m_iMouseHookCount
      If hWnd = m_lMouseHookhWnd(i) Then
         ' we already have it
         'Debug.Assert False
         Exit Sub
      End If
   Next i
   
   If Not (m_hMouseHook = 0) Then
      ReDim Preserve m_lMouseHookhWnd(1 To m_iMouseHookCount + 1) As Long
      m_iMouseHookCount = m_iMouseHookCount + 1
      m_lMouseHookhWnd(m_iMouseHookCount) = hWnd
   End If
   
End Sub

Public Sub DetachMouseHook(ByVal hWnd As Long)
Dim i As Long
Dim lPtr As Long
Dim iThis As Long
   
   For i = 1 To m_iMouseHookCount
      If m_lMouseHookhWnd(i) = hWnd Then
         iThis = i
         Exit For
      End If
   Next i
   
   If Not (iThis = 0) Then
      If m_iMouseHookCount > 1 Then
         For i = iThis To m_iMouseHookCount - 1
            m_lMouseHookhWnd(i) = m_lMouseHookhWnd(i + 1)
         Next i
      End If
      m_iMouseHookCount = m_iMouseHookCount - 1
      If m_iMouseHookCount >= 1 Then
         ReDim Preserve m_lMouseHookhWnd(1 To m_iMouseHookCount) As Long
      Else
         Erase m_lMouseHookhWnd
      End If
   Else
      ' hmmm
   End If
   
   If m_iMouseHookCount <= 0 Then
      If Not (m_hMouseHook = 0) Then
         UnhookWindowsHookEx m_hMouseHook
         m_hMouseHook = 0
      End If
   End If

End Sub


Public Sub AttachKeyboardHook(ByVal hWnd As Long)
   
   Dim lpfn As Long
   If m_iKeyHookCount = 0 Then
      lpfn = HookAddress(AddressOf KeyboardFilter)
      m_hKeyHook = SetWindowsHookEx(WH_KEYBOARD, lpfn, 0&, GetCurrentThreadId())
      Debug.Assert (m_hKeyHook <> 0)
   End If
   
   Dim i As Long
   For i = 1 To m_iKeyHookCount
      If hWnd = m_lKeyHookhWnd(i) Then
         ' we already have it:
         Debug.Assert False
         Exit Sub
      End If
   Next i
      
   If Not (m_hKeyHook = 0) Then
      ReDim Preserve m_lKeyHookhWnd(1 To m_iKeyHookCount + 1) As Long
      m_iKeyHookCount = m_iKeyHookCount + 1
      m_lKeyHookhWnd(m_iKeyHookCount) = hWnd
   End If
   
End Sub

Public Sub DetachKeyboardHook(ByVal hWnd As Long)
Dim i As Long
Dim lPtr As Long
Dim iThis As Long
   
   For i = 1 To m_iKeyHookCount
      If m_lKeyHookhWnd(i) = hWnd Then
         iThis = i
         Exit For
      End If
   Next i
   
   If Not (iThis = 0) Then
      If m_iKeyHookCount > 1 Then
         For i = iThis To m_iKeyHookCount - 1
            m_lKeyHookhWnd(i) = m_lKeyHookhWnd(i + 1)
         Next i
      End If
      m_iKeyHookCount = m_iKeyHookCount - 1
      If m_iKeyHookCount >= 1 Then
         ReDim Preserve m_lKeyHookhWnd(1 To m_iKeyHookCount) As Long
      Else
         Erase m_lKeyHookhWnd
      End If
   Else
      ' hmmm
   End If
   
   If m_iKeyHookCount <= 0 Then
      If Not (m_hKeyHook = 0) Then
         UnhookWindowsHookEx m_hKeyHook
         m_hKeyHook = 0
      End If
   End If

End Sub

Private Function HookAddress(ByVal lPtr As Long) As Long
   HookAddress = lPtr
End Function

Private Function KeyboardFilter(ByVal nCode As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Dim bProcessed As Boolean
Dim bConsume As Boolean
Dim bKeyUp As Boolean
Dim bShift As Boolean
Dim bAlt As Boolean
Dim bCtrl As Boolean
Dim shiftState As Long
Dim ctl As vbalCommandBar
Dim hWndActiveMenu As Long
Dim lhWndActiveMenu As Long
Dim bGotToEnd As Boolean

On Error GoTo ErrorHandler

   If nCode = HC_ACTION And m_iKeyHookCount > 0 Then
   
      bKeyUp = ((lParam And &H80000000) = &H80000000)
      bShift = (GetAsyncKeyState(vbKeyShift) <> 0)
      bAlt = (GetAsyncKeyState(vbKeyMenu) <> 0) Or (wParam = vbKeyMenu)
      bCtrl = (GetAsyncKeyState(vbKeyControl) <> 0)
      
      shiftState = Abs(bShift * vbShiftMask) Or Abs(bCtrl * vbCtrlMask) Or Abs(bAlt * vbAltMask)
      
      If (InMenuLoop) Then
         hWndActiveMenu = ActiveMenu
         If (bAlt And Not (bKeyUp)) Then
            bProcessed = True
            SetInMenuLoop False, 0
         Else
            If (ControlFromhWnd(hWndActiveMenu, ctl)) Then
               If Not (bKeyUp) Then
                  HighlightDisabledItems = True
                  ctl.fKeyDown wParam, shiftState
                  bConsume = True
                  bProcessed = True
               End If
            End If
         End If
      Else
         If (bAlt) Then
            If (bKeyUp) Then

               ' entering menu loop with no item showing:
               lhWndActiveMenu = FindActiveMenuControl()
               If Not (lhWndActiveMenu = 0) Then
                  If ControlFromhWnd(lhWndActiveMenu, ctl) Then
                     ActiveMenu = lhWndActiveMenu
                     If (wParam = vbKeyMenu) Then
                        ctl.fTrack 0, 1, , True
                     End If
                  End If
                  SetInMenuLoop True, lhWndActiveMenu
                  If Not (wParam = vbKeyMenu) Then
                     If Not (ctl Is Nothing) Then
                        ctl.fKeyDown wParam, shiftState
                     End If
                  End If
                  bConsume = True
                  bProcessed = True
               End If
            Else
               If Not (wParam = vbKeyMenu) Then
                  lhWndActiveMenu = FindActiveMenuControl()
                  If Not (lhWndActiveMenu = 0) Then
                     If ControlFromhWnd(lhWndActiveMenu, ctl) Then
                        ActiveMenu = lhWndActiveMenu
                        SetInMenuLoop True, lhWndActiveMenu
                        ctl.fKeyDown wParam, shiftState
                     End If
                  End If
               End If
            End If
         End If
      End If
   
      If Not (bProcessed) Then
         If Not (bKeyUp) Then
            bConsume = ProcessAccelerators(wParam, shiftState)
         End If
      End If
   
   End If


   bGotToEnd = True
   If (bConsume) Then
      KeyboardFilter = 1
   Else
      KeyboardFilter = CallNextHookEx(m_hKeyHook, nCode, wParam, lParam)
   End If
   Exit Function


ErrorHandler:
   Debug.Print "Keyboard Hook Error!", Err.Description, Err.Source
   If Not bGotToEnd Then
      On Error Resume Next
      KeyboardFilter = CallNextHookEx(m_hKeyHook, nCode, wParam, lParam)
   End If
   Exit Function
   
End Function

Private Function MouseFilter(ByVal nCode As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Dim bProcessed As Boolean

On Error GoTo ErrorHandler
   
   If (nCode = HC_ACTION) Then
      HighlightDisabledItems = False
      If (wParam = WM_LBUTTONDOWN) Or (wParam = WM_RBUTTONDOWN) Or (wParam = WM_MBUTTONDOWN) Or _
         (wParam = WM_NCLBUTTONDOWN) Or (wParam = WM_NCRBUTTONDOWN) Or (wParam = WM_NCMBUTTONDOWN) Then
         Dim tP As POINTAPI
         GetCursorPos tP
         Dim hWnd As Long
         hWnd = WindowFromPoint(tP.x, tP.y)
         If Not (hWnd = ActiveMenu) Then
            Dim ctl As vbalCommandBar
            On Error GoTo NoControl
            If (ControlFromhWnd(hWnd, ctl)) Then
               If (hWnd = menuInitiator) Then
                  ScreenToClient hWnd, tP
                  If (ctl.fHitTest(tP.x, tP.y) = 0) Then
                     SetInMenuLoop False, 0
                  End If
               ElseIf Not (ctl.fIsSetAsMenu) Then
                  SetInMenuLoop False, 0
               End If
            Else
NoControl:
               On Error GoTo ErrorHandler
               SetInMenuLoop False, 0
            End If
         End If
      End If
   End If
   
   bProcessed = True
   MouseFilter = CallNextHookEx(m_hMouseHook, nCode, wParam, lParam)
   Exit Function


ErrorHandler:
   Debug.Print "Mouse Hook Error!"
   If Not bProcessed Then
      On Error Resume Next
      MouseFilter = CallNextHookEx(m_hMouseHook, nCode, wParam, lParam)
   End If
   Exit Function
   
End Function

