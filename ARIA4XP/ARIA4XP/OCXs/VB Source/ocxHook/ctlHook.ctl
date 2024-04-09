VERSION 5.00
Begin VB.UserControl ctlHook 
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   ScaleHeight     =   3600
   ScaleWidth      =   4800
End
Attribute VB_Name = "ctlHook"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Private Type POINTAPI
   x As Long
   y As Long
End Type

Private Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Long) As Long
Private Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long

Private Const WM_KEYDOWN = &H100
Private Const WM_KEYUP = &H101
Private Const WM_CHAR = &H102
Private Const WM_MOUSEMOVE = &H200
Private Const WM_LBUTTONDOWN = &H201
Private Const WM_LBUTTONUP = &H202
Private Const WM_LBUTTONDBLCLK = &H203
Private Const WM_RBUTTONDOWN = &H204
Private Const WM_RBUTTONUP = &H205
Private Const WM_RBUTTONDBLCLK = &H206
Private Const WM_MBUTTONDOWN = &H207
Private Const WM_MBUTTONUP = &H208
Private Const WM_MBUTTONDBLCLK = &H209
Private Const WM_MOUSEWHEEL = &H20A
Private Const WM_SYSTEMKEYDOWN = &H104
Private Const WM_SYSTEMKEYUP = &H105

Private m_lLastTime As Long
Private m_tP As POINTAPI
Private m_lPixels As Long
Private m_lKeys As Long

Public Event MouseDownOn(ByVal lhWnd As Long, ByVal lParamLow As Long, ByVal lParamHigh As Long, ByVal shiftState As String)
Public Event KeyUp(ByVal keyCode As Integer, ByVal shiftState As String)

Implements IWindowsHook

Public lngmyWndH As Long

Private Function shiftState() As String
Dim sRet As String
   sRet = "  "
   If Not (GetAsyncKeyState(vbKeyControl) = 0) Then
      sRet = sRet & "Ctrl"
   End If
   If Not (GetAsyncKeyState(vbKeyMenu) = 0) Then
      If Len(sRet) > 0 Then
         sRet = sRet & "+"
      End If
      sRet = sRet & "Alt"
   End If
   If Not (GetAsyncKeyState(vbKeyShift) = 0) Then
      If Len(sRet) > 0 Then
         sRet = sRet & "+"
      End If
      sRet = sRet & "Shift"
   End If
   shiftState = sRet
End Function

Public Sub MyInstallHook(ByVal bolSet As Boolean)
   If bolSet Then
      InstallHook Me, WH_JOURNALRECORD
      InstallHook Me, WH_GETMESSAGE
   Else
      RemoveHook Me, WH_GETMESSAGE
      RemoveHook Me, WH_JOURNALRECORD
   End If
End Sub

Private Function IWindowsHook_HookProc( _
      ByVal eType As vbalWinHook6.EHTHookTypeConstants, _
      ByVal nCode As Long, _
      ByVal wParam As Long, _
      ByVal lParam As Long, _
      bConsume As Boolean _
   ) As Long

   If (eType = WH_GETMESSAGE) Then
      Dim cMsg As cGetMsglParam
      On Error Resume Next
      Set cMsg = GetMsglParam(lParam)
      If (Err.Number) Then
      End If
      
      ' LogEvent "Journalling cancelled by Ctrl+Esc or Ctrl+Alt+Del; Removing Hook"
      If (cMsg.Message = WM_CANCELJOURNAL) Then
        Call MyInstallHook(False)
      End If
      
   
   ElseIf (eType = WH_JOURNALRECORD) Then
   
      Dim keyCode As Integer
      Dim keyAscii As Integer
      Dim sMsg As String
      Dim sMsgBit As String
      Dim lhWnd As Long
      Dim tP As POINTAPI
      
      Dim cEvent As cJournallParam
      Set cEvent = JournalRecordlParam(lParam)
      
      lhWnd = cEvent.hWnd
      If Not (lhWnd = 0) Then
         sMsgBit = "  over Window " & _
            Hex$(lhWnd)
      End If
            
      Select Case cEvent.Msg
      
      Case WM_KEYDOWN
         m_lKeys = m_lKeys + 1
         
         keyCode = (cEvent.lParamLow And &HFF&)
         ' LogEvent "KeyDown " & keyCode & shiftState & sMsgBit
         
      Case WM_SYSTEMKEYDOWN
         m_lKeys = m_lKeys + 1
      
         keyCode = (cEvent.lParamLow And &HFF&)
         If Not (keyCode = vbKeyMenu) Then
            ' LogEvent "SystemKeyDown " & keyCode & shiftState & sMsgBit
         End If
      
      Case WM_KEYUP
         keyCode = (cEvent.lParamLow And &HFF&)
         If Not (keyCode = vbKeyMenu) Then
            ' LogEvent "KeyUp " & keyCode & shiftState & sMsgBit
         End If
         RaiseEvent KeyUp(keyCode, shiftState)
      
      Case WM_SYSTEMKEYUP
         keyCode = (cEvent.lParamLow And &HFF&)
         ' LogEvent "SystemKeyUp " & keyCode & shiftState & sMsgBit
      
      
      Case WM_MOUSEMOVE
         GetCursorPos tP
         If Not (((m_tP.x - tP.x) = 0) And ((m_tP.y - tP.y) = 0)) Then
            m_lPixels = m_lPixels + Sqr((m_tP.x - tP.x) ^ 2 + (m_tP.y - tP.y) ^ 2)
            LSet m_tP = tP
         End If
               
      Case WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN
         sMsg = "Mouse Down: "
         Select Case cEvent.Msg
         Case WM_LBUTTONDOWN
            sMsg = sMsg & "Left Button"
         Case WM_RBUTTONDOWN
            sMsg = sMsg & "Right Button"
         Case WM_MBUTTONDOWN
            sMsg = sMsg & "Middle Button"
         End Select
         ' LogEvent sMsg & ", x=" & cEvent.lParamLow & ",y=" & cEvent.lParamHigh & shiftState & sMsgBit
         RaiseEvent MouseDownOn(lhWnd, cEvent.lParamLow, cEvent.lParamHigh, shiftState)
      
      Case WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP
         sMsg = "Mouse Up: "
         Select Case cEvent.Msg
         Case WM_LBUTTONUP
            sMsg = sMsg & "Left Button"
         Case WM_RBUTTONUP
            sMsg = sMsg & "Right Button"
         Case WM_MBUTTONUP
            sMsg = sMsg & "Middle Button"
         End Select
         ' LogEvent sMsg & ", x=" & cEvent.lParamLow & ",y=" & cEvent.lParamHigh & shiftState & sMsgBit
         
      Case WM_MOUSEWHEEL
         ' it does not seem to be possible to determine what mouse wheel action was being taken
         ' LogEvent "MouseWheel" & ", x=" & cEvent.lParamLow & ",y=" & cEvent.lParamHigh & shiftState & sMsgBit
      
      Case WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK, WM_MBUTTONDBLCLK
         sMsg = "Double Click: "
         Select Case cEvent.Msg
         Case WM_LBUTTONDBLCLK
            sMsg = sMsg & "Left Button"
         Case WM_RBUTTONDBLCLK
            sMsg = sMsg & "Right Button"
         Case WM_MBUTTONDBLCLK
            sMsg = sMsg & "Middle Button"
         End Select
         ' LogEvent sMsg & ", x=" & cEvent.lParamLow & ",y=" & cEvent.lParamHigh & shiftState & sMsgBit
               
      End Select
               
   End If
End Function

