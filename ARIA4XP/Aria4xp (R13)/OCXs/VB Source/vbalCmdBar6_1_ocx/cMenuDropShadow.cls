VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "cMenuDropShadow"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit


Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" ( _
    lpvDest As Any, lpvSource As Any, ByVal cbCopy As Long)

Private Type SAFEARRAYBOUND
    cElements As Long
    lLbound As Long
End Type
Private Type SAFEARRAY2D
    cDims As Integer
    fFeatures As Integer
    cbElements As Long
    cLocks As Long
    pvData As Long
    Bounds(0 To 1) As SAFEARRAYBOUND
End Type
Private Declare Function VarPtrArray Lib "msvbvm50.dll" Alias "VarPtr" (Ptr() As Any) As Long

Private Type RGBQUAD
    rgbBlue As Byte
    rgbGreen As Byte
    rgbRed As Byte
    rgbReserved As Byte
End Type
Private Type BITMAPINFOHEADER '40 bytes
    biSize As Long
    biWidth As Long
    biHeight As Long
    biPlanes As Integer
    biBitCount As Integer
    biCompression As Long
    biSizeImage As Long
    biXPelsPerMeter As Long
    biYPelsPerMeter As Long
    biClrUsed As Long
    biClrImportant As Long
End Type
Private Type BITMAPINFO
    bmiHeader As BITMAPINFOHEADER
    bmiColors As RGBQUAD
End Type
Private Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hDC As Long) As Long
Private Declare Function GetDC Lib "user32" (ByVal hwnd As Long) As Long
Private Declare Function GetDesktopWindow Lib "user32" () As Long
Private Declare Function ReleaseDC Lib "user32" (ByVal hwnd As Long, ByVal hDC As Long) As Long
Private Declare Function GetClientRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT) As Long

' Note - this is not the declare in the API viewer - modify lplpVoid to be
' Byref so we get the pointer back:
Private Declare Function CreateDIBSection Lib "gdi32" _
    (ByVal hDC As Long, _
    pBitmapInfo As BITMAPINFO, _
    ByVal un As Long, _
    lplpVoid As Long, _
    ByVal handle As Long, _
    ByVal dw As Long) As Long

Private Const BI_RGB = 0&
Private Const BI_RLE4 = 2&
Private Const BI_RLE8 = 1&
Private Const DIB_RGB_COLORS = 0 '  color table in RGBs

Private Type POINTAPI
   x As Long
   y As Long
End Type

Private Type SIZEAPI
   cx As Long
   cy As Long
End Type

Private Type RECT
   left As Long
   top As Long
   right As Long
   bottom As Long
End Type

Private Type BLENDFUNCTION
   BlendOp As Byte
   BlendFlags As Byte
   SourceConstantAlpha As Byte
   AlphaFormat As Byte
End Type

Private Const AC_SRC_OVER As Long = &H0&
Private Const ULW_COLORKEY As Long = &H1&
Private Const ULW_ALPHA As Long = &H2&
Private Const ULW_OPAQUE As Long = &H4&
Private Const AC_SRC_ALPHA = &H1


Private Const WS_EX_TOPMOST As Long = &H8&
Private Const WS_EX_TRANSPARENT  As Long = &H20&
Private Const WS_EX_TOOLWINDOW As Long = &H80&
Private Const WS_EX_LAYERED As Long = &H80000
Private Const WS_POPUP = &H80000000
Private Const WS_VISIBLE = &H10000000
Private Const SPI_GETDROPSHADOW  As Long = &H1024&

Private Declare Function CreateWindowEX Lib "user32" Alias "CreateWindowExA" (ByVal dwExStyle As Long, ByVal lpClassName As String, ByVal lpWindowName As String, ByVal dwStyle As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hWndParent As Long, ByVal hMenu As Long, ByVal hInstance As Long, lpParam As Any) As Long
Private Declare Function DestroyWindow Lib "user32" (ByVal hwnd As Long) As Long

Private Declare Function SystemParametersInfo Lib "user32" Alias "SystemParametersInfoA" (ByVal uAction As Long, ByVal uParam As Long, ByRef lpvParam As Any, ByVal fuWinIni As Long) As Long
Private Declare Function UpdateLayeredWindow Lib "user32" ( _
    ByVal hwnd As Long, _
    ByVal hdcDst As Long, _
    pptDst As Any, _
    psize As Any, _
    ByVal hdcSrc As Long, _
    pptSrc As Any, _
    ByVal crKey As Long, _
    pblend As BLENDFUNCTION, _
    ByVal dwFlags As Long) As Long
Private Const GWL_STYLE = (-16)
Private Const GWL_EXSTYLE = (-20)
Private Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long

Private Declare Function IsWindowVisible Lib "user32" (ByVal hwnd As Long) As Long
Private Declare Function GetWindowRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT) As Long

Private Declare Function SelectObject Lib "gdi32" (ByVal hDC As Long, ByVal hObject As Long) As Long
Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long

Private Declare Function SetWindowPos Lib "user32" (ByVal hwnd As Long, ByVal hWndInsertAfter As Long, ByVal x As Long, ByVal y As Long, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long
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

Private Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long

Private Const SS_OWNERDRAW = &HD&
Private Const WM_PAINT = &HF&

Private Type DRAWITEMSTRUCT
   ctlType As Long
   ctlID As Long
   itemID As Long
   itemAction As Long
   itemState As Long
   hWndItem As Long
   hDC As Long
   rcItem As RECT
   itemData As Long
End Type

Public Enum EStaticShadowType
   ERightShadow
   EBottomShadow
End Enum

Private m_eShadowType As EStaticShadowType
Private m_lShadowSize As Long
Private m_x As Long
Private m_y As Long
Private m_lWidth As Long
Private m_lHeight As Long
Private m_hWnd As Long
Private m_hWndOwner As Long

Private m_tBI As BITMAPINFO
Private m_lPtr As Long
Private m_hDC As Long
Private m_hDib As Long
Private m_hBmpOld As Long
Private m_lShadowColor As Long

Implements ISubclass

Public Sub Initialise( _
      ByVal x As Long, ByVal y As Long, _
      ByVal lWidth As Long, ByVal lHeight As Long, _
      ByVal hWndOwner As Long _
   )
   m_x = x
   m_y = y
   m_lWidth = lWidth
   m_lHeight = lHeight
   m_hWndOwner = hWndOwner
End Sub

Public Property Get ShadowSize() As Long
   ShadowSize = m_lShadowSize
End Property
Public Property Let ShadowSize(ByVal lSize As Long)
   m_lShadowSize = lSize
End Property

Public Property Get ShadowColor() As Long
   ShadowColor = m_lShadowColor
End Property
Public Property Let ShadowColor(ByVal lColor As Long)
   m_lShadowColor = lColor
End Property

Public Property Get ShadowType() As EStaticShadowType
   ShadowType = m_eShadowType
End Property

Public Property Let ShadowType(ByVal value As EStaticShadowType)
   m_eShadowType = value
End Property

Public Property Get hwnd() As Long
   hwnd = m_hWnd
End Property

Private Function CreateDIB( _
      ByVal lhDC As Long, _
      ByVal lWidth As Long, _
      ByVal lHeight As Long, _
      ByRef hDib As Long _
   ) As Boolean
    
   With m_tBI.bmiHeader
      .biSize = Len(m_tBI.bmiHeader)
      .biWidth = lWidth
      .biHeight = lHeight
      .biPlanes = 1
      .biBitCount = 32
      .biCompression = BI_RGB
      .biSizeImage = BytesPerScanLine * .biHeight
   End With
   hDib = CreateDIBSection( _
           lhDC, _
           m_tBI, _
           DIB_RGB_COLORS, _
           m_lPtr, _
           0, 0)
   CreateDIB = (hDib <> 0)
End Function

Private Function CreateDisplay( _
        ByVal lWidth As Long, _
        ByVal lHeight As Long _
    ) As Boolean
   ClearUpDisplay
   m_hDC = CreateCompatibleDC(0)
   If (m_hDC <> 0) Then
       If (CreateDIB(m_hDC, lWidth, lHeight, m_hDib)) Then
           m_hBmpOld = SelectObject(m_hDC, m_hDib)
           CreateDisplay = True
       Else
           DeleteObject m_hDC
           m_hDC = 0
       End If
   End If
End Function
Private Sub ClearUpDisplay()
    If (m_hDC <> 0) Then
        If (m_hDib <> 0) Then
            SelectObject m_hDC, m_hBmpOld
            DeleteObject m_hDib
        End If
        DeleteObject m_hDC
    End If
    m_hDC = 0: m_hDib = 0: m_hBmpOld = 0: m_lPtr = 0
End Sub

Private Property Get BytesPerScanLine() As Long
    ' Scans must align on dword boundaries; easy with an alpha bitmap!
    BytesPerScanLine = m_tBI.bmiHeader.biWidth * 4
End Property

Private Property Get DibWidth() As Long
    DibWidth = m_tBI.bmiHeader.biWidth
End Property
Private Property Get DibHeight() As Long
    DibHeight = m_tBI.bmiHeader.biHeight
End Property

Private Sub CreateDropShadow( _
      ByVal bHorizontal As Boolean, _
      ByVal bLeftTop As Boolean _
   )
Dim bDib() As Byte
Dim x As Long, y As Long
Dim lC As Long, lInitC As Long, lSize As Long
Dim lR As Long, lG As Long, lB As Long
Dim lNewR As Long, lNewG As Long, lNewB As Long
Dim tSA As SAFEARRAY2D
    
   ' Get the bits in the from DIB section:
   With tSA
      .cbElements = 1
      .cDims = 2
      .Bounds(0).lLbound = 0
      .Bounds(0).cElements = m_tBI.bmiHeader.biHeight
      .Bounds(1).lLbound = 0
      .Bounds(1).cElements = BytesPerScanLine()
      .pvData = m_lPtr
   End With
   CopyMemory ByVal VarPtrArray(bDib()), VarPtr(tSA), 4
   
   lR = m_lShadowColor And &HFF&
   lG = (m_lShadowColor And &HFF00&) \ &H100&
   lB = (m_lShadowColor And &HFF0000) \ &H10000
    
   If (Is2000OrAbove) Then
   
      If (bHorizontal) Then
         ' horizontal:
         lSize = DibHeight
         If (bLeftTop) Then
         
         Else
            ' at bottom:
            For x = 0 To BytesPerScanLine - 1 Step 4
               ' if x is within lSize of the start or end, then
               ' it contributes to alpha:
               
               If (x < lSize * 4) Then
                  lInitC = (255 * x) \ (lSize * 4)
               ElseIf (x >= (BytesPerScanLine - lSize * 4)) Then
                  lInitC = (((BytesPerScanLine - x) * 255) \ (4 * lSize))
               Else
                  lInitC = 255
               End If
               
               For y = 0 To DibHeight - 1
                  lC = (lInitC * y) \ DibHeight
                  bDib(x + 3, y) = lC
                  bDib(x + 2, y) = lR * lC / &HFF&
                  bDib(x + 1, y) = lG * lC / &HFF&
                  bDib(x, y) = lB * lC / &HFF&
               Next y
               
            Next x
         End If
      Else
         ' vertical:
         lSize = BytesPerScanLine \ 4
         If (bLeftTop) Then
         Else
            ' at right:
            For y = 0 To DibHeight - 1
               ' if y is within lSize of the start or end, then
               ' it contributes to the alpha:
               
               'If (y < lSize) Then
               '   lInitC = (255 * y) \ lSize
               If (y >= (DibHeight - lSize)) Then
                  lInitC = (255 * (DibHeight - y)) \ lSize
               Else
                  lInitC = 255
               End If
               
               For x = 0 To BytesPerScanLine - 1 Step 4
                  ' the amount of alpha depends on how far we are from the left:
                  lC = (lInitC * (BytesPerScanLine - x)) \ BytesPerScanLine
                  bDib(x + 3, y) = lC
                  bDib(x + 2, y) = lR * lC / &HFF&
                  bDib(x + 1, y) = lG * lC / &HFF&
                  bDib(x, y) = lB * lC / &HFF&
               Next x
            Next y
         End If
      End If
      
   Else
      
      If (bHorizontal) Then
         ' horizontal:
         lSize = DibHeight
         If (bLeftTop) Then
         
         Else
            ' at bottom:
            For x = 0 To BytesPerScanLine - 1 Step 4
               ' if x is within lSize of the start or end, then
               ' it contributes to alpha:
               If (x < lSize * 4) Then
                  lInitC = (255 * x) \ (lSize * 4)
               ElseIf (x >= (BytesPerScanLine - lSize * 4)) Then
                  lInitC = (((BytesPerScanLine - x) * 255) \ (4 * lSize))
               Else
                  lInitC = 255
               End If
               
               For y = 0 To DibHeight - 1
                  lC = (lInitC * y) \ DibHeight
                  lC = lC * 3 / 8
                  bDib(x + 3, y) = 0 'lC
                  bDib(x + 2, y) = ((lR * lC) / &HFF&) + ((bDib(x + 2, y) * (&HFF& - lC)) / &HFF&)
                  bDib(x + 1, y) = ((lG * lC) / &HFF&) + ((bDib(x + 1, y) * (&HFF& - lC)) / &HFF&)
                  bDib(x, y) = ((lB * lC) / &HFF&) + ((bDib(x, y) * (&HFF& - lC)) / &HFF&)
               Next y
               
            Next x
         End If
      Else
         ' vertical:
         lSize = BytesPerScanLine \ 4
         If (bLeftTop) Then
         Else
            ' at right:
            For y = 0 To DibHeight - 1
               ' if y is within lSize of the start or end, then
               ' it contributes to the alpha:
               
               'If (y < lSize) Then
               '   lInitC = (255 * y) \ lSize
               If (y >= (DibHeight - lSize)) Then
                  lInitC = (255 * (DibHeight - y)) \ lSize
               Else
                  lInitC = 255
               End If
               
               For x = 0 To BytesPerScanLine - 1 Step 4
                  ' the amount of alpha depends on how far we are from the left:
                  lC = (lInitC * (BytesPerScanLine - x)) \ BytesPerScanLine
                  lC = lC * 3 / 8
                  bDib(x + 3, y) = 0 'lC
                  bDib(x + 2, y) = ((lR * lC) / &HFF&) + ((bDib(x + 2, y) * (&HFF& - lC)) / &HFF&)
                  bDib(x + 1, y) = ((lG * lC) / &HFF&) + ((bDib(x + 1, y) * (&HFF& - lC)) / &HFF&)
                  bDib(x, y) = ((lB * lC) / &HFF&) + ((bDib(x, y) * (&HFF& - lC)) / &HFF&)
               Next x
            Next y
         End If
      End If
   
   
   End If
    
   ' Clear the temporary array descriptor
   ' (This does not appear to be necessary, but
   ' for safety do it anyway)
   CopyMemory ByVal VarPtrArray(bDib), 0&, 4
    
End Sub

Public Sub Create()
   
   Destroy
   
   ' set as shadow form:
   Dim lExStyle As Long
   Dim lStyle As Long
   
   lExStyle = WS_EX_TOPMOST Or WS_EX_TOOLWINDOW
   If (Is2000OrAbove) Then
      lExStyle = lExStyle Or WS_EX_LAYERED Or WS_EX_TRANSPARENT
   End If
   lStyle = WS_POPUP Or SS_OWNERDRAW
   
   Dim tR As RECT
   If (m_eShadowType = EBottomShadow) Then
      tR.left = m_x + m_lShadowSize
      tR.top = m_y
      tR.right = tR.left + m_lWidth
      tR.bottom = m_y + m_lShadowSize + 1
   Else
      tR.left = m_x
      tR.top = m_y + m_lShadowSize
      tR.right = m_x + m_lShadowSize
      tR.bottom = tR.top + m_lHeight
   End If
      
   If Not (Is2000OrAbove) Then
      renderShadow tR, True
   End If
      
   m_hWnd = CreateWindowEX( _
      lExStyle, "Static", "", lStyle, _
      tR.left, tR.top, tR.right - tR.left, tR.bottom - tR.top, _
      m_hWndOwner, 0, App.hInstance, ByVal 0&)
   
   If Not (m_hWnd = 0) Then
      If (Is2000OrAbove) Then
         renderShadow tR, True
      Else
         AttachMessage Me, m_hWnd, WM_PAINT
      End If
      
      If (m_eShadowType = ERightShadow) Then
         SetWindowPos m_hWnd, HWND_TOPMOST, tR.left, tR.top, m_lWidth, m_lHeight, _
            SWP_SHOWWINDOW Or SWP_NOACTIVATE Or SWP_NOOWNERZORDER Or SWP_NOZORDER
      Else
         SetWindowPos m_hWnd, HWND_TOPMOST, tR.left, tR.top, m_lWidth, m_lHeight, _
            SWP_SHOWWINDOW Or SWP_NOACTIVATE Or SWP_NOOWNERZORDER Or SWP_NOZORDER
      End If
   End If
   
End Sub

Public Sub Destroy()
   If Not (m_hWnd = 0) Then
      DetachMessage Me, m_hWnd, WM_PAINT
      DestroyWindow m_hWnd
      m_hWnd = 0
   End If
   ClearUpDisplay
End Sub


Private Sub renderShadow(tR As RECT, ByVal bChange As Boolean)
   
   If bChange Then
      Dim tSize As SIZEAPI
      If (m_eShadowType = EBottomShadow) Then
         tSize.cx = (tR.right - tR.left)
         tSize.cy = m_lShadowSize
      Else
         tSize.cx = m_lShadowSize
         tSize.cy = (tR.bottom - tR.top) - m_lShadowSize
      End If
      CreateDisplay tSize.cx, tSize.cy
   End If
   
   If (Is2000OrAbove) Then
      
      If (bChange) Then
         If (m_eShadowType = EBottomShadow) Then
            CreateDropShadow True, False
         Else
            CreateDropShadow False, False
         End If
         
         ' Draw:
         Dim tBlend As BLENDFUNCTION
         tBlend.BlendOp = AC_SRC_OVER
         tBlend.BlendFlags = 0
         tBlend.AlphaFormat = AC_SRC_ALPHA
         tBlend.SourceConstantAlpha = 96
         Dim tPtSrc As POINTAPI
         tPtSrc.x = 0
         tPtSrc.y = 0
         
         UpdateLayeredWindow m_hWnd, ByVal 0&, ByVal 0&, tSize, m_hDC, tPtSrc, 0, _
                  tBlend, ULW_ALPHA
      End If
      
   Else
   
      Dim lhWndD As Long
      Dim lhDC As Long
      
      lhWndD = GetDesktopWindow()
      lhDC = GetDC(lhWndD)
      BitBlt m_hDC, 0, 0, tSize.cx, tSize.cy, lhDC, tR.left, tR.top, vbSrcCopy
      ReleaseDC 0, lhDC

      If (m_eShadowType = EBottomShadow) Then
         CreateDropShadow True, False
      Else
         CreateDropShadow False, False
      End If
         
   End If
   
End Sub

Private Sub Class_Initialize()
   m_lShadowSize = 5
   m_lShadowColor = RGB(0, 0, 192)
End Sub

Private Sub Class_Terminate()
   Destroy
End Sub

Private Property Let ISubclass_MsgResponse(ByVal RHS As EMsgResponse)
   '
End Property

Private Property Get ISubclass_MsgResponse() As EMsgResponse
   ISubclass_MsgResponse = emrPostProcess
End Property

Private Function ISubclass_WindowProc(ByVal hwnd As Long, ByVal iMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
   If (hwnd = m_hWnd) And (iMsg = WM_PAINT) Then
      Dim lhDC As Long
      Dim tR As RECT
      lhDC = GetDC(m_hWnd)
      GetClientRect m_hWnd, tR
      BitBlt lhDC, tR.left, tR.top, tR.right - tR.left, tR.bottom - tR.top, m_hDC, 0, 0, vbSrcCopy
      ReleaseDC m_hWnd, lhDC
   End If
End Function


