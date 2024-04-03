Attribute VB_Name = "mCommandBarUtility"
Option Explicit


Public Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (lpvDest As Any, lpvSource As Any, ByVal cbCopy As Long)
Private Declare Function GetProp Lib "user32" Alias "GetPropA" (ByVal hWnd As Long, ByVal lpString As String) As Long
Private Declare Function SetProp Lib "user32" Alias "SetPropA" (ByVal hWnd As Long, ByVal lpString As String, ByVal hData As Long) As Long
Private Declare Function RemoveProp Lib "user32" Alias "RemovePropA" (ByVal hWnd As Long, ByVal lpString As String) As Long

Private Const CTLOBJECTPOINTERPROPNAME As String = "vbalCommandBar:Control"

Public Const COMMANDBARSIZESTYLEMENU As Long = 1
Public Const COMMANDBARSIZESTYLEMENUVISIBLECHECK As Long = 2
Public Const COMMANDBARSIZESTYLETOOLBARMENU As Long = 3
Public Const COMMANDBARSIZESTYLETOOLBAR As Long = 4
Public Const COMMANDBARSIZESTYLETOOLBARWRAPPABLE As Long = 5

Public Const CHANGENOTIFICATIONBARCONTENTCHANGE = 1
Public Const CHANGENOTIFICATIONBARTITLECHANGE = 3
Public Const CHANGENOTIFICATIONBUTTONSIZECHANGE = 4
Public Const CHANGENOTIFICATIONBUTTONREDRAW = 5
Public Const CHANGENOTIFICATIONBUTTONCHECKCHANGE = 6

Private Declare Function OleTranslateColor Lib "OLEPRO32.DLL" (ByVal OLE_COLOR As Long, ByVal HPALETTE As Long, pccolorref As Long) As Long
Public Const CLR_INVALID = -1
Public Const CLR_NONE = CLR_INVALID
Private Declare Function GetVersion Lib "kernel32" () As Long

Private Type OSVERSIONINFO
   dwVersionInfoSize As Long
   dwMajorVersion As Long
   dwMinorVersion As Long
   dwBuildNumber As Long
   dwPlatformId As Long
   szCSDVersion(0 To 127) As Byte
End Type
Private Declare Function GetVersionEx Lib "kernel32" Alias "GetVersionExA" (lpVersionInfo As OSVERSIONINFO) As Long
Private Const VER_PLATFORM_WIN32_NT = 2

Private Type TRIVERTEX
   x As Long
   y As Long
   Red As Integer
   Green As Integer
   Blue As Integer
   Alpha As Integer
End Type
Private Type GRADIENT_RECT
    UpperLeft As Long
    LowerRight As Long
End Type
Private Type GRADIENT_TRIANGLE
    Vertex1 As Long
    Vertex2 As Long
    Vertex3 As Long
End Type
Private Declare Function GradientFill Lib "msimg32" ( _
   ByVal hdc As Long, _
   pVertex As TRIVERTEX, _
   ByVal dwNumVertex As Long, _
   pMesh As GRADIENT_RECT, _
   ByVal dwNumMesh As Long, _
   ByVal dwMode As Long) As Long
Private Declare Function GradientFillTriangle Lib "msimg32" Alias "GradientFill" ( _
   ByVal hdc As Long, _
   pVertex As TRIVERTEX, _
   ByVal dwNumVertex As Long, _
   pMesh As GRADIENT_TRIANGLE, _
   ByVal dwNumMesh As Long, _
   ByVal dwMode As Long) As Long
Private Const GRADIENT_FILL_TRIANGLE = &H2&

Public Enum GradientFillRectType
   GRADIENT_FILL_RECT_H = 0
   GRADIENT_FILL_RECT_V = 1
End Enum

Public Declare Function MoveToEx Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, lpPoint As POINTAPI) As Long
Public Declare Function LineTo Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long) As Long

Public Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long
Public Declare Function ScreenToClient Lib "user32" (ByVal hWnd As Long, lpPoint As POINTAPI) As Long
Public Declare Function ClientToScreen Lib "user32" (ByVal hWnd As Long, lpPoint As POINTAPI) As Long

Private Const LF_FACESIZE = 32
Public Type LOGFONT
    lfHeight As Long ' The font size (see below)
    lfWidth As Long ' Normally you don't set this, just let Windows create the default
    lfEscapement As Long ' The angle, in 0.1 degrees, of the font
    lfOrientation As Long ' Leave as default
    lfWeight As Long ' Bold, Extra Bold, Normal etc
    lfItalic As Byte ' As it says
    lfUnderline As Byte ' As it says
    lfStrikeOut As Byte ' As it says
    lfCharSet As Byte ' As it says
    lfOutPrecision As Byte ' Leave for default
    lfClipPrecision As Byte ' Leave for default
    lfQuality As Byte ' Leave as default (see end of article)
    lfPitchAndFamily As Byte ' Leave as default (see end of article)
    lfFaceName(LF_FACESIZE) As Byte ' The font name converted to a byte array
End Type
Private Declare Function GetDeviceCaps Lib "gdi32" ( _
        ByVal hdc As Long, ByVal nIndex As Long _
    ) As Long
    Private Const LOGPIXELSX = 88 ' Logical pixels/inch in X
    Private Const LOGPIXELSY = 90 ' Logical pixels/inch in Y
Private Declare Function MulDiv Lib "kernel32" ( _
    ByVal nNumber As Long, ByVal nNumerator As Long, ByVal nDenominator As Long _
    ) As Long
Public Declare Function CreateFontIndirect Lib "gdi32" Alias "CreateFontIndirectA" ( _
        lpLogFont As LOGFONT _
    ) As Long
Private Const FW_NORMAL = 400
Private Const FW_BOLD = 700
Private Const FF_DONTCARE = 0

Private Const DEFAULT_PITCH = 0
Private Const DEFAULT_CHARSET = 1

Private Const DEFAULT_QUALITY = 0 ' Appearance of the font is set to default
Private Const DRAFT_QUALITY = 1 ' Appearance is less important that PROOF_QUALITY.
Private Const PROOF_QUALITY = 2 ' Best character quality
Private Const NONANTIALIASED_QUALITY = 3 ' Don't smooth font edges even if system is set to smooth font edges
Private Const ANTIALIASED_QUALITY = 4 ' Ensure font edges are smoothed if system is set to smooth font edges
Private Const CLEARTYPE_QUALITY = 5

Private Declare Function SetGraphicsMode Lib "gdi32" _
   (ByVal hdc As Long, ByVal iMode As Long) As Long
Private Const GM_ADVANCED = 2
Private Declare Function CreateRectRgnIndirect Lib "gdi32" (lpRect As RECT) As Long
Private Declare Function SelectClipRgn Lib "gdi32" (ByVal hdc As Long, ByVal hRgn As Long) As Long

Private Declare Function OpenThemeData Lib "uxtheme.dll" _
   (ByVal hWnd As Long, ByVal pszClassList As Long) As Long
Private Declare Function CloseThemeData Lib "uxtheme.dll" _
   (ByVal hTheme As Long) As Long
Private Declare Function DrawThemeBackground Lib "uxtheme.dll" _
   (ByVal hTheme As Long, ByVal lHDC As Long, _
    ByVal iPartId As Long, ByVal iStateId As Long, _
    pRect As RECT, pClipRect As RECT) As Long

Private m_bIsXp As Boolean
Private m_bIsNt As Boolean
Private m_bIs2000OrAbove As Boolean
Private m_bHasGradientAndTransparency As Boolean


Public Sub TagControl(ByVal hWnd As Long, ByRef ctl As vbalCommandBar, ByVal state As Boolean)
   If (state) Then
      SetProp hWnd, CTLOBJECTPOINTERPROPNAME, ObjPtr(ctl)
   Else
      RemoveProp hWnd, CTLOBJECTPOINTERPROPNAME
   End If
End Sub

Public Function ControlFromhWnd(ByVal hWnd As Long, ByRef ctl As vbalCommandBar) As Boolean
   Dim lPtr As Long
   If Not (hWnd = 0) Then
      If IsWindow(hWnd) Then
         lPtr = GetProp(hWnd, CTLOBJECTPOINTERPROPNAME)
         If Not (lPtr = 0) Then
            Set ctl = ObjectFromPtr(lPtr)
            ControlFromhWnd = True
            Exit Function
         End If
      End If
   End If
   gErr 2

End Function

Public Sub gErr(ByVal lErr As Long)
Dim sDesc As String
Dim lErrNum As Long
Const lBase As Long = vbObjectError + 25260

   Select Case lErr
   Case 1
      ' Cannot find owner object
      lErrNum = 364
      sDesc = "Object has been unloaded."
   Case 2
      ' Bar does not exist
      lErrNum = lBase + lErr
      sDesc = "Owning vbalCommandBar Control does not exist."
      
   Case 3
      ' Item does not exist
      lErrNum = lBase + lErr
      sDesc = "Item does not exist."
      
   Case 4
      ' Invalid key: numeric
      lErrNum = 13
      sDesc = "Type Mismatch."
      
   Case 5
      ' Invalid Key: duplicate
      lErrNum = 457
      sDesc = "This key is already associated with an element of this collection."
   
   Case 6
      ' Subscript out of range
      lErrNum = 9
      sDesc = "Subscript out of range."
   
   Case 7
      lErrNum = lBase + lErr
      sDesc = "Failed to add the item"

   Case 8
      lErrNum = 91
      sDesc = "Object variable or With block variable not set."

   Case Else
      Debug.Assert "Unexpected Error" = ""
      lErrNum = lErr + vbObjectError
   End Select
   
   
   Err.Raise lErrNum, App.EXEName & ".vbalPicker", sDesc
   
End Sub

Public Property Get ObjectFromPtr(ByVal lPtr As Long) As Object
Dim objT As Object
   If Not (lPtr = 0) Then
      ' Turn the pointer into an illegal, uncounted interface
      CopyMemory objT, lPtr, 4
      ' Do NOT hit the End button here! You will crash!
      ' Assign to legal reference
      Set ObjectFromPtr = objT
      ' Still do NOT hit the End button here! You will still crash!
      ' Destroy the illegal reference
      CopyMemory objT, 0&, 4
   End If
End Property

Public Function CollectionContains(col As Collection, ByVal Key As String) As Boolean
Dim v As Variant
Dim lErr As Long
   On Error Resume Next
   If VarType(col.Item(Key)) Then
   End If
   lErr = Err.Number
   On Error GoTo 0
   CollectionContains = (lErr = 0)
End Function

Public Sub UtilDrawIcon( _
      ByVal hdc As Long, _
      ByVal iml As cCommandBarImageList, _
      ByVal IconIndex As Long, _
      ByVal colourBox As OLE_COLOR, _
      ByVal iconX As Long, _
      ByVal iconY As Long, _
      ByVal eStyle As EIconProcessorStyle _
   )
Dim lFlags As Long
Dim lR As Long
Dim scaleIconX As Single
Dim scaleIconY As Single

   If (IconIndex <= -1) Then
   
      If Not (colourBox = CLR_NONE) Then
         Dim tR As RECT
         Dim hBr As Long
         
         tR.left = iconX '+ 1
         tR.top = iconY '+ 1
         tR.bottom = tR.top + 15
         tR.right = tR.left + 15
         hBr = CreateSolidBrush(TranslateColor(colourBox))
         FillRect hdc, tR, hBr
         DeleteObject hBr
         
         ' outline:
         UtilDrawBorderRectangle hdc, MenuBorderColor, tR.left, tR.top, tR.right - tR.left, tR.bottom - tR.top, False
      End If
      
   Else
      
      iml.Draw hdc, IconIndex, eStyle, iconX, iconY
      
   End If
   
End Sub

Public Sub UtilDrawBorderRectangle( _
      ByVal hdc As Long, _
      ByVal lColor As Long, _
      ByVal left As Long, _
      ByVal top As Long, _
      ByVal Width As Long, _
      ByVal Height As Long, _
      ByVal bInset As Boolean _
   )
Dim tJ As POINTAPI
Dim hPen As Long
Dim hPenOld As Long
   
   hPen = CreatePen(PS_SOLID, 1, lColor)
   hPenOld = SelectObject(hdc, hPen)
   MoveToEx hdc, left, top + Height - 1, tJ
   LineTo hdc, left, top
   LineTo hdc, left + Width - 1, top
   LineTo hdc, left + Width - 1, top + Height - 1
   LineTo hdc, left, top + Height - 1
   SelectObject hdc, hPenOld
   DeleteObject hPen
   
End Sub

Public Sub UtilDrawText( _
      ByVal hdc As Long, _
      ByVal sCaption As String, _
      ByVal lTextX As Long, _
      ByVal lTextY As Long, _
      ByVal lTextWidth As Long, _
      ByVal lTextHeight As Long, _
      ByVal bEnabled As Boolean, _
      ByVal color As Long, _
      ByVal Orientation As ECommandBarOrientation, _
      ByVal bCentreHorizontal As Boolean _
   )
Dim tR As RECT
Dim lFlags As Long
Dim tPOrg As POINTAPI
Dim bResetViewport As Boolean
Dim iPos As Long
Dim lPtr As Long
   
   If (Orientation = eBottom) Or (Orientation = eTop) Then
      tR.left = lTextX
      tR.top = lTextY
      tR.right = lTextX + lTextWidth
      tR.bottom = lTextY + lTextHeight
      lFlags = DT_SINGLELINE Or DT_VCENTER
      If (bCentreHorizontal) Then
         lFlags = lFlags Or DT_CENTER
      End If
   Else
      lFlags = DT_SINGLELINE
      If (m_bIsNt) Then
         tR.left = lTextX + lTextWidth
         tR.right = lTextX
         tR.top = lTextY
         tR.bottom = lTextY + lTextHeight + 4
      Else
         tR.left = lTextX
         tR.right = lTextX + lTextWidth
         tR.top = lTextY
         tR.bottom = lTextY + lTextHeight
         SetTextAlign hdc, TA_BASELINE
         SetViewportOrgEx hdc, tR.left, tR.bottom, tPOrg
         OffsetRect tR, 0, -tR.bottom
         bResetViewport = True
         Do
            iPos = InStr(sCaption, "&")
            If (iPos > 0) Then
               If (iPos = 1) Then
                  If (iPos < Len(sCaption)) Then
                     sCaption = Mid(sCaption, iPos + 1)
                  Else
                     sCaption = ""
                  End If
               ElseIf (iPos > 1) Then
                  If (iPos < Len(sCaption)) Then
                     sCaption = left(sCaption, iPos - 1) & Mid(sCaption, iPos + 1)
                  Else
                     sCaption = left(sCaption, iPos - 1)
                  End If
               End If
            End If
         Loop While (iPos > 0)
      End If
   End If
   
   SetBkMode hdc, TRANSPARENT
   SetTextColor hdc, color
   If (m_bIsNt) Then
      lPtr = StrPtr(sCaption)
      If Not (lPtr = 0) Then
         DrawTextW hdc, ByVal lPtr, -1, tR, lFlags
      End If
   Else
      DrawTextA hdc, sCaption, -1, tR, lFlags
   End If
   
   If (bResetViewport) Then
      SetViewportOrgEx hdc, tPOrg.x, tPOrg.y, ByVal 0&
   End If

End Sub

Public Sub UtilDrawSystemStyleButton( _
      ByVal hWnd As Long, _
      ByVal hdc As Long, _
      ByVal lLeft As Long, _
      ByVal lTop As Long, _
      ByVal lWidth As Long, _
      ByVal lHeight As Long, _
      ByVal bEnabled As Boolean, _
      ByVal bHot As Boolean, _
      ByVal eStyle As EButtonStyle, _
      ByVal bSplit As Boolean, _
      ByVal eOrientation As ECommandBarOrientation, _
      ByVal bChecked As Boolean, _
      ByVal bDown As Boolean _
   )
Dim hTheme As Long
Dim tR As RECT
Dim lPartId As Long
Dim lStateId As Long

   tR.left = lLeft
   tR.top = lTop
   tR.right = lLeft + lWidth
   tR.bottom = lTop + lHeight

   If (IsXp) Then
      hTheme = OpenThemeData(hWnd, StrPtr("TOOLBAR"))
   End If
   
   If Not (hTheme = 0) Then
      
      If (bEnabled) Then
         If (bDown) Then
            lStateId = 3
         Else
            If (bHot) Then
               If (bChecked) Then
                  lStateId = 6
               Else
                  lStateId = 2
               End If
            Else
               If (bChecked) Then
                  lStateId = 5
               Else
                  lStateId = 1
               End If
            End If
         End If
      Else
         lStateId = 4
      End If
      
      If (eStyle = eSeparator) Then
         If (eOrientation = eLeft) Or (eOrientation = eRight) Then
            lPartId = 6
         Else
            lPartId = 5
         End If
      ElseIf (eStyle = eSplit) Then
         If (bSplit) Then
            lPartId = 4
         Else
            lPartId = 3
         End If
      Else
         lPartId = 1
      End If
      
      DrawThemeBackground hTheme, hdc, _
         lPartId, _
         lStateId, _
         tR, tR
      CloseThemeData hTheme
   Else
   
   End If
   
End Sub

Public Sub UtilDrawSplitGlyph( _
      ByVal hdc As Long, _
      ByVal lLeft As Long, _
      ByVal lTop As Long, _
      ByVal lWidth As Long, _
      ByVal lHeight As Long, _
      ByVal bEnabled As Boolean, _
      ByVal color As Long, _
      ByVal Orientation As ECommandBarOrientation _
   )
Dim lCentreY As Long
Dim lCentreX As Long
   
   lCentreX = lLeft + lWidth \ 2
   lCentreY = lTop + lHeight \ 2

   If (Orientation = eLeft) Or (Orientation = eRight) Then
      SetPixel hdc, lCentreX + 1, lCentreY - 2, color
      SetPixel hdc, lCentreX + 1, lCentreY - 1, color
      SetPixel hdc, lCentreX + 1, lCentreY, color
      SetPixel hdc, lCentreX + 1, lCentreY + 1, color
      SetPixel hdc, lCentreX + 1, lCentreY + 2, color
      
      SetPixel hdc, lCentreX, lCentreY - 1, color
      SetPixel hdc, lCentreX, lCentreY, color
      SetPixel hdc, lCentreX, lCentreY + 1, color
      
      SetPixel hdc, lCentreX - 1, lCentreY, color
   Else
      SetPixel hdc, lCentreX - 2, lCentreY - 1, color
      SetPixel hdc, lCentreX - 1, lCentreY - 1, color
      SetPixel hdc, lCentreX, lCentreY - 1, color
      SetPixel hdc, lCentreX + 1, lCentreY - 1, color
      SetPixel hdc, lCentreX + 2, lCentreY - 1, color
      SetPixel hdc, lCentreX - 1, lCentreY, color
      SetPixel hdc, lCentreX, lCentreY, color
      SetPixel hdc, lCentreX + 1, lCentreY, color
      SetPixel hdc, lCentreX, lCentreY + 1, color
   End If
   
End Sub
Public Sub UtilDrawSubMenuGlyph( _
      ByVal hdc As Long, _
      ByVal lLeft As Long, _
      ByVal lTop As Long, _
      ByVal lWidth As Long, _
      ByVal lHeight As Long, _
      ByVal bEnabled As Boolean, _
      ByVal color As Long _
   )
Dim lCentreY As Long
Dim lCentreX As Long
Dim tJ As POINTAPI
Dim hPen As Long
Dim hPenOld As Long
   
   lCentreX = lLeft + lWidth \ 2
   lCentreY = lTop + lHeight \ 2
   
   hPen = CreatePen(PS_SOLID, 1, &H0)
   hPenOld = SelectObject(hdc, hPenOld)
   
   MoveToEx hdc, lCentreX - 2, lCentreY - 3, tJ
   LineTo hdc, lCentreX - 2, lCentreY + 4
   MoveToEx hdc, lCentreX - 1, lCentreY - 2, tJ
   LineTo hdc, lCentreX - 1, lCentreY + 3
   MoveToEx hdc, lCentreX, lCentreY - 1, tJ
   LineTo hdc, lCentreX, lCentreY + 2
   SetPixel hdc, lCentreX + 1, lCentreY, &H0
   
   SelectObject hdc, hPenOld
   DeleteObject hPen
   
End Sub
Public Sub UtilDrawCheckGlyph( _
      ByVal hdc As Long, _
      ByVal lLeft As Long, _
      ByVal lTop As Long, _
      ByVal lWidth As Long, _
      ByVal lHeight As Long, _
      ByVal bEnabled As Boolean, _
      ByVal color As Long _
   )
Dim lCentreY As Long
Dim lCentreX As Long
Dim tJ As POINTAPI
Dim hPen As Long
Dim hPenOld As Long
   
   lCentreX = lLeft + lWidth \ 2
   lCentreY = lTop + lHeight \ 2
   
   hPen = CreatePen(PS_SOLID, 1, &H0)
   hPenOld = SelectObject(hdc, hPenOld)
   
   MoveToEx hdc, lCentreX - 3, lCentreY, tJ
   LineTo hdc, lCentreX - 1, lCentreY + 2
   MoveToEx hdc, lCentreX - 3, lCentreY + 1, tJ
   LineTo hdc, lCentreX - 1, lCentreY + 3
   
   MoveToEx hdc, lCentreX - 1, lCentreY + 3, tJ
   LineTo hdc, lCentreX + 5, lCentreY - 3
   MoveToEx hdc, lCentreX - 1, lCentreY + 2, tJ
   LineTo hdc, lCentreX + 5, lCentreY - 4
   
   SelectObject hdc, hPenOld
   DeleteObject hPen
   
End Sub
Public Sub UtilDrawBackground( _
      ByVal hdc As Long, _
      ByVal colorStart As Long, _
      ByVal colorEnd As Long, _
      ByVal left As Long, _
      ByVal top As Long, _
      ByVal Width As Long, _
      ByVal Height As Long, _
      Optional ByVal horizontal As Boolean = False _
   )
   If (colorStart = -1) Or (colorEnd = -1) Then
      ' do nothing
   Else
      Dim tR As RECT
      tR.left = left
      tR.top = top
      tR.right = left + Width
      tR.bottom = top + Height
      If (colorStart = colorEnd) Or Not (m_bHasGradientAndTransparency) Then
         ' solid fill:
         Dim hBr As Long
         hBr = CreateSolidBrush(colorStart)
         FillRect hdc, tR, hBr
         DeleteObject hBr
      Else
         ' gradient fill vertical:
         GradientFillRect hdc, tR, _
            colorStart, colorEnd, _
            IIf(horizontal, GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V)
      End If
   End If
End Sub

Public Sub UtilDrawBackgroundPortion( _
      ByVal hWnd As Long, _
      ByVal hWndParent As Long, _
      ByVal hdc As Long, _
      ByVal colorStart As Long, _
      ByVal colorEnd As Long, _
      ByVal left As Long, _
      ByVal top As Long, _
      ByVal Width As Long, _
      ByVal Height As Long, _
      ByVal horizontal As Boolean, _
      ByVal systemStyle As Boolean _
   )
Dim hRgn As Long
Dim rcWindow As RECT
Dim rcControl As RECT
Dim tP As POINTAPI
Dim rcPortion As RECT
Dim rcDraw As RECT
Dim xpComCtlBack As Boolean
Dim hTheme As Long

   xpComCtlBack = (systemStyle And TrueColor)

   ' Can skip this under some cases:
   If Not (xpComCtlBack) Then
      If (colorStart = colorEnd) Or Not (m_bHasGradientAndTransparency) Then
         UtilDrawBackground hdc, colorStart, colorEnd, _
            left, top, Width, Height, horizontal
      Exit Sub
   End If
   
   End If

   ' Get bounding rectangle of parent (TODO: exclude borders and title bar)
   GetWindowRect hWndParent, rcWindow
   
   ' Get position of control relative to the screen:
   GetWindowRect hWnd, rcControl
   
   ' Offset the drawing rectangle so it the correct portion is
   ' drawn into the control
   LSet rcDraw = rcWindow
   OffsetRect rcDraw, -rcDraw.left, -rcDraw.top
   OffsetRect rcDraw, rcWindow.left - rcControl.left, rcWindow.top - rcControl.top
   
   ' Now set up a clip region in the control to only
   ' include the portion:
   rcPortion.left = left
   rcPortion.top = top
   rcPortion.right = left + Width
   rcPortion.bottom = top + Height
   hRgn = CreateRectRgnIndirect(rcPortion)
   ' Select the region into the DC:
   SelectClipRgn hdc, hRgn
   
   ' Now draw the entire background but with the portion clipped
      ' solid fill:
   If (xpComCtlBack) Then
      GradientFillRect hdc, rcDraw, _
         BlendColor(vb3DHighlight, vbButtonFace, 192), TranslateColor(vbButtonFace), _
         IIf(horizontal, GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V)
   Else
      ' gradient fill vertical:
      GradientFillRect hdc, rcDraw, _
         colorStart, colorEnd, _
         IIf(horizontal, GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V)
   End If
   
   
   ' Select the old region back in
   SelectClipRgn hdc, 0&
   DeleteObject hRgn
   
   
End Sub

Public Sub TileArea( _
        ByVal hDCTo As Long, _
        ByVal x As Long, _
        ByVal y As Long, _
        ByVal Width As Long, _
        ByVal Height As Long, _
        ByVal hDcSrc As Long, _
        ByVal SrcWidth As Long, _
        ByVal SrcHeight As Long, _
        ByVal lOffsetY As Long _
    )
Dim lSrcX As Long
Dim lSrcY As Long
Dim lSrcStartX As Long
Dim lSrcStartY As Long
Dim lSrcStartWidth As Long
Dim lSrcStartHeight As Long
Dim lDstX As Long
Dim lDstY As Long
Dim lDstWidth As Long
Dim lDstHeight As Long

    lSrcStartX = (x Mod SrcWidth)
    lSrcStartY = ((y + lOffsetY) Mod SrcHeight)
    lSrcStartWidth = (SrcWidth - lSrcStartX)
    lSrcStartHeight = (SrcHeight - lSrcStartY)
    lSrcX = lSrcStartX
    lSrcY = lSrcStartY
    
    lDstY = y
    lDstHeight = lSrcStartHeight
    
    Do While lDstY < (y + Height)
        If (lDstY + lDstHeight) > (y + Height) Then
            lDstHeight = y + Height - lDstY
        End If
        lDstWidth = lSrcStartWidth
        lDstX = x
        lSrcX = lSrcStartX
        Do While lDstX < (x + Width)
            If (lDstX + lDstWidth) > (x + Width) Then
                lDstWidth = x + Width - lDstX
                If (lDstWidth = 0) Then
                    lDstWidth = 4
                End If
            End If
            'If (lDstWidth > Width) Then lDstWidth = Width
            'If (lDstHeight > Height) Then lDstHeight = Height
            BitBlt hDCTo, lDstX, lDstY, lDstWidth, lDstHeight, hDcSrc, lSrcX, lSrcY, vbSrcCopy
            lDstX = lDstX + lDstWidth
            lSrcX = 0
            lDstWidth = SrcWidth
        Loop
        lDstY = lDstY + lDstHeight
        lSrcY = 0
        lDstHeight = SrcHeight
    Loop
End Sub

Private Sub GradientFillRect( _
      ByVal lHDC As Long, _
      tR As RECT, _
      ByVal oStartColor As OLE_COLOR, _
      ByVal oEndColor As OLE_COLOR, _
      ByVal eDir As GradientFillRectType _
   )
Dim hBrush As Long
Dim lStartColor As Long
Dim lEndColor As Long
Dim lR As Long
   
   ' Use GradientFill:
   If (HasGradientAndTransparency) Then
      lStartColor = TranslateColor(oStartColor)
      lEndColor = TranslateColor(oEndColor)
   
      Dim tTV(0 To 1) As TRIVERTEX
      Dim tGR As GRADIENT_RECT
      
      setTriVertexColor tTV(0), lStartColor
      tTV(0).x = tR.left
      tTV(0).y = tR.top
      setTriVertexColor tTV(1), lEndColor
      tTV(1).x = tR.right
      tTV(1).y = tR.bottom
      
      tGR.UpperLeft = 0
      tGR.LowerRight = 1
      
      GradientFill lHDC, tTV(0), 2, tGR, 1, eDir
      
   Else
      ' Fill with solid brush:
      hBrush = CreateSolidBrush(TranslateColor(oEndColor))
      FillRect lHDC, tR, hBrush
      DeleteObject hBrush
   End If
   
End Sub

Private Sub setTriVertexColor(tTV As TRIVERTEX, lColor As Long)
Dim lRed As Long
Dim lGreen As Long
Dim lBlue As Long
   lRed = (lColor And &HFF&) * &H100&
   lGreen = (lColor And &HFF00&)
   lBlue = (lColor And &HFF0000) \ &H100&
   setTriVertexColorComponent tTV.Red, lRed
   setTriVertexColorComponent tTV.Green, lGreen
   setTriVertexColorComponent tTV.Blue, lBlue
End Sub
Private Sub setTriVertexColorComponent(ByRef iColor As Integer, ByVal lComponent As Long)
   If (lComponent And &H8000&) = &H8000& Then
      iColor = (lComponent And &H7F00&)
      iColor = iColor Or &H8000
   Else
      iColor = lComponent
   End If
End Sub

Public Sub VerInitialise()
   
   Dim tOSV As OSVERSIONINFO
   tOSV.dwVersionInfoSize = Len(tOSV)
   GetVersionEx tOSV
   
   m_bIsNt = ((tOSV.dwPlatformId And VER_PLATFORM_WIN32_NT) = VER_PLATFORM_WIN32_NT)
   If (tOSV.dwMajorVersion > 5) Then
      m_bHasGradientAndTransparency = True
      m_bIsXp = True
      m_bIs2000OrAbove = True
   ElseIf (tOSV.dwMajorVersion = 5) Then
      m_bHasGradientAndTransparency = True
      m_bIs2000OrAbove = True
      If (tOSV.dwMinorVersion >= 1) Then
         m_bIsXp = True
      End If
   ElseIf (tOSV.dwMajorVersion = 4) Then ' NT4 or 9x/ME/SE
      If (tOSV.dwMinorVersion >= 10) Then
         m_bHasGradientAndTransparency = True
      End If
   Else ' Too old
   End If
   
End Sub
Public Property Get Is2000OrAbove() As Boolean
   Is2000OrAbove = m_bIs2000OrAbove
End Property
Public Property Get IsXp() As Boolean
   IsXp = m_bIsXp
End Property
Public Property Get IsNt() As Boolean
   IsNt = m_bIsNt
End Property
Public Property Get HasGradientAndTransparency()
   HasGradientAndTransparency = m_bHasGradientAndTransparency
End Property

Public Function TranslateColor(ByVal oClr As OLE_COLOR, _
                        Optional hPal As Long = 0) As Long
    ' Convert Automation color to Windows color
    If OleTranslateColor(oClr, hPal, TranslateColor) Then
        TranslateColor = CLR_INVALID
    End If
End Function

Public Property Get BlendColor( _
      ByVal oColorFrom As OLE_COLOR, _
      ByVal oColorTo As OLE_COLOR, _
      Optional ByVal Alpha As Long = 128 _
   ) As Long
Dim lCFrom As Long
Dim lCTo As Long
   lCFrom = TranslateColor(oColorFrom)
   lCTo = TranslateColor(oColorTo)
Dim lSrcR As Long
Dim lSrcG As Long
Dim lSrcB As Long
Dim lDstR As Long
Dim lDstG As Long
Dim lDstB As Long
   lSrcR = lCFrom And &HFF
   lSrcG = (lCFrom And &HFF00&) \ &H100&
   lSrcB = (lCFrom And &HFF0000) \ &H10000
   lDstR = lCTo And &HFF
   lDstG = (lCTo And &HFF00&) \ &H100&
   lDstB = (lCTo And &HFF0000) \ &H10000
     
   
   BlendColor = RGB( _
      ((lSrcR * Alpha) / 255) + ((lDstR * (255 - Alpha)) / 255), _
      ((lSrcG * Alpha) / 255) + ((lDstG * (255 - Alpha)) / 255), _
      ((lSrcB * Alpha) / 255) + ((lDstB * (255 - Alpha)) / 255) _
      )
      
End Property

Public Sub RGBToHLS( _
      ByVal r As Long, ByVal g As Long, ByVal b As Long, _
      h As Single, s As Single, l As Single _
   )
Dim Max As Single
Dim Min As Single
Dim delta As Single
Dim rR As Single, rG As Single, rB As Single

   rR = r / 255: rG = g / 255: rB = b / 255

'{Given: rgb each in [0,1].
' Desired: h in [0,360] and s in [0,1], except if s=0, then h=UNDEFINED.}
        Max = Maximum(rR, rG, rB)
        Min = Minimum(rR, rG, rB)
        l = (Max + Min) / 2    '{This is the lightness}
        '{Next calculate saturation}
        If Max = Min Then
            'begin {Acrhomatic case}
            s = 0
            h = 0
           'end {Acrhomatic case}
        Else
           'begin {Chromatic case}
                '{First calculate the saturation.}
           If l <= 0.5 Then
               s = (Max - Min) / (Max + Min)
           Else
               s = (Max - Min) / (2 - Max - Min)
            End If
            '{Next calculate the hue.}
            delta = Max - Min
           If rR = Max Then
                h = (rG - rB) / delta    '{Resulting color is between yellow and magenta}
           ElseIf rG = Max Then
                h = 2 + (rB - rR) / delta '{Resulting color is between cyan and yellow}
           ElseIf rB = Max Then
                h = 4 + (rR - rG) / delta '{Resulting color is between magenta and cyan}
            End If
            'Debug.Print h
            'h = h * 60
           'If h < 0# Then
           '     h = h + 360            '{Make degrees be nonnegative}
           'End If
        'end {Chromatic Case}
      End If
'end {RGB_to_HLS}
End Sub

Public Sub HLSToRGB( _
      ByVal h As Single, ByVal s As Single, ByVal l As Single, _
      r As Long, g As Long, b As Long _
   )
Dim rR As Single, rG As Single, rB As Single
Dim Min As Single, Max As Single

   If s = 0 Then
      ' Achromatic case:
      rR = l: rG = l: rB = l
   Else
      ' Chromatic case:
      ' delta = Max-Min
      If l <= 0.5 Then
         's = (Max - Min) / (Max + Min)
         ' Get Min value:
         Min = l * (1 - s)
      Else
         's = (Max - Min) / (2 - Max - Min)
         ' Get Min value:
         Min = l - s * (1 - l)
      End If
      ' Get the Max value:
      Max = 2 * l - Min
      
      ' Now depending on sector we can evaluate the h,l,s:
      If (h < 1) Then
         rR = Max
         If (h < 0) Then
            rG = Min
            rB = rG - h * (Max - Min)
         Else
            rB = Min
            rG = h * (Max - Min) + rB
         End If
      ElseIf (h < 3) Then
         rG = Max
         If (h < 2) Then
            rB = Min
            rR = rB - (h - 2) * (Max - Min)
         Else
            rR = Min
            rB = (h - 2) * (Max - Min) + rR
         End If
      Else
         rB = Max
         If (h < 4) Then
            rR = Min
            rG = rR - (h - 4) * (Max - Min)
         Else
            rG = Min
            rR = (h - 4) * (Max - Min) + rG
         End If
         
      End If
            
   End If
   r = rR * 255: g = rG * 255: b = rB * 255
End Sub
Private Function Maximum(rR As Single, rG As Single, rB As Single) As Single
   If (rR > rG) Then
      If (rR > rB) Then
         Maximum = rR
      Else
         Maximum = rB
      End If
   Else
      If (rB > rG) Then
         Maximum = rB
      Else
         Maximum = rG
      End If
   End If
End Function
Private Function Minimum(rR As Single, rG As Single, rB As Single) As Single
   If (rR < rG) Then
      If (rR < rB) Then
         Minimum = rR
      Else
         Minimum = rB
      End If
   Else
      If (rB < rG) Then
         Minimum = rB
      Else
         Minimum = rG
      End If
   End If
End Function

Public Sub DrawText( _
      ByVal lHDC As Long, _
      ByVal sText As String, _
      ByVal lLength As Long, _
      tR As RECT, _
      ByVal lFlags As Long _
   )
Dim lPtr As Long
   If (m_bIsNt) Then
      lPtr = StrPtr(sText)
      If Not (lPtr = 0) Then ' NT4 crashes with ptr = 0
         DrawTextW lHDC, lPtr, -1, tR, lFlags
      End If
   Else
      DrawTextA lHDC, sText, -1, tR, lFlags
   End If
End Sub


Public Function IFontOf(ifnt As IFont) As IFont
   Set IFontOf = ifnt
End Function

Public Sub OLEFontToLogFont(fntThis As StdFont, hdc As Long, tLF As LOGFONT)
Dim sFont As String
Dim iChar As Integer

   ' Convert an OLE StdFont to a LOGFONT structure:
   With tLF
      sFont = fntThis.Name
      ' There is a quicker way involving StrConv and CopyMemory, but
      ' this is simpler!:
      For iChar = 1 To Len(sFont)
          .lfFaceName(iChar - 1) = CByte(Asc(Mid$(sFont, iChar, 1)))
      Next iChar
      ' Based on the Win32SDK documentation:
      .lfHeight = -MulDiv((fntThis.Size), (GetDeviceCaps(hdc, LOGPIXELSY)), 72)
      .lfItalic = fntThis.Italic
      If (fntThis.Bold) Then
          .lfWeight = FW_BOLD
      Else
          .lfWeight = FW_NORMAL
      End If
      .lfUnderline = fntThis.Underline
      .lfStrikeOut = fntThis.Strikethrough
      ' Fix to ensure the correct character set is selected. Otherwise you
      ' cannot display Wingdings or international fonts:
      .lfCharSet = fntThis.Charset
        
      If (IsXp) Then
         .lfQuality = CLEARTYPE_QUALITY
      Else
         .lfQuality = ANTIALIASED_QUALITY
      End If
        
   End With

End Sub

