VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "cVBALSysImageList"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

' =========================================================================
' vbAccelerator Image List Control Demonstrator
' Copyright � 1998 Steve McMahon (steve@dogma.demon.co.uk)
'
' Implements an Image List control in VB using COMCTL32.DLL
'
' Visit vbAccelerator at www.dogma.demon.co.uk
' =========================================================================

' -----------
' API
' -----------
' General:
Private Declare Function GetWindowWord Lib "user32" (ByVal hwnd As Long, ByVal nIndex As Long) As Integer
    Private Const GWW_HINSTANCE = (-6)
    
' GDI object functions:
Private Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Private Declare Function DestroyCursor Lib "user32" (ByVal hCursor As Long) As Long
Private Declare Function GetObjectAPI Lib "gdi32" Alias "GetObjectA" (ByVal hObject As Long, ByVal nCount As Long, lpObject As Any) As Long
Private Declare Function GetDC Lib "user32" (ByVal hwnd As Long) As Long
Private Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long
Private Declare Function ReleaseDC Lib "user32" (ByVal hwnd As Long, ByVal hdc As Long) As Long
Private Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hdc As Long) As Long
Private Declare Function CreateCompatibleBitmap Lib "gdi32" (ByVal hdc As Long, ByVal nWidth As Long, ByVal nHeight As Long) As Long
Private Declare Function GetDeviceCaps Lib "gdi32" (ByVal hdc As Long, ByVal nIndex As Long) As Long
    Private Const BITSPIXEL = 12
    Private Const LOGPIXELSX = 88    '  Logical pixels/inch in X
    Private Const LOGPIXELSY = 90    '  Logical pixels/inch in Y
' System metrics:
Private Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Long) As Long
    Private Const SM_CXICON = 11
    Private Const SM_CYICON = 12
    Private Const SM_CXFRAME = 32
    Private Const SM_CYCAPTION = 4
    Private Const SM_CYFRAME = 33
    Private Const SM_CYBORDER = 6
    Private Const SM_CXBORDER = 5

' Region paint and fill functions:
Private Declare Function PaintRgn Lib "gdi32" (ByVal hdc As Long, ByVal hRgn As Long) As Long
Private Declare Function ExtFloodFill Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal crColor As Long, ByVal wFillType As Long) As Long
    Private Const FLOODFILLBORDER = 0
    Private Const FLOODFILLSURFACE = 1
Private Declare Function GetPixel Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long) As Long

' Pen functions:
Private Declare Function CreatePen Lib "gdi32" (ByVal nPenStyle As Long, ByVal nWidth As Long, ByVal crColor As Long) As Long
    Private Const PS_DASH = 1
    Private Const PS_DASHDOT = 3
    Private Const PS_DASHDOTDOT = 4
    Private Const PS_DOT = 2
    Private Const PS_SOLID = 0
    Private Const PS_NULL = 5

' Brush functions:
Private Declare Function CreateSolidBrush Lib "gdi32" (ByVal crColor As Long) As Long
Private Declare Function CreatePatternBrush Lib "gdi32" (ByVal hBitmap As Long) As Long

' Line functions:
Private Declare Function LineTo Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long) As Long
Private Type POINTAPI
   x As Long
   y As Long
End Type
Private Declare Function MoveToEx Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, lpPoint As POINTAPI) As Long

' Colour functions:
Private Declare Function SetTextColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long
Private Declare Function SetBkColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long
Private Declare Function SetBkMode Lib "gdi32" (ByVal hdc As Long, ByVal nBkMode As Long) As Long
    Private Const OPAQUE = 2
    Private Const TRANSPARENT = 1
Private Declare Function GetSysColor Lib "user32" (ByVal nIndex As Long) As Long
    Private Const COLOR_ACTIVEBORDER = 10
    Private Const COLOR_ACTIVECAPTION = 2
    Private Const COLOR_ADJ_MAX = 100
    Private Const COLOR_ADJ_MIN = -100
    Private Const COLOR_APPWORKSPACE = 12
    Private Const COLOR_BACKGROUND = 1
    Private Const COLOR_BTNFACE = 15
    Private Const COLOR_BTNHIGHLIGHT = 20
    Private Const COLOR_BTNSHADOW = 16
    Private Const COLOR_BTNTEXT = 18
    Private Const COLOR_CAPTIONTEXT = 9
    Private Const COLOR_GRAYTEXT = 17
    Private Const COLOR_HIGHLIGHT = 13
    Private Const COLOR_HIGHLIGHTTEXT = 14
    Private Const COLOR_INACTIVEBORDER = 11
    Private Const COLOR_INACTIVECAPTION = 3
    Private Const COLOR_INACTIVECAPTIONTEXT = 19
    Private Const COLOR_MENU = 4
    Private Const COLOR_MENUTEXT = 7
    Private Const COLOR_SCROLLBAR = 0
    Private Const COLOR_WINDOW = 5
    Private Const COLOR_WINDOWFRAME = 6
    Private Const COLOR_WINDOWTEXT = 8
    Private Const COLORONCOLOR = 3

' Shell Extract icon functions:
Private Declare Function FindExecutable Lib "shell32.dll" Alias "FindExecutableA" (ByVal lpFile As String, ByVal lpDirectory As String, ByVal lpResult As String) As Long
Private Declare Function ExtractIcon Lib "shell32.dll" Alias "ExtractIconA" (ByVal hInst As Long, ByVal lpszExeFileName As String, ByVal nIconIndex As Long) As Long

' Icon functions:
Private Declare Function DrawIcon Lib "user32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal hIcon As Long) As Long
Private Declare Function DestroyIcon Lib "user32" (ByVal hIcon As Long) As Long
Private Declare Function DrawIconEx Lib "user32" (ByVal hdc As Long, ByVal xLeft As Long, ByVal yTop As Long, ByVal hIcon As Long, ByVal cxWidth As Long, ByVal cyWidth As Long, ByVal istepIfAniCur As Long, ByVal hbrFlickerFreeDraw As Long, ByVal diFlags As Long) As Boolean
Private Declare Function LoadImage Lib "user32" Alias "LoadImageA" (ByVal hInst As Long, ByVal lpsz As String, ByVal un1 As Long, ByVal n1 As Long, ByVal n2 As Long, ByVal un2 As Long) As Long
Private Declare Function LoadImageLong Lib "user32" Alias "LoadImageA" (ByVal hInst As Long, ByVal lpsz As Long, ByVal un1 As Long, ByVal n1 As Long, ByVal n2 As Long, ByVal un2 As Long) As Long
    Private Const LR_LOADMAP3DCOLORS = &H1000
    Private Const LR_LOADFROMFILE = &H10
    Private Const LR_LOADTRANSPARENT = &H20
    Private Const LR_COPYRETURNORG = &H4

' Blitting functions
Private Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long
    Private Const SRCAND = &H8800C6
    Private Const SRCCOPY = &HCC0020
    Private Const SRCERASE = &H440328
    Private Const SRCINVERT = &H660046
    Private Const SRCPAINT = &HEE0086
    Private Const BLACKNESS = &H42
    Private Const WHITENESS = &HFF0062
Private Declare Function PatBlt Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal dwRop As Long) As Long
Private Declare Function LoadBitmapBynum Lib "user32" Alias "LoadBitmapA" (ByVal hInstance As Long, ByVal lpBitmapName As Long) As Long
Private Type BITMAP '14 bytes
    bmType As Long
    bmWidth As Long
    bmHeight As Long
    bmWidthBytes As Long
    bmPlanes As Integer
    bmBitsPixel As Integer
    bmBits As Long
End Type
Private Declare Function CreateBitmapIndirect Lib "gdi32" (lpBitmap As BITMAP) As Long

' Text functions:
Private Type RECT
    left As Long
    tOp As Long
    Right As Long
    Bottom As Long
End Type
Private Declare Function PtInRect Lib "user32" (lpRect As RECT, ByVal ptX As Long, ByVal ptY As Long) As Long
Private Declare Function DrawText Lib "user32" Alias "DrawTextA" (ByVal hdc As Long, ByVal lpStr As String, ByVal nCount As Long, lpRect As RECT, ByVal wFormat As Long) As Long
    Private Const DT_BOTTOM = &H8&
    Private Const DT_CENTER = &H1&
    Private Const DT_LEFT = &H0&
    Private Const DT_CALCRECT = &H400&
    Private Const DT_WORDBREAK = &H10&
    Private Const DT_VCENTER = &H4&
    Private Const DT_TOP = &H0&
    Private Const DT_TABSTOP = &H80&
    Private Const DT_SINGLELINE = &H20&
    Private Const DT_RIGHT = &H2&
    Private Const DT_NOCLIP = &H100&
    Private Const DT_INTERNAL = &H1000&
    Private Const DT_EXTERNALLEADING = &H200&
    Private Const DT_EXPANDTABS = &H40&
    Private Const DT_CHARSTREAM = 4&
    Private Const DT_NOPREFIX = &H800&
Private Type DRAWTEXTPARAMS
    cbSize As Long
    iTabLength As Long
    iLeftMargin As Long
    iRightMargin As Long
    uiLengthDrawn As Long
End Type
Private Declare Function DrawTextEx Lib "user32" Alias "DrawTextExA" (ByVal hdc As Long, ByVal lpsz As String, ByVal n As Long, lpRect As RECT, ByVal un As Long, lpDrawTextParams As DRAWTEXTPARAMS) As Long
Private Declare Function DrawTextExAsNull Lib "user32" Alias "DrawTextExA" (ByVal hdc As Long, ByVal lpsz As String, ByVal n As Long, lpRect As RECT, ByVal un As Long, ByVal lpDrawTextParams As Long) As Long
    Private Const DT_EDITCONTROL = &H2000&
    Private Const DT_PATH_ELLIPSIS = &H4000&
    Private Const DT_END_ELLIPSIS = &H8000&
    Private Const DT_MODIFYSTRING = &H10000
    Private Const DT_RTLREADING = &H20000
    Private Const DT_WORD_ELLIPSIS = &H40000

Private Type SIZEAPI
    cX As Long
    cY As Long
End Type
Private Declare Function GetTextExtentPoint32 Lib "gdi32" Alias "GetTextExtentPoint32A" (ByVal hdc As Long, ByVal lpsz As String, ByVal cbString As Long, lpSize As SIZEAPI) As Long
Private Declare Function GetStockObject Lib "gdi32" (ByVal nIndex As Long) As Long
    Private Const ANSI_FIXED_FONT = 11
    Private Const ANSI_VAR_FONT = 12
    Private Const SYSTEM_FONT = 13
    Private Const DEFAULT_GUI_FONT = 17 'win95 only
Private Declare Function FillRect Lib "user32" (ByVal hdc As Long, lpRect As RECT, ByVal hBrush As Long) As Long
Private Declare Function Rectangle Lib "gdi32" (ByVal hdc As Long, ByVal X1 As Long, ByVal y1 As Long, ByVal x2 As Long, ByVal y2 As Long) As Long
Private Declare Function DrawEdge Lib "user32" (ByVal hdc As Long, qrc As RECT, ByVal edge As Long, ByVal grfFlags As Long) As Long
    Private Const BF_LEFT = 1
    Private Const BF_TOP = 2
    Private Const BF_RIGHT = 4
    Private Const BF_BOTTOM = 8
    Private Const BF_RECT = BF_LEFT Or BF_TOP Or BF_RIGHT Or BF_BOTTOM
    Private Const BF_MIDDLE = 2048
    Private Const BDR_SUNKENINNER = 8
    Private Const BDR_SUNKENOUTER = 2
    Private Const BDR_RAISEDOUTER = 1
    Private Const BDR_RAISEDINNER = 4

Private Declare Function ShowWindow Lib "user32" (ByVal hwnd As Long, ByVal nCmdShow As Long) As Long
    Private Const SW_SHOWNOACTIVATE = 4

' Scrolling and region functions:
Private Declare Function ScrollDC Lib "user32" (ByVal hdc As Long, ByVal dx As Long, ByVal dy As Long, lprcScroll As RECT, lprcClip As RECT, ByVal hrgnUpdate As Long, lprcUpdate As RECT) As Long
Private Declare Function SetWindowRgn Lib "user32" (ByVal hwnd As Long, ByVal hRgn As Long, ByVal bRedraw As Long) As Long
Private Declare Function SelectClipRgn Lib "gdi32" (ByVal hdc As Long, ByVal hRgn As Long) As Long
Private Declare Function CreateRectRgn Lib "gdi32" (ByVal X1 As Long, ByVal y1 As Long, ByVal x2 As Long, ByVal y2 As Long) As Long
Private Declare Function CreateRectRgnIndirect Lib "gdi32" (lpRect As RECT) As Long
Private Declare Function CreatePolyPolygonRgn Lib "gdi32" (lpPoint As POINTAPI, lpPolyCounts As Long, ByVal nCount As Long, ByVal nPolyFillMode As Long) As Long
Private Declare Function CreatePolygonRgn Lib "gdi32" (lpPoint As POINTAPI, ByVal nCount As Long, ByVal nPolyFillMode As Long)
Private Declare Function SaveDC Lib "gdi32" (ByVal hdc As Long) As Long
Private Declare Function RestoreDC Lib "gdi32" (ByVal hdc As Long, ByVal hSavedDC As Long) As Long
Private Declare Function CreateDCAsNull Lib "gdi32" Alias "CreateDCA" (ByVal lpDriverName As String, lpDeviceName As Any, lpOutput As Any, lpInitData As Any) As Long

Private Const LF_FACESIZE = 32
Private Type LOGFONT
    lfHeight As Long
    lfWidth As Long
    lfEscapement As Long
    lfOrientation As Long
    lfWeight As Long
    lfItalic As Byte
    lfUnderline As Byte
    lfStrikeOut As Byte
    lfCharSet As Byte
    lfOutPrecision As Byte
    lfClipPrecision As Byte
    lfQuality As Byte
    lfPitchAndFamily As Byte
    lfFaceName(LF_FACESIZE) As Byte
End Type
Private Const FW_NORMAL = 400
Private Const FW_BOLD = 700
Private Const FF_DONTCARE = 0
Private Const DEFAULT_QUALITY = 0
Private Const DEFAULT_PITCH = 0
Private Const DEFAULT_CHARSET = 1
Private Declare Function CreateFontIndirect& Lib "gdi32" Alias "CreateFontIndirectA" (lpLogFont As LOGFONT)
Private Declare Function MulDiv Lib "kernel32" (ByVal nNumber As Long, ByVal nNumerator As Long, ByVal nDenominator As Long) As Long
Private Declare Function DrawFocusRect Lib "user32" (ByVal hdc As Long, lpRect As RECT) As Long

Private Declare Function DrawState Lib "user32" Alias "DrawStateA" _
    (ByVal hdc As Long, _
    ByVal hBrush As Long, _
    ByVal lpDrawStateProc As Long, _
    ByVal lParam As Long, _
    ByVal wParam As Long, _
    ByVal x As Long, _
    ByVal y As Long, _
    ByVal cX As Long, _
    ByVal cY As Long, _
    ByVal fuFlags As Long) As Long

'/* Image type */
Private Const DST_COMPLEX = &H0&
Private Const DST_TEXT = &H1&
Private Const DST_PREFIXTEXT = &H2&
Private Const DST_ICON = &H3&
Private Const DST_BITMAP = &H4&

' /* State type */
Private Const DSS_NORMAL = &H0&
Private Const DSS_UNION = &H10& ' Dither
Private Const DSS_DISABLED = &H20&
Private Const DSS_MONO = &H80& ' Draw in colour of brush specified in hBrush
Private Const DSS_RIGHT = &H8000&

Private Declare Function OleTranslateColor Lib "olepro32.dll" (ByVal OLE_COLOR As Long, ByVal HPALETTE As Long, pccolorref As Long) As Long
Private Const CLR_INVALID = -1

' Shell Functions for SystemImageList
Private Const MAX_PATH = 260
Private Type SHFILEINFO
    hIcon As Long
    iIcon As Long
    dwAttributes As Long
    szDisplayName As String * MAX_PATH
    szTypeName As String * 80
End Type
Private Declare Function SHGetFileInfo Lib "shell32.dll" Alias "SHGetFileInfoA" _
    (ByVal pszPath As String, ByVal dwAttributes As Long, psfi As SHFILEINFO, ByVal cbSizeFileInfo As Long, ByVal uFlags As Long) As Long
Private Enum EShellGetFileInfoConstants
   SHGFI_ICON = &H100                       ' // get icon
   SHGFI_DISPLAYNAME = &H200                ' // get display name
   SHGFI_TYPENAME = &H400                   ' // get type name
   SHGFI_ATTRIBUTES = &H800                 ' // get attributes
   SHGFI_ICONLOCATION = &H1000              ' // get icon location
   SHGFI_EXETYPE = &H2000                   ' // return exe type
   SHGFI_SYSICONINDEX = &H4000              ' // get system icon index
   SHGFI_LINKOVERLAY = &H8000               ' // put a link overlay on icon
   SHGFI_SELECTED = &H10000                 ' // show icon in selected state
   SHGFI_ATTR_SPECIFIED = &H20000           ' // get only specified attributes
   SHGFI_LARGEICON = &H0                    ' // get large icon
   SHGFI_SMALLICON = &H1                    ' // get small icon
   SHGFI_OPENICON = &H2                     ' // get open icon
   SHGFI_SHELLICONSIZE = &H4                ' // get shell size icon
   SHGFI_PIDL = &H8                         ' // pszPath is a pidl
   SHGFI_USEFILEATTRIBUTES = &H10           ' // use passed dwFileAttribute
End Enum
Private Const FILE_ATTRIBUTE_NORMAL = &H80

' Image list functions:
Private Declare Function ImageList_GetBkColor Lib "COMCTL32" (ByVal hImageList As Long) As Long
Private Declare Function ImageList_ReplaceIcon Lib "COMCTL32" (ByVal hImageList As Long, ByVal i As Long, ByVal hIcon As Long) As Long
Private Declare Function ImageList_Convert Lib "COMCTL32" Alias "ImageList_Draw" (ByVal hImageList As Long, ByVal ImgIndex As Long, ByVal hDCDest As Long, ByVal x As Long, ByVal y As Long, ByVal flags As Long) As Long
Private Declare Function ImageList_Create Lib "COMCTL32" (ByVal MinCx As Long, ByVal MinCy As Long, ByVal flags As Long, ByVal cInitial As Long, ByVal cGrow As Long) As Long
Private Declare Function ImageList_AddMasked Lib "COMCTL32" (ByVal hImageList As Long, ByVal hbmImage As Long, ByVal crMask As Long) As Long
Private Declare Function ImageList_Replace Lib "COMCTL32" (ByVal hImageList As Long, ByVal ImgIndex As Long, ByVal hbmImage As Long, ByVal hBmMask As Long) As Long
Private Declare Function ImageList_Add Lib "COMCTL32" (ByVal hImageList As Long, ByVal hbmImage As Long, hBmMask As Long) As Long
Private Declare Function ImageList_Remove Lib "COMCTL32" (ByVal hImageList As Long, ByVal ImgIndex As Long) As Long
Private Type IMAGEINFO
    hBitmapImage As Long
    hBitmapMask As Long
    cPlanes As Long
    cBitsPerPixel As Long
    rcImage As RECT
End Type
Private Declare Function ImageList_GetImageInfo Lib "COMCTL32.DLL" ( _
        ByVal hIml As Long, _
        ByVal i As Long, _
        pImageInfo As IMAGEINFO _
    ) As Long
Private Declare Function ImageList_AddIcon Lib "COMCTL32" (ByVal hIml As Long, ByVal hIcon As Long) As Long
Private Declare Function ImageList_GetIcon Lib "COMCTL32" (ByVal hImageList As Long, ByVal ImgIndex As Long, ByVal fuFlags As Long) As Long
Private Declare Function ImageList_SetImageCount Lib "COMCTL32" (ByVal hImageList As Long, uNewCount As Long)
Private Declare Function ImageList_GetImageCount Lib "COMCTL32" (ByVal hImageList As Long) As Long
Private Declare Function ImageList_Destroy Lib "COMCTL32" (ByVal hImageList As Long) As Long
Private Declare Function ImageList_GetIconSize Lib "COMCTL32" (ByVal hImageList As Long, cX As Long, cY As Long) As Long
Private Declare Function ImageList_SetIconSize Lib "COMCTL32" (ByVal hImageList As Long, cX As Long, cY As Long) As Long

' ImageList functions:
' Draw:
Private Declare Function ImageList_Draw Lib "COMCTL32.DLL" ( _
        ByVal hIml As Long, _
        ByVal i As Long, _
        ByVal hdcDst As Long, _
        ByVal x As Long, _
        ByVal y As Long, _
        ByVal fStyle As Long _
    ) As Long
Private Const ILD_NORMAL = 0&
Private Const ILD_TRANSPARENT = 1&
Private Const ILD_BLEND25 = 2&
Private Const ILD_SELECTED = 4&
Private Const ILD_FOCUS = 4&
Private Const ILD_MASK = &H10&
Private Const ILD_IMAGE = &H20&
Private Const ILD_ROP = &H40&
Private Const ILD_OVERLAYMASK = 3840&
Private Declare Function ImageList_GetImageRect Lib "COMCTL32.DLL" ( _
        ByVal hIml As Long, _
        ByVal i As Long, _
        prcImage As RECT _
    ) As Long
' Messages:
Private Declare Function ImageList_DrawEx Lib "COMCTL32" (ByVal hIml As Long, ByVal i As Long, ByVal hdcDst As Long, ByVal x As Long, ByVal y As Long, ByVal dx As Long, ByVal dy As Long, ByVal rgbBk As Long, ByVal rgbFg As Long, ByVal fStyle As Long) As Long
Private Declare Function ImageList_LoadImage Lib "COMCTL32" Alias "ImageList_LoadImageA" (ByVal hInst As Long, ByVal lpbmp As String, ByVal cX As Long, ByVal cGrow As Long, ByVal crMask As Long, ByVal uType As Long, ByVal uFlags As Long)
Private Declare Function ImageList_SetBkColor Lib "COMCTL32" (ByVal hImageList As Long, ByVal clrBk As Long) As Long

Private Const ILC_MASK = &H1&
 
Private Const CLR_DEFAULT = -16777216
Private Const CLR_HILIGHT = -16777216
Private Const CLR_NONE = -1

Private Const ILCF_MOVE = &H0&
Private Const ILCF_SWAP = &H1&
Private Declare Function ImageList_Copy Lib "COMCTL32" (ByVal himlDst As Long, ByVal iDst As Long, ByVal himlSrc As Long, ByVal iSrc As Long, ByVal uFlags As Long) As Long

Private Declare Function GetTempFileName Lib "kernel32" Alias "GetTempFileNameA" (ByVal lpszPath As String, ByVal lpPrefixString As String, ByVal wUnique As Long, ByVal lpTempFileName As String) As Long
Private Declare Function GetTempPath Lib "kernel32" Alias "GetTempPathA" (ByVal nBufferLength As Long, ByVal lpBuffer As String) As Long
Private Declare Function lstrlen Lib "kernel32" Alias "lstrlenA" (ByVal lpString As String) As Long

Private Type PictDesc
    cbSizeofStruct As Long
    picType As Long
    hImage As Long
    xExt As Long
    yExt As Long
End Type
Private Type Guid
    Data1 As Long
    Data2 As Integer
    Data3 As Integer
    Data4(0 To 7) As Byte
End Type
Private Declare Function OleCreatePictureIndirect Lib "olepro32.dll" (lpPictDesc As PictDesc, riid As Guid, ByVal fPictureOwnsHandle As Long, ipic As IPicture) As Long

' -----------
' ENUMS
' -----------

' ------------------
' Private variables:
' ------------------
Private m_hIml As Long
Private m_lIconSizeX As Long
Private m_lIconSizeY As Long

Public Property Get SystemColourDepth() As eilColourDepth
Dim lR As Long
Dim lHDC As Long
   lHDC = CreateDCAsNull("DISPLAY", ByVal 0&, ByVal 0&, ByVal 0&)
   lR = GetDeviceCaps(lHDC, BITSPIXEL)
   DeleteDC lHDC
   SystemColourDepth = lR
End Property

Public Function Create() As Boolean
Dim dwFlags As Long
Dim hIml As Long
Dim FileInfo As SHFILEINFO
     
     ' Do we already have an image list?  Kill it if we have:
    Destroy

   dwFlags = SHGFI_USEFILEATTRIBUTES Or SHGFI_SYSICONINDEX
   If IconSizeX < 32 Then
      dwFlags = dwFlags Or SHGFI_SMALLICON
   End If

   '// Load the image list - use an arbitrary file extension for the
   '// call to SHGetFileInfo (we don't want to touch the disk, so use
   '// FILE_ATTRIBUTE_NORMAL && SHGFI_USEFILEATTRIBUTES).
   hIml = SHGetFileInfo(".txt", FILE_ATTRIBUTE_NORMAL, FileInfo, LenB(FileInfo), dwFlags)

   'Create the Imagelist:
   If (hIml <> 0) And (hIml <> -1) Then
      ' Ok
      m_hIml = hIml
      Create = True
   Else
      m_hIml = 0
   End If
    
End Function
Public Sub Destroy()
   ' No need to do anything other than clear our
   ' handle:
   m_hIml = 0
End Sub
Public Sub DrawImage( _
        ByVal vKey As Variant, _
        ByVal hdc As Long, _
        ByVal xPixels As Integer, _
        ByVal yPixels As Integer, _
        Optional ByVal bSelected = False, _
        Optional ByVal bCut = False, _
        Optional ByVal bDisabled = False, _
        Optional ByVal oCutDitherColour As OLE_COLOR = vbWindowBackground, _
        Optional ByVal hExternalIml As Long = 0 _
    )
Dim hIcon As Long
Dim lFlags As Long
Dim lhIml As Long
Dim lColor As Long
Dim iImgIndex As Long

   ' Draw the image at 1 based index or key supplied in vKey.
   ' on the hDC at xPixels,yPixels with the supplied options.
   ' You can even draw an ImageList from another ImageList control
   ' if you supply the handle to hExternalIml with this function.
   
   iImgIndex = ItemIndex(vKey)
   If (iImgIndex > -1) Then
      If (hExternalIml <> 0) Then
          lhIml = hExternalIml
      Else
          lhIml = hIml
      End If
      
      lFlags = ILD_TRANSPARENT
      If (bSelected) Or (bCut) Then
          lFlags = lFlags Or ILD_SELECTED
      End If
      
      If (bCut) Then
        ' Draw dithered:
        lColor = TranslateColor(oCutDitherColour)
        If (lColor = -1) Then lColor = GetSysColor(COLOR_WINDOW)
        ImageList_DrawEx _
              lhIml, _
              iImgIndex, _
              hdc, _
              xPixels, yPixels, 0, 0, _
              CLR_NONE, lColor, _
              lFlags
      ElseIf (bDisabled) Then
        ' extract a copy of the icon:
        hIcon = ImageList_GetIcon(hIml, iImgIndex, 0)
        ' Draw it disabled at x,y:
        DrawState hdc, 0, 0, hIcon, 0, xPixels, yPixels, m_lIconSizeX, m_lIconSizeY, DST_ICON Or DSS_DISABLED
        ' Clear up the icon:
        DestroyIcon hIcon
              
      Else
        ' Standard draw:
        ImageList_Draw _
            lhIml, _
            iImgIndex, _
            hdc, _
            xPixels, _
            yPixels, _
            lFlags
      End If
   End If
End Sub

Public Property Get IconSizeX() As Long
   ' Returns the icon width
    IconSizeX = m_lIconSizeX
End Property
Public Property Let IconSizeX(ByVal lSizeX As Long)
   ' Sets the icon width.  NB no change at runtime unless you
   ' call Create and add all the images in again.
    m_lIconSizeX = lSizeX
End Property
Public Property Get IconSizeY() As Long
   ' Returns the icon height:
    IconSizeY = m_lIconSizeY
End Property
Public Property Let IconSizeY(ByVal lSizeY As Long)
   ' Sets the icon height.  NB no change at runtime unless you
   ' call Create and add all the images in again.
    m_lIconSizeY = lSizeY
End Property
Public Property Get ItemIndex( _
      ByVal vKey As Variant, _
      Optional ByVal bForceLoadFromDisk As Boolean = False _
   ) As Long
Dim lR As Long
Dim i As Long
Dim dwFlags As Long
Dim FileInfo As SHFILEINFO

   ' Returns the 0 based Index for the selected
   ' Image list item:
   If (IsNumeric(vKey)) Then
      ItemIndex = vKey
   Else

      dwFlags = SHGFI_SYSICONINDEX
      If IconSizeX >= 32 Then
         dwFlags = dwFlags Or SHGFI_LARGEICON
      Else
         dwFlags = dwFlags Or SHGFI_SMALLICON
      End If

      ' We choose whether to access the disk or not. If you don't
      ' hit the disk, you may get the wrong icon if the icon is
      ' not cached. But the speed is very good!
      If Not bForceLoadFromDisk Then
         dwFlags = dwFlags Or SHGFI_USEFILEATTRIBUTES
      End If

      ' sFileSpec can be any file. You can specify a
      ' file that does not exist and still get the
      ' icon, for example sFileSpec = "C:\PANTS.DOC"
      lR = SHGetFileInfo( _
            vKey, FILE_ATTRIBUTE_NORMAL, FileInfo, LenB(FileInfo), _
            dwFlags _
            )

      If (lR = 0) Then
         ' Failed
      Else
         ItemIndex = FileInfo.iIcon
      End If
      
   End If
   
End Property
Public Property Get ItemPicture(ByVal vKey As Variant) As IPicture
Dim lIndex As Long
Dim hIcon As Long
   ' Returns a StdPicture for an image in the ImageList:
   lIndex = ItemIndex(vKey)
   If (lIndex > -1) Then
      hIcon = ImageList_GetIcon(m_hIml, lIndex, ILD_TRANSPARENT)
      If (hIcon <> 0) Then
         Set ItemPicture = IconToPicture(hIcon)
         ' Don't destroy the icon - it is now owned by
         ' the picture object
      End If
   End If
   
End Property
Public Property Get ItemCopyOfIcon(ByVal vKey As Variant) As Long
Dim lIndex As Long
   ' Returns a hIcon for an image in the ImageList.  User must
   ' call DestroyIcon on the returned handle.
   lIndex = ItemIndex(vKey)
   If (lIndex > -1) Then
      ItemCopyOfIcon = ImageList_GetIcon(m_hIml, lIndex, ILD_TRANSPARENT)
   End If
End Property
Public Sub Clear()
   ' Recreates the image list.
   Create
End Sub
Public Property Get hIml() As Long
   ' Returns the ImageList handle:
    hIml = m_hIml
End Property

Public Function ImagePictureStrip( _
      vKeys() As Variant, _
      Optional ByVal oBackColor As OLE_COLOR = vbButtonFace, _
      Optional ByVal bForceLoadFromDisk As Boolean = False _
   ) As IPicture
Dim iStart As Long
Dim iEnd As Long
Dim iImgIndex As Long
Dim lHDC As Long
Dim lcHDC As Long
Dim lParenthDC As Long
Dim lhBmp As Long
Dim lhBmpOld As Long
Dim lSizeX As Long
Dim hBr As Long
Dim tR As RECT
Dim lColor As Long
   
   If (m_hIml <> 0) Then
      
      On Error Resume Next
      iStart = LBound(vKeys)
      iEnd = UBound(vKeys)
      
      On Error GoTo 0
      If (iEnd >= iStart) And Err.Number = 0 Then
         lcHDC = CreateDCAsNull("DISPLAY", ByVal 0&, ByVal 0&, ByVal 0&)
         lHDC = CreateCompatibleDC(lcHDC)
         If (lHDC <> 0) Then
            lSizeX = (iEnd - iStart + 1) * m_lIconSizeX
            lhBmp = CreateCompatibleBitmap(lcHDC, lSizeX, m_lIconSizeY)
            If (lhBmp <> 0) Then
               lhBmpOld = SelectObject(lHDC, lhBmp)
               If (lhBmpOld <> 0) Then
                  lColor = TranslateColor(oBackColor)
                  tR.Bottom = m_lIconSizeY
                  tR.Right = lSizeX
                  hBr = CreateSolidBrush(lColor)
                  FillRect lHDC, tR, hBr
                  DeleteObject hBr
                  For iImgIndex = iStart To iEnd
                     ImageList_Draw m_hIml, ItemIndex(vKeys(iImgIndex), bForceLoadFromDisk), lHDC, (iImgIndex - iStart) * m_lIconSizeX, 0, ILD_TRANSPARENT
                  Next iImgIndex
                  SelectObject lHDC, lhBmpOld
                  Set ImagePictureStrip = BitmapToPicture(lhBmp)
               Else
                  DeleteObject lhBmp
               End If
            End If
            DeleteDC lHDC
            DeleteDC lcHDC
         End If
      End If
   End If
   
End Function

Public Function IconToPicture(ByVal hIcon As Long) As IPicture
    
    If hIcon = 0 Then Exit Function
        
    ' This is all magic if you ask me:
    Dim NewPic As Picture, PicConv As PictDesc, IGuid As Guid
    
    PicConv.cbSizeofStruct = Len(PicConv)
    PicConv.picType = vbPicTypeIcon
    PicConv.hImage = hIcon
    
    ' Fill in magic IPicture GUID {7BF80980-BF32-101A-8BBB-00AA00300CAB}
    With IGuid
        .Data1 = &H7BF80980
        .Data2 = &HBF32
        .Data3 = &H101A
        .Data4(0) = &H8B
        .Data4(1) = &HBB
        .Data4(2) = &H0
        .Data4(3) = &HAA
        .Data4(4) = &H0
        .Data4(5) = &H30
        .Data4(6) = &HC
        .Data4(7) = &HAB
    End With
    OleCreatePictureIndirect PicConv, IGuid, True, NewPic
    
    Set IconToPicture = NewPic
    
End Function

Public Function BitmapToPicture(ByVal hBmp As Long) As IPicture

   If (hBmp = 0) Then Exit Function
   
   Dim NewPic As Picture, tPicConv As PictDesc, IGuid As Guid
   
   ' Fill PictDesc structure with necessary parts:
   With tPicConv
      .cbSizeofStruct = Len(tPicConv)
      .picType = vbPicTypeBitmap
      .hImage = hBmp
   End With
   
   ' Fill in IDispatch Interface ID
   With IGuid
      .Data1 = &H20400
      .Data4(0) = &HC0
      .Data4(7) = &H46
   End With
   
   ' Create a picture object:
   OleCreatePictureIndirect tPicConv, IGuid, True, NewPic
   
   ' Return it:
   Set BitmapToPicture = NewPic
      

End Function

Public Function TranslateColor(ByVal clr As OLE_COLOR, _
                        Optional hPal As Long = 0) As Long
   If OleTranslateColor(clr, hPal, TranslateColor) Then
      TranslateColor = CLR_INVALID
   End If
End Function

Private Sub Class_Initialize()
   m_lIconSizeX = 16
   m_lIconSizeY = 16
End Sub

Private Sub Class_Terminate()
    Destroy
End Sub

