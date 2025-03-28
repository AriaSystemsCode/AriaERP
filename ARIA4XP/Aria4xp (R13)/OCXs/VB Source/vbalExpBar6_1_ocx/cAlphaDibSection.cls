VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "pcAlphaDibSection"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

' ==================================================================================
' cDIBSection.cls
' Copyright � 1998 Steve McMahon (steve@vbaccelerator.com)
' Visit vbAccelerator at www.vbaccelerator.com
'
' Creates and manages an ARGB (32bit) GDI DibSection.
' This is DIB in which the bitmap bits are stored in windows
' memory so can be modified.
' See the RandomiseBits and Resample methods for how to do it.
'
' For example, fading in an out a 256x256 true colour DIB by
' directly modifying the bytes runs at 38fps on my machine
' (PII 266Mhz, 32Mb RAM, 8Mb ATI Xpert@Work AGP card)
'
' Note: for best performance, when compiling an executable check
' all the boxes on the Properties-Compile tab Advanced Optimisations
' button.  This really makes a difference! (e.g. the fading example
' ran at 22fps before I did this so > 50%!).
' ==================================================================================
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
Private Declare Function VarPtrArray Lib "msvbvm60.dll" Alias "VarPtr" (Ptr() As Any) As Long

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
Private Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hdc As Long) As Long
Private Declare Function ReleaseDC Lib "user32" (ByVal hWnd As Long, ByVal hdc As Long) As Long
Private Declare Function GetDC Lib "user32" (ByVal hWnd As Long) As Long
Private Declare Function GetDesktopWindow Lib "user32" () As Long
' Note - this is not the declare in the API viewer - modify lplpVoid to be
' Byref so we get the pointer back:
Private Declare Function CreateDIBSection Lib "gdi32" _
    (ByVal hdc As Long, _
    pBitmapInfo As BITMAPINFO, _
    ByVal un As Long, _
    lplpVoid As Long, _
    ByVal handle As Long, _
    ByVal dw As Long) As Long
Private Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long
Private Type BLENDFUNCTION
  BlendOp As Byte
  BlendFlags As Byte
  SourceConstantAlpha As Byte
  AlphaFormat As Byte
End Type
' BlendOp:
Private Const AC_SRC_OVER = &H0
' AlphaFormat:
Private Const AC_SRC_ALPHA = &H1

Private Declare Function AlphaBlend Lib "msimg32.dll" ( _
  ByVal hdcDest As Long, _
  ByVal nXOriginDest As Long, _
  ByVal nYOriginDest As Long, _
  ByVal nWidthDest As Long, _
  ByVal nHeightDest As Long, _
  ByVal hdcSrc As Long, _
  ByVal nXOriginSrc As Long, _
  ByVal nYOriginSrc As Long, _
  ByVal nWidthSrc As Long, _
  ByVal nHeightSrc As Long, _
  ByVal lBlendFunction As Long _
) As Long

Private Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Private Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long
Private Declare Function LoadImage Lib "user32" Alias "LoadImageA" (ByVal hInst As Long, ByVal lpsz As String, ByVal un1 As Long, ByVal n1 As Long, ByVal n2 As Long, ByVal un2 As Long) As Long
Private Const BI_RGB = 0&
Private Const BI_RLE4 = 2&
Private Const BI_RLE8 = 1&
Private Const DIB_RGB_COLORS = 0 '  color table in RGBs

Private Type BITMAP
    bmType As Long
    bmWidth As Long
    bmHeight As Long
    bmWidthBytes As Long
    bmPlanes As Integer
    bmBitsPixel As Integer
    bmBits As Long
End Type
Private Declare Function GetObjectAPI Lib "gdi32" Alias "GetObjectA" (ByVal hObject As Long, ByVal nCount As Long, lpObject As Any) As Long

' Start of structure:
Private Const BITMAPTYPE As Integer = &H4D42
Private Type BITMAPFILEHEADER
   bfType As Integer '- type  ="BM" i.e &H4D42 - 2
   bfSize As Long ' - size in bytes of file - 6
   bfReserved1 As Integer ' - reserved, must be 0 - 8
   bfReserved2 As Integer ' - reserved, must be 0 - 10
   bfOffBits As Long ' offset from this structure to the bitmap bits - 14
End Type
Private Declare Function CreateFile Lib "kernel32" Alias "CreateFileA" (ByVal lpFileName As String, ByVal dwDesiredAccess As Long, ByVal dwShareMode As Long, lpSecurityAttributes As Any, ByVal dwCreationDisposition As Long, ByVal dwFlagsAndAttributes As Long, ByVal hTemplateFile As Long) As Long
Private Declare Function ReadFile Lib "kernel32" (ByVal hFile As Long, lpBuffer As Any, ByVal nNumberOfBytesToRead As Long, lpNumberOfBytesRead As Long, lpOverlapped As Any) As Long
Private Declare Function WriteFile Lib "kernel32" (ByVal hFile As Long, lpBuffer As Any, ByVal nNumberOfBytesToWrite As Long, lpNumberOfBytesWritten As Long, lpOverlapped As Any) As Long
Private Declare Function SetFilePointer Lib "kernel32" (ByVal hFile As Long, ByVal lDistanceToMove As Long, lpDistanceToMoveHigh As Long, ByVal dwMoveMethod As Long) As Long
Private Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long
Private Const INVALID_HANDLE_VALUE = -1
Private Const CREATE_ALWAYS = 2
Private Const GENERIC_READ = &H80000000
Private Const GENERIC_WRITE = &H40000000
Private Const FILE_ATTRIBUTE_ARCHIVE = &H20
Private Const FILE_ATTRIBUTE_COMPRESSED = &H800
Private Const FILE_ATTRIBUTE_DIRECTORY = &H10
Private Const FILE_ATTRIBUTE_HIDDEN = &H2
Private Const FILE_ATTRIBUTE_NORMAL = &H80
Private Const FILE_ATTRIBUTE_READONLY = &H1
Private Const FILE_ATTRIBUTE_SYSTEM = &H4
Private Const FILE_ATTRIBUTE_TEMPORARY = &H100
Private Const FILE_BEGIN = 0
Private Declare Function GlobalLock Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Function GlobalAlloc Lib "kernel32" (ByVal wFlags As Long, ByVal dwBytes As Long) As Long
Private Declare Function GlobalFree Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Function GlobalUnlock Lib "kernel32" (ByVal hMem As Long) As Long
Private Const GMEM_FIXED = &H0
Private Const GMEM_ZEROINIT = &H40
Private Const GPTR = (GMEM_FIXED Or GMEM_ZEROINIT)

Private Const FORMAT_MESSAGE_ALLOCATE_BUFFER = &H100
Private Const FORMAT_MESSAGE_ARGUMENT_ARRAY = &H2000
Private Const FORMAT_MESSAGE_FROM_HMODULE = &H800
Private Const FORMAT_MESSAGE_FROM_STRING = &H400
Private Const FORMAT_MESSAGE_FROM_SYSTEM = &H1000
Private Const FORMAT_MESSAGE_IGNORE_INSERTS = &H200
Private Const FORMAT_MESSAGE_MAX_WIDTH_MASK = &HFF
Private Declare Function FormatMessage Lib "kernel32" Alias "FormatMessageA" (ByVal dwFlags As Long, lpSource As Any, ByVal dwMessageId As Long, ByVal dwLanguageId As Long, ByVal lpBuffer As String, ByVal nSize As Long, Arguments As Long) As Long

Private Declare Function GetDIBits Lib "gdi32" (ByVal aHDC As Long, ByVal hBitmap As Long, ByVal nStartScan As Long, ByVal nNumScans As Long, lpBits As Long, lpBI As BITMAPINFO, ByVal wUsage As Long) As Long
Private Declare Function CreateDIBitmap Lib "gdi32" (ByVal hdc As Long, lpInfoHeader As BITMAPINFOHEADER, ByVal dwUsage As Long, lpInitBits As Any, lpInitInfo As BITMAPINFO, ByVal wUsage As Long) As Long

' DrawDIB functions:
Private Declare Function DrawDibOpen Lib "msvfw32.dll" () As Long
Private Declare Function DrawDibClose Lib "msvfw32.dll" (ByVal hDD As Long) As Long
Private Declare Function DrawDibDraw Lib "msvfw32.dll" (ByVal hDD As Long, ByVal hdc As Long, _
      ByVal xDst As Long, ByVal yDst As Long, ByVal dxDst As Long, ByVal dyDst As Long, _
      lpBI As Any, lpBits As Any, _
      ByVal xSrc As Long, ByVal ySrc As Long, ByVal dxSrc As Long, ByVal dySrc As Long, _
      ByVal wFlags As Long) As Long

Private Type PictDesc
    cbSizeofStruct As Long
    picType As Long
    hImage As Long
    xExt As Long
    yExt As Long
End Type

Private Type GUID
    Data1 As Long
    Data2 As Integer
    Data3 As Integer
    Data4(0 To 7) As Byte
End Type

Private Declare Function OleCreatePictureIndirect Lib "OLEPRO32.DLL" ( _
      lpPictDesc As PictDesc, _
      riid As GUID, _
      ByVal fPictureOwnsHandle As Long, _
      ipic As IPicture _
    ) As Long

Private m_hDib As Long
Private m_hBmpOld As Long
Private m_hDC As Long
Private m_hDD As Long
Private m_lPtr As Long
Private m_tBI As BITMAPINFO

Public Property Get UseDrawDib() As Boolean
On Error Resume Next
   UseDrawDib = Not (m_hDD = 0)
End Property
Public Property Let UseDrawDib(ByVal bState As Boolean)
   On Error Resume Next
   If bState Then
      If m_hDD = 0 Then
         m_hDD = DrawDibOpen()
      End If
   Else
      If Not (m_hDD = 0) Then
         DrawDibClose m_hDD
      End If
   End If
End Property

Public Function Picture() As IPicture
   On Error Resume Next
   If (m_hDC <> 0) And (m_hDib <> 0) Then
   
      Dim hBmp As Long
      Dim cCopy As New pcAlphaDibSection
      cCopy.Create Width, Height
      cCopy.LoadPictureBlt m_hDC
      hBmp = cCopy.ExtractDib
      Set cCopy = Nothing
   
      Dim NewPic As Picture, tPicConv As PictDesc, IGuid As GUID
   
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
      Set Picture = NewPic
      
   End If
   
End Function

Public Function CreateDIB( _
        ByVal lHDC As Long, _
        ByVal lWidth As Long, _
        ByVal lHeight As Long, _
        ByRef hDib As Long _
    ) As Boolean
    On Error Resume Next
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
            lHDC, _
            m_tBI, _
            DIB_RGB_COLORS, _
            m_lPtr, _
            0, 0)
    CreateDIB = (hDib <> 0)
End Function
Public Function CreateFromPicture( _
        ByRef picThis As StdPicture _
    )
    On Error Resume Next
    CreateFromHBitmap picThis.handle
End Function
Public Function CreateFromHBitmap( _
      ByVal hBmp As Long _
   )
On Error Resume Next
Dim lHDC As Long
Dim lhWnd As Long
Dim lhDCDesktop As Long
Dim lhBmpOld As Long
Dim tBmp As BITMAP
   GetObjectAPI hBmp, Len(tBmp), tBmp
   If (Create(tBmp.bmWidth, tBmp.bmHeight)) Then
      lhWnd = GetDesktopWindow()
      lhDCDesktop = GetDC(lhWnd)
      If (lhDCDesktop <> 0) Then
         lHDC = CreateCompatibleDC(lhDCDesktop)
         ReleaseDC lhWnd, lhDCDesktop ' 2003-07-05: Corrected for GDI leak in Win98
         If (lHDC <> 0) Then
            lhBmpOld = SelectObject(lHDC, hBmp)
            LoadPictureBlt lHDC
            SelectObject lHDC, lhBmpOld
            DeleteObject lHDC
         End If
      End If
   End If
   
End Function
Public Function Create( _
        ByVal lWidth As Long, _
        ByVal lHeight As Long _
    ) As Boolean
On Error Resume Next
Dim bDrawDib As Boolean
   bDrawDib = UseDrawDib()
    ClearUp
    m_hDC = CreateCompatibleDC(0)
    If (m_hDC <> 0) Then
        If (CreateDIB(m_hDC, lWidth, lHeight, m_hDib)) Then
            m_hBmpOld = SelectObject(m_hDC, m_hDib)
            UseDrawDib = bDrawDib
            Create = True
        Else
            DeleteObject m_hDC
            m_hDC = 0
        End If
    End If
End Function
Public Property Get BytesPerScanLine() As Long
    On Error Resume Next
    ' Scans must align on dword boundaries; easy with an alpha bitmap!
    BytesPerScanLine = m_tBI.bmiHeader.biWidth * 4
End Property
Public Sub PreMultiplyAlpha()
On Error Resume Next
Dim bDib() As Byte
Dim x As Long, y As Long
Dim lC As Long
Dim tSA As SAFEARRAY2D
Dim bAlpha As Long
    
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
   For y = 0 To Me.Height - 1
      For x = 0 To Me.BytesPerScanLine - 4 Step 4 ' each item has 4 bytes: R,G,B,A
         ' Get the red value from the mask to use as the alpha
         ' value:
         bAlpha = bDib(x + 3, y)
         ' Now premultiply the r/g/b values by the alpha divided
         ' by 255.  This is required for the AlphaBlend GDI function,
         ' see MSDN/Platform SDK/GDI/BLENDFUNCTION for more
         ' details:
         bDib(x, y) = bDib(x, y) * bAlpha \ 255
         bDib(x + 1, y) = bDib(x + 1, y) * bAlpha \ 255
         bDib(x + 2, y) = bDib(x + 2, y) * bAlpha \ 255
      Next x
   Next y

    ' Clear the temporary array descriptor
    ' (This does not appear to be necessary, but
    ' for safety do it anyway)
    CopyMemory ByVal VarPtrArray(bDib), 0&, 4

End Sub

Public Property Get Width() As Long
    On Error Resume Next
    Width = m_tBI.bmiHeader.biWidth
End Property
Public Property Get Height() As Long
    On Error Resume Next
    Height = m_tBI.bmiHeader.biHeight
End Property

Public Sub LoadPictureBlt( _
        ByVal lHDC As Long, _
        Optional ByVal lSrcLeft As Long = 0, _
        Optional ByVal lSrcTop As Long = 0, _
        Optional ByVal lSrcWidth As Long = -1, _
        Optional ByVal lSrcHeight As Long = -1, _
        Optional ByVal eRop As RasterOpConstants = vbSrcCopy _
    )
    On Error Resume Next
    If lSrcWidth < 0 Then lSrcWidth = m_tBI.bmiHeader.biWidth
    If lSrcHeight < 0 Then lSrcHeight = m_tBI.bmiHeader.biHeight
    BitBlt m_hDC, 0, 0, lSrcWidth, lSrcHeight, lHDC, lSrcLeft, lSrcTop, eRop
End Sub
Public Function SavePicture(ByVal sFileName As String) As Boolean
On Error Resume Next
Dim lC As Long, i As Long

   ' Save to BMP:
   SavePicture = SaveToBitmap(m_lPtr, sFileName)

End Function
Private Function SaveToBitmap(ByVal lPtrBits As Long, ByVal sFileName As String)
On Error Resume Next
Dim tBH As BITMAPFILEHEADER
Dim tRGBQ As RGBQUAD
Dim hFile As Long
Dim lBytesWritten As Long
Dim lSize As Long
Dim lR As Long
Dim bErr As Boolean
Dim hMem As Long, lPtr As Long
Dim lErr As Long

   ' Prepare the BITMAPFILEHEADER
   With tBH
      .bfType = BITMAPTYPE
      .bfOffBits = 14 + Len(m_tBI)
      .bfSize = .bfOffBits + m_tBI.bmiHeader.biSizeImage
   End With
   hFile = CreateFile(sFileName, _
                 GENERIC_READ Or GENERIC_WRITE, _
                  ByVal 0&, _
                  ByVal 0&, _
                  CREATE_ALWAYS, _
                  FILE_ATTRIBUTE_NORMAL, _
                  0)
   lErr = Err.LastDllError
   If (hFile = INVALID_HANDLE_VALUE) Then
      ' error
      Err.Raise 17, App.EXEName & ".cDIBSection", ApiError(lErr)
   Else
      
      ' Writing the BITMAPFILEINFOHEADER is somewhat painful
      ' due to non-byte alignment of structure...
      hMem = GlobalAlloc(GPTR, 14)
      lPtr = GlobalLock(hMem)
      CopyMemory ByVal lPtr, tBH.bfType, 2
      CopyMemory ByVal lPtr + 2, tBH.bfSize, 4
      CopyMemory ByVal lPtr + 6, 0&, 4
      CopyMemory ByVal lPtr + 10, tBH.bfOffBits, 4
      lSize = 14
      lR = WriteFile(hFile, ByVal lPtr, lSize, lBytesWritten, ByVal 0&)
      GlobalUnlock hMem
      GlobalFree hMem
      
      ' Add the BITMAPINFOHEADER and colour palette:
      bErr = FileErrHandler(lR, lSize, lBytesWritten)
      If Not bErr Then
         lSize = Len(m_tBI)
         lR = WriteFile(hFile, m_tBI, lSize, lBytesWritten, ByVal 0&)
         bErr = FileErrHandler(lR, lSize, lBytesWritten)
      End If
      
      If Not bErr Then
         ' Its easy to write the bitmap data, though...
         lSize = m_tBI.bmiHeader.biSizeImage
         lR = WriteFile(hFile, ByVal lPtrBits, lSize, lBytesWritten, ByVal 0&)
         bErr = FileErrHandler(lR, lSize, lBytesWritten)
      End If
      
      
      CloseHandle hFile
      SaveToBitmap = Not (bErr)
   End If

End Function
Private Function ApiError(ByVal e As Long) As String
    On Error Resume Next
    Dim s As String, c As Long
    s = String(256, 0)
    c = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM Or _
                      FORMAT_MESSAGE_IGNORE_INSERTS, _
                      0, e, 0&, s, Len(s), ByVal 0)
    If c Then ApiError = left$(s, c)
End Function
Private Function FileErrHandler(ByVal lR As Long, ByVal lSize As Long, ByVal lBytes As Long) As Boolean
   On Error Resume Next
   If (lR = 0) Or Not (lSize = lBytes) Then
      'Err.Raise
      FileErrHandler = True
   End If
End Function


Public Sub PaintPicture( _
        ByVal lHDC As Long, _
        Optional ByVal lDestLeft As Long = 0, _
        Optional ByVal lDestTop As Long = 0, _
        Optional ByVal lDestWidth As Long = -1, _
        Optional ByVal lDestHeight As Long = -1, _
        Optional ByVal lSrcLeft As Long = 0, _
        Optional ByVal lSrcTop As Long = 0, _
        Optional ByVal eRop As RasterOpConstants = vbSrcCopy _
    )
   On Error Resume Next
   If (lDestWidth < 0) Then lDestWidth = m_tBI.bmiHeader.biWidth
   If (lDestHeight < 0) Then lDestHeight = m_tBI.bmiHeader.biHeight
   If Not (m_hDD = 0) Then
      ' DrawDib method:
      DrawDibDraw m_hDD, lHDC, lDestLeft, lDestTop, lDestWidth, lDestHeight, m_tBI, ByVal m_lPtr, lSrcLeft, lSrcTop, lDestWidth, lDestHeight, 0
   Else
      BitBlt lHDC, lDestLeft, lDestTop, lDestWidth, lDestHeight, m_hDC, lSrcLeft, lSrcTop, eRop
   End If
End Sub
Public Sub AlphaPaintPicture( _
        ByVal lHDC As Long, _
        Optional ByVal lDestLeft As Long = 0, _
        Optional ByVal lDestTop As Long = 0, _
        Optional ByVal lDestWidth As Long = -1, _
        Optional ByVal lDestHeight As Long = -1, _
        Optional ByVal lSrcLeft As Long = 0, _
        Optional ByVal lSrcTop As Long = 0, _
        Optional ByVal lConstantAlpha As Byte = 255, _
        Optional ByVal bHasAlphaChannel As Boolean = True _
    )
   On Error Resume Next
   If (lDestWidth < 0) Then lDestWidth = m_tBI.bmiHeader.biWidth
   If (lDestHeight < 0) Then lDestHeight = m_tBI.bmiHeader.biHeight
   
   If (HasGradientAndTransparency) Then ' Added check 2003-07-05 for NT4/95 compatibility
      
      Dim lBlend As Long
      Dim bf As BLENDFUNCTION
      bf.BlendOp = AC_SRC_OVER
      bf.BlendFlags = 0
      bf.SourceConstantAlpha = lConstantAlpha
      If (bHasAlphaChannel) Then
         bf.AlphaFormat = AC_SRC_ALPHA
      End If
      CopyMemory lBlend, bf, 4
      
      Dim lR As Long
      lR = AlphaBlend( _
         lHDC, _
         lDestLeft, lDestTop, lDestWidth, lDestHeight, _
         m_hDC, _
         lSrcLeft, lSrcTop, lDestWidth, lDestHeight, _
         lBlend)
      If (lR = 0) Then
         Debug.Print ApiError(Err.LastDllError)
      End If
      
   Else
      PaintPicture lHDC, _
         lDestLeft, lDestTop, _
         lDestWidth, lDestHeight, _
         lSrcLeft, lSrcTop, _
         vbSrcCopy
   End If
End Sub

Public Property Get hdc() As Long
    On Error Resume Next
    hdc = m_hDC
End Property
Public Property Get hDib() As Long
    On Error Resume Next
    hDib = m_hDib
End Property
Public Function ExtractDib() As Long
   On Error Resume Next
   SelectObject m_hDC, m_hBmpOld
   ExtractDib = m_hDib
   DeleteObject m_hDC
   m_hBmpOld = 0
   m_hDC = 0
   m_hDib = 0 ' up to caller to delete
End Function
Public Property Get DIBSectionBitsPtr() As Long
    On Error Resume Next
    DIBSectionBitsPtr = m_lPtr
End Property
Public Sub RandomiseBits( _
        Optional ByVal bGray As Boolean = False _
    )
On Error Resume Next
Dim bDib() As Byte
Dim x As Long, y As Long
Dim lC As Long
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

    ' random:
    Randomize Timer
    
    If (bGray) Then
        For y = 0 To m_tBI.bmiHeader.biHeight - 1
            For x = 0 To BytesPerScanLine - 1 Step 4
                lC = Rnd * 255
                bDib(x + 3, y) = Rnd * 255 '255 * (y / m_tBI.bmiHeader.biHeight)
                bDib(x, y) = lC * bDib(x + 3, y) / 255
                bDib(x + 1, y) = lC * bDib(x + 3, y) / 255
                bDib(x + 2, y) = lC * bDib(x + 3, y) / 255
            Next x
        Next y
    Else
         For y = 0 To m_tBI.bmiHeader.biHeight - 1
            For x = 0 To BytesPerScanLine - 1 Step 4
                bDib(x + 3, y) = Rnd * 255 '255 * (y / m_tBI.bmiHeader.biHeight)
                bDib(x, y) = Rnd * 255 * bDib(x + 3, y) / 255
                bDib(x + 1, y) = Rnd * 255 * bDib(x + 3, y) / 255
                bDib(x + 2, y) = Rnd * 255 * bDib(x + 3, y) / 255
            Next x
        Next y
    End If
    
    ' Clear the temporary array descriptor
    ' (This does not appear to be necessary, but
    ' for safety do it anyway)
    CopyMemory ByVal VarPtrArray(bDib), 0&, 4
    
End Sub

Public Sub ClearUp()
    On Error Resume Next
    If (m_hDC <> 0) Then
        If (m_hDib <> 0) Then
            SelectObject m_hDC, m_hBmpOld
            DeleteObject m_hDib
        End If
        DeleteObject m_hDC
    End If
    m_hDC = 0: m_hDib = 0: m_hBmpOld = 0: m_lPtr = 0
    If Not (m_hDD = 0) Then
      DrawDibClose m_hDD
      m_hDD = 0
   End If
End Sub

Public Function Resample( _
        ByVal lNewHeight As Long, _
        ByVal lNewWidth As Long _
    ) As pcAlphaDibSection
On Error Resume Next
Dim cDib As pcAlphaDibSection
    Set cDib = New pcAlphaDibSection
    If cDib.Create(lNewWidth, lNewHeight) Then
        If (lNewWidth <> m_tBI.bmiHeader.biWidth) Or (lNewHeight <> m_tBI.bmiHeader.biHeight) Then
            ' Change in size, do resample:
            ResampleDib cDib
        Else
            ' No size change so just return a copy:
            cDib.LoadPictureBlt m_hDC
        End If
        Set Resample = cDib
    End If
End Function

Private Function ResampleDib(ByRef cDibTo As pcAlphaDibSection) As Boolean
On Error Resume Next
Dim bDibFrom() As Byte
Dim bDibTo() As Byte
Dim tSAFrom As SAFEARRAY2D
Dim tSATo As SAFEARRAY2D

    ' Get the bits in the from DIB section:
    With tSAFrom
        .cbElements = 1
        .cDims = 2
        .Bounds(0).lLbound = 0
        .Bounds(0).cElements = m_tBI.bmiHeader.biHeight
        .Bounds(1).lLbound = 0
        .Bounds(1).cElements = BytesPerScanLine()
        .pvData = m_lPtr
    End With
    CopyMemory ByVal VarPtrArray(bDibFrom()), VarPtr(tSAFrom), 4

    ' Get the bits in the to DIB section:
    With tSATo
        .cbElements = 1
        .cDims = 2
        .Bounds(0).lLbound = 0
        .Bounds(0).cElements = cDibTo.Height
        .Bounds(1).lLbound = 0
        .Bounds(1).cElements = cDibTo.BytesPerScanLine()
        .pvData = cDibTo.DIBSectionBitsPtr
    End With
    CopyMemory ByVal VarPtrArray(bDibTo()), VarPtr(tSATo), 4

Dim xScale As Single
Dim yScale As Single

Dim x As Long, y As Long, xEnd As Long, xOut As Long

Dim fX As Single, fY As Single
Dim ifY As Long, ifX As Long
Dim dX As Single, dy As Single
Dim r As Long, r1 As Single, r2 As Single, r3 As Single, r4 As Single
Dim g As Long, g1 As Single, g2 As Single, g3 As Single, g4 As Single
Dim b As Long, b1 As Single, b2 As Single, b3 As Single, b4 As Single
Dim ir1 As Long, ig1 As Long, ib1 As Long
Dim ir2 As Long, ig2 As Long, ib2 As Long

    xScale = (Width - 1) / cDibTo.Width
    yScale = (Height - 1) / cDibTo.Height
    
    xEnd = cDibTo.Width - 1
        
    For y = 0 To cDibTo.Height - 1
        
        fY = y * yScale
        ifY = Int(fY)
        dy = fY - ifY
        
        For x = 0 To xEnd
            fX = x * xScale
            ifX = Int(fX)
            dX = fX - ifX
            
            ifX = ifX * 3
            ' Interpolate using the four nearest pixels in the source
            b1 = bDibFrom(ifX, ifY): g1 = bDibFrom(ifX + 1, ifY): r1 = bDibFrom(ifX + 2, ifY)
            b2 = bDibFrom(ifX + 3, ifY): g2 = bDibFrom(ifX + 4, ifY): r2 = bDibFrom(ifX + 5, ifY)
            b3 = bDibFrom(ifX, ifY + 1): g3 = bDibFrom(ifX + 1, ifY + 1): r3 = bDibFrom(ifX + 2, ifY + 1)
            b4 = bDibFrom(ifX + 3, ifY + 1): g4 = bDibFrom(ifX + 4, ifY + 1): r4 = bDibFrom(ifX + 5, ifY + 1)
            
            ' Interplate in x direction:
            ir1 = r1 * (1 - dy) + r3 * dy: ig1 = g1 * (1 - dy) + g3 * dy: ib1 = b1 * (1 - dy) + b3 * dy
            ir2 = r2 * (1 - dy) + r4 * dy: ig2 = g2 * (1 - dy) + g4 * dy: ib2 = b2 * (1 - dy) + b4 * dy
            ' Interpolate in y:
            r = ir1 * (1 - dX) + ir2 * dX: g = ig1 * (1 - dX) + ig2 * dX: b = ib1 * (1 - dX) + ib2 * dX
            
            ' Set output:
            If (r < 0) Then r = 0
            If (r > 255) Then r = 255
            If (g < 0) Then g = 0
            If (g > 255) Then g = 255
            If (b < 0) Then b = 0
            If (b > 255) Then
                b = 255
            End If
            xOut = x * 3
            bDibTo(xOut, y) = b
            bDibTo(xOut + 1, y) = g
            bDibTo(xOut + 2, y) = r
            
        Next x
        
    Next y

    ' Clear the temporary array descriptor
    ' (This does not appear to be necessary, but
    ' for safety do it anyway)
    CopyMemory ByVal VarPtrArray(bDibFrom), 0&, 4
    CopyMemory ByVal VarPtrArray(bDibTo), 0&, 4


End Function

Private Sub Class_Terminate()
    On Error Resume Next
    ClearUp
End Sub


