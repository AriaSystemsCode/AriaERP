VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "PaintEffects"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Description = "Provides methods for painting transparent and disabled looking images."
Option Explicit

'-------------------------------------------------------------------------
' This class provides methods needed for painting masked bitmaps and
' disabled or embossed bitmaps and icons
'-------------------------------------------------------------------------

' VB Errors
Private Const giINVALID_PICTURE As Integer = 481

Private Declare Function GetDC Lib "user32" (ByVal hWnd As Long) As Long
Private Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hDC As Long) As Long
Private Declare Function SelectObject Lib "gdi32" (ByVal hDC As Long, ByVal hObject As Long) As Long
Private Declare Function SelectPalette Lib "gdi32" (ByVal hDC As Long, ByVal hPalette As Long, ByVal bForceBackground As Long) As Long
Private Declare Function RealizePalette Lib "gdi32" (ByVal hDC As Long) As Long
Private Declare Function DeleteDC Lib "gdi32" (ByVal hDC As Long) As Long
Private Declare Function ReleaseDC Lib "user32" (ByVal hWnd As Long, ByVal hDC As Long) As Long
Private Declare Function CreateCompatibleBitmap Lib "gdi32" (ByVal hDC As Long, ByVal nWidth As Long, ByVal nHeight As Long) As Long
Private Declare Function OleTranslateColor Lib "oleaut32.dll" (ByVal lOleColor As Long, ByVal lHPalette As Long, lColorRef As Long) As Long
Private Declare Function SetBkColor Lib "gdi32" (ByVal hDC As Long, ByVal crColor As Long) As Long
Private Declare Function CreateSolidBrush Lib "gdi32" (ByVal crColor As Long) As Long
Private Declare Function FillRect Lib "user32" (ByVal hDC As Long, lpRect As RECT, ByVal hBrush As Long) As Long
Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Private Declare Function DrawIconEx Lib "user32" (ByVal hDC As Long, ByVal xLeft As Long, ByVal yTop As Long, ByVal hIcon As Long, ByVal cxWidth As Long, ByVal cyHeight As Long, ByVal istepIfAniCur As Long, ByVal hbrFlickerFreeDraw As Long, ByVal diFlags As Long) As Long
Private Declare Function CreateDIBSection Lib "gdi32" (ByVal hDC As Long, pBitmapInfo As BITMAPINFO, ByVal un As Long, ByVal lplpVoid As Long, ByVal handle As Long, ByVal dw As Long) As Long
Private Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long
Private Declare Function SetDIBColorTable Lib "gdi32" (ByVal hDC As Long, ByVal un1 As Long, ByVal un2 As Long, pcRGBQuad As RGBQUAD) As Long
Private Declare Function CreateBitmap Lib "gdi32" (ByVal nWidth As Long, ByVal nHeight As Long, ByVal nPlanes As Long, ByVal nBitCount As Long, lpBits As Any) As Long
Private Declare Function GetMapMode Lib "gdi32" (ByVal hDC As Long) As Long
Private Declare Function SetMapMode Lib "gdi32" (ByVal hDC As Long, ByVal nMapMode As Long) As Long
Private Declare Function SetTextColor Lib "gdi32" (ByVal hDC As Long, ByVal crColor As Long) As Long
Private Declare Function GetBkColor Lib "gdi32" (ByVal hDC As Long) As Long
Private Declare Function GetTextColor Lib "gdi32" (ByVal hDC As Long) As Long
Private Declare Function CreateHalftonePalette Lib "gdi32" (ByVal hDC As Long) As Long
Private Declare Function GetPixel Lib "gdi32" (ByVal hDC As Long, ByVal x As Long, ByVal y As Long) As Long
Private Declare Function DrawState Lib "user32" Alias "DrawStateA" (ByVal hDC As Long, ByVal hBrush As Long, ByVal lpDrawStateProc As Long, ByVal lParam As Long, ByVal wParam As Long, ByVal n1 As Long, ByVal n2 As Long, ByVal n3 As Long, ByVal n4 As Long, ByVal un As Long) As Long

' Draw text Functios and constants
Private Declare Function DrawText Lib "user32" Alias "DrawTextA" (ByVal hDC As Long, ByVal lpStr As String, ByVal nCount As Long, lpRect As RECT, ByVal wFormat As Long) As Long
Private Const DT_BOTTOM = &H8
Private Const DT_CENTER = &H1
Private Const DT_SINGLELINE = &H20
Private Const DT_VCENTER = &H4
Private Const DT_CALCRECT = &H400
Private Const DT_LEFT = &H0

' DrawIconEx Flags
Private Const DI_MASK = &H1
Private Const DI_IMAGE = &H2
Private Const DI_NORMAL = &H3
Private Const DI_COMPAT = &H4
Private Const DI_DEFAULTSIZE = &H8

' DIB Section constants
Private Const BI_RGB = 0&
Private Const DIB_RGB_COLORS = 0 '  color table in RGBs

' Raster Operation Codes
Private Const DSna = &H220326 '0x00220326

' Constant for GetPixel
Private Const CLR_INVALID = &HFFFF

' DrawState constants
Private Const DSS_DISABLED = &H20
Private Const DSS_MONO = &H80
Private Const DSS_NORMAL = &H0
Private Const DSS_RIGHT = &H8000
Private Const DSS_UNION = &H10
Private Const DST_BITMAP = &H4
Private Const DST_COMPLEX = &H0
Private Const DST_ICON = &H3
Private Const DST_PREFIXTEXT = &H2
Private Const DST_TEXT = &H1

Private Type RECT
  Left As Long
  Top As Long
  Right As Long
  Bottom As Long
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

Private Type RGBQUAD
  rgbBlue As Byte
  rgbGreen As Byte
  rgbRed As Byte
  rgbReserved As Byte
End Type

Private Type BITMAPINFO
  bmiHeader As BITMAPINFOHEADER
  bmiColors(1) As RGBQUAD
End Type

' Halftone created for default palette use
Private m_hpalHalftone As Long

'-------------------------------------------------------------------------
' Purpose:   Creates a disabled-appearing (grayed) bitmap, given any format of
'            input bitmap.
' In:
'    [hdcDest]
'            Device context to paint the picture on
'    [xDest]
'            X coordinate of the upper left corner of the area that the
'            picture is to be painted on. (in pixels)
'    [yDest]
'            Y coordinate of the upper left corner of the area that the
'            picture is to be painted on. (in pixels)
'    [Width]
'            Width of picture area to paint in pixels.  Note: If this value
'            is outrageous (i.e.: you passed a forms ScaleWidth in twips
'            instead of the pictures' width in pixels), this procedure will
'            attempt to create bitmaps that require outrageous
'            amounts of memory.
'    [Height]
'            Height of picture area to paint in pixels.  Note: If this
'            value is outrageous (i.e.: you passed a forms ScaleHeight in
'            twips instead of the pictures' height in pixels), this
'            procedure will attempt to create bitmaps that require
'            outrageous amounts of memory.
'    [picSource]
'            Standard Picture object to be used as the image source
'    [xSrc]
'            X coordinate of the upper left corner of the area in the picture
'            to use as the source. (in pixels)
'            Ignored if picSource is an Icon.
'    [ySrc]
'            Y coordinate of the upper left corner of the area in the picture
'            to use as the source. (in pixels)
'            Ignored if picSource is an Icon.
'    [clrMask]
'            Color of pixels to be masked out
'    [clrHighlight]
'            Color to be used as outline highlight
'    [clrShadow]
'            Color to be used as outline shadow
'    [hPal]
'            Handle of palette to select into the memory DC's used to create
'            the painting effect.
'            If not provided, a HalfTone palette is used.
'-------------------------------------------------------------------------
Public Sub PaintDisabledStdPic(ByVal hdcDest As Long, _
                                ByVal xDest As Long, _
                                ByVal yDest As Long, _
                                ByVal Width As Long, _
                                ByVal Height As Long, _
                                ByVal picSource As StdPicture, _
                                ByVal xSrc As Long, _
                                ByVal ySrc As Long, _
                                Optional ByVal clrMask As OLE_COLOR = vbWhite, _
                                Optional ByVal clrHighlight As OLE_COLOR = vb3DHighlight, _
                                Optional ByVal clrShadow As OLE_COLOR = vb3DShadow, _
                                Optional ByVal hPal As Long = 0)
Attribute PaintDisabledStdPic.VB_Description = "Paints a disabled appearing image (embossed) given a source picture object."
    
  ' HDC that the source bitmap is selected into
  Dim hdcSrc As Long
  Dim hbmMemSrcOld As Long
  Dim hbmMemSrc As Long
  Dim udtRect As RECT
  Dim hbrMask As Long
  Dim lMaskColor As Long
  Dim hDcScreen As Long
  Dim hPalOld As Long
  
  ' Verify that the passed picture is not nothing
  If Not picSource Is Nothing Then
    Select Case picSource.Type
      Case vbPicTypeBitmap
        'Select passed picture into an HDC
        hDcScreen = GetDC(0&)
        'Validate palette
        If hPal = 0 Then
          hPal = m_hpalHalftone
        End If
        hdcSrc = CreateCompatibleDC(hDcScreen)
        hbmMemSrcOld = SelectObject(hdcSrc, picSource.handle)
        hPalOld = SelectPalette(hdcSrc, hPal, True)
        RealizePalette hdcSrc
        
        'Draw the bitmap
        PaintDisabledDC hdcDest, xDest, yDest, Width, Height, hdcSrc, xSrc, ySrc, clrMask, clrHighlight, clrShadow, hPal
        
        SelectObject hdcSrc, hbmMemSrcOld
        SelectPalette hdcSrc, hPalOld, True
        RealizePalette hdcSrc
        DeleteDC hdcSrc
        ReleaseDC 0&, hDcScreen
      
      Case vbPicTypeIcon
        'Create a bitmap and select it into a DC
        hDcScreen = GetDC(0&)
        'Validate palette
        If hPal = 0 Then
          hPal = m_hpalHalftone
        End If
        hdcSrc = CreateCompatibleDC(hDcScreen)
        hbmMemSrc = CreateCompatibleBitmap(hDcScreen, Width, Height)
        hbmMemSrcOld = SelectObject(hdcSrc, hbmMemSrc)
        hPalOld = SelectPalette(hdcSrc, hPal, True)
        RealizePalette hdcSrc
        'Draw Icon onto DC
        udtRect.Bottom = Height
        udtRect.Right = Width
        OleTranslateColor clrMask, 0&, lMaskColor
        SetBkColor hdcSrc, lMaskColor
        hbrMask = CreateSolidBrush(lMaskColor)
        FillRect hdcSrc, udtRect, hbrMask
        DeleteObject hbrMask
        'DrawIcon hdcSrc, 0, 0, picSource.handle
        DrawIconEx hdcSrc, 0, 0, picSource.handle, Width, Height, 0&, 0&, DI_NORMAL
        'Draw Disabled image
        PaintDisabledDC hdcDest, xDest, yDest, Width, Height, hdcSrc, 0&, 0&, clrMask, clrHighlight, clrShadow, hPal
        'Clean up
        SelectPalette hdcSrc, hPalOld, True
        RealizePalette hdcSrc
        DeleteObject SelectObject(hdcSrc, hbmMemSrcOld)
        DeleteDC hdcSrc
        ReleaseDC 0&, hDcScreen
      
      Case Else
        ' Do nothing
    End Select
  End If
End Sub

'-------------------------------------------------------------------------
'Purpose:   Creates a disabled-appearing (grayed) bitmap, given any format of
'           input bitmap.
'In:
'   [hdcDest]
'           Device context to paint the picture on
'   [xDest]
'           X coordinate of the upper left corner of the area that the
'           picture is to be painted on. (in pixels)
'   [yDest]
'           Y coordinate of the upper left corner of the area that the
'           picture is to be painted on. (in pixels)
'   [Width]
'           Width of picture area to paint in pixels.  Note: If this value
'           is outrageous (i.e.: you passed a forms ScaleWidth in twips
'           instead of the pictures' width in pixels), this procedure will
'           attempt to create bitmaps that require outrageous
'           amounts of memory.
'   [Height]
'           Height of picture area to paint in pixels.  Note: If this
'           value is outrageous (i.e.: you passed a forms ScaleHeight in
'           twips instead of the pictures' height in pixels), this
'           procedure will attempt to create bitmaps that require
'           outrageous amounts of memory.
'   [hdcSrc]
'           Device context that contains the source picture
'   [xSrc]
'           X coordinate of the upper left corner of the area in the picture
'           to use as the source. (in pixels)
'   [ySrc]
'           Y coordinate of the upper left corner of the area in the picture
'           to use as the source. (in pixels)
'   [clrMask]
'           Color of pixels to be masked out
'   [clrHighlight]
'           Color to be used as outline highlight
'   [clrShadow]
'           Color to be used as outline shadow
'   [hPal]
'           Handle of palette to select into the memory DC's used to create
'           the painting effect.
'           If not provided, a HalfTone palette is used.
'-------------------------------------------------------------------------
Public Sub PaintDisabledDC(ByVal hdcDest As Long, _
                                ByVal xDest As Long, _
                                ByVal yDest As Long, _
                                ByVal Width As Long, _
                                ByVal Height As Long, _
                                ByVal hdcSrc As Long, _
                                ByVal xSrc As Long, _
                                ByVal ySrc As Long, _
                                Optional ByVal clrMask As OLE_COLOR = vbWhite, _
                                Optional ByVal clrHighlight As OLE_COLOR = vb3DHighlight, _
                                Optional ByVal clrShadow As OLE_COLOR = vb3DShadow, _
                                Optional ByVal hPal As Long = 0)
Attribute PaintDisabledDC.VB_Description = "Paints a disabled appearing image (embossed) given a source hDC."
    Dim hDcScreen As Long
    Dim hbmMonoSection As Long
    Dim hbmMonoSectionSav As Long
    Dim hdcMonoSection As Long
    Dim hdcColor As Long
    Dim hdcDisabled As Long
    Dim hbmDisabledSav As Long
    Dim lpbi As BITMAPINFO
    Dim hbmMono As Long
    Dim hdcMono As Long
    Dim hbmMonoSav As Long
    Dim lMaskColor As Long
    Dim lMaskColorCompare As Long
    Dim hdcMaskedSource As Long
    Dim hbmMasked As Long
    Dim hbmMaskedOld As Long
    Dim hpalMaskedOld As Long
    Dim hpalDisabledOld As Long
    Dim hpalMonoOld As Long
    Dim rgbBlack As RGBQUAD
    Dim rgbWhite As RGBQUAD
    Dim dwSys3dShadow As Long
    Dim dwSys3dHighlight As Long
    Dim pvBits As Long
    Dim rgbnew(1) As RGBQUAD
    Dim hbmDisabled As Long
    Dim lMonoBkGrnd As Long
    Dim lMonoBkGrndChoices(2) As Long
    Dim lIndex As Long  'For ... Next index
    Dim hbrWhite As Long
    Dim udtRect As RECT
    
    'TODO: handle pictures with dark masks
    If hPal = 0 Then
        hPal = m_hpalHalftone
    End If
    
    ' Define some colors
    OleTranslateColor clrShadow, hPal, dwSys3dShadow
    OleTranslateColor clrHighlight, hPal, dwSys3dHighlight
    
    hDcScreen = GetDC(0&)
    With rgbBlack
        .rgbBlue = 0
        .rgbGreen = 0
        .rgbRed = 0
        .rgbReserved = 0
    End With
    With rgbWhite
        .rgbBlue = 255
        .rgbGreen = 255
        .rgbRed = 255
        .rgbReserved = 255
    End With

    ' The first step is to create a monochrome bitmap with two colors:
    ' white where colors in the original are light, and black
    ' where the original is dark.  We can't simply bitblt to a bitmap.
    ' Instead, we create a monochrome (bichrome?) DIB section and bitblt
    ' to that.  Windows will do the conversion automatically based on the
    ' DIB section's palette.  (I.e. using a DIB section, Windows knows how
    ' to map "light" colors and "dark" colors to white/black, respectively.
    With lpbi.bmiHeader
        .biSize = LenB(lpbi.bmiHeader)
        .biWidth = Width
        .biHeight = -Height
        .biPlanes = 1
        .biBitCount = 1         ' monochrome
        .biCompression = BI_RGB
        .biSizeImage = 0
        .biXPelsPerMeter = 0
        .biYPelsPerMeter = 0
        .biClrUsed = 0          ' max colors used (2^1 = 2)
        .biClrImportant = 0     ' all (both :-]) colors are important
    End With
    With lpbi
        .bmiColors(0) = rgbBlack
        .bmiColors(1) = rgbWhite
    End With

    hbmMonoSection = CreateDIBSection(hDcScreen, lpbi, DIB_RGB_COLORS, pvBits, 0&, 0)
    
    hdcMonoSection = CreateCompatibleDC(hDcScreen)
    hbmMonoSectionSav = SelectObject(hdcMonoSection, hbmMonoSection)
    
    'Bitblt to the Monochrome DIB section
    'If a mask color is provided, create a new bitmap and copy the source
    'to it transparently.  If we don't do this, a dark mask color will be
    'turned into the outline part of the monochrome DIB section
    'Convert mask color and white before comparing
    'because the Mask color might be a system color that would be evaluated
    'to white.
    OleTranslateColor vbWhite, hPal, lMaskColorCompare
    OleTranslateColor clrMask, hPal, lMaskColor
    If lMaskColor = lMaskColorCompare Then
        BitBlt hdcMonoSection, 0, 0, Width, Height, hdcSrc, xSrc, ySrc, vbSrcCopy
    Else
        hbmMasked = CreateCompatibleBitmap(hDcScreen, Width, Height)
        hdcMaskedSource = CreateCompatibleDC(hDcScreen)
        hbmMaskedOld = SelectObject(hdcMaskedSource, hbmMasked)
        hpalMaskedOld = SelectPalette(hdcMaskedSource, hPal, True)
        RealizePalette hdcMaskedSource
        'Fill the bitmap with white
        With udtRect
            .Left = 0
            .Top = 0
            .Right = Width
            .Bottom = Height
        End With
        hbrWhite = CreateSolidBrush(vbWhite)
        FillRect hdcMaskedSource, udtRect, hbrWhite
        DeleteObject hbrWhite
        'Do the transparent paint
        PaintTransparentDC hdcMaskedSource, 0, 0, Width, Height, hdcSrc, xSrc, ySrc, lMaskColor, hPal
        'BitBlt to the Mono DIB section.  The mask color has been turned to white.
        BitBlt hdcMonoSection, 0, 0, Width, Height, hdcMaskedSource, 0, 0, vbSrcCopy
        'Clean up
        SelectPalette hdcMaskedSource, hpalMaskedOld, True
        RealizePalette hdcMaskedSource
        DeleteObject SelectObject(hdcMaskedSource, hbmMaskedOld)
        DeleteDC hdcMaskedSource
    End If
      
    ' Okay, we've got our B&W DIB section.
    ' Now that we have our monochrome bitmap, the final appearance that we
    ' want is this:  First, think of the black portion of the monochrome
    ' bitmap as our new version of the original bitmap.  We want to have a dark
    ' gray version of this with a light version underneath it, shifted down and
    ' to the right.  The light acts as a highlight, and it looks like the original
    ' image is a gray inset.
    
    ' First, create a copy of the destination.  Draw the light gray transparently,
    ' and then draw the dark gray transparently
    
    hbmDisabled = CreateCompatibleBitmap(hDcScreen, Width, Height)
    
    hdcDisabled = CreateCompatibleDC(hDcScreen)
    hbmDisabledSav = SelectObject(hdcDisabled, hbmDisabled)
    hpalDisabledOld = SelectPalette(hdcDisabled, hPal, True)
    RealizePalette hdcDisabled
    'We used to fill the background with gray, instead copy the
    'destination to memory DC.  This will allow a disabled image
    'to be drawn over a background image.
    BitBlt hdcDisabled, 0, 0, Width, Height, hdcDest, xDest, yDest, vbSrcCopy
    
    'When painting the monochrome bitmaps transparently onto the background
    'we need a background color that is not the light color of the dark color
    'Provide three choices to ensure a unique color is picked.
    OleTranslateColor vbBlack, hPal, lMonoBkGrndChoices(0)
    OleTranslateColor vbRed, hPal, lMonoBkGrndChoices(1)
    OleTranslateColor vbBlue, hPal, lMonoBkGrndChoices(2)
    
    'Pick a background color choice that doesn't match
    'the shadow or highlight color
    For lIndex = 0 To 2
        If lMonoBkGrndChoices(lIndex) <> dwSys3dHighlight And _
                lMonoBkGrndChoices(lIndex) <> dwSys3dShadow Then
            'This color can be used for a mask
            lMonoBkGrnd = lMonoBkGrndChoices(lIndex)
            Exit For
        End If
    Next

    ' Now paint a the light color shifted and transparent over the background
    ' It is not necessary to change the DIB section's color table
    ' to equal the highlight color and mask color.  In fact, setting
    ' the color table to anything besides black and white causes unpredictable
    ' results (seen in win95 with IE4, using 256 colors).
    ' Setting the Back and Text colors of the Monochrome bitmap, ensure
    ' that the desired colors are produced.
    With rgbnew(0)
        .rgbRed = (vbWhite \ 2 ^ 16) And &HFF
        .rgbGreen = (vbWhite \ 2 ^ 8) And &HFF
        .rgbBlue = vbWhite And &HFF
    End With
    With rgbnew(1)
        .rgbRed = (vbBlack \ 2 ^ 16) And &HFF
        .rgbGreen = (vbBlack \ 2 ^ 8) And &HFF
        .rgbBlue = vbBlack And &HFF
    End With
        
    SetDIBColorTable hdcMonoSection, 0, 2, rgbnew(0)
    
    '...We can't pass a DIBSection to PaintTransparentDC(), so we need to
    ' make a copy of our mono DIBSection.  Notice that we only need a monochrome
    ' bitmap, but we must set its back/fore colors to the monochrome colors we
    ' want (light gray and black), and PaintTransparentDC() will honor them.
    hbmMono = CreateBitmap(Width, Height, 1, 1, ByVal 0&)
    hdcMono = CreateCompatibleDC(hDcScreen)
    hbmMonoSav = SelectObject(hdcMono, hbmMono)
    SetMapMode hdcMono, GetMapMode(hdcSrc)
    SetBkColor hdcMono, dwSys3dHighlight
    SetTextColor hdcMono, lMonoBkGrnd
    hpalMonoOld = SelectPalette(hdcMono, hPal, True)
    RealizePalette hdcMono
    BitBlt hdcMono, 0, 0, Width, Height, hdcMonoSection, 0, 0, vbSrcCopy

    '...We can go ahead and call PaintTransparentDC with our monochrome
    ' copy
    ' Draw this transparently over the disabled bitmap
    '...Don't forget to shift right and left....
    PaintTransparentDC hdcDisabled, 1, 1, Width, Height, hdcMono, 0, 0, lMonoBkGrnd, hPal
    
    ' Now draw a transparent copy, using dark gray where the monochrome had
    ' black, and transparent elsewhere.  We'll use a transparent color of black.

    '...We can't pass a DIBSection to PaintTransparentDC(), so we need to
    ' make a copy of our mono DIBSection.  Notice that we only need a monochrome
    ' bitmap, but we must set its back/fore colors to the monochrome colors we
    ' want (dark gray and black), and PaintTransparentDC() will honor them.
    ' Use hbmMono and hdcMono; already created for first color
    SetBkColor hdcMono, dwSys3dShadow
    SetTextColor hdcMono, lMonoBkGrnd
    BitBlt hdcMono, 0, 0, Width, Height, hdcMonoSection, 0, 0, vbSrcCopy

    '...We can go ahead and call PaintTransparentDC with our monochrome
    ' copy
    ' Draw this transparently over the disabled bitmap
    PaintTransparentDC hdcDisabled, 0, 0, Width, Height, hdcMono, 0, 0, lMonoBkGrnd, hPal
    BitBlt hdcDest, xDest, yDest, Width, Height, hdcDisabled, 0, 0, vbSrcCopy
    ' Okay, we're done!
    SelectPalette hdcDisabled, hpalDisabledOld, True
    RealizePalette hdcDisabled
    DeleteObject SelectObject(hdcMonoSection, hbmMonoSectionSav)
    DeleteDC hdcMonoSection
    DeleteObject SelectObject(hdcDisabled, hbmDisabledSav)
    DeleteDC hdcDisabled
    DeleteObject SelectObject(hdcMono, hbmMonoSav)
    SelectPalette hdcMono, hpalMonoOld, True
    RealizePalette hdcMono
    DeleteDC hdcMono
    ReleaseDC 0&, hDcScreen
End Sub

'-------------------------------------------------------------------------
'Purpose:   Draws a transparent bitmap to a DC.  The pixels of the passed
'           bitmap that match the passed mask color will not be painted
'           to the destination DC
'In:
'   [hdcDest]
'           Device context to paint the picture on
'   [xDest]
'           X coordinate of the upper left corner of the area that the
'           picture is to be painted on. (in pixels)
'   [yDest]
'           Y coordinate of the upper left corner of the area that the
'           picture is to be painted on. (in pixels)
'   [Width]
'           Width of picture area to paint in pixels.  Note: If this value
'           is outrageous (i.e.: you passed a forms ScaleWidth in twips
'           instead of the pictures' width in pixels), this procedure will
'           attempt to create bitmaps that require outrageous
'           amounts of memory.
'   [Height]
'           Height of picture area to paint in pixels.  Note: If this
'           value is outrageous (i.e.: you passed a forms ScaleHeight in
'           twips instead of the pictures' height in pixels), this
'           procedure will attempt to create bitmaps that require
'           outrageous amounts of memory.
'   [hdcSrc]
'           Device context that contains the source picture
'   [xSrc]
'           X coordinate of the upper left corner of the area in the picture
'           to use as the source. (in pixels)
'   [ySrc]
'           Y coordinate of the upper left corner of the area in the picture
'           to use as the source. (in pixels)
'   [clrMask]
'           Color of pixels to be masked out
'   [hPal]
'           Handle of palette to select into the memory DC's used to create
'           the painting effect.
'           If not provided, a HalfTone palette is used.
'-------------------------------------------------------------------------
Public Sub PaintTransparentDC(ByVal hdcDest As Long, _
                                    ByVal xDest As Long, _
                                    ByVal yDest As Long, _
                                    ByVal Width As Long, _
                                    ByVal Height As Long, _
                                    ByVal hdcSrc As Long, _
                                    ByVal xSrc As Long, _
                                    ByVal ySrc As Long, _
                                    ByVal clrMask As OLE_COLOR, _
                                    Optional ByVal hPal As Long = 0)
Attribute PaintTransparentDC.VB_Description = "Paints an image with transparent pixels defined by the mask color.  Accepts an hDC as its image source."
    Dim hdcMask As Long        'HDC of the created mask image
    Dim hdcColor As Long       'HDC of the created color image
    Dim hbmMask As Long        'Bitmap handle to the mask image
    Dim hbmColor As Long       'Bitmap handle to the color image
    Dim hbmColorOld As Long
    Dim hbmMaskOld As Long
    Dim hPalOld As Long
    Dim hDcScreen As Long
    Dim hdcScnBuffer As Long         'Buffer to do all work on
    Dim hbmScnBuffer As Long
    Dim hbmScnBufferOld As Long
    Dim hPalBufferOld As Long
    Dim lMaskColor As Long
    
    hDcScreen = GetDC(0&)
    'Validate palette
    If hPal = 0 Then
        hPal = m_hpalHalftone
    End If
    OleTranslateColor clrMask, hPal, lMaskColor
    
    'Create a color bitmap to server as a copy of the destination
    'Do all work on this bitmap and then copy it back over the destination
    'when it's done.
    hbmScnBuffer = CreateCompatibleBitmap(hDcScreen, Width, Height)
    'Create DC for screen buffer
    hdcScnBuffer = CreateCompatibleDC(hDcScreen)
    hbmScnBufferOld = SelectObject(hdcScnBuffer, hbmScnBuffer)
    hPalBufferOld = SelectPalette(hdcScnBuffer, hPal, True)
    RealizePalette hdcScnBuffer
    'Copy the destination to the screen buffer
    BitBlt hdcScnBuffer, 0, 0, Width, Height, hdcDest, xDest, yDest, vbSrcCopy
    
    'Create a (color) bitmap for the cover (can't use CompatibleBitmap with
    'hdcSrc, because this will create a DIB section if the original bitmap
    'is a DIB section)
    hbmColor = CreateCompatibleBitmap(hDcScreen, Width, Height)
    'Now create a monochrome bitmap for the mask
    hbmMask = CreateBitmap(Width, Height, 1, 1, ByVal 0&)
    'First, blt the source bitmap onto the cover.  We do this first
    'and then use it instead of the source bitmap
    'because the source bitmap may be
    'a DIB section, which behaves differently than a bitmap.
    '(Specifically, copying from a DIB section to a monochrome bitmap
    'does a nearest-color selection rather than painting based on the
    'backcolor and forecolor.
    hdcColor = CreateCompatibleDC(hDcScreen)
    hbmColorOld = SelectObject(hdcColor, hbmColor)
    hPalOld = SelectPalette(hdcColor, hPal, True)
    RealizePalette hdcColor
    'In case hdcSrc contains a monochrome bitmap, we must set the destination
    'foreground/background colors according to those currently set in hdcSrc
    '(because Windows will associate these colors with the two monochrome colors)
    SetBkColor hdcColor, GetBkColor(hdcSrc)
    SetTextColor hdcColor, GetTextColor(hdcSrc)
    BitBlt hdcColor, 0, 0, Width, Height, hdcSrc, xSrc, ySrc, vbSrcCopy
    'Paint the mask.  What we want is white at the transparent color
    'from the source, and black everywhere else.
    hdcMask = CreateCompatibleDC(hDcScreen)
    hbmMaskOld = SelectObject(hdcMask, hbmMask)

    'When bitblt'ing from color to monochrome, Windows sets to 1
    'all pixels that match the background color of the source DC.  All
    'other bits are set to 0.
    SetBkColor hdcColor, lMaskColor
    SetTextColor hdcColor, vbWhite
    BitBlt hdcMask, 0, 0, Width, Height, hdcColor, 0, 0, vbSrcCopy
    'Paint the rest of the cover bitmap.
    '
    'What we want here is black at the transparent color, and
    'the original colors everywhere else.  To do this, we first
    'paint the original onto the cover (which we already did), then we
    'AND the inverse of the mask onto that using the DSna ternary raster
    'operation (0x00220326 - see Win32 SDK reference, Appendix, "Raster
    'Operation Codes", "Ternary Raster Operations", or search in MSDN
    'for 00220326).  DSna [reverse polish] means "(not SRC) and DEST".
    '
    'When bitblt'ing from monochrome to color, Windows transforms all white
    'bits (1) to the background color of the destination hdc.  All black (0)
    'bits are transformed to the foreground color.
    SetTextColor hdcColor, vbBlack
    SetBkColor hdcColor, vbWhite
    BitBlt hdcColor, 0, 0, Width, Height, hdcMask, 0, 0, DSna
    'Paint the Mask to the Screen buffer
    BitBlt hdcScnBuffer, 0, 0, Width, Height, hdcMask, 0, 0, vbSrcAnd
    'Paint the Color to the Screen buffer
    BitBlt hdcScnBuffer, 0, 0, Width, Height, hdcColor, 0, 0, vbSrcPaint
    'Copy the screen buffer to the screen
    BitBlt hdcDest, xDest, yDest, Width, Height, hdcScnBuffer, 0, 0, vbSrcCopy
    'All done!
    DeleteObject SelectObject(hdcColor, hbmColorOld)
    SelectPalette hdcColor, hPalOld, True
    RealizePalette hdcColor
    DeleteDC hdcColor
    DeleteObject SelectObject(hdcScnBuffer, hbmScnBufferOld)
    SelectPalette hdcScnBuffer, hPalBufferOld, True
    RealizePalette hdcScnBuffer
    DeleteDC hdcScnBuffer
    
    DeleteObject SelectObject(hdcMask, hbmMaskOld)
    DeleteDC hdcMask
    ReleaseDC 0&, hDcScreen
End Sub

'-------------------------------------------------------------------------
'Purpose:   Draws a transparent bitmap to a DC.  The pixels of the passed
'           bitmap that match the passed mask color will not be painted
'           to the destination DC
'In:
'   [hdcDest]
'           Device context to paint the picture on
'   [xDest]
'           X coordinate of the upper left corner of the area that the
'           picture is to be painted on. (in pixels)
'   [yDest]
'           Y coordinate of the upper left corner of the area that the
'           picture is to be painted on. (in pixels)
'   [Width]
'           Width of picture area to paint in pixels.  Note: If this value
'           is outrageous (i.e.: you passed a forms ScaleWidth in twips
'           instead of the pictures' width in pixels), this procedure will
'           attempt to create bitmaps that require outrageous
'           amounts of memory.
'   [Height]
'           Height of picture area to paint in pixels.  Note: If this
'           value is outrageous (i.e.: you passed a forms ScaleHeight in
'           twips instead of the pictures' height in pixels), this
'           procedure will attempt to create bitmaps that require
'           outrageous amounts of memory.
'   [picSource]
'           Standard Picture object to be used as the image source
'   [xSrc]
'           X coordinate of the upper left corner of the area in the picture
'           to use as the source. (in pixels)
'           Ignored if picSource is an Icon.
'   [ySrc]
'           Y coordinate of the upper left corner of the area in the picture
'           to use as the source. (in pixels)
'           Ignored if picSource is an Icon.
'   [clrMask]
'           Color of pixels to be masked out
'   [hPal]
'           Handle of palette to select into the memory DC's used to create
'           the painting effect.
'           If not provided, a HalfTone palette is used.
'-------------------------------------------------------------------------
Public Sub PaintTransparentStdPic(ByVal hdcDest As Long, _
                                    ByVal xDest As Long, _
                                    ByVal yDest As Long, _
                                    ByVal Width As Long, _
                                    ByVal Height As Long, _
                                    ByVal picSource As Picture, _
                                    ByVal xSrc As Long, _
                                    ByVal ySrc As Long, _
                                    ByVal clrMask As OLE_COLOR, _
                                    Optional ByVal hPal As Long = 0)
Attribute PaintTransparentStdPic.VB_Description = "Paints an image with transparent pixels defined by the mask color.  Accepts a picture object as its image source."
    Dim hdcSrc As Long         'HDC that the source bitmap is selected into
    Dim hbmMemSrcOld As Long
    Dim hbmMemSrc As Long
    Dim udtRect As RECT
    Dim hbrMask As Long
    Dim lMaskColor As Long
    Dim hDcScreen As Long
    Dim hPalOld As Long
    'Verify that the passed picture is a Bitmap
    If picSource Is Nothing Then GoTo PaintTransparentStdPic_InvalidParam
    
    Select Case picSource.Type
        Case vbPicTypeBitmap
            hDcScreen = GetDC(0&)
            'Validate palette
            If hPal = 0 Then
                hPal = m_hpalHalftone
            End If
            'Select passed picture into an HDC
            hdcSrc = CreateCompatibleDC(hDcScreen)
            hbmMemSrcOld = SelectObject(hdcSrc, picSource.handle)
            hPalOld = SelectPalette(hdcSrc, hPal, True)
            RealizePalette hdcSrc
            'Draw the bitmap
            PaintTransparentDC hdcDest, xDest, yDest, Width, Height, hdcSrc, xSrc, ySrc, clrMask, hPal
            
            SelectObject hdcSrc, hbmMemSrcOld
            SelectPalette hdcSrc, hPalOld, True
            RealizePalette hdcSrc
            DeleteDC hdcSrc
            ReleaseDC 0&, hDcScreen
        Case vbPicTypeIcon
            'Create a bitmap and select it into an DC
            hDcScreen = GetDC(0&)
            'Validate palette
            If hPal = 0 Then
                hPal = m_hpalHalftone
            End If
            hdcSrc = CreateCompatibleDC(hDcScreen)
            hbmMemSrc = CreateCompatibleBitmap(hDcScreen, Width, Height)
            hbmMemSrcOld = SelectObject(hdcSrc, hbmMemSrc)
            hPalOld = SelectPalette(hdcSrc, hPal, True)
            RealizePalette hdcSrc
            'Draw Icon onto DC
            udtRect.Bottom = Height
            udtRect.Right = Width
            OleTranslateColor clrMask, 0&, lMaskColor
            hbrMask = CreateSolidBrush(lMaskColor)
            FillRect hdcSrc, udtRect, hbrMask
            DeleteObject hbrMask
            'DrawIcon hdcSrc, 0, 0, picSource.handle
            DrawIconEx hdcSrc, 0, 0, picSource.handle, Width, Height, 0&, 0&, DI_NORMAL
            'Draw Transparent image
            PaintTransparentDC hdcDest, xDest, yDest, Width, Height, hdcSrc, 0, 0, lMaskColor, hPal
            'Clean up
            DeleteObject SelectObject(hdcSrc, hbmMemSrcOld)
            SelectPalette hdcSrc, hPalOld, True
            RealizePalette hdcSrc
            DeleteDC hdcSrc
            ReleaseDC 0&, hDcScreen
        Case Else
            GoTo PaintTransparentStdPic_InvalidParam
    End Select
    Exit Sub
PaintTransparentStdPic_InvalidParam:
    Err.Raise giINVALID_PICTURE
    Exit Sub
End Sub

'-------------------------------------------------------------------------
'Purpose:   Draws a standard picture object to a DC
'In:
'   [hdcDest]
'           Handle of the device context to paint the picture on
'   [xDest]
'           X coordinate of the upper left corner of the area that the
'           picture is to be painted on. (in pixels)
'   [yDest]
'           Y coordinate of the upper left corner of the area that the
'           picture is to be painted on. (in pixels)
'   [Width]
'           Width of picture area to paint in pixels.  Note: If this value
'           is outrageous (i.e.: you passed a forms ScaleWidth in twips
'           instead of the pictures' width in pixels), this procedure will
'           attempt to create bitmaps that require outrageous
'           amounts of memory.
'   [Height]
'           Height of picture area to paint in pixels.  Note: If this
'           value is outrageous (i.e.: you passed a forms ScaleHeight in
'           twips instead of the pictures' height in pixels), this
'           procedure will attempt to create bitmaps that require
'           outrageous amounts of memory.
'   [picSource]
'           Standard Picture object to be used as the image source
'   [xSrc]
'           X coordinate of the upper left corner of the area in the picture
'           to use as the source. (in pixels)
'           Ignored if picSource is an Icon.
'   [ySrc]
'           Y coordinate of the upper left corner of the area in the picture
'           to use as the source. (in pixels)
'           Ignored if picSource is an Icon.
'   [hPal]
'           Handle of palette to select into the memory DC's used to create
'           the painting effect.
'           If not provided, a HalfTone palette is used.
'-------------------------------------------------------------------------
Public Sub PaintNormalStdPic(ByVal hdcDest As Long, _
                                    ByVal xDest As Long, _
                                    ByVal yDest As Long, _
                                    ByVal Width As Long, _
                                    ByVal Height As Long, _
                                    ByVal picSource As Picture, _
                                    ByVal xSrc As Long, _
                                    ByVal ySrc As Long, _
                                    Optional ByVal hPal As Long = 0)
Attribute PaintNormalStdPic.VB_Description = "Paints an image provided by a picture object to an hDC with no effects."
    Dim hdcTemp As Long
    Dim hPalOld As Long
    Dim hbmMemSrcOld As Long
    Dim hDcScreen As Long
    Dim hbmMemSrc As Long
    'Validate that a bitmap was passed in
    If picSource Is Nothing Then GoTo PaintNormalStdPic_InvalidParam
    Select Case picSource.Type
        Case vbPicTypeBitmap
            If hPal = 0 Then
                hPal = m_hpalHalftone
            End If
            hDcScreen = GetDC(0&)
            'Create a DC to select bitmap into
            hdcTemp = CreateCompatibleDC(hDcScreen)
            hPalOld = SelectPalette(hdcTemp, hPal, True)
            RealizePalette hdcTemp
            'Select bitmap into DC
            hbmMemSrcOld = SelectObject(hdcTemp, picSource.handle)
            'Copy to destination DC
            BitBlt hdcDest, xDest, yDest, Width, Height, hdcTemp, xSrc, ySrc, vbSrcCopy
            'Cleanup
            SelectObject hdcTemp, hbmMemSrcOld
            SelectPalette hdcTemp, hPalOld, True
            RealizePalette hdcTemp
            DeleteDC hdcTemp
            ReleaseDC 0&, hDcScreen
        Case vbPicTypeIcon
            'Create a bitmap and select it into an DC
            'Draw Icon onto DC
            DrawIconEx hdcDest, xDest, yDest, picSource.handle, Width, Height, 0&, 0&, DI_NORMAL
        Case Else
            GoTo PaintNormalStdPic_InvalidParam
    End Select
    Exit Sub
PaintNormalStdPic_InvalidParam:
    Err.Raise giINVALID_PICTURE
End Sub

Private Sub Class_Initialize()
    Dim hDcScreen As Long
    'Create halftone palette
    hDcScreen = GetDC(0&)
    m_hpalHalftone = CreateHalftonePalette(hDcScreen)
    ReleaseDC 0&, hDcScreen
End Sub

Private Sub Class_Terminate()
    DeleteObject m_hpalHalftone
End Sub

Public Sub PaintTransCornerDC(ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long)
   '
   ' 32-Bit Transparent BitBlt Function
   ' Written by Geoff Glaze 2/13/98
   '
   ' Purpose:
   '    Creates a transparent bitmap using lower left pixel of source bitmap
   '
   ' Parameters ************************************************************
   '   hDestDC:     Destination device context
   '   x, y:        Upper-left destination coordinates (pixels)
   '   nWidth:      Width of destination
   '   nHeight:     Height of destination
   '   hSrcDC:      Source device context
   '   xSrc, ySrc:  Upper-left source coordinates (pixels)
   ' ***********************************************************************
   
   Dim iBackColor As Long
    
   iBackColor = GetPixel(hSrcDC, 0, 0)
   If iBackColor = CLR_INVALID Then
        'invalid color (specified point is outside of the clipping region)
        'use default grey (standard bitmap back color)
        iBackColor = &HC0C0C0
    End If
   
   PaintTransparentDC hDestDC, x, y, nWidth, nHeight, hSrcDC, xSrc, ySrc, iBackColor
   
End Sub

Public Sub PaintDisabledCornerDC(ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long)
   '
   ' 32-Bit Transparent BitBlt Function
   ' Written by Geoff Glaze 2/13/98
   '
   ' Purpose:
   '    Creates a transparent bitmap using lower left pixel of source bitmap
   '
   ' Parameters ************************************************************
   '   hDestDC:     Destination device context
   '   x, y:        Upper-left destination coordinates (pixels)
   '   nWidth:      Width of destination
   '   nHeight:     Height of destination
   '   hSrcDC:      Source device context
   '   xSrc, ySrc:  Upper-left source coordinates (pixels)
   ' ***********************************************************************
   
   Dim iBackColor As Long
    
   iBackColor = GetPixel(hSrcDC, 0, 0)
   If iBackColor = CLR_INVALID Then
        'invalid color (specified point is outside of the clipping region)
        'use default grey (standard bitmap back color)
        iBackColor = &HC0C0C0
    End If
   
   PaintDisabledDC hDestDC, x, y, nWidth, nHeight, hSrcDC, xSrc, ySrc, iBackColor
   
End Sub

Public Sub PaintTransCornerStdPic(ByVal hdcDest As Long, _
                                    ByVal xDest As Long, _
                                    ByVal yDest As Long, _
                                    ByVal Width As Long, _
                                    ByVal Height As Long, _
                                    ByVal picSource As Picture, _
                                    ByVal xSrc As Long, _
                                    ByVal ySrc As Long, _
                                    Optional ByVal hPal As Long = 0)
   '
   ' 32-Bit Transparent BitBlt Function
   ' Written by Geoff Glaze 2/13/98
   '
   ' Purpose:
   '    Creates a transparent bitmap using lower left pixel of source bitmap
   '
   ' Parameters ************************************************************
   '   hDestDC:     Destination device context
   '   x, y:        Upper-left destination coordinates (pixels)
   '   nWidth:      Width of destination
   '   nHeight:     Height of destination
   '   hSrcDC:      Source device context
   '   xSrc, ySrc:  Upper-left source coordinates (pixels)
   ' ***********************************************************************
   
    Dim hdcSrc As Long         'HDC that the source bitmap is selected into
    Dim hbmMemSrcOld As Long
    Dim hbmMemSrc As Long
    Dim udtRect As RECT
    Dim hbrMask As Long
    Dim lMaskColor As Long
    Dim hDcScreen As Long
    Dim hPalOld As Long
    'Verify that the passed picture is a Bitmap
    If picSource Is Nothing Then GoTo PaintTransCornerStdPic_InvalidParam
    
    Select Case picSource.Type
        Case vbPicTypeBitmap
            hDcScreen = GetDC(0&)
            'Validate palette
            If hPal = 0 Then
                hPal = m_hpalHalftone
            End If
            'Select passed picture into an HDC
            hdcSrc = CreateCompatibleDC(hDcScreen)
            hbmMemSrcOld = SelectObject(hdcSrc, picSource.handle)
            hPalOld = SelectPalette(hdcSrc, hPal, True)
            RealizePalette hdcSrc
            
            'get back color
            lMaskColor = GetPixel(hdcSrc, 0, 0)
            If lMaskColor = CLR_INVALID Then
                 'invalid color (specified point is outside of the clipping region)
                 'use default grey (standard bitmap back color)
                 lMaskColor = &HC0C0C0
            End If
            
            'Draw the bitmap
            PaintTransparentDC hdcDest, xDest, yDest, Width, Height, hdcSrc, xSrc, ySrc, lMaskColor, hPal
            
            SelectObject hdcSrc, hbmMemSrcOld
            SelectPalette hdcSrc, hPalOld, True
            RealizePalette hdcSrc
            DeleteDC hdcSrc
            ReleaseDC 0&, hDcScreen
        Case vbPicTypeIcon
'            'Create a bitmap and select it into an DC
'            hDcScreen = GetDC(0&)
'            'Validate palette
'            If hPal = 0 Then
'                hPal = m_hpalHalftone
'            End If
'            hdcSrc = CreateCompatibleDC(hDcScreen)
'            hbmMemSrc = CreateCompatibleBitmap(hDcScreen, Width, Height)
'            hbmMemSrcOld = SelectObject(hdcSrc, hbmMemSrc)
'            hPalOld = SelectPalette(hdcSrc, hPal, True)
'            RealizePalette hdcSrc
'            'Draw Icon onto DC
'            udtRect.Bottom = Height
'            udtRect.Right = Width
'
'            'get back color
'            lMaskColor = GetPixel(hdcSrc, 0, 0)
'            If lMaskColor = CLR_INVALID Then
'                 'invalid color (specified point is outside of the clipping region)
'                 'use default grey (standard bitmap back color)
'                 lMaskColor = &HC0C0C0
'            End If
'
''            OleTranslateColor clrMask, 0&, lMaskColor
'            hbrMask = CreateSolidBrush(lMaskColor)
'            FillRect hdcSrc, udtRect, hbrMask
'            DeleteObject hbrMask
'            DrawIcon hdcSrc, 0, 0, picSource.handle
'            'Draw Transparent image
'            PaintTransparentDC hdcDest, xDest, yDest, Width, Height, hdcSrc, 0, 0, lMaskColor, hPal
'            'Clean up
'            DeleteObject SelectObject(hdcSrc, hbmMemSrcOld)
'            SelectPalette hdcSrc, hPalOld, True
'            RealizePalette hdcSrc
'            DeleteDC hdcSrc
'            ReleaseDC 0&, hDcScreen
        
            'Create a bitmap and select it into an DC
            'Draw Icon onto DC
            DrawIconEx hdcDest, xDest, yDest, picSource.handle, Width, Height, 0&, 0&, DI_NORMAL
        Case Else
            GoTo PaintTransCornerStdPic_InvalidParam
    End Select
    Exit Sub

PaintTransCornerStdPic_InvalidParam:
    Err.Raise giINVALID_PICTURE
    Exit Sub
   
End Sub

Public Sub PaintDisabledCornerStdPic(ByVal hdcDest As Long, _
                                    ByVal xDest As Long, _
                                    ByVal yDest As Long, _
                                    ByVal Width As Long, _
                                    ByVal Height As Long, _
                                    ByVal picSource As Picture, _
                                    ByVal xSrc As Long, _
                                    ByVal ySrc As Long, _
                                    Optional ByVal hPal As Long = 0)
   '
   ' 32-Bit Transparent BitBlt Function
   ' Written by Geoff Glaze 2/13/98
   '
   ' Purpose:
   '    Creates a transparent bitmap using lower left pixel of source bitmap
   '
   ' Parameters ************************************************************
   '   hDestDC:     Destination device context
   '   x, y:        Upper-left destination coordinates (pixels)
   '   nWidth:      Width of destination
   '   nHeight:     Height of destination
   '   hSrcDC:      Source device context
   '   xSrc, ySrc:  Upper-left source coordinates (pixels)
   ' ***********************************************************************
   
    Dim hdcSrc As Long         'HDC that the source bitmap is selected into
    Dim hbmMemSrcOld As Long
    Dim hbmMemSrc As Long
    Dim udtRect As RECT
    Dim hbrMask As Long
    Dim lMaskColor As Long
    Dim hDcScreen As Long
    Dim hPalOld As Long
    'Verify that the passed picture is a Bitmap
    If picSource Is Nothing Then GoTo PaintDisabledCornerStdPic_InvalidParam
    
    Select Case picSource.Type
        Case vbPicTypeBitmap
            hDcScreen = GetDC(0&)
            'Validate palette
            If hPal = 0 Then
                hPal = m_hpalHalftone
            End If
            'Select passed picture into an HDC
            hdcSrc = CreateCompatibleDC(hDcScreen)
            hbmMemSrcOld = SelectObject(hdcSrc, picSource.handle)
            hPalOld = SelectPalette(hdcSrc, hPal, True)
            RealizePalette hdcSrc
            
            'get back color
            lMaskColor = GetPixel(hdcSrc, 0, 0)
            If lMaskColor = CLR_INVALID Then
                 'invalid color (specified point is outside of the clipping region)
                 'use default grey (standard bitmap back color)
                 lMaskColor = &HC0C0C0
            End If
            
            'Draw the bitmap
            PaintDisabledDC hdcDest, xDest, yDest, Width, Height, hdcSrc, xSrc, ySrc, lMaskColor, , , hPal
            
            SelectObject hdcSrc, hbmMemSrcOld
            SelectPalette hdcSrc, hPalOld, True
            RealizePalette hdcSrc
            DeleteDC hdcSrc
            ReleaseDC 0&, hDcScreen
        Case vbPicTypeIcon
'            'Create a bitmap and select it into an DC
'            hDcScreen = GetDC(0&)
'            'Validate palette
'            If hPal = 0 Then
'                hPal = m_hpalHalftone
'            End If
'            hdcSrc = CreateCompatibleDC(hDcScreen)
'            hbmMemSrc = CreateCompatibleBitmap(hDcScreen, Width, Height)
'            hbmMemSrcOld = SelectObject(hdcSrc, hbmMemSrc)
'            hPalOld = SelectPalette(hdcSrc, hPal, True)
'            RealizePalette hdcSrc
'            'Draw Icon onto DC
'            udtRect.Bottom = Height
'            udtRect.Right = Width
''            OleTranslateColor clrMask, 0&, lMaskColor
'
'            'get back color
'            lMaskColor = GetPixel(hdcSrc, 0, 0)
'            If lMaskColor = CLR_INVALID Then
'                 'invalid color (specified point is outside of the clipping region)
'                 'use default grey (standard bitmap back color)
'                 lMaskColor = &HC0C0C0
'            End If
'
'            hbrMask = CreateSolidBrush(lMaskColor)
'            FillRect hdcSrc, udtRect, hbrMask
'            DeleteObject hbrMask
'            DrawIcon hdcSrc, 0, 0, picSource.handle
'            'Draw Transparent image
'            PaintDisabledDC hdcDest, xDest, yDest, Width, Height, hdcSrc, 0, 0, lMaskColor, , , hPal
'            'Clean up
'            DeleteObject SelectObject(hdcSrc, hbmMemSrcOld)
'            SelectPalette hdcSrc, hPalOld, True
'            RealizePalette hdcSrc
'            DeleteDC hdcSrc
'            ReleaseDC 0&, hDcScreen

            'Create a bitmap and select it into an DC
            'Draw Icon onto DC
            DrawIconEx hdcDest, xDest, yDest, picSource.handle, Width, Height, 0&, 0&, DI_NORMAL
        Case Else
            GoTo PaintDisabledCornerStdPic_InvalidParam
    End Select
    Exit Sub

PaintDisabledCornerStdPic_InvalidParam:
    Err.Raise giINVALID_PICTURE
    Exit Sub
   
End Sub

Public Sub PaintGreyScaleCornerStdPic(ByVal hdcDest As Long, _
                                    ByVal xDest As Long, _
                                    ByVal yDest As Long, _
                                    ByVal Width As Long, _
                                    ByVal Height As Long, _
                                    ByVal picSource As Picture, _
                                    ByVal xSrc As Long, _
                                    ByVal ySrc As Long, _
                                    Optional ByVal hPal As Long = 0)
   '
   ' 32-Bit GreyScale BitBlt Function
   ' Written by Geoff Glaze 2/13/98
   '
   ' Purpose:
   '    Creates a greyscale version of a bitmap
   '
   ' Parameters ************************************************************
   '   hDestDC:     Destination device context
   '   x, y:        Upper-left destination coordinates (pixels)
   '   nWidth:      Width of destination
   '   nHeight:     Height of destination
   '   hSrcDC:      Source device context
   '   xSrc, ySrc:  Upper-left source coordinates (pixels)
   ' ***********************************************************************
   
    Dim hdcSrc As Long         'HDC that the source bitmap is selected into
    Dim hbmMemSrcOld As Long
    Dim hbmMemSrc As Long
    Dim udtRect As RECT
    Dim hbrMask As Long
    Dim lMaskColor As Long
    Dim hDcScreen As Long
    Dim hPalOld As Long
    Dim hBrush As Long
    'Verify that the passed picture is a Bitmap
    If picSource Is Nothing Then GoTo PaintGreyScaleCornerStdPic_InvalidParam
    
    hBrush = CreateSolidBrush(vbButtonShadow)
    Select Case picSource.Type
        Case vbPicTypeBitmap
            Call DrawState(hdcDest, hBrush, 0&, picSource, 0&, xDest, yDest, Width, Height, DST_BITMAP Or DSS_MONO)
        Case vbPicTypeIcon
            Call DrawState(hdcDest, hBrush, 0&, picSource, 0&, xDest, yDest, Width, Height, DST_ICON Or DSS_MONO)
        Case Else
            GoTo PaintGreyScaleCornerStdPic_InvalidParam
    End Select
    Exit Sub

PaintGreyScaleCornerStdPic_InvalidParam:
    Err.Raise giINVALID_PICTURE
    Exit Sub
   
End Sub

'-------------------------------------------------------------------------
'Purpose:   Draws a standard picture object to a DC in Greyscale
'In:
'   [hdcDest]
'           Handle of the device context to paint the picture on
'   [xDest]
'           X coordinate of the upper left corner of the area that the
'           picture is to be painted on. (in pixels)
'   [yDest]
'           Y coordinate of the upper left corner of the area that the
'           picture is to be painted on. (in pixels)
'   [Width]
'           Width of picture area to paint in pixels.  Note: If this value
'           is outrageous (i.e.: you passed a forms ScaleWidth in twips
'           instead of the pictures' width in pixels), this procedure will
'           attempt to create bitmaps that require outrageous
'           amounts of memory.
'   [Height]
'           Height of picture area to paint in pixels.  Note: If this
'           value is outrageous (i.e.: you passed a forms ScaleHeight in
'           twips instead of the pictures' height in pixels), this
'           procedure will attempt to create bitmaps that require
'           outrageous amounts of memory.
'   [picSource]
'           Standard Picture object to be used as the image source
'   [xSrc]
'           X coordinate of the upper left corner of the area in the picture
'           to use as the source. (in pixels)
'           Ignored if picSource is an Icon.
'   [ySrc]
'           Y coordinate of the upper left corner of the area in the picture
'           to use as the source. (in pixels)
'           Ignored if picSource is an Icon.
'   [hPal]
'           Handle of palette to select into the memory DC's used to create
'           the painting effect.
'           If not provided, a HalfTone palette is used.
'-------------------------------------------------------------------------
Public Sub PaintGreyScaleStdPic(ByVal hdcDest As Long, _
                                    ByVal xDest As Long, _
                                    ByVal yDest As Long, _
                                    ByVal Width As Long, _
                                    ByVal Height As Long, _
                                    ByVal picSource As Picture, _
                                    ByVal xSrc As Long, _
                                    ByVal ySrc As Long, _
                                    Optional ByVal hPal As Long = 0)
    Dim hdcTemp As Long
    Dim hPalOld As Long
    Dim hbmMemSrcOld As Long
    Dim hDcScreen As Long
    Dim hbmMemSrc As Long
    'Validate that a bitmap was passed in
    If picSource Is Nothing Then GoTo PaintGreyScaleStdPic_InvalidParam
    Select Case picSource.Type
        Case vbPicTypeBitmap
            If hPal = 0 Then
                hPal = m_hpalHalftone
            End If
            hDcScreen = GetDC(0&)
            'Create a DC to select bitmap into
            hdcTemp = CreateCompatibleDC(hDcScreen)
            hPalOld = SelectPalette(hdcTemp, hPal, True)
            RealizePalette hdcTemp
            'Select bitmap into DC
            hbmMemSrcOld = SelectObject(hdcTemp, picSource.handle)
            'Copy to destination DC
            BitBlt hdcDest, xDest, yDest, Width, Height, hdcTemp, xSrc, ySrc, vbSrcAnd
            'Cleanup
            SelectObject hdcTemp, hbmMemSrcOld
            SelectPalette hdcTemp, hPalOld, True
            RealizePalette hdcTemp
            DeleteDC hdcTemp
            ReleaseDC 0&, hDcScreen
        Case vbPicTypeIcon
            'Create a bitmap and select it into an DC
            'Draw Icon onto DC
            DrawIconEx hdcDest, xDest, yDest, picSource.handle, Width, Height, 0&, 0&, DI_NORMAL
        Case Else
            GoTo PaintGreyScaleStdPic_InvalidParam
    End Select
    Exit Sub
PaintGreyScaleStdPic_InvalidParam:
    'Err.Raise giINVALID_PICTURE
End Sub

Public Function GetRedAmount(ByVal iColor As Long) As Long
    GetRedAmount = iColor Mod 256
End Function

Public Function GetGreenAmount(ByVal iColor As Long) As Long
    GetGreenAmount = (iColor \ 256) Mod 256
End Function

Public Function GetBlueAmount(ByVal iColor As Long) As Long
    GetBlueAmount = (iColor \ 256 ^ 2) Mod 256
End Function

Public Function AverageColors(ByVal iColor1 As Long, iColor2 As Long) As Long
    Dim xRed As Long
    Dim xGreen As Long
    Dim xBlue As Long
    xRed = (GetRedAmount(iColor1) + GetRedAmount(iColor2)) \ 2
    xGreen = (GetGreenAmount(iColor1) + GetGreenAmount(iColor2)) \ 2
    xBlue = (GetBlueAmount(iColor1) + GetBlueAmount(iColor2)) \ 2
    AverageColors = RGB(xRed, xGreen, xBlue)
End Function

Public Function WriteText(ByVal strText As String, _
                          ByVal lngDesthDC As Long, _
                          ByVal lngLeft As Long, _
                          ByVal lngTop As Long, _
                          ByVal lngRight As Long, _
                          ByVal lngBottom As Long) As Long
  
  
  Dim typRect As RECT
  With typRect
    .Left = lngLeft
    .Top = lngTop
    .Right = lngRight
    .Bottom = lngBottom
  End With
  
  WriteText = DrawText(lngDesthDC, strText, Len(strText), typRect, _
                       DT_LEFT Or DT_SINGLELINE Or DT_VCENTER)
End Function


