VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "cFontCache"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

' private font caching object for a control.

Private Type tCache
   sFaceName As String
   fFaceSize As Currency
   lEscapement As Long
   hFont As Long
End Type
Private m_tCache() As tCache
Private m_iCacheCount As Long

Public Property Get hFont( _
      f As StdFont, _
      escapement As Long, _
      hDC As Long _
   ) As Long
Dim iIndex As Long
Dim i As Long
   
   For i = 1 To m_iCacheCount
      If StrComp(m_tCache(i).sFaceName, f.Name) = 0 Then
         If m_tCache(i).fFaceSize = f.Size Then
            If m_tCache(i).lEscapement = escapement Then
               iIndex = i
               Exit For
            End If
         End If
      End If
   Next i
   
   If (iIndex = 0) Then
      m_iCacheCount = m_iCacheCount + 1
      ReDim Preserve m_tCache(1 To m_iCacheCount) As tCache
      With m_tCache(m_iCacheCount)
         .fFaceSize = f.Size
         .sFaceName = f.Name
         .lEscapement = escapement
      End With
      iIndex = m_iCacheCount
   End If
   
   If (m_tCache(iIndex).hFont = 0) Then
      Dim tLF As LOGFONT
      OLEFontToLogFont f, hDC, tLF
      tLF.lfEscapement = escapement
      tLF.lfOrientation = escapement
      m_tCache(iIndex).hFont = CreateFontIndirect(tLF)
   End If
   
   hFont = m_tCache(iIndex).hFont
   
   
End Property


Private Sub Class_Terminate()
Dim i As Long
   For i = 1 To m_iCacheCount
      If Not (m_tCache(i).hFont = 0) Then
         DeleteObject m_tCache(i).hFont
      End If
   Next i
   Erase m_tCache
   m_iCacheCount = 0
End Sub
