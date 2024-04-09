VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "cMeasureButtonParams"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public hWnd As Long
Public hDC As Long
Public FontFace As String
Public FontSize As Long
Public hFont As Long
Public IconWidth As Long
Public IconHeight As Long
Public Height As Long
Public RightToLeft As Boolean
Public SizeStyle As Long ' see constants COMMANDBARSIZESTYLE
Public Orientation As ECommandBarOrientation
Public ButtonPosition As ECommandBarButtonTextPosition

Public Size As Long

Public Sub FromDrawButtonParams(cDP As cDrawButtonParams)
   Me.hWnd = cDP.hWnd
   Me.hDC = cDP.hDC
   Me.FontFace = cDP.FontFace
   Me.FontSize = cDP.FontSize
   Me.hFont = cDP.hFont
   Me.IconWidth = cDP.ImageList.IconWidth
   Me.IconHeight = cDP.ImageList.IconHeight
   Me.Height = cDP.Height
   Me.RightToLeft = cDP.RightToLeft
   Me.SizeStyle = cDP.SizeStyle
   Me.Orientation = cDP.Orientation
   Me.ButtonPosition = cDP.ButtonPosition
End Sub
Public Sub FromMeasureButtonParams(cDP As cMeasureButtonParams)
   Me.hWnd = cDP.hWnd
   Me.hDC = cDP.hDC
   Me.FontFace = cDP.FontFace
   Me.FontSize = cDP.FontSize
   Me.hFont = cDP.hFont
   Me.IconWidth = cDP.IconWidth
   Me.IconHeight = cDP.IconHeight
   Me.Height = cDP.Height
   Me.RightToLeft = cDP.RightToLeft
   Me.SizeStyle = cDP.SizeStyle
   Me.Orientation = cDP.Orientation
   Me.ButtonPosition = cDP.ButtonPosition
End Sub

Public Function CompareTo(cMP As cMeasureButtonParams) As Integer
   CompareTo = 1
   If (Me.hWnd = cMP.hWnd) Then
      If (Me.SizeStyle = cMP.SizeStyle) Then
         If (Me.Orientation = cMP.Orientation) Then
            If (Me.Height = cMP.Height) Then
               If StrComp(Me.FontFace, cMP.FontFace) = 0 Then
                  If (Me.FontSize = cMP.FontSize) Then
                     If (Me.IconWidth = cMP.IconWidth) Then
                        If (Me.IconHeight = cMP.IconHeight) Then
                           If (Me.RightToLeft = cMP.RightToLeft) Then
                              If (Me.ButtonPosition = cMP.ButtonPosition) Then
                                 CompareTo = 0
                              End If
                           End If
                        End If
                     End If
                  End If
               End If
            End If
         End If
      End If
   End If

End Function
