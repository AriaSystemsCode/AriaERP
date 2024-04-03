VERSION 5.00
Begin VB.UserControl ctlAnimatedGIF 
   ClientHeight    =   180
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   2595
   ScaleHeight     =   180
   ScaleWidth      =   2595
   Begin VB.Timer AnimationTimer 
      Interval        =   100
      Left            =   1320
      Top             =   0
   End
   Begin VB.Image AnimatedGIF 
      Appearance      =   0  'Flat
      Height          =   150
      Index           =   0
      Left            =   0
      Top             =   0
      Width           =   2580
   End
End
Attribute VB_Name = "ctlAnimatedGIF"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Dim RepeatTimes&
Dim RepeatCount&

Private Declare Function SetParent Lib "user32" (ByVal hWndChild As Long, ByVal hWndNewParent As Long) As Long
Private Declare Function SetWindowPos Lib "user32" (ByVal hwnd As Long, ByVal hWndInsertAfter As Long, ByVal x As Long, ByVal y As Long, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long
Private Declare Function ShowWindow Lib "user32" (ByVal hwnd As Long, ByVal nCmdShow As Long) As Long


Public BMPFolder As String

Sub LoadAniGif(xFile As String, xImgArray)
    If Not IIf(Dir$(xFile) = "", False, True) Or xFile = "" Then
        MsgBox "File not found.", vbExclamation, "File Error"
        Exit Sub
    End If
        
    Dim F1, f2
    Dim AnimatedGIFs() As String
    Dim imgHeader As String
    Static buf$, picbuf$
    Dim fileHeader As String
    Dim imgCount
    Dim i&, j&, xOff&, yOff&, TimeWait&
    Dim GifEnd
    GifEnd = Chr(0) & "!ù"
    
    AnimationTimer.Enabled = False
    For i = 1 To xImgArray.Count - 1
        Unload xImgArray(i)
    Next i
    
    F1 = FreeFile
On Error GoTo badFile:
    Open xFile For Binary Access Read As F1
        buf = String(LOF(F1), Chr(0))
        Get #F1, , buf
    Close F1
    
    i = 1
    imgCount = 0
    
    j = (InStr(1, buf, GifEnd) + Len(GifEnd)) - 2
    fileHeader = Left(buf, j)
    i = j + 2
    
    Dim a As Long
    Dim b As Long
    a = Asc(Mid(fileHeader, 126, 1))
    b = (Asc(Mid(fileHeader, 127, 1)))
    b = b * 256
    
    RepeatTimes& = a + b
    
    Do
        imgCount = imgCount + 1
        j = InStr(i, buf, GifEnd) + Len(GifEnd)
        If j > Len(GifEnd) Then
            f2 = FreeFile
            Open "tmp.gif" For Binary As f2
                picbuf = String(Len(fileHeader) + j - i, Chr(0))
                picbuf = fileHeader & Mid(buf, i - 1, j - i)
                Put #f2, 1, picbuf
                imgHeader = Left(Mid(buf, i - 1, j - i), 16)
            Close f2
            
            TimeWait = ((Asc(Mid(imgHeader, 4, 1))) + (Asc(Mid(imgHeader, 5, 1)) * 256)) * 10
            If imgCount > 1 Then
                xOff = Asc(Mid(imgHeader, 9, 1)) + (Asc(Mid(imgHeader, 10, 1)) * 256)
                yOff = Asc(Mid(imgHeader, 11, 1)) + (Asc(Mid(imgHeader, 12, 1)) * 2561)
                Load xImgArray(imgCount - 1)
                xImgArray(imgCount - 1).ZOrder 0
                xImgArray(imgCount - 1).Left = xImgArray(0).Left + (xOff * 15)
                xImgArray(imgCount - 1).Top = xImgArray(0).Top + (yOff * 15)
            End If
            xImgArray(imgCount - 1).Tag = TimeWait
            xImgArray(imgCount - 1).Picture = LoadPicture("tmp.gif")
            Kill ("tmp.gif")
            
            i = j '+ 1
        End If
    Loop Until j = Len(GifEnd)
    
    If i < Len(buf) Then
        f2 = FreeFile
        Open "tmp.gif" For Binary As f2
            picbuf = String(Len(fileHeader) + Len(buf) - i, Chr(0))
            picbuf = fileHeader & Mid(buf, i - 1, Len(buf) - i)
            Put #f2, 1, picbuf
            imgHeader = Left(Mid(buf, i - 1, Len(buf) - i), 16)
        Close f2

        TimeWait = ((Asc(Mid(imgHeader, 4, 1))) + (Asc(Mid(imgHeader, 5, 1)) * 256)) * 10
        
        If imgCount > 1 Then
            xOff = Asc(Mid(imgHeader, 9, 1)) + (Asc(Mid(imgHeader, 10, 1)) * 256)
            yOff = Asc(Mid(imgHeader, 11, 1)) + (Asc(Mid(imgHeader, 12, 1)) * 2561)
            Load xImgArray(imgCount - 1)
            xImgArray(imgCount - 1).ZOrder 0
            xImgArray(imgCount - 1).Left = xImgArray(0).Left + (xOff * 15)
            xImgArray(imgCount - 1).Top = xImgArray(0).Top + (yOff * 15)
        End If
        xImgArray(imgCount - 1).Tag = TimeWait
        xImgArray(imgCount - 1).Picture = LoadPicture("tmp.gif")
        Kill ("tmp.gif")
    End If
    
On Error GoTo badTime
    'AnimationTimer.Interval = CInt(xImgArray(0).Tag)
badTime:
    AnimationTimer.Enabled = True
Exit Sub
badFile:
    MsgBox "File not found.", vbExclamation, "File Error"

End Sub

Private Sub AnimationTimer_Timer()

    For i = 0 To AnimatedGIF.Count
        If i = AnimatedGIF.Count Then
            If RepeatTimes > 0 Then
                RepeatCount = RepeatCount + 1
                If RepeatCount > RepeatTimes Then
                    AnimationTimer.Enabled = False
                    Exit Sub
                End If
            End If
            For j = 1 To AnimatedGIF.Count - 1
                AnimatedGIF(j).Visible = False
            Next j
On Error GoTo badTime
            'AnimationTimer.Interval = CLng(AnimatedGIF(0).Tag)
badTime:
            Exit For
        End If
        If AnimatedGIF(i).Visible = False Then
            'AnimationTimer.Interval = CLng(AnimatedGIF(i).Tag)
On Error GoTo badTime2
            AnimatedGIF(i).Visible = True
badTime2:
            Exit For
        End If
    Next i

End Sub

Public Sub LoadAnimatedGif()
  Call LoadAniGif(BMPFolder + "\progbar.gif", AnimatedGIF)
End Sub


