VERSION 5.00
Begin VB.UserControl PictureViewer 
   BackColor       =   &H80000005&
   ClientHeight    =   3510
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   2700
   ScaleHeight     =   234
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   180
   Begin VB.PictureBox picOrg 
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      Height          =   405
      Left            =   -90
      ScaleHeight     =   27
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   17
      TabIndex        =   6
      Top             =   3060
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.ComboBox cboZoom 
      Height          =   315
      Left            =   240
      Style           =   2  'Dropdown List
      TabIndex        =   3
      Top             =   3120
      Width           =   1545
   End
   Begin VB.PictureBox picFrame 
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      Height          =   3045
      Left            =   0
      ScaleHeight     =   203
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   180
      TabIndex        =   0
      Top             =   0
      Width           =   2700
      Begin VB.PictureBox picCorner 
         BackColor       =   &H80000005&
         BorderStyle     =   0  'None
         Enabled         =   0   'False
         Height          =   315
         Left            =   2430
         ScaleHeight     =   315
         ScaleWidth      =   255
         TabIndex        =   7
         Top             =   2700
         Width           =   255
      End
      Begin VB.HScrollBar HSScroll 
         Height          =   240
         LargeChange     =   10
         Left            =   0
         TabIndex        =   2
         Top             =   2790
         Visible         =   0   'False
         Width           =   2385
      End
      Begin VB.VScrollBar VSScroll 
         Height          =   2175
         LargeChange     =   10
         Left            =   2370
         TabIndex        =   1
         Top             =   0
         Visible         =   0   'False
         Width           =   240
      End
      Begin VB.Label lblError 
         BackStyle       =   0  'Transparent
         Height          =   225
         Left            =   0
         TabIndex        =   8
         Top             =   2490
         Width           =   1455
      End
      Begin VB.Image imgPicture 
         Height          =   2025
         Left            =   0
         Stretch         =   -1  'True
         Top             =   30
         Visible         =   0   'False
         Width           =   2655
      End
   End
   Begin VB.CommandButton cmdZoomIn 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   2340
      Picture         =   "PictureViewer.ctx":0000
      Style           =   1  'Graphical
      TabIndex        =   5
      Top             =   3150
      Width           =   300
   End
   Begin VB.CommandButton cmdZoomOut 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1980
      Picture         =   "PictureViewer.ctx":0342
      Style           =   1  'Graphical
      TabIndex        =   4
      Top             =   3120
      Width           =   300
   End
End
Attribute VB_Name = "PictureViewer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Private mstrNoValidPictureMsg As String
Private mstrWholePicture      As String
Private sngWHRatio            As Single
Private msngWholeZoomPercent  As Single
Private mintCurrZoomPercent   As Integer
Private mintNextZoomPercent   As Integer
Private mintPrevZoomPercent   As Integer
Private mbolInternal          As Boolean

Public Property Get NotValidPicture() As Boolean
  NotValidPicture = lblError.Visible
End Property

Public Property Get NoValidPictureMsg() As String
  NoValidPictureMsg = mstrNoValidPictureMsg
End Property

Public Property Let NoValidPictureMsg(ByVal strNewValue As String)
  mstrNoValidPictureMsg = strNewValue
End Property

Public Property Get WholePicture() As String
  WholePicture = mstrWholePicture
End Property

Public Property Let WholePicture(ByVal strNewValue As String)
  mstrWholePicture = strNewValue
End Property

Private Sub GetNextPrevZoom()
  If mintCurrZoomPercent = 0 Then
    If msngWholeZoomPercent > 100 / 100 Then
      mintNextZoomPercent = -1
      mintPrevZoomPercent = 5
    ElseIf msngWholeZoomPercent > 75 / 100 Then
      mintNextZoomPercent = 5
      mintPrevZoomPercent = 4
    ElseIf msngWholeZoomPercent > 50 / 100 Then
      mintNextZoomPercent = 4
      mintPrevZoomPercent = 3
    ElseIf msngWholeZoomPercent > 25 / 100 Then
      mintNextZoomPercent = 3
      mintPrevZoomPercent = 2
    ElseIf msngWholeZoomPercent > 10 / 100 Then
      mintNextZoomPercent = 2
      mintPrevZoomPercent = 1
    Else
      mintNextZoomPercent = 1
      mintPrevZoomPercent = -1
    End If
  ElseIf mintCurrZoomPercent = 1 Then
    mintNextZoomPercent = 2
    
    If msngWholeZoomPercent < 10 / 100 Then
      mintPrevZoomPercent = 0
    Else
      mintPrevZoomPercent = -1
    End If
  Else
    mintNextZoomPercent = mintCurrZoomPercent + 1
    If mintNextZoomPercent > cboZoom.ListCount - 1 Then
      mintNextZoomPercent = -1
    End If
    mintPrevZoomPercent = mintCurrZoomPercent - 1
  End If
End Sub


Private Sub ShowNoPicture()
  VSScroll.Visible = False
  HSScroll.Visible = False
  
  cboZoom.Enabled = False
  cboZoom.ListIndex = -1
  cmdZoomIn.Enabled = False
  cmdZoomOut.Enabled = False
  
  imgPicture.Visible = False
  
  lblError.Caption = NoValidPictureMsg
  lblError.Visible = True
End Sub

Private Sub ShowPicture()
  VSScroll.Visible = False
  HSScroll.Visible = False
  
  cboZoom.Enabled = True

  cboZoom.ListIndex = 0
  
  Call imgPicture.Move(picFrame.ScaleLeft, _
                       picFrame.ScaleTop, _
                       picFrame.ScaleWidth, _
                       picFrame.ScaleHeight)
  
  
  sngWHRatio = (picOrg.Width / picOrg.Height)
  
  If sngWHRatio > (imgPicture.Width / imgPicture.Height) Then
    ' Decrease image hight
    imgPicture.Height = imgPicture.Width / sngWHRatio
  Else
    ' Decrease image width
    imgPicture.Height = imgPicture.Height * sngWHRatio
  End If
  
  msngWholeZoomPercent = imgPicture.Width / picOrg.Width
  
  mintCurrZoomPercent = 0
  
  Call GetNextPrevZoom
  cmdZoomIn.Enabled = mintNextZoomPercent > -1
  cmdZoomOut.Enabled = mintPrevZoomPercent > -1
  
 
  imgPicture.Picture = picOrg.Picture
  
  imgPicture.Visible = True
End Sub

Public Sub AssignPicture(ByVal strPictureName As String)
  On Error Resume Next
  mbolInternal = True
  
  picOrg.Picture = LoadPicture(strPictureName)
  
  If Err.Number = 0 Then
    Call ShowPicture
  Else
    Call ShowNoPicture
  End If
  mbolInternal = False
End Sub

Private Sub cboZoom_Click()
  If Not mbolInternal Then
    mintCurrZoomPercent = cboZoom.ListIndex
    
    Call GetNextPrevZoom
    
    Call Zoom
  End If
End Sub

Private Sub cmdZoomIn_Click()
  If Not mbolInternal Then
    mintCurrZoomPercent = mintNextZoomPercent
    
    Call GetNextPrevZoom
    
    cboZoom.ListIndex = mintCurrZoomPercent
    
    On Error Resume Next
    Call picFrame.SetFocus
  End If
End Sub

Private Sub cmdZoomOut_Click()
  If Not mbolInternal Then
    mintCurrZoomPercent = mintPrevZoomPercent
    
    Call GetNextPrevZoom
    
    cboZoom.ListIndex = mintCurrZoomPercent
  
    On Error Resume Next
    Call picFrame.SetFocus
  End If
End Sub

Public Sub InitUserControl()
  mbolInternal = True
  
  Set imgPicture.Picture = Nothing
  imgPicture.Visible = False
  
  Set picOrg.Picture = Nothing
  
  VSScroll.Visible = False
  HSScroll.Visible = False
  
  Call cboZoom.Clear
  Call cboZoom.AddItem(WholePicture)
  Call cboZoom.AddItem("10%")
  Call cboZoom.AddItem("25%")
  Call cboZoom.AddItem("50%")
  Call cboZoom.AddItem("75%")
  Call cboZoom.AddItem("100%")
  cboZoom.ListIndex = -1
  
  cboZoom.Enabled = False

  cmdZoomIn.Enabled = False
  cmdZoomOut.Enabled = False
  
  lblError.Visible = False
  
  Call UserControl_Resize
  
  mbolInternal = False
End Sub

Private Sub Zoom()
  cmdZoomIn.Enabled = mintNextZoomPercent > -1
  cmdZoomOut.Enabled = mintPrevZoomPercent > -1
  
  If cboZoom.ListIndex = 0 Then
    VSScroll.Visible = False
    HSScroll.Visible = False
    picCorner.Visible = False
    
    Call ShowPicture
  Else
    imgPicture.Width = picOrg.Width * Val(cboZoom.Text) / 100
    imgPicture.Height = picOrg.Height * Val(cboZoom.Text) / 100
    
    Dim bolShowVS As Boolean
    Dim bolShowHS As Boolean
    
    bolShowHS = imgPicture.Width > picFrame.Width
    bolShowVS = imgPicture.Height > picFrame.Height
    
    If bolShowHS Or bolShowVS Then
      VSScroll.Visible = True
      HSScroll.Visible = True
      picCorner.Visible = True
      
      If imgPicture.Width >= picFrame.Width - VSScroll.Width Then
        HSScroll.Enabled = True
        HSScroll.Max = imgPicture.Width - (picFrame.Width - VSScroll.Width)
        HSScroll.Value = 0
      Else
        HSScroll.Enabled = False
      End If
          
      If imgPicture.Height >= picFrame.Height - HSScroll.Width Then
        VSScroll.Enabled = True
        VSScroll.Max = imgPicture.Height - (picFrame.Height - HSScroll.Height)
        VSScroll.Value = 0
      Else
        HSScroll.Enabled = False
      End If
    Else
      picCorner.Visible = False
      VSScroll.Visible = False
      HSScroll.Visible = False
    End If
  End If
End Sub

Private Sub UserControl_Resize()
  Call picFrame.Move(ScaleLeft, ScaleTop, ScaleWidth, ScaleHeight - 35)
  
  Call imgPicture.Move(picFrame.ScaleLeft, _
                       picFrame.ScaleTop, _
                       picFrame.ScaleWidth, _
                       picFrame.ScaleHeight)
  
  Call lblError.Move(picFrame.ScaleLeft, _
                     picFrame.ScaleHeight - lblError.Height, _
                     picFrame.ScaleWidth)
  
  Call HSScroll.Move(picFrame.ScaleLeft, _
                     picFrame.Height - HSScroll.Height, _
                     picFrame.ScaleWidth - VSScroll.Width, _
                     HSScroll.Height)

  Call VSScroll.Move(picFrame.ScaleWidth - VSScroll.Width, _
                     picFrame.ScaleTop, _
                     VSScroll.Width, _
                     picFrame.ScaleHeight - HSScroll.Height)

  Call picCorner.Move(picFrame.ScaleWidth - VSScroll.Width, _
                      picFrame.Height - HSScroll.Height, _
                      VSScroll.Width, _
                      HSScroll.Height)

  Call cmdZoomOut.Move(picFrame.ScaleWidth - 48, _
                       picFrame.ScaleHeight + 10)

  Call cmdZoomIn.Move(picFrame.ScaleWidth - 24, _
                      picFrame.ScaleHeight + 10)
  
  Call cboZoom.Move(4, _
                    picFrame.ScaleHeight + 10, _
                    cmdZoomOut.Left - 4 - 4)
  
  If picOrg.Picture <> 0 Then
    Call Zoom
  End If
End Sub

Private Sub VSScroll_Change()
  If Not mbolInternal Then
    imgPicture.Top = -VSScroll.Value
  End If
End Sub

Private Sub HSScroll_Change()
  If Not mbolInternal Then
    imgPicture.Left = -HSScroll.Value
  End If
End Sub

