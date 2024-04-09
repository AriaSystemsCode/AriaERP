VERSION 5.00
Object = "{E757ECCC-406C-45B7-A2B8-2B71E1D0A772}#1.0#0"; "ocxAriaButton.ocx"
Begin VB.UserControl ctlTitleBar 
   AutoRedraw      =   -1  'True
   ClientHeight    =   1785
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   3465
   ScaleHeight     =   119
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   231
   Begin vbpAriaButton.Button cmdClose 
      Height          =   255
      Left            =   2640
      TabIndex        =   1
      Top             =   840
      Width           =   255
      _ExtentX        =   450
      _ExtentY        =   450
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Picture         =   "ctlTitleBar.ctx":0000
      Picture         =   "ctlTitleBar.ctx":0452
      Caption         =   ""
      MaskColor       =   -2147483633
      ShowFlatGrey    =   -1  'True
      AutoSize        =   0   'False
   End
   Begin vbpSearchBar.ctlGradient ctlGradient 
      Height          =   135
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   1575
      _ExtentX        =   2778
      _ExtentY        =   238
      Color1          =   16250355
      Color2          =   -2147483633
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin VB.Line Line1 
      BorderColor     =   &H00C0C0C0&
      X1              =   16
      X2              =   184
      Y1              =   32
      Y2              =   32
   End
   Begin VB.Line Line2 
      BorderColor     =   &H80000004&
      X1              =   8
      X2              =   166
      Y1              =   49
      Y2              =   49
   End
End
Attribute VB_Name = "ctlTitleBar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Event CloseClicked()

Public Function Refresh()
  On Error Resume Next
  If Parent.Control.plGetTheme() = 0 Then
    Line1.Visible = False
    Line2.Visible = False
    cmdClose.BackColor = ctlGradient.Color2
    ctlGradient.Color1 = ctlGradient.Color2
  Else
    Line1.Visible = True
    Line2.Visible = True
  
    cmdClose.BackColor = &H8000000F
    ctlGradient.Color1 = &HF7F5F3
  End If
  Call UserControl.Refresh
End Function

Public Property Let Caption(ByVal strValue As String)
  On Error Resume Next
  ctlGradient.Caption = strValue
  Call PropertyChanged("Caption")
End Property

Public Property Get Caption() As String
  On Error Resume Next
  Caption = ctlGradient.Caption
End Property

Private Sub cmdClose_Click()
  On Error Resume Next
  RaiseEvent CloseClicked
End Sub

Private Sub UserControl_Resize()
  On Error Resume Next
  If ctlGradient.Height <> 22 Then
    Call ctlGradient.Move(ScaleLeft, ScaleTop, ScaleWidth, 22)
  End If
  
  Call cmdClose.Move(ScaleWidth - cmdClose.Width - 3, ScaleTop + 3)
  Line1.Y1 = 22
  Line1.Y2 = 22
  Line1.X1 = 0
  Line1.X2 = ScaleWidth
  Line2.Y1 = 23
  Line2.Y2 = 23
  Line2.X1 = 0
  Line2.X2 = ScaleWidth
End Sub

Private Sub UserControl_InitProperties()
  On Error Resume Next
  ctlGradient.Caption = ""
End Sub

Private Sub UserControl_ReadProperties(PropBag As PropertyBag)
  On Error Resume Next
  ctlGradient.Caption = PropBag.ReadProperty("Caption", "")
End Sub


Private Sub UserControl_WriteProperties(PropBag As PropertyBag)
  On Error Resume Next
  Call PropBag.WriteProperty("Caption", ctlGradient.Caption, "")
End Sub


