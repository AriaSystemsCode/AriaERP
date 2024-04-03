VERSION 5.00
Begin VB.UserControl ctlGradient 
   AutoRedraw      =   -1  'True
   ClientHeight    =   315
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   1995
   ScaleHeight     =   21
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   133
   Begin VB.Label lblCaption 
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   1095
   End
End
Attribute VB_Name = "ctlGradient"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Private mobjGradient As clsGradient

Private mclrColor1 As OLE_COLOR
Private mclrColor2 As OLE_COLOR
Private mintAngle As Integer

Public Enum enmBorderStyle
  envBSNone = 0
  envBSFixedSingle = 1
End Enum

Public Event Click()
Public Event DblClick()
Public Event Resize()

Public Sub Refresh()
  UserControl.Refresh
End Sub

Public Sub MyPSet(ByRef X As Single, ByRef Y As Single, ByVal lngColor As Long)
  UserControl.PSet (X, Y), lngColor
End Sub

Public Function Point(ByRef X As Single, ByRef Y As Single) As Long
  Point = UserControl.Point(X, Y)
End Function

Public Property Get hWnd() As Long
  hWnd = UserControl.hWnd
End Property

Public Property Get hDC() As Long
  hDC = UserControl.hDC
End Property

Public Property Let ForeColor(ByVal clrValue As OLE_COLOR)
  UserControl.ForeColor = clrValue
  Call PropertyChanged("ForeColor")
End Property

Public Property Get ForeColor() As OLE_COLOR
  ForeColor = UserControl.ForeColor
End Property

Public Property Set Font(ByVal fntValue As StdFont)
  Set UserControl.Font = fntValue
  UserControl.FontBold = fntValue.Bold
  UserControl.FontItalic = fntValue.Italic
  UserControl.FontName = fntValue.Name
  UserControl.FontSize = fntValue.Size
  UserControl.FontStrikethru = fntValue.Strikethrough
  UserControl.FontUnderline = fntValue.Underline
  
  Set UserControl.Font = fntValue
  Call PropertyChanged("Font")
End Property

Public Property Get Font() As StdFont
  Set Font = UserControl.Font
End Property

Public Property Let BorderStyle(ByVal evlValue As enmBorderStyle)
  If evlValue >= 0 And evlValue <= 1 Then
    UserControl.BorderStyle = evlValue
    Call PropertyChanged("BorderStyle")
  End If
End Property

Public Property Get BorderStyle() As enmBorderStyle
  BorderStyle = UserControl.BorderStyle
End Property

Public Property Let Caption(ByVal strValue As String)
  lblCaption.Caption = strValue
  Call PropertyChanged("Caption")
End Property

Public Property Get Caption() As String
  Caption = lblCaption.Caption
End Property

Public Property Let Color1(ByVal clrValue As OLE_COLOR)
  mclrColor1 = clrValue
  Call ApplyGradient
  Call PropertyChanged("Color1")
End Property

Public Property Get Color1() As OLE_COLOR
  Color1 = mclrColor1
End Property

Public Property Let Color2(ByVal clrValue As OLE_COLOR)
  mclrColor2 = clrValue
  Call ApplyGradient
  Call PropertyChanged("Color2")
End Property

Public Property Get Color2() As OLE_COLOR
  Color2 = mclrColor2
End Property

Public Property Let Angle(ByVal intValue As Integer)
  If intValue >= 0 And intValue <= 360 Then
    mintAngle = intValue
    Call ApplyGradient
    Call PropertyChanged("Angle")
  End If
End Property

Public Property Get Angle() As Integer
  Angle = mintAngle
End Property

Private Sub ApplyGradient()
  With mobjGradient
    .Color1 = mclrColor1
    .Color2 = mclrColor2
    .Angle = mintAngle
    
    .Draw Me
  End With
  UserControl.Refresh
End Sub

Private Sub UserControl_Initialize()
  Set mobjGradient = New clsGradient
End Sub

Private Sub UserControl_Terminate()
  Set mobjGradient = Nothing
End Sub

Private Sub UserControl_Resize()
  On Error Resume Next
  Call lblCaption.Move(ScaleLeft + 4, ScaleTop + 4, ScaleWidth - 4, ScaleHeight - 4)
  Call ApplyGradient
  RaiseEvent Resize
End Sub

Private Sub UserControl_Click()
  RaiseEvent Click
End Sub

Private Sub UserControl_DblClick()
  RaiseEvent DblClick
End Sub

Private Sub UserControl_InitProperties()
  lblCaption.Caption = ""
  mclrColor1 = vbActiveTitleBar
  mclrColor2 = vbWindowBackground
  mintAngle = 0
  UserControl.BorderStyle = envBSNone
  Set Me.Font = Ambient.Font
  UserControl.ForeColor = vbActiveTitleBarText
  
  Call ApplyGradient
End Sub

Private Sub UserControl_ReadProperties(PropBag As PropertyBag)
  lblCaption.Caption = PropBag.ReadProperty("Caption", "")
  mclrColor1 = PropBag.ReadProperty("Color1", vbActiveTitleBar)
  mclrColor2 = PropBag.ReadProperty("Color2", vbWindowBackground)
  mintAngle = PropBag.ReadProperty("Angle", 0)
  UserControl.BorderStyle = PropBag.ReadProperty("BorderStyle", envBSNone)
  Set Me.Font = PropBag.ReadProperty("Font", Ambient.Font)
  UserControl.ForeColor = PropBag.ReadProperty("ForeColor", vbActiveTitleBarText)
  
  Call ApplyGradient
End Sub

Private Sub UserControl_WriteProperties(PropBag As PropertyBag)
  Call PropBag.WriteProperty("Caption", lblCaption.Caption, "")
  Call PropBag.WriteProperty("Color1", mclrColor1, vbActiveTitleBar)
  Call PropBag.WriteProperty("Color2", mclrColor2, vbWindowBackground)
  Call PropBag.WriteProperty("Angle", mintAngle, 0)
  Call PropBag.WriteProperty("BorderStyle", UserControl.BorderStyle, envBSNone)
  Call PropBag.WriteProperty("Font", UserControl.Font, Ambient.Font)
  Call PropBag.WriteProperty("ForeColor", UserControl.ForeColor, vbActiveTitleBarText)
End Sub

