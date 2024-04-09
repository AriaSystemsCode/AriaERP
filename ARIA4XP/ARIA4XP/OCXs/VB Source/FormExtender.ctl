VERSION 5.00
Begin VB.UserControl FormExtender 
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   InvisibleAtRuntime=   -1  'True
   Picture         =   "FormExtender.ctx":0000
   ScaleHeight     =   3600
   ScaleWidth      =   4800
End
Attribute VB_Name = "FormExtender"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
'Place this code in the Declaration section
Option Explicit

'Win32 API Type declarations
Private Type RECT
    Left As Long
    Top As Long
    Right As Long
    Bottom As Long
End Type


'Win32 API Function declarations
Private Declare Function GetClientRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT) As Long
Private Declare Function DrawEdge Lib "user32" (ByVal hdc As Long, qrc As RECT, ByVal edge As Long, ByVal grfFlags As Long) As Long

'Win32 API Constant declarations
Private Const BF_BOTTOM = &H8
Private Const BF_LEFT = &H1
Private Const BF_RIGHT = &H4
Private Const BF_TOP = &H2
Private Const BF_RECT = (BF_LEFT Or BF_TOP Or BF_RIGHT Or BF_BOTTOM)
Private Const BDR_RAISED = &H5
'Default Property Values:
'Const m_def_BackColor = 0
'Const m_def_BackStyle = 0
'Const m_def_DrawMode = 0
'Const m_def_Enabled = 0
'Const m_def_ForeColor = 0
Const m_def_CustomizeHwnd = 0
Const m_def_MaxWidth = 300
Const m_def_MaxHeight = 300
Const m_def_MinWidth = 100
Const m_def_MinHeight = 100
'Property Variables:
'Dim m_BackColor As Long
'Dim m_BackStyle As Integer
'Dim m_DrawMode As Integer
'Dim m_Font As Font
'Dim m_Enabled As Boolean
'Dim m_ForeColor As Long
Dim m_MaxWidth As Long
Dim m_MaxHeight As Long
Dim m_MinWidth As Long
Dim m_MinHeight As Long
'Event Declarations:
'Event Click() 'MappingInfo=UserControl,UserControl,-1,Click
'Event DblClick() 'MappingInfo=UserControl,UserControl,-1,DblClick
'Event KeyDown(KeyCode As Integer, Shift As Integer) 'MappingInfo=UserControl,UserControl,-1,KeyDown
'Event KeyPress(KeyAscii As Integer) 'MappingInfo=UserControl,UserControl,-1,KeyPress
'Event KeyUp(KeyCode As Integer, Shift As Integer) 'MappingInfo=UserControl,UserControl,-1,KeyUp
'Event MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single) 'MappingInfo=UserControl,UserControl,-1,MouseDown
'Event MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single) 'MappingInfo=UserControl,UserControl,-1,MouseMove
'Event MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single) 'MappingInfo=UserControl,UserControl,-1,MouseUp
'Event Click()
'Event DblClick()
'Event KeyDown(KeyCode As Integer, Shift As Integer)
'Event KeyPress(KeyAscii As Integer)
'Event KeyUp(KeyCode As Integer, Shift As Integer)
'Event MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
'Event MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
'Event MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
Event Activate()
Event Deactivate()
Event DisplayChange()
Event ApplicationResize()





'--- End of Declarations section ---

Private Sub UserControl_Paint()

    'Draw a 3D raised border on the control using the Win32 API
    Dim rct As RECT

    'First retrieve the control's dimensions into a RECT structure
    GetClientRect UserControl.hwnd, rct

    'Use the DrawEdge Function to draw the 3D border
    DrawEdge UserControl.hdc, rct, BDR_RAISED, BF_RECT

End Sub

Private Sub UserControl_Resize()
    'Restrict the size of the UserControl to 32x32 pixels.
    UserControl.Size 32 * Screen.TwipsPerPixelX, 32 * Screen.TwipsPerPixelY
End Sub
'WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
'MemberInfo=8,0,0,300
Public Property Get MaxWidth() As Long
    MaxWidth = m_MaxWidth
End Property

Public Property Let MaxWidth(ByVal New_MaxWidth As Long)
    m_MaxWidth = New_MaxWidth
    PropertyChanged "MaxWidth"
    MaxX = m_MaxWidth
End Property

'WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
'MemberInfo=8,0,0,300
Public Property Get MaxHeight() As Long
    MaxHeight = m_MaxHeight
End Property

Public Property Let MaxHeight(ByVal New_MaxHeight As Long)
    m_MaxHeight = New_MaxHeight
    PropertyChanged "MaxHeight"
     
     MaxY = m_MaxHeight
End Property

'WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
'MemberInfo=8,0,0,100
Public Property Get MinWidth() As Long
    MinWidth = m_MinWidth
End Property

Public Property Let MinWidth(ByVal New_MinWidth As Long)
    m_MinWidth = New_MinWidth
    PropertyChanged "MinWidth"
    MinX = m_MinWidth
End Property

'WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
'MemberInfo=8,0,0,100
Public Property Get MinHeight() As Long
    MinHeight = m_MinHeight
End Property

Public Property Let MinHeight(ByVal New_MinHeight As Long)
    m_MinHeight = New_MinHeight
    PropertyChanged "MinHeight"
    MinY = m_MinHeight
End Property

'Initialize Properties for User Control
Private Sub UserControl_InitProperties()
    m_MaxWidth = m_def_MaxWidth
    m_MaxHeight = m_def_MaxHeight
    m_MinWidth = m_def_MinWidth
    m_MinHeight = m_def_MinHeight
'    m_BackColor = m_def_BackColor
'    m_BackStyle = m_def_BackStyle
'    m_DrawMode = m_def_DrawMode
'    Set m_Font = Ambient.Font
'    m_Enabled = m_def_Enabled
'    m_ForeColor = m_def_ForeColor
'    Set UserControl.Font = Ambient.Font
End Sub

'Load property values from storage
Private Sub UserControl_ReadProperties(PropBag As PropertyBag)

    m_MaxWidth = PropBag.ReadProperty("MaxWidth", m_def_MaxWidth)
    m_MaxHeight = PropBag.ReadProperty("MaxHeight", m_def_MaxHeight)
    m_MinWidth = PropBag.ReadProperty("MinWidth", m_def_MinWidth)
    m_MinHeight = PropBag.ReadProperty("MinHeight", m_def_MinHeight)
   
   'Assign values to the public variables used in the subclassing module
    MaxX = m_MaxWidth
    MaxY = m_MaxHeight
    MinX = m_MinWidth
    MinY = m_MinHeight

'    If Ambient.UserMode Then
'        'reference to this instance of the FormExtender
'        Set objFE = Me
'        'subclass the parent form
'        SubClass UserControl.Extender.Parent.hwnd
'    End If
    

'    m_BackColor = PropBag.ReadProperty("BackColor", m_def_BackColor)
'    m_BackStyle = PropBag.ReadProperty("BackStyle", m_def_BackStyle)
'    m_DrawMode = PropBag.ReadProperty("DrawMode", m_def_DrawMode)
'    Set m_Font = PropBag.ReadProperty("Font", Ambient.Font)
'    m_Enabled = PropBag.ReadProperty("Enabled", m_def_Enabled)
'    m_ForeColor = PropBag.ReadProperty("ForeColor", m_def_ForeColor)
'    UserControl.BackColor = PropBag.ReadProperty("BackColor", &H8000000F)
'    UserControl.ForeColor = PropBag.ReadProperty("ForeColor", &H80000012)
'    UserControl.Enabled = PropBag.ReadProperty("Enabled", True)
'    UserControl.DrawMode = PropBag.ReadProperty("DrawMode", 13)
'    Set UserControl.Font = PropBag.ReadProperty("Font", Ambient.Font)
'    UserControl.BackStyle = PropBag.ReadProperty("BackStyle", 1)
End Sub


'Write property values to storage
Private Sub UserControl_WriteProperties(PropBag As PropertyBag)

    Call PropBag.WriteProperty("MaxWidth", m_MaxWidth, m_def_MaxWidth)
    Call PropBag.WriteProperty("MaxHeight", m_MaxHeight, m_def_MaxHeight)
    Call PropBag.WriteProperty("MinWidth", m_MinWidth, m_def_MinWidth)
    Call PropBag.WriteProperty("MinHeight", m_MinHeight, m_def_MinHeight)
   If Ambient.UserMode Then
        'return message control to Windows
        UnSubClass lHwnd
        'Clean up
        Set objFE = Nothing
    End If
    

'    Call PropBag.WriteProperty("BackColor", m_BackColor, m_def_BackColor)
'    Call PropBag.WriteProperty("BackStyle", m_BackStyle, m_def_BackStyle)
'    Call PropBag.WriteProperty("DrawMode", m_DrawMode, m_def_DrawMode)
'    Call PropBag.WriteProperty("Font", m_Font, Ambient.Font)
'    Call PropBag.WriteProperty("Enabled", m_Enabled, m_def_Enabled)
'    Call PropBag.WriteProperty("ForeColor", m_ForeColor, m_def_ForeColor)
'    Call PropBag.WriteProperty("BackColor", UserControl.BackColor, &H8000000F)
'    Call PropBag.WriteProperty("ForeColor", UserControl.ForeColor, &H80000012)
'    Call PropBag.WriteProperty("Enabled", UserControl.Enabled, True)
'    Call PropBag.WriteProperty("DrawMode", UserControl.DrawMode, 13)
'    Call PropBag.WriteProperty("Font", UserControl.Font, Ambient.Font)
'    Call PropBag.WriteProperty("BackStyle", UserControl.BackStyle, 1)
End Sub

Friend Sub FormActivated()
    RaiseEvent Activate
End Sub

Friend Sub FormDeActivated()
    RaiseEvent Deactivate
End Sub

Friend Sub DisplayChanged()
    RaiseEvent DisplayChange
End Sub

Friend Sub AppResize()
  RaiseEvent ApplicationResize
End Sub


''
''Public Property Get lhwin() As Long
''lhwin = lHwnd
''End Property
'''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
'''MemberInfo=8,0,0,0
''Public Property Get BackColor() As Long
''    BackColor = m_BackColor
''End Property
''
''Public Property Let BackColor(ByVal New_BackColor As Long)
''    m_BackColor = New_BackColor
''    PropertyChanged "BackColor"
''End Property
''
'''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
'''MemberInfo=7,0,0,0
''Public Property Get BackStyle() As Integer
''    BackStyle = m_BackStyle
''End Property
''
''Public Property Let BackStyle(ByVal New_BackStyle As Integer)
''    m_BackStyle = New_BackStyle
''    PropertyChanged "BackStyle"
''End Property
''
'''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
'''MemberInfo=7,0,0,0
''Public Property Get DrawMode() As Integer
''    DrawMode = m_DrawMode
''End Property
''
''Public Property Let DrawMode(ByVal New_DrawMode As Integer)
''    m_DrawMode = New_DrawMode
''    PropertyChanged "DrawMode"
''End Property
''
'''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
'''MemberInfo=6,0,0,0
''Public Property Get Font() As Font
''    Set Font = m_Font
''End Property
''
''Public Property Set Font(ByVal New_Font As Font)
''    Set m_Font = New_Font
''    PropertyChanged "Font"
''End Property
''
'''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
'''MemberInfo=0,0,0,0
''Public Property Get Enabled() As Boolean
''    Enabled = m_Enabled
''End Property
''
''Public Property Let Enabled(ByVal New_Enabled As Boolean)
''    m_Enabled = New_Enabled
''    PropertyChanged "Enabled"
''End Property
''
'''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
'''MemberInfo=8,0,0,0
''Public Property Get ForeColor() As Long
''    ForeColor = m_ForeColor
''End Property
''
''Public Property Let ForeColor(ByVal New_ForeColor As Long)
''    m_ForeColor = New_ForeColor
''    PropertyChanged "ForeColor"
''End Property
''
'''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
'''MemberInfo=5
''Public Sub Refresh()
''
''End Sub
''
''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
''MappingInfo=UserControl,UserControl,-1,BackColor
'Public Property Get BackColor() As OLE_COLOR
'    BackColor = UserControl.BackColor
'End Property
'
'Public Property Let BackColor(ByVal New_BackColor As OLE_COLOR)
'    UserControl.BackColor() = New_BackColor
'    PropertyChanged "BackColor"
'End Property
'
''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
''MappingInfo=UserControl,UserControl,-1,ForeColor
'Public Property Get ForeColor() As OLE_COLOR
'    ForeColor = UserControl.ForeColor
'End Property
'
'Public Property Let ForeColor(ByVal New_ForeColor As OLE_COLOR)
'    UserControl.ForeColor() = New_ForeColor
'    PropertyChanged "ForeColor"
'End Property
'
''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
''MappingInfo=UserControl,UserControl,-1,Enabled
'Public Property Get Enabled() As Boolean
'    Enabled = UserControl.Enabled
'End Property
'
'Public Property Let Enabled(ByVal New_Enabled As Boolean)
'    UserControl.Enabled() = New_Enabled
'    PropertyChanged "Enabled"
'End Property
'
''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
''MappingInfo=UserControl,UserControl,-1,DrawMode
'Public Property Get DrawMode() As Integer
'    DrawMode = UserControl.DrawMode
'End Property
'
'Public Property Let DrawMode(ByVal New_DrawMode As Integer)
'    UserControl.DrawMode() = New_DrawMode
'    PropertyChanged "DrawMode"
'End Property
'
''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
''MappingInfo=UserControl,UserControl,-1,Font
'Public Property Get Font() As Font
'    Set Font = UserControl.Font
'End Property
'
'Public Property Set Font(ByVal New_Font As Font)
'    Set UserControl.Font = New_Font
'    PropertyChanged "Font"
'End Property
'
''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
''MappingInfo=UserControl,UserControl,-1,BackStyle
'Public Property Get BackStyle() As Integer
'    BackStyle = UserControl.BackStyle
'End Property
'
'Public Property Let BackStyle(ByVal New_BackStyle As Integer)
'    UserControl.BackStyle() = New_BackStyle
'    PropertyChanged "BackStyle"
'End Property
'
''WARNING! DO NOT REMOVE OR MODIFY THE FOLLOWING COMMENTED LINES!
''MappingInfo=UserControl,UserControl,-1,Refresh
'Public Sub Refresh()
'    UserControl.Refresh
'End Sub
'
'Private Sub UserControl_Click()
'    RaiseEvent Click
'End Sub
'
'Private Sub UserControl_DblClick()
'    RaiseEvent DblClick
'End Sub
'
'Private Sub UserControl_KeyDown(KeyCode As Integer, Shift As Integer)
'    RaiseEvent KeyDown(KeyCode, Shift)
'End Sub
'
'Private Sub UserControl_KeyPress(KeyAscii As Integer)
'    RaiseEvent KeyPress(KeyAscii)
'End Sub
'
'Private Sub UserControl_KeyUp(KeyCode As Integer, Shift As Integer)
'    RaiseEvent KeyUp(KeyCode, Shift)
'End Sub
'
'Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
'    RaiseEvent MouseDown(Button, Shift, X, Y)
'End Sub
'
'Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
'    RaiseEvent MouseMove(Button, Shift, X, Y)
'End Sub
'
'Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
'    RaiseEvent MouseUp(Button, Shift, X, Y)
'End Sub
'
Public Property Let WindHwnd(nWindHwnd As Long)
'   If Ambient.UserMode And lHwnd <> nWindHwnd Then
'        'return message control to Windows
'        UnSubClass lHwnd
'        'Clean up
'        Set objFE = Nothing
'    End If
   lHwnd = nWindHwnd
    If Ambient.UserMode Then
        'reference to this instance of the FormExtender
        Set objFE = Me
        'subclass the parent form
        SubClass lHwnd
    End If
   
End Property

Public Property Get WindHwnd() As Long
    WindHwnd = lHwnd
End Property


Public Sub TerminateTrace()
   If Ambient.UserMode Then
        'return message control to Windows
        UnSubClass lHwnd
        'Clean up
        Set objFE = Nothing
    End If

End Sub

