VERSION 5.00
Object = "{8245A918-4CF7-11D2-8E21-10B404C10000}#8.1#0"; "vbaliml.ocx"
Object = "{D76CA550-D077-4CE5-99F3-F50DB236AB79}#1.0#0"; "vbalcmdbar61.ocx"
Begin VB.UserControl ctlExpToolbar 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   ClientHeight    =   3090
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   11130
   ScaleHeight     =   3090
   ScaleWidth      =   11130
   Begin vbalCmdBar61.vbalCommandBar vbalCommandBar1 
      Height          =   600
      Index           =   0
      Left            =   0
      Top             =   0
      Width           =   10695
      _ExtentX        =   18865
      _ExtentY        =   1058
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Style           =   3
      Begin vbpExpToolbar.ctlAddressBar ctlAddressBar1 
         Height          =   380
         Left            =   4560
         TabIndex        =   0
         Top             =   140
         Width           =   3855
         _ExtentX        =   6800
         _ExtentY        =   847
      End
   End
   Begin vbalIml.vbalImageList vbalImageList1 
      Left            =   1680
      Top             =   1440
      _ExtentX        =   953
      _ExtentY        =   953
      IconSizeX       =   32
      IconSizeY       =   32
      ColourDepth     =   24
      Size            =   22060
      Images          =   "ctlExpToolBar.ctx":0000
      Version         =   131072
      KeyCount        =   5
      Keys            =   "ÿÿÿÿ"
   End
   Begin vbalIml.vbalImageList vbalImageList2 
      Left            =   0
      Top             =   1000
      _ExtentX        =   953
      _ExtentY        =   953
      ColourDepth     =   24
      Size            =   1148
      Images          =   "ctlExpToolBar.ctx":564C
      Version         =   131072
      KeyCount        =   1
      Keys            =   ""
   End
End
Attribute VB_Name = "ctlExpToolbar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit


Public Event BackMenuSelect(ByVal intKey As Integer)
Public Event ForwardMenuSelect(ByVal intKey As Integer)

Public Event Up(ByVal strKey As String)

Public Event ShowInFolders()
Public Event ShowInGroups()

Public Event ClickLink(ByVal strKey As String)
Public Event Configure()
'mariam
Public Event SwitchToAria5()
'mariam
Dim objBnt As Object
Dim btn As cButton
Dim strCurrent As String
Dim strUp As String
Dim strCaption As String
Dim strViewMode As String
Dim strLinksID As String
Dim strLinksDesc As String
Dim strOS As String

Dim intBackCount As Integer
Dim intForeCount As Integer

Dim mbolGroupEnabled As Boolean
'Mariam
Dim mbolSwitch As Boolean
'Mariam
'Mahmoud added
Dim mbolArabic As Boolean



Public Property Get Arabic() As Boolean
  Arabic = mbolArabic
End Property

Public Property Let Arabic(ByVal bolNewValue As Boolean)
    mbolArabic = bolNewValue
End Property
'Mahmoud added


Public Sub SetUp(ByVal strKey As String)
 On Error Resume Next
 strUp = strKey
 vbalCommandBar1(0).Buttons.Item("UP").Enabled = Trim$(strUp) <> ""
End Sub

Public Sub SetCurrent(ByVal strKey As String, ByVal vCaption As String, ByVal bolPushOld As Boolean)
  On Error Resume Next
  If Trim$(strCurrent) <> "" And bolPushOld And Trim$(strCurrent) <> Trim$(strKey) Then
    PushBackMenu strCurrent, strCaption
  End If
  
  strCurrent = strKey
  strCaption = vCaption
End Sub

Public Function GetCurrent() As String
  On Error Resume Next
  GetCurrent = strCurrent
End Function

Public Sub ClearUp()
 On Error Resume Next
 strUp = ""
 vbalCommandBar1(0).Buttons.Item("UP").Enabled = False
End Sub

Public Sub PushBackMenu(ByVal strKey As String, ByVal strDescription As String)
  On Error Resume Next
  Dim objBar As cCommandBar
  Set objBar = vbalCommandBar1(0).Buttons.Item("BACK").Bar
  
  Dim strBtnKey As String
  strBtnKey = "BackKey" + Trim(intBackCount + 1)
  
  Dim btn1 As cButton
  Set btn1 = vbalCommandBar1(0).Buttons.Add(strBtnKey, , strDescription)
  btn1.Tag = strKey
  
  If objBar.Buttons.Count > 0 Then
    Call objBar.Buttons.InsertBefore(btn1, objBar.Buttons(1))
  Else
    Call objBar.Buttons.Add(btn1)
  End If
  
  If objBar.Buttons.Count > 9 Then
    Dim intIndex As Integer
    For intIndex = 1 To intBackCount
      On Error Resume Next
      If objBar.Buttons.Item(intIndex) Then
        Call objBar.Buttons.Remove("BackKey" + Trim(intIndex))
        Call vbalCommandBar1(0).Buttons.Remove("BackKey" + Trim(intIndex))
      End If
      
      If objBar.Buttons.Count = 9 Then
        Exit For
      End If
    Next intIndex
  End If
  
  vbalCommandBar1(0).Buttons.Item("BACK").Enabled = True
  
  intBackCount = intBackCount + 1
End Sub

Public Function PopBackMenu() As String
  On Error Resume Next
  Dim objBar As cCommandBar
  Set objBar = vbalCommandBar1(0).Buttons.Item("BACK").Bar
  Dim intCount As Integer
  intCount = intBackCount
  
  
  PopBackMenu = objBar.Buttons.Item(1).Tag
  Call objBar.Buttons.Remove("BackKey" + Trim(intCount))
  Call vbalCommandBar1(0).Buttons.Remove("BackKey" + Trim(intCount))
  vbalCommandBar1(0).Buttons.Item("BACK").Enabled = objBar.Buttons.Count > 0
  
  intBackCount = intBackCount - 1
  If intBackCount < 0 Then
    intBackCount = 0
  End If
End Function

Public Sub PushForwardMenu(ByVal strKey As String, ByVal strDescription As String)
  On Error Resume Next
  Dim objBar As cCommandBar
  Set objBar = vbalCommandBar1(0).Buttons.Item("FORWARD").Bar
  
  Dim strBtnKey As String
  strBtnKey = "ForwardKey" + Trim(intForeCount + 1)
  
  Dim btn1 As cButton
  Set btn1 = vbalCommandBar1(0).Buttons.Add(strBtnKey, , strDescription)
  btn1.Tag = strKey
  
  
  If objBar.Buttons.Count > 0 Then
    Call objBar.Buttons.InsertBefore(btn1, objBar.Buttons(1))
  Else
    Call objBar.Buttons.Add(btn1)
  End If
  
  
  If objBar.Buttons.Count > 9 Then
    Dim intIndex As Integer
    For intIndex = 1 To intForeCount
      On Error Resume Next
      If objBar.Buttons.Item(intIndex) Then
        Call objBar.Buttons.Remove("ForwardKey" + Trim(intIndex))
        Call vbalCommandBar1(0).Buttons.Remove("ForwardKey" + Trim(intIndex))
      End If
    
      If objBar.Buttons.Count = 9 Then
        Exit For
      End If
    Next intIndex
  End If
  
  vbalCommandBar1(0).Buttons.Item("FORWARD").Enabled = True
  
  intForeCount = intForeCount + 1
End Sub

Public Function PopForwardMenu() As String
  On Error Resume Next
  Dim objBar As cCommandBar
  Set objBar = vbalCommandBar1(0).Buttons.Item("FORWARD").Bar
  Dim intCount As Integer
  intCount = intForeCount

  PopForwardMenu = objBar.Buttons.Item(1).Tag
  Call objBar.Buttons.Remove("ForwardKey" + Trim(intCount))
  Call vbalCommandBar1(0).Buttons.Remove("ForwardKey" + Trim(intCount))
  
  vbalCommandBar1(0).Buttons.Item("FORWARD").Enabled = objBar.Buttons.Count > 0
  
  intForeCount = intForeCount - 1

  If intForeCount < 0 Then
    intForeCount = 0
  End If
End Function

Public Sub SetOS(ByVal vOP As String)
  On Error Resume Next
  strOS = vOP
  On Error Resume Next
  If strOS = "XP" Then
    vbalCommandBar1(0).Buttons("IN_FOLDERS").Enabled = True
    vbalCommandBar1(0).Buttons("IN_GROUPS").Enabled = True
    strViewMode = "IN_GROUPS"
  Else
    vbalCommandBar1(0).Buttons("IN_FOLDERS").Enabled = True
    vbalCommandBar1(0).Buttons("IN_GROUPS").Enabled = False
    strViewMode = "IN_FOLDERS"
  End If
  SetViewMode strViewMode
End Sub

Public Sub SetViewMode(ByVal strKey As String)
  On Error Resume Next
  If strKey = "IN_FOLDERS" Then
    strViewMode = "IN_FOLDERS"
    vbalCommandBar1(0).Buttons("IN_FOLDERS").IconIndex = 0
    vbalCommandBar1(0).Buttons("IN_GROUPS").IconIndex = -1
  End If

  If strKey = "IN_GROUPS" Then
    strViewMode = "IN_GROUPS"
    vbalCommandBar1(0).Buttons("IN_FOLDERS").IconIndex = -1
    vbalCommandBar1(0).Buttons("IN_GROUPS").IconIndex = 0
  End If
End Sub

Public Function GetViewMode() As String
  On Error Resume Next
  GetViewMode = strViewMode
End Function

Public Sub PushLink(ByVal vLinkID As String, ByVal vLinkDesc As String)
  On Error Resume Next
  strLinksID = strLinksID + ">" + vLinkID
  strLinksDesc = strLinksDesc + ">" + vLinkDesc
  Call SetLinks(strLinksID, strLinksDesc, False)
End Sub

Public Sub PopLink()
  On Error Resume Next
  strLinksID = Trim$(Mid$(strLinksID, 1, InStrRev(strLinksID, ">")))
  strLinksDesc = Trim$(Mid$(strLinksDesc, 1, InStrRev(strLinksDesc, ">")))
End Sub

Public Sub SetLinks(ByVal vLinksID As String, ByVal vLinksDesc As String, ByVal bolNotSetUP As Boolean)
  On Error Resume Next
  strLinksID = Trim$(vLinksID)
  strLinksDesc = Trim$(vLinksDesc)
  
  Dim astrLinkID() As String
  Dim astrLinkDesc() As String
  astrLinkID = Split(strLinksID, ">")
  astrLinkDesc = Split(strLinksDesc, ">")
  
  Dim strText As String
  strText = "<HTML> <BODY SCROLL=No TOPMARGIN = 2 oncontextmenu=""return false""> "
  
  Dim intIndex As Integer
  For intIndex = 0 To UBound(astrLinkID) - 1
    'strText = strText & "<a href= ""-" & astrLinkID(intIndex) & """" & ">" & astrLinkDesc(intIndex) & "</a> > "
    strText = strText & "<a href= ""-" & astrLinkID(intIndex) & """" & ">" & _
             "<FONT face=""Franklin Gothic Book"" size=2>" & _
              astrLinkDesc(intIndex) & _
              "</FONT>" & _
              "</a> > "
  Next intIndex
  
  If UBound(astrLinkID) > -1 Then
    'strText = strText & " <B> " & astrLinkDesc(UBound(astrLinkID)) & " </B> "
    strText = strText & "<FONT face=""Franklin Gothic Book"" size=2><STRONG>" & _
              astrLinkDesc(UBound(astrLinkID)) & _
              "</FONT>"

    
  End If
  
  strText = strText & "</BODY> </HTML>"
  
  Dim objFileSys As New FileSystemObject
  Dim objFolder As Folder
  Set objFolder = objFileSys.GetSpecialFolder(TemporaryFolder)
  
  Dim strTempName As String
  strTempName = objFileSys.GetTempName()
  Dim objTextStream As TextStream
  Set objTextStream = objFolder.CreateTextFile(strTempName)
  Call objTextStream.Write(strText)
  Call objTextStream.Close
  ctlAddressBar1.strNavTo = objFolder.Path & "\" & strTempName
  Call ctlAddressBar1.Control.Navigate(objFolder.Path & "\" & strTempName)
  
  If Not bolNotSetUP Then
    If UBound(astrLinkID) > 0 Then
      SetUp astrLinkID(UBound(astrLinkID) - 1)
    Else
      SetUp ""
    End If
  End If
End Sub

Private Sub ConfigureButtons()
  On Error Resume Next
  With vbalCommandBar1(0).Buttons
    
    
    
    Set btn = .Add("BACK", 0, "Back", eSplit, "Back")
    btn.ShowCaptionInToolbar = True
    btn.Enabled = False
    
    Set btn = .Add("FORWARD", 1, "Forward", eSplit, "Forward")
    btn.ToolTip = "Forwared"
    btn.Enabled = False
    
    Set btn = .Add("UP", 2, "Up", , "Up")
    btn.ToolTip = "Up"
    btn.Enabled = False
    
    .Add "SEP1", , , eSeparator
    Set btn = .Add("GROUP", 3, "Group", eNormal)
    btn.ToolTip = "Show in Groups"
    btn.ShowDropDownInToolbar = True
    
    ' mahmoud add
    .Add "SEP3", , , eSeparator
    Set btn = .Add("ARIA5ERP", 4, "Aria5ERP", eNormal)
    btn.ToolTip = "Aria5ERP"
    btn.ShowDropDownInToolbar = True
    ' mahmoud add

    
    .Add "SEP2", , , eSeparator
    Set objBnt = .Add("ADDRESS", , , ePanel)
    objBnt.ToolTip = "Address bar"
    objBnt.PanelWidth = 100
    
    objBnt.PanelControl = ctlAddressBar1
    
    On Error Resume Next
    ' Mahmoud Modify
    'objBnt.PanelWidth = Width / Screen.TwipsPerPixelX - 248
    'ctlAddressBar1.Width = Width - Screen.TwipsPerPixelX * 240
    ResizeAddressBar
    ' Mahmoud End
  
  End With
End Sub

Private Sub ConfigureBars()
  On Error Resume Next
  Dim barStandard As cCommandBar
  Dim barBack As cCommandBar
  Dim barNext As cCommandBar
  Dim barGroup As cCommandBar
  Dim barButtons As cCommandBarButtons
  

   With vbalCommandBar1(0).CommandBars
      Set barStandard = .Add("STANDARD", "Standard Buttons")
      Set barBack = .Add("BACK", "Back")
      Set barNext = .Add("NEXT", "Next")
      Set barGroup = .Add("GROUP", "Next")
      
   End With
   
   Set barButtons = barStandard.Buttons
   With vbalCommandBar1(0).Buttons
     
      barButtons.Add .Item("BACK")
      barButtons.Add .Item("FORWARD")
      barButtons.Add .Item("UP")
      barButtons.Add .Item("SEP1")
      barButtons.Add .Item("GROUP")
        'Mahmoud added
       barButtons.Add .Item("ARIA5ERP")
       barButtons.Add .Item("SEP3")
       'Mahmoud added
      barButtons.Add .Item("SEP2")
      barButtons.Add .Item("ADDRESS")
   End With
   
   Set btn = vbalCommandBar1(0).Buttons.Add("IN_FOLDERS", , "Show in Folders", eNormal)
   btn.IconIndex = 0
   strViewMode = "IN_FOLDERS"
   Call barGroup.Buttons.Add(btn)
   
   Set btn = vbalCommandBar1(0).Buttons.Add("IN_GROUPS", , "Show in Groups", eNormal)
   btn.IconIndex = -1
   Call barGroup.Buttons.Add(btn)
   
   With vbalCommandBar1(0).Buttons
      .Item("BACK").Bar = barBack
      .Item("FORWARD").Bar = barNext
      .Item("GROUP").Bar = barGroup
   End With
End Sub

Private Sub ctlAddressBar1_ClickLink(ByVal strKey As String)
  On Error Resume Next
  Dim astrLinkID() As String
  Dim astrLinkDesc() As String
  astrLinkID = Split(strLinksID, ">")
  astrLinkDesc = Split(strLinksDesc, ">")
  
  Dim intIndex As Integer
  For intIndex = 0 To UBound(astrLinkID) - 1
    If UCase$(Right$(strKey, Len(Trim$(astrLinkID(intIndex))))) = UCase$(Trim$(astrLinkID(intIndex))) Then
      RaiseEvent ClickLink(astrLinkID(intIndex))
      Exit For
    End If
  Next intIndex
End Sub

' Mahmoud Add
Public Sub ResizeAddressBar()

    objBnt.PanelWidth = Width / Screen.TwipsPerPixelX - 248 - 52
    ctlAddressBar1.Width = Width - Screen.TwipsPerPixelX * (240 + 52)
    'objBnt.PanelWidth = 100
    'ctlAddressBar1.Width = 100
End Sub
' Mahmoud Add

Public Sub UserControl_Resize()
  On Error Resume Next
  'If Width > 11130 / 2 Then
    vbalCommandBar1(0).Width = Width
    
    ' Mahmoud Modify
    'objBnt.PanelWidth = Width / Screen.TwipsPerPixelX - 248
    'ctlAddressBar1.Width = Width - Screen.TwipsPerPixelX * (240)
    ResizeAddressBar
    ' Mahmoud end
  'End If
End Sub


Private Sub vbalCommandBar1_ButtonClick(index As Integer, btn As vbalCmdBar61.cButton)
  On Error Resume Next
  If btn.Key = "BACK" Then
    RaiseEvent BackMenuSelect(1)
  End If
  
  If Left$(btn.Key, 7) = "BackKey" Then
    RaiseEvent BackMenuSelect(intBackCount - Val(Mid$(btn.Key, 8)) + 1)
  End If
  
  If btn.Key = "UP" Then
    RaiseEvent Up(strUp)
  End If

  If btn.Key = "FORWARD" Then
    RaiseEvent ForwardMenuSelect(1)
  End If
 'Mariam
 If btn.Key = "ARIA5ERP" Then
    RaiseEvent SwitchToAria5
  End If
 'Mariam

  If Left$(btn.Key, 10) = "ForwardKey" Then
    RaiseEvent ForwardMenuSelect(intForeCount - Val(Mid$(btn.Key, 11)) + 1)
  End If

  If btn.Key = "GROUP" Then
    Call vbalCommandBar1(0).pShowDropDown(5)
  End If
  
  If btn.Key = "IN_FOLDERS" Then
    vbalCommandBar1(0).Buttons("IN_FOLDERS").IconIndex = 0
    vbalCommandBar1(0).Buttons("IN_GROUPS").IconIndex = -1
    strViewMode = "IN_FOLDERS"
    RaiseEvent ShowInFolders
  End If

  If btn.Key = "IN_GROUPS" Then
    vbalCommandBar1(0).Buttons("IN_FOLDERS").IconIndex = -1
    vbalCommandBar1(0).Buttons("IN_GROUPS").IconIndex = 0
    strViewMode = "IN_GROUPS"
    RaiseEvent ShowInGroups
  End If
End Sub

Private Sub vbalCommandBar1_RequestNewInstance(index As Integer, ctl As Object)
  On Error Resume Next
   Dim i As Long
   i = vbalCommandBar1.UBound + 1
   Load vbalCommandBar1(i)
   vbalCommandBar1(i).Align = 0
   Set ctl = vbalCommandBar1(i)
End Sub

Private Sub UserControl_Show()
   On Error Resume Next
   SetLinks "", "", False
  
   ConfigureButtons
   
   ConfigureBars
   
   vbalCommandBar1(0).ToolbarImageList = vbalImageList1.hIml
   vbalCommandBar1(0).MenuImageList = vbalImageList2.hIml
   
   vbalCommandBar1(0).ToolBar = vbalCommandBar1(0).CommandBars("STANDARD")
   vbalCommandBar1(0).Style = eComCtl32
   
   SetOS strOS
   
   UserControl_Resize
   
   RaiseEvent Configure
End Sub

Public Property Get hWnd() As Long
  On Error Resume Next
  hWnd = UserControl.hWnd
End Property

Public Property Let hWnd(ByVal vNewValue As Long)
On Error Resume Next
End Property

Public Property Get ScaleMode() As Variant
  On Error Resume Next
  ScaleMode = UserControl.ScaleMode
End Property

Public Property Let ScaleMode(ByVal vNewValue As Variant)
End Property

Public Function ScaleX(a, b, c) As Single
  On Error Resume Next
  ScaleX = UserControl.ScaleX(a, b, c)
End Function

Public Function ScaleY(a, b, c) As Single
  On Error Resume Next
  ScaleY = UserControl.ScaleY(a, b, c)
End Function


Public Property Get BackEnabled() As Boolean
  BackEnabled = intBackCount > 0
End Property

Public Property Let BackEnabled(ByVal bolNewValue As Boolean)
End Property


Public Property Get GroupEnabled() As Boolean
  GroupEnabled = mbolGroupEnabled
 
End Property

Public Property Let GroupEnabled(ByVal bolNewValue As Boolean)
  mbolGroupEnabled = bolNewValue

  Dim btn1 As cButton
  Set btn1 = vbalCommandBar1(0).Buttons("GROUP")
    'btn1.Enabled = mbolGroupEnabled
    btn1.Enabled = True
End Property

'Mariam
Public Property Get Switch() As Boolean
   Switch = mbolSwitch
End Property

Public Property Let Switch(ByVal bolSwValue As Boolean)
  mbolSwitch = bolSwValue
  Dim btn1 As cButton
  Set btn1 = vbalCommandBar1(0).Buttons("ARIA5ERP")
  btn1.Enabled = mbolSwitch
End Property
'Mariam

