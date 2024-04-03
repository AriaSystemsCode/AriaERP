VERSION 5.00
Object = "{2210EC79-A724-4033-AAF4-790E2467C0E8}#1.0#0"; "vbalCmdBar6.ocx"
Begin VB.Form frmQuickStart 
   Caption         =   "vbAccelerator CommandBar QuickStart"
   ClientHeight    =   4305
   ClientLeft      =   3480
   ClientTop       =   2610
   ClientWidth     =   7380
   Icon            =   "frmQuickStart.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   4305
   ScaleWidth      =   7380
   Begin vbalCmdBar6.vbalCommandBar cmdBar 
      Align           =   1  'Align Top
      Height          =   735
      Index           =   0
      Left            =   0
      Top             =   0
      Width           =   7380
      _ExtentX        =   13018
      _ExtentY        =   1296
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Style           =   0
   End
End
Attribute VB_Name = "frmQuickStart"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

' Declares to detect whether to use 32bit icons
' in the ImageList or not:
Private Type OSVERSIONINFO
   dwVersionInfoSize As Long
   dwMajorVersion As Long
   dwMinorVersion As Long
   dwBuildNumber As Long
   dwPlatformId As Long
   szCSDVersion(0 To 127) As Byte
End Type
Private Declare Function GetVersionEx Lib "kernel32" Alias "GetVersionExA" (lpVersionInfo As OSVERSIONINFO) As Long
Private Const VER_PLATFORM_WIN32_NT = 2
Private Declare Function DestroyIcon Lib "user32" (ByVal hIcon As Long) As Long

Private m_ilsTools As cVBALImageList
Private m_ilsMenu As cVBALImageList

Private Function IsXPOrAbove() As Boolean
   Dim tOSV As OSVERSIONINFO
   tOSV.dwVersionInfoSize = Len(tOSV)
   GetVersionEx tOSV
   If (tOSV.dwMajorVersion > 5) Then
      IsXPOrAbove = True
   ElseIf (tOSV.dwMajorVersion = 5) Then
      If (tOSV.dwMinorVersion >= 1) Then
         IsXPOrAbove = True
      End If
   End If
End Function

Private Sub ConfigureImageList()
Dim iconSize As Long
Dim eColourDepth As eilColourDepth
Dim sFile As String
Dim sFileBase As String

   ' Load the toolbar images from disk file
   ' (in a real app these would be embedded as a resource)
   sFileBase = App.Path
   If Not (Right(sFileBase, 1) = "\") Then sFileBase = sFileBase & "\"

   If (IsXPOrAbove()) Then
      iconSize = 24
      eColourDepth = ILC_COLOR32
      sFile = "32bit.bmp"
   Else
      iconSize = 20
      eColourDepth = ILC_COLOR
      sFile = "4bit.bmp"
   End If
   
   Set m_ilsTools = New cVBALImageList
   With m_ilsTools
      .IconSizeX = iconSize
      .IconSizeY = iconSize
      .ColourDepth = eColourDepth
      .Create
      .AddFromFile sFileBase & sFile, IMAGE_BITMAP
   End With
   
   ' Here copy the notepad and wordpad icons
   ' from SysImage list into another ImageList.
   Dim ilsSys As New cVBALSysImageList
   With ilsSys
      .IconSizeX = 16
      .IconSizeY = 16
      .Create
   End With
   
   Dim hIcon As Long
   Set m_ilsMenu = New cVBALImageList
   With m_ilsMenu
      .IconSizeX = 16
      .IconSizeY = 16
      .ColourDepth = eColourDepth
      .Create
      hIcon = ilsSys.ItemCopyOfIcon("c:\test.txt")
      .AddFromHandle hIcon, IMAGE_ICON
      DestroyIcon hIcon
      hIcon = ilsSys.ItemCopyOfIcon("c:\test.wri")
      .AddFromHandle hIcon, IMAGE_ICON
      DestroyIcon hIcon
   End With
   
End Sub

Private Sub ConfigureButtons()
Dim btn As cButton
   
   With cmdBar(0).Buttons
   
      ' -------------------------------
      ' Create tool bar buttons:
      ' -------------------------------
      Set btn = .Add("BACK", 0, "Back", eSplit, "Back", _
         vbKeyLeft, vbAltMask)
      btn.ShowCaptionInToolbar = True
      btn.Enabled = False
      Set btn = .Add("FORWARD", 1, "Forward", eSplit, "Forward", _
         vbKeyRight, vbAltMask)
      btn.Enabled = False
      .Add "STOP", 2, "Stop", , "Stop"
      .Add "REFRESH", 3, "Refresh", , "Refresh"
      .Add "HOME", 4, "Home", , "Home", vbKeyHome, vbAltMask
      .Add "SEP1", , , eSeparator
      
      Set btn = .Add("SEARCH", 5, "Search", eRadioNullable, , _
         vbKeyE, vbCtrlMask)
      btn.ShowCaptionInToolbar = True
      Set btn = .Add("FAVOURITES", 6, "Favourites", eRadioNullable, , _
         vbKeyI, vbCtrlMask)
      btn.ShowCaptionInToolbar = True
      Set btn = .Add("MEDIA", 11, "Media", eRadioNullable)
      btn.ShowCaptionInToolbar = True
      .Add "HISTORY", 12, "History", eRadioNullable
      .Add "SEP2", , , eSeparator
      
      Set btn = .Add("MAIL", 13, "Mail")
      btn.ShowDropDownInToolbar = True
      .Add "PRINT", 7, "Print"
      Set btn = .Add("FONTSIZE", 8, "Text Size")
      btn.Visible = False
      .Add "EDIT", 9, "Edit", eSplit
      Set btn = .Add("TOOLS", 10, "Show Tools", , "Show Tools")
      btn.Visible = False
      Set btn = .Add("FULLSCREEN", 14, "Full Screen")
      btn.Visible = False
      .Add "DISCUSS", 15, "Discuss", , "Discuss"
      
      ' -------------------------------
      ' Create mail menu buttons
      ' -------------------------------
      .Add "MAIL:READ", , "&Read Mail"
      .Add "MAIL:NEW", , "&New Message..."
      .Add "MAIL:LINK", , "Send a &Link..."
      .Add "MAIL:PAGE", , "Send &Page..."
      .Add "MAIL:SEP1", , , eSeparator
      .Add "MAIL:NEWS", , "Read Ne&ws"
      
      ' -------------------------------
      ' Create edit menu buttons
      ' -------------------------------
      .Add "EDIT:NOTEPAD", 0, "Edit with &Notepad"
      .Add "EDIT:WORDPAD", 1, "Edit with &Wordpad"
            
   End With
   
End Sub

Private Sub ConfigureBars()
Dim barStandard As cCommandBar
Dim barBack As cCommandBar
Dim barNext As cCommandBar
Dim barMail As cCommandBar
Dim barEdit As cCommandBar
Dim barButtons As cCommandBarButtons

   With cmdBar(0).CommandBars
      ' Create the standard buttons bar:
      Set barStandard = .Add("STANDARD", "Standard Buttons")
      
      ' Create bars for the back and next
      Set barBack = .Add("BACK", "Back")
      Set barNext = .Add("NEXT", "Next")
      
      ' Create the edit bar:
      Set barEdit = .Add("EDIT")
      ' Create the mail bar:
      Set barMail = .Add("MAIL")
      
   End With
   
   ' Assign the buttons
   Set barButtons = barStandard.Buttons
   With cmdBar(0).Buttons
      barButtons.Add .Item("BACK")
      barButtons.Add .Item("FORWARD")
      barButtons.Add .Item("STOP")
      barButtons.Add .Item("REFRESH")
      barButtons.Add .Item("HOME")
      barButtons.Add .Item("SEP1")
      barButtons.Add .Item("SEARCH")
      barButtons.Add .Item("FAVOURITES")
      barButtons.Add .Item("MEDIA")
      barButtons.Add .Item("HISTORY")
      barButtons.Add .Item("SEP2")
      barButtons.Add .Item("MAIL")
      barButtons.Add .Item("PRINT")
      barButtons.Add .Item("EDIT")
      barButtons.Add .Item("TOOLS")
      barButtons.Add .Item("FULLSCREEN")
      barButtons.Add .Item("DISCUSS")
   End With
   
   Set barButtons = barMail.Buttons
   With cmdBar(0).Buttons
      barButtons.Add .Item("MAIL:READ")
      barButtons.Add .Item("MAIL:NEW")
      barButtons.Add .Item("MAIL:LINK")
      barButtons.Add .Item("MAIL:PAGE")
      barButtons.Add .Item("MAIL:SEP1")
      barButtons.Add .Item("MAIL:NEWS")
   End With
   
   Set barButtons = barEdit.Buttons
   With cmdBar(0).Buttons
      barButtons.Add .Item("EDIT:NOTEPAD")
      barButtons.Add .Item("EDIT:WORDPAD")
   End With
   
      ' Assign the drop-downs to their buttons:
   With cmdBar(0).Buttons
      .Item("BACK").Bar = barBack
      .Item("FORWARD").Bar = barNext
      .Item("MAIL").Bar = barMail
      .Item("EDIT").Bar = barEdit
   End With

End Sub

Private Sub cmdBar_ButtonClick(Index As Integer, btn As cButton)
   '
   Debug.Print "Clicked button : " & btn.Key & ", (" & btn.Caption & ")"
   Debug.Print "       checked : " & btn.Checked
   '
End Sub

Private Sub cmdBar_RequestNewInstance(Index As Integer, ctl As Object)
   Dim i As Long
   i = cmdBar.UBound + 1
   Load cmdBar(i)
   cmdBar(i).Align = 0
   Set ctl = cmdBar(i)
End Sub

Private Sub Form_Load()
   
   ConfigureImageList
   
   ConfigureButtons
   
   ConfigureBars
   
   cmdBar(0).ToolbarImageList = m_ilsTools.hIml
   cmdBar(0).MenuImageList = m_ilsMenu.hIml
   cmdBar(0).Toolbar = cmdBar(0).CommandBars("STANDARD")
   cmdBar(0).Style = eComCtl32
   
End Sub
