VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{2210EC79-A724-4033-AAF4-790E2467C0E8}#1.0#0"; "vbalCmdBar6.ocx"
Begin VB.Form frmTestCommandBar 
   Caption         =   "vbAccelerator CommandBar Control Tester"
   ClientHeight    =   8805
   ClientLeft      =   2520
   ClientTop       =   2385
   ClientWidth     =   10470
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmTest.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   8805
   ScaleWidth      =   10470
   Begin VB.PictureBox picClient 
      BorderStyle     =   0  'None
      Height          =   2775
      Left            =   600
      ScaleHeight     =   2775
      ScaleWidth      =   5775
      TabIndex        =   3
      Top             =   1680
      Width           =   5775
      Begin VB.TextBox txtTesting 
         Height          =   315
         Left            =   0
         TabIndex        =   8
         Text            =   "Text1"
         Top             =   0
         Width           =   4575
      End
      Begin VB.CommandButton cmdPopup 
         Caption         =   "&Popup"
         Height          =   375
         Left            =   4080
         TabIndex        =   7
         Top             =   1860
         Width           =   1095
      End
      Begin VB.CommandButton cmdChevron 
         Caption         =   "&Chevron"
         Height          =   375
         Left            =   4080
         TabIndex        =   6
         Top             =   1440
         Width           =   1095
      End
      Begin VB.CommandButton cmdMRUAdd 
         Caption         =   "Add M&RU"
         Height          =   375
         Left            =   4080
         TabIndex        =   5
         Top             =   1020
         Width           =   1095
      End
      Begin VB.ListBox lstEvents 
         Height          =   2220
         IntegralHeight  =   0   'False
         Left            =   0
         TabIndex        =   4
         Top             =   360
         Width           =   4575
      End
   End
   Begin VB.ComboBox cboScript 
      Height          =   315
      Left            =   4140
      TabIndex        =   0
      Text            =   "English"
      Top             =   6540
      Width           =   1215
   End
   Begin VB.ComboBox cboSize 
      Height          =   315
      Left            =   2820
      TabIndex        =   1
      Text            =   "8"
      Top             =   6540
      Width           =   1215
   End
   Begin VB.ComboBox cboFont 
      Height          =   315
      Left            =   1500
      TabIndex        =   2
      Text            =   "Tahoma"
      Top             =   6540
      Width           =   1215
   End
   Begin MSComctlLib.ImageList ilsIcons 
      Left            =   7380
      Top             =   7140
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   23
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":45A2
            Key             =   ""
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":46FC
            Key             =   ""
         EndProperty
         BeginProperty ListImage3 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":4856
            Key             =   ""
         EndProperty
         BeginProperty ListImage4 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":49B0
            Key             =   ""
         EndProperty
         BeginProperty ListImage5 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":4B0A
            Key             =   ""
         EndProperty
         BeginProperty ListImage6 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":4C64
            Key             =   ""
         EndProperty
         BeginProperty ListImage7 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":4DBE
            Key             =   ""
         EndProperty
         BeginProperty ListImage8 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":4F18
            Key             =   ""
         EndProperty
         BeginProperty ListImage9 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":5072
            Key             =   ""
         EndProperty
         BeginProperty ListImage10 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":51CC
            Key             =   ""
         EndProperty
         BeginProperty ListImage11 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":5326
            Key             =   ""
         EndProperty
         BeginProperty ListImage12 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":5480
            Key             =   ""
         EndProperty
         BeginProperty ListImage13 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":55DA
            Key             =   ""
         EndProperty
         BeginProperty ListImage14 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":5734
            Key             =   ""
         EndProperty
         BeginProperty ListImage15 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":588E
            Key             =   ""
         EndProperty
         BeginProperty ListImage16 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":59E8
            Key             =   ""
         EndProperty
         BeginProperty ListImage17 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":5B42
            Key             =   ""
         EndProperty
         BeginProperty ListImage18 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":5C9C
            Key             =   ""
         EndProperty
         BeginProperty ListImage19 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":5DF6
            Key             =   ""
         EndProperty
         BeginProperty ListImage20 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":5F50
            Key             =   ""
         EndProperty
         BeginProperty ListImage21 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":60AA
            Key             =   ""
         EndProperty
         BeginProperty ListImage22 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":6204
            Key             =   ""
         EndProperty
         BeginProperty ListImage23 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmTest.frx":635E
            Key             =   ""
         EndProperty
      EndProperty
   End
   Begin vbalCmdBar6.vbalCommandBar cmdBar 
      Align           =   1  'Align Top
      Height          =   495
      Index           =   1
      Left            =   0
      Top             =   0
      Width           =   10470
      _ExtentX        =   18468
      _ExtentY        =   873
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin vbalCmdBar6.vbalCommandBar cmdBar 
      Align           =   2  'Align Bottom
      Height          =   495
      Index           =   2
      Left            =   0
      Top             =   8310
      Width           =   10470
      _ExtentX        =   18468
      _ExtentY        =   873
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Orientation     =   3
   End
   Begin vbalCmdBar6.vbalCommandBar cmdBar 
      Align           =   3  'Align Left
      Height          =   6825
      Index           =   3
      Left            =   0
      Top             =   1485
      Width           =   495
      _ExtentX        =   873
      _ExtentY        =   12039
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Orientation     =   1
   End
   Begin vbalCmdBar6.vbalCommandBar cmdBar 
      Align           =   4  'Align Right
      Height          =   6825
      Index           =   4
      Left            =   9975
      Top             =   1485
      Width           =   495
      _ExtentX        =   873
      _ExtentY        =   12039
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Orientation     =   2
   End
   Begin vbalCmdBar6.vbalCommandBar cmdBar 
      Align           =   1  'Align Top
      Height          =   495
      Index           =   0
      Left            =   0
      Top             =   495
      Width           =   10470
      _ExtentX        =   18468
      _ExtentY        =   873
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Orientation     =   3
   End
   Begin vbalCmdBar6.vbalCommandBar cmdBar 
      Align           =   1  'Align Top
      Height          =   495
      Index           =   5
      Left            =   0
      Top             =   990
      Width           =   10470
      _ExtentX        =   18468
      _ExtentY        =   873
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Orientation     =   3
   End
End
Attribute VB_Name = "frmTestCommandBar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub createCommandBars()

   Dim i As Long, j As Long, k As Long
   Dim Bar As cCommandBar
   Dim barSub As cCommandBar
   Dim barSubSub As cCommandBar
   Dim btn As cButton
   Dim btnSub As cButton
   Dim btnSubSub As cButton
   Dim btns As cCommandBarButtons

   ' --------------------------------------------------------
   '
   ' create the items we're going to use.
   ' Buttons and CommandBars are global: it doesn't matter
   ' which control instance you use to create them,
   ' the other toolbars will reflect the same set of items.
   
   ' Remember the command bars are global to your *project*
   ' not just the form that holds the controls
   '
   ' --------------------------------------------------------
      
   With cmdBar(0)
      
      With .Buttons
      
         Set btn = .Add("NEW", 0, "&New...", , "New", vbKeyN, vbCtrlMask)
         btn.ShowCaptionInToolbar = True
         .Add "OPEN", 1, "&Open...", , "Open", vbKeyO, vbCtrlMask
         Set btn = .Add("SAVE", 2, "&Save", , "Save", vbKeyS, vbCtrlMask)
         btn.Enabled = False
         .Add "TOOLS:FILE:SEPARATOR", , , eSeparator
         .Add "PRINT", 3, "&Print...", , "Print", vbKeyP, vbCtrlMask
         .Add "PRINTPREVIEW", 4, "Print Pre&view", , "Print Preview"
         .Add "TOOLS:PRINT:SEPARATOR", , , eSeparator
         .Add "FIND", 5, "&Find...", , "Find", vbKeyF, vbCtrlMask
         .Add "TOOLS:FIND:SEPARATOR", , , eSeparator
         .Add "CUT", 6, "Cu&t", , "Cut", vbKeyX, vbCtrlMask
         .Add "COPY", 7, "&Copy", , "Copy", vbKeyC, vbCtrlMask
         Set btn = .Add("PASTE", 8, "&Paste", , "Paste", vbKeyV, vbCtrlMask)
         btn.Enabled = False
         .Add "UNDO", 9, "&Undo", , "Undo", vbKeyZ, vbCtrlMask
         .Add "TOOLS:CLIP:SEPARATOR", , , eSeparator
         .Add "INSERTDATETIME", 10, "Insert Date/Time", , "Insert Date/Time"
         
         .Add "FONT", 19, "&Font...", , "Change Font"
         Set btn = .Add("VIEWOPTION", , "&View", , "Set Viewing Options")
         btn.ShowCaptionInToolbar = True
         btn.ShowDropDownInToolbar = True
         .Add "FMT:INIT:SEPARATOR", , , eSeparator
         Set btn = .Add("FONTFACE", 19, "Font Face", ePanel, "Font face")
         btn.PanelWidth = 90
         btn.PanelControl = cboFont
         Set btn = .Add("FONTSIZE", 19, "Font Size", ePanel, "Font Size")
         btn.PanelWidth = 36
         btn.PanelControl = cboSize
         Set btn = .Add("FONTSCRIPT", 19, "Font Script", ePanel, "Font Script")
         btn.PanelWidth = 90
         btn.PanelControl = cboScript
         .Add "FMT:FONT:SEPARATOR", , , eSeparator
         .Add "BOLD", 11, "&Bold", eCheck, "Bold", vbKeyB, vbCtrlMask
         .Add "ITALIC", 12, "&Italic", eCheck, "Italic", vbKeyI, vbCtrlMask
         .Add "UNDERLINE", 13, "&Underline", eCheck, "Underline", vbKeyU, vbCtrlMask
         .Add "COLOUR", 21, "&Colour", eSplit, "Formats the selection with colour"
         .Add "FMT:STYLE:SEPARATOR", , , eSeparator
         .Add "ALIGNLEFT", 15, "Align &Left", eRadio, "Align text left"
         .Add "ALIGNCENTRE", 16, "Centre", eRadio, "Align text centre"
         .Add "ALIGNRIGHT", 17, "Align &Right", eRadio, "Align text right"
         .Add "FMT:PAR:SEPARATOR", , , eSeparator
         Set btn = .Add("BULLETS", 18, "&Bullets", eCheck, "Bullets")
         btn.Checked = True
         
         Set btn = .Add("FILE", , "&File")
         btn.ShowCaptionInToolbar = True
         Set btn = .Add("EDIT", , "&Edit")
         btn.ShowCaptionInToolbar = True
         Set btn = .Add("VIEW", , "&View")
         btn.ShowCaptionInToolbar = True
         Set btn = .Add("INSERT", , "&Insert")
         btn.ShowCaptionInToolbar = True
         btn.Enabled = False
         Set btn = .Add("FORMAT", , "F&ormat")
         btn.ShowCaptionInToolbar = True
         Set btn = .Add("HELP", , "&Help")
         btn.ShowCaptionInToolbar = True
         
         Set btn = .Add("SAVEAS", , "Save &As...")
         btn.Enabled = False
         .Add "PAGESETUP", , "Page Set&up..."
         For i = 1 To 8
            Set btn = .Add("FILE:MRU:" & i, , "")
            btn.Visible = False
         Next i
         Set btn = .Add("FILE:MRU:SEPARATOR", , , eSeparator)
         btn.Visible = False
         
         .Add "SEND", , "Sen&d...", , "Send as a file through electronic mail"
         .Add "FILE:SEND:SEPARATOR", , , eSeparator
         .Add "EXIT", , "E&xit"
         
         .Add "EDIT:UNDO:SEPARATOR", , , eSeparator
         .Add "PASTESPECIAL", , "Paste &Special", , "Insert clipboard content with options"
         .Add "CLEAR", , "&Clear", , , vbKeyDelete, 0
         .Add "SELECTALL", , "Select &All", , , vbKeyA, vbCtrlMask
         .Add "FINDNEXT", , "&Find Next", , , vbKeyF3
         .Add "REPLACE", , "&Replace", , , vbKeyH, vbCtrlMask
         .Add "LINKS", , "Lin&ks..."
         .Add "OBJECTPROPERTIES", , "Object &Properties..."
         .Add "OBJECT", , "&Object..."
         
         .Add "TOOLBARS", , "&Toolbars"
         Set btn = .Add("STANDARD", , "&Tool bar", eCheck)
         btn.Checked = True
         Set btn = .Add("FORMATBAR", , "&Format bar", eCheck)
         btn.Checked = True
         Set btn = .Add("RULER", , "&Ruler", eCheck)
         btn.Checked = True
         Set btn = .Add("STATUSBAR", , "&Status Bar", eCheck)
         btn.Checked = True
         .Add "VIEW:SEPARATOR", , , eSeparator
         .Add "OPTIONS", , "&Options..."
      
         .Add "INSERTOBJECT", , "&Object..."
         
         .Add "FORMATFONT", , "&Font..."
         .Add "BULLETSTYLE", , "&Bullet Style..."
         .Add "PARAGRAPH", , "&Paragraph..."
         .Add "TABS", , "&Tabs..."
         
         .Add "HELPTOPICS", 22, "&Help Topics", , , vbKeyF1
         .Add "HELP:SEPARATOR", , , eSeparator
         .Add "HELP:ABOUT", , "&About..."
         
         ' Some menu items to demonstrate multi-level menus:
         For i = 1 To 5
            .Add "MULTILEVELTEST:" & i, , "&" & i & " in Menu (1)"
            If (i = 1) Or (i >= 4) Then
               For j = 1 To 5
                  .Add "MULTILEVELTEST:" & i & ":" & j, , "&" & j & " in Menu (1," & i & ")"
                  If (j < 3) Then
                     For k = 1 To 7
                        .Add "MULTILEVELTEST:" & i & ":" & j & ":" & k, , "&" & k & " in Menu (1," & i & "," & j & ")"
                     Next k
                  End If
               Next j
            End If
         Next i
         
         ' Colour menu
         addColourButton "AUTOMATIC", &H0&, "Automatic Colour", "Automatic"
         .Add "FORECOLOUR:SEPARATOR:1", , , eSeparator
         
         addColourButton "BLACK", &H0&, "Black"
         addColourButton "BROWN", &H3399&, "Brown"
         addColourButton "OLIVEGREEN", &H3333&, "Olive Green"
         addColourButton "DARKGREEN", &H3300&, "Dark Green"
         addColourButton "DARKTEAL", &H663300, "Dark Teal"
         addColourButton "DARKBLUE", &H800000, "Dark Blue"
         addColourButton "INDIGO", &H993333, "Indigo"
         addColourButton "GRAY-80%", &H333333, "Gray - 80%"
         
         addColourButton "DARKRED", &H80&, "Dark Red"
         addColourButton "ORANGE", &H66FF&, "Orange"
         addColourButton "DARKYELLOW", &H8080&, "Dark Yellow"
         addColourButton "GREEN", &H8000&, "Green"
         addColourButton "TEAL", &H808000, "Teal"
         addColourButton "BLUE", &HFF0000, "Blue"
         addColourButton "BLUEGRAY", &H996666, "Blue-Gray"
         addColourButton "GRAY-50%", &H808080, "Gray - 50%"
         
         addColourButton "RED", &HFF&, "Red"
         addColourButton "LIGHTORANGE", &H99FF&, "Light Orange"
         addColourButton "LIME", &HCC99&, "Lime"
         addColourButton "SEAGREEN", &H669933, "Sea Green"
         addColourButton "AQUA", &HCCCC33, "Aqua"
         addColourButton "LIGHTBLUE", &HFF6633, "Light Blue"
         addColourButton "VIOLET", &H800080, "Violet"
         addColourButton "GRAY-40%", &H969696, "Gray - 40%"
         
         addColourButton "PINK", &HFF00FF, "Pink"
         addColourButton "GOLD", &HCCFF&, "Gold"
         addColourButton "YELLOW", &HFFFF&, "Yellow"
         addColourButton "BRIGHTGREEN", &HFF00&, "Bright Green"
         addColourButton "TURQUOISE", &HFFFF00, "Turquoise"
         addColourButton "SKYBLUE", &HFFCC00, "Sky Blue"
         addColourButton "PLUM", &H663399, "Plum"
         addColourButton "GRAY-25%", &HC0C0C0, "Gray - 25%"
         
         addColourButton "ROSE", &HCC99FF, "Rose"
         addColourButton "TAN", &H99CCFF, "Tan"
         addColourButton "LIGHTYELLOW", &H99FFFF, "Light Yellow"
         addColourButton "LIGHTGREEN", &HCCFFCC, "Light Green"
         addColourButton "LIGHTTURQUOISE", &HFFFFCC, "Light Turquoise"
         addColourButton "PALEBLUE", &HFFCC99, "Pale Blue"
         addColourButton "LAVENDER", &HFF99CC, "Lavender"
         addColourButton "WHITE", &HFFFFFF, "White"
         
         .Add "FORECOLOUR:SEPARATOR:2", , , eSeparator
                  
         .Add "FORECOLOUR:CUSTOM", , "&Custom...", eCheck
                  
         ' Style menu:
         Set btn = .Add("STYLE:CAPTION", , "Style:")
         'btn.Locked = True
         btn.Enabled = False
         btn.ShowCaptionInToolbar = True
         Set btn = .Add("STYLE:0", , "Office XP", eRadio)
         btn.ShowCaptionInToolbar = True
         Set btn = .Add("STYLE:1", , "Office 2003", eRadio)
         btn.Checked = True
         btn.ShowCaptionInToolbar = True
         Set btn = .Add("STYLE:2", , "MS Money", eRadio)
         btn.ShowCaptionInToolbar = True
         Set btn = .Add("STYLE:3", , "System", eRadio)
         btn.ShowCaptionInToolbar = True
                  
      End With

      ' --------------------------------------------------------
      ' Create the command bars:
      ' --------------------------------------------------------
      
      ' Toolbar:
      Set Bar = .CommandBars.Add("STANDARD", "Tool Bar")
      Set btns = Bar.Buttons
      btns.Add .Buttons("NEW")
      btns.Add .Buttons("OPEN")
      btns.Add .Buttons("SAVE")
      btns.Add .Buttons("TOOLS:FILE:SEPARATOR")
      btns.Add .Buttons("PRINT")
      btns.Add .Buttons("PRINTPREVIEW")
      btns.Add .Buttons("TOOLS:PRINT:SEPARATOR")
      btns.Add .Buttons("FIND")
      btns.Add .Buttons("TOOLS:FIND:SEPARATOR")
      btns.Add .Buttons("CUT")
      btns.Add .Buttons("COPY")
      btns.Add .Buttons("PASTE")
      btns.Add .Buttons("UNDO")
      btns.Add .Buttons("TOOLS:CLIP:SEPARATOR")
      btns.Add .Buttons("INSERTDATETIME")
      
      
      ' Format bar:
      Set Bar = .CommandBars.Add("FORMATBAR", "Format")
      Set btns = Bar.Buttons
      btns.Add .Buttons("FONT")
      btns.Add .Buttons("VIEWOPTION")
      btns.Add .Buttons("FMT:INIT:SEPARATOR")
      btns.Add .Buttons("FONTFACE")
      btns.Add .Buttons("FONTSIZE")
      btns.Add .Buttons("FONTSCRIPT")
      btns.Add .Buttons("FMT:FONT:SEPARATOR")
      btns.Add .Buttons("BOLD")
      btns.Add .Buttons("ITALIC")
      btns.Add .Buttons("UNDERLINE")
      btns.Add .Buttons("COLOUR")
      btns.Add .Buttons("FMT:STYLE:SEPARATOR")
      btns.Add .Buttons("ALIGNLEFT")
      btns.Add .Buttons("ALIGNCENTRE")
      btns.Add .Buttons("ALIGNRIGHT")
      btns.Add .Buttons("FMT:PAR:SEPARATOR")
      btns.Add .Buttons("BULLETS")
      
      ' Top level menu:
      Set Bar = .CommandBars.Add("MENU", "Menu")
      Set btns = Bar.Buttons
      btns.Add .Buttons("FILE")
      btns.Add .Buttons("EDIT")
      btns.Add .Buttons("VIEW")
      btns.Add .Buttons("INSERT")
      btns.Add .Buttons("FORMAT")
      btns.Add .Buttons("HELP")
      
      ' File menu items:
      Set Bar = .CommandBars.Add("FILEMENU")
      Set btns = Bar.Buttons
      btns.Add .Buttons("NEW")
      btns.Add .Buttons("OPEN")
      btns.Add .Buttons("SAVE")
      btns.Add .Buttons("SAVEAS")
      btns.Add .Buttons("TOOLS:FILE:SEPARATOR")
      btns.Add .Buttons("PRINT")
      btns.Add .Buttons("PRINTPREVIEW")
      btns.Add .Buttons("PAGESETUP")
      btns.Add .Buttons("TOOLS:PRINT:SEPARATOR")
      For i = 1 To 8
         btns.Add .Buttons("FILE:MRU:" & i)
      Next i
      btns.Add .Buttons("FILE:MRU:SEPARATOR")
      btns.Add .Buttons("SEND")
      btns.Add .Buttons("FILE:SEND:SEPARATOR")
      btns.Add .Buttons("EXIT")
      .Buttons("FILE").Bar = Bar
      
      ' Edit menu items:
      Set Bar = .CommandBars.Add("EDITMENU")
      Set btns = Bar.Buttons
      btns.Add .Buttons("UNDO")
      btns.Add .Buttons("EDIT:UNDO:SEPARATOR")
      btns.Add .Buttons("CUT")
      btns.Add .Buttons("COPY")
      btns.Add .Buttons("PASTE")
      btns.Add .Buttons("PASTESPECIAL")
      btns.Add .Buttons("CLEAR")
      btns.Add .Buttons("SELECTALL")
      btns.Add .Buttons("TOOLS:CLIP:SEPARATOR")
      btns.Add .Buttons("FIND")
      btns.Add .Buttons("FINDNEXT")
      btns.Add .Buttons("REPLACE")
      btns.Add .Buttons("TOOLS:FIND:SEPARATOR")
      btns.Add .Buttons("LINKS")
      btns.Add .Buttons("OBJECTPROPERTIES")
      btns.Add .Buttons("OBJECT")
      .Buttons("EDIT").Bar = Bar
      
      ' View - Toolbars sub menu
      Set Bar = .CommandBars.Add("TOOLBARS")
      Set btns = Bar.Buttons
      btns.Add .Buttons("STANDARD")
      btns.Add .Buttons("FORMATBAR")
      .Buttons("TOOLBARS").Bar = Bar
      
      ' View menu items:
      Set Bar = .CommandBars.Add("VIEWMENU")
      Set btns = Bar.Buttons
      btns.Add .Buttons("TOOLBARS")
      btns.Add .Buttons("RULER")
      btns.Add .Buttons("STATUSBAR")
      btns.Add .Buttons("VIEW:SEPARATOR")
      btns.Add .Buttons("OPTIONS")
      .Buttons("VIEW").Bar = Bar
      .Buttons("VIEWOPTION").Bar = Bar
      
      ' Multi level menu
      Set Bar = .CommandBars.Add("FORMATMENU")
      Set btns = Bar.Buttons
      
      For i = 1 To 5
         Set btn = .Buttons("MULTILEVELTEST:" & i)
         btns.Add btn
         If (i = 1) Or (i >= 4) Then
            Set barSub = .CommandBars.Add("FORMATMENU:" & i)
            For j = 1 To 5
               Set btnSub = .Buttons("MULTILEVELTEST:" & i & ":" & j)
               barSub.Buttons.Add btnSub
               If (j < 3) Then
                  Set barSubSub = .CommandBars.Add("FORMATMENU:" & i & ":" & j)
                  For k = 1 To 7
                     barSubSub.Buttons.Add .Buttons("MULTILEVELTEST:" & i & ":" & j & ":" & k)
                  Next k
                  Set btnSub.Bar = barSubSub
               End If
            Next j
            Set btn.Bar = barSub
         End If
      Next i
      
      .Buttons("FORMAT").Bar = Bar
      
      
      ' Help menu items:
      Set Bar = .CommandBars.Add("HELPMENU")
      Set btns = Bar.Buttons
      btns.Add .Buttons("HELPTOPICS")
      btns.Add .Buttons("HELP:SEPARATOR")
      btns.Add .Buttons("HELP:ABOUT")
      .Buttons("HELP").Bar = Bar
      
      ' Colour menu items:
      Set Bar = .CommandBars.Add("FORECOLOUR")
      Set btns = Bar.Buttons
      btns.Add .Buttons("FORECOLOUR:AUTOMATIC")
      btns.Add .Buttons("FORECOLOUR:SEPARATOR:1")
      
      btns.Add .Buttons("FORECOLOUR:BLACK")
      btns.Add .Buttons("FORECOLOUR:BROWN")
      btns.Add .Buttons("FORECOLOUR:OLIVEGREEN")
      btns.Add .Buttons("FORECOLOUR:DARKGREEN")
      btns.Add .Buttons("FORECOLOUR:DARKTEAL")
      btns.Add .Buttons("FORECOLOUR:DARKBLUE")
      btns.Add .Buttons("FORECOLOUR:INDIGO")
      btns.Add .Buttons("FORECOLOUR:GRAY-80%")
      
      btns.Add .Buttons("FORECOLOUR:DARKRED")
      btns.Add .Buttons("FORECOLOUR:ORANGE")
      btns.Add .Buttons("FORECOLOUR:DARKYELLOW")
      btns.Add .Buttons("FORECOLOUR:GREEN")
      btns.Add .Buttons("FORECOLOUR:TEAL")
      btns.Add .Buttons("FORECOLOUR:BLUE")
      btns.Add .Buttons("FORECOLOUR:BLUEGRAY")
      btns.Add .Buttons("FORECOLOUR:GRAY-50%")
      
      btns.Add .Buttons("FORECOLOUR:RED")
      btns.Add .Buttons("FORECOLOUR:LIGHTORANGE")
      btns.Add .Buttons("FORECOLOUR:LIME")
      btns.Add .Buttons("FORECOLOUR:SEAGREEN")
      btns.Add .Buttons("FORECOLOUR:AQUA")
      btns.Add .Buttons("FORECOLOUR:LIGHTBLUE")
      btns.Add .Buttons("FORECOLOUR:VIOLET")
      btns.Add .Buttons("FORECOLOUR:GRAY-40%")
      
      btns.Add .Buttons("FORECOLOUR:PINK")
      btns.Add .Buttons("FORECOLOUR:GOLD")
      btns.Add .Buttons("FORECOLOUR:YELLOW")
      btns.Add .Buttons("FORECOLOUR:BRIGHTGREEN")
      btns.Add .Buttons("FORECOLOUR:TURQUOISE")
      btns.Add .Buttons("FORECOLOUR:SKYBLUE")
      btns.Add .Buttons("FORECOLOUR:PLUM")
      btns.Add .Buttons("FORECOLOUR:GRAY-25%")
      
      btns.Add .Buttons("FORECOLOUR:ROSE")
      btns.Add .Buttons("FORECOLOUR:TAN")
      btns.Add .Buttons("FORECOLOUR:LIGHTYELLOW")
      btns.Add .Buttons("FORECOLOUR:LIGHTGREEN")
      btns.Add .Buttons("FORECOLOUR:LIGHTTURQUOISE")
      btns.Add .Buttons("FORECOLOUR:PALEBLUE")
      btns.Add .Buttons("FORECOLOUR:LAVENDER")
      btns.Add .Buttons("FORECOLOUR:WHITE")
      
      btns.Add .Buttons("FORECOLOUR:SEPARATOR:2")
      btns.Add .Buttons("FORECOLOUR:CUSTOM")
      
      .Buttons("COLOUR").Bar = Bar
      
      ' Style bar:
      Set Bar = .CommandBars.Add("STYLE", "Style")
      Set btns = Bar.Buttons
      btns.Add .Buttons("STYLE:CAPTION")
      btns.Add .Buttons("STYLE:0")
      btns.Add .Buttons("STYLE:1")
      btns.Add .Buttons("STYLE:2")
      btns.Add .Buttons("STYLE:3")
      
   End With

End Sub

Private Sub addColourButton(ByVal sKeyPart As String, ByVal oColor As OLE_COLOR, ByVal sToolTip As String, Optional ByVal sCaption As String = "")
   With cmdBar(0).Buttons
      Dim btn As cButton
      Set btn = .Add("FORECOLOUR:" & sKeyPart, , sCaption, eRadio, sToolTip)
      btn.ColorBox = oColor
   End With
End Sub

Private Sub cmdBar_AfterShowMenu(Index As Integer, Bar As cCommandBar)
   lstEvents.AddItem "After Show Menu " & Bar.Key
End Sub

Private Sub cmdBar_BeforeShowMenu(Index As Integer, Bar As cCommandBar)
   lstEvents.AddItem "Before Show Menu " & Bar.Key
End Sub

Private Sub cmdBar_ButtonClick(Index As Integer, btn As cButton)
   '
   lstEvents.AddItem "ButtonClick (control " & Index & ") - " & btn.Key & " (" & btn.Caption & ")"
   
   Debug.Print ActiveControl.Name
   
   If Left(btn.Key, 5) = "STYLE" Then
      cmdBar(0).Style = Mid(btn.Key, 7)
   End If
   '
End Sub

Private Sub cmdBar_ButtonDropDown(Index As Integer, btn As cButton, cancel As Boolean)
   '
   lstEvents.AddItem "ButtonDropDown (control " & Index & ") - " & btn.Key & " (" & btn.Caption & ")"
   '
End Sub


Private Sub cmdBar_RequestNewInstance(Index As Integer, ctl As Object)
   '
   Dim lNewIndex As Long
   lNewIndex = cmdBar.UBound + 1
   Load cmdBar(lNewIndex)
   cmdBar(lNewIndex).Align = 0
   Set ctl = cmdBar(lNewIndex)
   lstEvents.AddItem "New Control Instance Obtained:" & ctl.hWnd
   '
End Sub

Private Sub cmdBar_Resize(Index As Integer)
   Form_Resize
End Sub

Private Sub cmdChevron_Click()
   Dim xPixels As Long
   Dim yPixels As Long
   xPixels = Me.ScaleX(cmdChevron.Left + cmdChevron.Width, Me.ScaleMode, vbPixels)
   yPixels = Me.ScaleY(cmdChevron.Top, Me.ScaleMode, vbPixels)
   cmdBar(0).ClientCoordinatesToScreen xPixels, yPixels, picClient.hWnd
   cmdBar(0).ShowChevronMenu xPixels, yPixels
End Sub

Private Sub cmdMRUAdd_Click()
Dim i As Long
Dim btn As cButton
   For i = 1 To 8
      Set btn = cmdBar(0).Buttons("FILE:MRU:" & i)
      If Not (btn.Visible) Then
         btn.Caption = "&" & i & ") MRUFile " & i & ".txt"
         btn.Visible = True
         If (i = 1) Then
            cmdBar(0).Buttons("FILE:MRU:SEPARATOR").Visible = True
         End If
         If (i = 8) Then
            cmdMRUAdd.Enabled = False
         End If
         Exit For
      End If
   Next i
   
End Sub


Private Sub cmdPopup_Click()
   Dim xPixels As Long
   Dim yPixels As Long
   xPixels = Me.ScaleX(cmdPopup.Left, Me.ScaleMode, vbPixels)
   yPixels = Me.ScaleY(cmdPopup.Top + cmdPopup.Height, Me.ScaleMode, vbPixels)
   cmdBar(0).ClientCoordinatesToScreen xPixels, yPixels, picClient.hWnd
   cmdBar(0).ShowPopupMenu xPixels, yPixels, cmdBar(0).CommandBars("EDITMENU")
End Sub

Private Sub Form_Load()
   
   ' Create all the buttons and command bars
   createCommandBars
      
   ' associate the image lists
   Dim i As Long
   For i = 0 To 4
      cmdBar(i).ToolbarImageList = ilsIcons
      cmdBar(i).MenuImageList = ilsIcons
   Next i
   
   ' Show the toolbars:
   cmdBar(1).MainMenu = True
   cmdBar(0).Toolbar = cmdBar(0).CommandBars("FORMATBAR")
   cmdBar(1).Toolbar = cmdBar(0).CommandBars("MENU")
   cmdBar(3).Toolbar = cmdBar(0).CommandBars("STANDARD")
   cmdBar(2).Toolbar = cmdBar(0).CommandBars("MENU")
   cmdBar(4).Toolbar = cmdBar(0).CommandBars("MENU")
   cmdBar(5).Toolbar = cmdBar(0).CommandBars("STYLE")
   
End Sub


Private Sub Form_Resize()
Dim lWidth As Long
Dim lHeight As Long
Dim lLeft As Long
Dim lTop As Long
   On Error Resume Next
   lLeft = cmdBar(3).Left + cmdBar(3).Width
   lTop = cmdBar(5).Top + cmdBar(5).Height
   lWidth = Me.ScaleWidth - lLeft - cmdBar(4).Width
   lHeight = Me.ScaleHeight - lTop - cmdBar(2).Height
   picClient.Move lLeft, lTop, lWidth, lHeight
End Sub


Private Sub picClient_Resize()
   On Error Resume Next
   txtTesting.Move Screen.TwipsPerPixelX, Screen.TwipsPerPixelY, picClient.ScaleWidth - 2 * Screen.TwipsPerPixelX
   Dim lTop As Long
   lTop = 2 * Screen.TwipsPerPixelY + txtTesting.Top + txtTesting.Height
   lstEvents.Move Screen.TwipsPerPixelX, lTop, picClient.ScaleWidth - 2 * Screen.TwipsPerPixelX, picClient.ScaleHeight - Screen.TwipsPerPixelY - lTop
End Sub
