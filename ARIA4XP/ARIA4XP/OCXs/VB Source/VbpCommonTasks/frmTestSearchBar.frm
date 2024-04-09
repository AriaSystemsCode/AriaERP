VERSION 5.00
Object = "{8245A918-4CF7-11D2-8E21-10B404C10000}#8.1#0"; "vbalIml.ocx"
Object = "{D3D6FDC7-C9A0-4D16-99C2-E7FA5234DE4A}#4.0#0"; "vbalExpBar.ocx"
Begin VB.Form frmTestSearchBar 
   Caption         =   "vbAccelerator - Explorer Bar Control Demonstration - Search Tester"
   ClientHeight    =   6990
   ClientLeft      =   5010
   ClientTop       =   3660
   ClientWidth     =   8835
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmTestSearchBar.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   6990
   ScaleWidth      =   8835
   Begin VB.CheckBox chkAlign 
      Caption         =   "&Aligned"
      Height          =   375
      Left            =   3960
      TabIndex        =   30
      Top             =   1320
      Value           =   1  'Checked
      Width           =   4095
   End
   Begin VB.CheckBox chkVisible 
      Caption         =   "&Visible"
      Height          =   375
      Left            =   3960
      TabIndex        =   29
      Top             =   960
      Value           =   1  'Checked
      Width           =   4095
   End
   Begin VB.PictureBox pnlAction 
      BorderStyle     =   0  'None
      Height          =   435
      Left            =   3900
      ScaleHeight     =   435
      ScaleWidth      =   2955
      TabIndex        =   25
      Top             =   1800
      Width           =   2955
      Begin VB.CommandButton cmdAction 
         Caption         =   "Action"
         Height          =   375
         Index           =   1
         Left            =   420
         TabIndex        =   27
         Top             =   60
         Width           =   1275
      End
      Begin VB.CommandButton cmdAction 
         Caption         =   "Action"
         Height          =   375
         Index           =   0
         Left            =   1680
         TabIndex        =   26
         Top             =   60
         Width           =   1275
      End
   End
   Begin VB.PictureBox pnlAdvanced 
      BorderStyle     =   0  'None
      Height          =   915
      Left            =   3960
      ScaleHeight     =   915
      ScaleWidth      =   2835
      TabIndex        =   23
      Top             =   6120
      Width           =   2835
      Begin VB.Label lblInfo 
         BackStyle       =   0  'Transparent
         Caption         =   "Add controls to the Advanced panel here..."
         Height          =   495
         Left            =   60
         TabIndex        =   24
         Top             =   0
         Width           =   2715
      End
   End
   Begin VB.PictureBox pnlSize 
      BorderStyle     =   0  'None
      Height          =   1635
      Left            =   3960
      ScaleHeight     =   1635
      ScaleWidth      =   2835
      TabIndex        =   15
      Top             =   4440
      Width           =   2835
      Begin VB.TextBox txtSize 
         BackColor       =   &H8000000F&
         Enabled         =   0   'False
         Height          =   315
         Left            =   1200
         TabIndex        =   22
         Text            =   "0"
         Top             =   1200
         Width           =   1095
      End
      Begin VB.ComboBox cboSize 
         BackColor       =   &H8000000F&
         Enabled         =   0   'False
         Height          =   315
         ItemData        =   "frmTestSearchBar.frx":09AA
         Left            =   240
         List            =   "frmTestSearchBar.frx":09B4
         Style           =   2  'Dropdown List
         TabIndex        =   21
         Top             =   1200
         Width           =   915
      End
      Begin VB.OptionButton optSize 
         Caption         =   "Specify size (in KB)"
         Height          =   195
         Index           =   4
         Left            =   0
         TabIndex        =   20
         Top             =   960
         Width           =   2715
      End
      Begin VB.OptionButton optSize 
         Caption         =   "Large (more than 1MB)"
         Height          =   195
         Index           =   3
         Left            =   0
         TabIndex        =   19
         Top             =   720
         Width           =   2715
      End
      Begin VB.OptionButton optSize 
         Caption         =   "Medium (less than 1MB)"
         Height          =   195
         Index           =   2
         Left            =   0
         TabIndex        =   18
         Top             =   480
         Width           =   2715
      End
      Begin VB.OptionButton optSize 
         Caption         =   "Small (less than 100KB)"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   195
         Index           =   1
         Left            =   0
         TabIndex        =   17
         Top             =   240
         Width           =   2715
      End
      Begin VB.OptionButton optSize 
         Caption         =   "Don't Remember"
         Height          =   195
         Index           =   0
         Left            =   0
         TabIndex        =   16
         Top             =   0
         Value           =   -1  'True
         Width           =   2715
      End
   End
   Begin VB.PictureBox pnlModified 
      BorderStyle     =   0  'None
      Height          =   2235
      Left            =   3960
      ScaleHeight     =   2235
      ScaleWidth      =   2835
      TabIndex        =   4
      Top             =   2100
      Width           =   2835
      Begin VB.TextBox txtTo 
         BackColor       =   &H8000000F&
         Enabled         =   0   'False
         Height          =   285
         Left            =   900
         TabIndex        =   14
         Top             =   1860
         Width           =   1815
      End
      Begin VB.TextBox txtFrom 
         BackColor       =   &H8000000F&
         Enabled         =   0   'False
         Height          =   285
         Left            =   900
         TabIndex        =   13
         Top             =   1560
         Width           =   1815
      End
      Begin VB.ComboBox cboDateType 
         BackColor       =   &H8000000F&
         Enabled         =   0   'False
         Height          =   315
         ItemData        =   "frmTestSearchBar.frx":09CB
         Left            =   240
         List            =   "frmTestSearchBar.frx":09D8
         Style           =   2  'Dropdown List
         TabIndex        =   10
         Top             =   1200
         Width           =   2475
      End
      Begin VB.OptionButton optModified 
         Caption         =   "Specify Dates"
         Height          =   195
         Index           =   4
         Left            =   0
         TabIndex        =   9
         Top             =   960
         Width           =   2715
      End
      Begin VB.OptionButton optModified 
         Caption         =   "Within the last year"
         Height          =   195
         Index           =   3
         Left            =   0
         TabIndex        =   8
         Top             =   720
         Width           =   2715
      End
      Begin VB.OptionButton optModified 
         Caption         =   "Past month"
         Height          =   195
         Index           =   2
         Left            =   0
         TabIndex        =   7
         Top             =   480
         Width           =   2715
      End
      Begin VB.OptionButton optModified 
         Caption         =   "Within the last week"
         Height          =   195
         Index           =   1
         Left            =   0
         TabIndex        =   6
         Top             =   240
         Width           =   2715
      End
      Begin VB.OptionButton optModified 
         Caption         =   "Don't Remember"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   195
         Index           =   0
         Left            =   0
         TabIndex        =   5
         Top             =   0
         Value           =   -1  'True
         Width           =   2715
      End
      Begin VB.Label lblTo 
         BackStyle       =   0  'Transparent
         Caption         =   "To:"
         Height          =   255
         Left            =   240
         TabIndex        =   12
         Top             =   1920
         Width           =   1035
      End
      Begin VB.Label lblFrom 
         BackStyle       =   0  'Transparent
         Caption         =   "From:"
         Height          =   255
         Left            =   240
         TabIndex        =   11
         Top             =   1620
         Width           =   855
      End
   End
   Begin VB.ComboBox cboLookIn 
      Height          =   315
      Left            =   3900
      TabIndex        =   3
      Top             =   1500
      Width           =   2955
   End
   Begin VB.TextBox txtContains 
      Height          =   315
      Left            =   3900
      TabIndex        =   2
      Top             =   900
      Width           =   2955
   End
   Begin VB.TextBox txtFileName 
      Height          =   315
      Left            =   3900
      TabIndex        =   1
      Top             =   360
      Width           =   2955
   End
   Begin vbalExplorerBarLib.vbalExplorerBarCtl barSearch 
      Align           =   3  'Align Left
      Height          =   6990
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   3915
      _ExtentX        =   6906
      _ExtentY        =   12330
      BackColorEnd    =   0
      BackColorStart  =   0
   End
   Begin vbalIml.vbalImageList ilsIcons 
      Left            =   7740
      Top             =   4440
      _ExtentX        =   953
      _ExtentY        =   953
      ColourDepth     =   32
      Size            =   28700
      Images          =   "frmTestSearchBar.frx":0A08
      Version         =   131072
      KeyCount        =   25
      Keys            =   "ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ"
   End
   Begin VB.Label lblSearch 
      Height          =   675
      Left            =   4020
      TabIndex        =   28
      Top             =   180
      Width           =   4755
   End
End
Attribute VB_Name = "frmTestSearchBar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private m_bShowCharacter As Boolean
Private m_bShowBalloonTips As Boolean
Private m_bUseAutoComplete As Boolean
Private m_sCurrentPage As String
Private m_sPreviousPage As String

Private Sub initOtherSearchOptions()
Dim cBar As cExplorerBar
Dim cItem As cExplorerBarItem
Dim sText As String
   
   m_sPreviousPage = m_sCurrentPage
   m_sCurrentPage = "OTHERSEARCH"
   
   With barSearch
      .Bars.Clear
      
      Set cBar = .Bars.Add(, "TITLE", "What do you want to search for?")
      cBar.CanExpand = False
      cBar.TitleForeColor = vbWindowText
      cBar.TitleForeColorOver = vbWindowText
      
      Set cItem = cBar.Items.Add(, "SEARCHPICTURES", "&Pictures, music, or video", 3)
      Set cItem = cBar.Items.Add(, "SEARCHDOCUMENTS", "D&ocuments (word processing, spreadsheet, etc)", 3)
      Set cItem = cBar.Items.Add(, "SEARCHALL", "D&ocuments (word processing, spreadsheet, etc)", 3)
      Set cItem = cBar.Items.Add(, "SEARCHCOMPUTERSPEOPLE", "&Computers or people", 3)
      
      Set cItem = cBar.Items.Add(, "HELP", "&Information in Help and Support Center", 4)
      
      Set cItem = cBar.Items.Add(, "HINT", "You may also want to...", , eItemText)
      Set cItem = cBar.Items.Add(, "SEARCHINTERNET", "&Search the Internet", 1)
      Set cItem = cBar.Items.Add(, "PREFERENCES", "Chan&ge preferences", 2)
      If (m_bShowCharacter) Then
         sText = "T&urn off animated character"
      Else
         sText = "T&urn on animated character"
      End If
      Set cItem = cBar.Items.Add(, "SHOWCHARACTER", sText, 3)
      cItem.ItemData = m_bShowCharacter
      
      Set cItem = cBar.Items.Add(, "SEARCHBUTTONPANEL", , , eItemControlPlaceHolder)
      cmdAction(1).Visible = False
      cmdAction(0).Caption = "&Back"
      cmdAction(0).Visible = True
      cItem.Control = pnlAction
      
   End With
   
End Sub

Private Sub initPreferences()
Dim cBar As cExplorerBar
Dim cItem As cExplorerBarItem
Dim sText As String
   
   m_sPreviousPage = m_sCurrentPage
   m_sCurrentPage = "PREFERENCES"
   
   With barSearch
      .Bars.Clear
      
      Set cBar = .Bars.Add(, "TITLE", "How do you want to use Search Companion?")
      cBar.CanExpand = False
      cBar.TitleForeColor = vbWindowText
      cBar.TitleForeColorOver = vbWindowText
      
      If (m_bShowCharacter) Then
         sText = "Without an animated screen character"
      Else
         sText = "With an animated screen character"
      End If
      Set cItem = cBar.Items.Add(, "SHOWCHARACTER", sText, 3)
      cItem.ItemData = m_bShowCharacter
      
      If (m_bShowCharacter) Then
         Set cItem = cBar.Items.Add(, "CHOOSECHARACTER", "With a different character", 3)
      End If
      
      Set cItem = cBar.Items.Add(, "INDEXINGSERVICE", "Without Indexing Service", 3)
      Set cItem = cBar.Items.Add(, "FILESEARCHBEHAVIOUR", "Change files and folders search behaviour", 3)
      Set cItem = cBar.Items.Add(, "INTERNETSEARCHBEHAVIOUR", "Change Internet search behaviour", 3)
      
      If (m_bShowBalloonTips) Then
         sText = "Don't show balloon tips"
      Else
         sText = "Show ballon tips"
      End If
      Set cItem = cBar.Items.Add(, "SHOWBALLONTIPS", sText, 3)
      cItem.ItemData = m_bShowBalloonTips
      
      If (m_bUseAutoComplete) Then
         sText = "Turn AutoComplete off"
      Else
         sText = "Turn AutoComplete on"
      End If
      Set cItem = cBar.Items.Add(, "USEAUTOCOMPLETE", sText, 3)
      cItem.ItemData = m_bUseAutoComplete
      
      Set cItem = cBar.Items.Add(, "ACTIONBUTTONPANEL", , , eItemControlPlaceHolder)
      cmdAction(1).Visible = False
      cmdAction(0).Caption = "&Back"
      cmdAction(0).Visible = True
      cItem.Control = pnlAction
      
   End With
End Sub

Private Sub initAdvancedSearch()
Dim cBar As cExplorerBar
Dim cItem As cExplorerBarItem
Dim i As Long

   m_sPreviousPage = m_sCurrentPage
   m_sCurrentPage = "ADVANCEDSEARCH"
   
   With barSearch
      .Redraw = False
      
      .Bars.Clear
   
      Set cBar = .Bars.Add(, "TITLE", "Search by any or all of the criteria below.")
      cBar.CanExpand = False
      cBar.TitleForeColor = vbWindowText
      cBar.TitleForeColorOver = vbWindowText
      Set cItem = cBar.Items.Add(, "FILECAPTION", "All or part of the file name:", , eItemText)
      Set cItem = cBar.Items.Add(, "FILECONTROL", , , eItemControlPlaceHolder)
      cItem.Control = txtFileName
      
      Set cItem = cBar.Items.Add(, "CONTENTCAPTION", "A word or phrase in the file:", , eItemText)
      Set cItem = cBar.Items.Add(, "CONTENTCONTROL", , , eItemControlPlaceHolder)
      cItem.Control = txtContains
      
      Set cItem = cBar.Items.Add(, "LOCATIONCAPTION", "Look in:", , eItemText)
      Set cItem = cBar.Items.Add(, "LOCATIONCONTROL", , , eItemControlPlaceHolder)
      cItem.Control = cboLookIn
      
      Set cBar = .Bars.Add(, "MODIFIED", "When was it modified?")
      cBar.State = eBarCollapsed
      Set cItem = cBar.Items.Add(, "MODIFIEDPANEL", , , eItemControlPlaceHolder)
      cItem.Control = pnlModified
      For i = 0 To 4
         optModified(i).BackColor = pnlModified.BackColor
      Next i
      
      Set cBar = .Bars.Add(, "SIZE", "What size is it?")
      cBar.State = eBarCollapsed
      Set cItem = cBar.Items.Add(, "SIZEPANEL", , , eItemControlPlaceHolder)
      cItem.Control = pnlSize
      For i = 0 To 4
         optSize(i).BackColor = pnlModified.BackColor
      Next i
      
      Set cBar = .Bars.Add(, "ADVANCED", "More advanced options")
      cBar.State = eBarCollapsed
      Set cItem = cBar.Items.Add(, "ADVANCEDPANEL", , , eItemControlPlaceHolder)
      cItem.Control = pnlAdvanced
      
      Set cBar = .Bars.Add(, "LINKS")
      cBar.CanExpand = False
      Set cItem = cBar.Items.Add(, "OTHER", "Other search options", 1)
      Set cItem = cBar.Items.Add(, "PREFERENCES", "Change preferences", 2)
      Set cItem = cBar.Items.Add(, "SEARCHBUTTONPANEL", , , eItemControlPlaceHolder)
      cmdAction(1).Visible = False
      cmdAction(0).Caption = "&Search"
      cmdAction(0).Visible = True
      cItem.Control = pnlAction
      
      .Redraw = True
   End With
End Sub

Private Sub initChooseCharacter()
   
   m_sPreviousPage = m_sCurrentPage
   m_sCurrentPage = "CHOOSECHARACTER"
   With barSearch
      .Bars.Clear
   End With
   
End Sub

Private Sub barSearch_ItemClick(itm As vbalExplorerBarLib.cExplorerBarItem)

   Select Case itm.Key

   ' General responses:
   Case "PREFERENCES"
      initPreferences
   Case "OTHER"
      initOtherSearchOptions

   ' Search options:
   Case "SEARCHALL"
      initAdvancedSearch
   Case "SEARCHPICTURES"
      ' not implemented
   Case "SEARCHDOCUMENTS"
   Case "SEARCHCOMPUTERSPEOPLE"



   ' Preference screen responses:
   Case "SHOWCHARACTER"
      ' here we would toggle whether the irritating
      ' animated character is shown or not
      MsgBox "Choose to toggle annoying animated character.  Not implemented in this demonstration.", vbInformation

   Case "CHOOSECHARACTER"
      initChooseCharacter

   Case "INDEXINGSERVICE"
      ' here we would show the indexing service
      ' options
      MsgBox "Choose Indexing Service Options.  Not implemented in this demonstration.", vbInformation

   Case "FILESEARCHBEHAVIOUR"
      ' here we would show the file search behaviour
      ' options
      MsgBox "Choose File Search Behaviour.  Not implemented in this demonstration.", vbInformation

   Case "INTERNETSEARCHBEHAVIOUR"
      ' here we would show the internet search behaviour
      ' options.  use MSN as default search engine?
      ' what a great idea.
      MsgBox "Choose Internet Search Behaviour.  Not implemented in this demonstration.", vbInformation

   Case "SHOWBALLONTIPS"
      ' toogle show balloon tips on/off
      If (itm.ItemData = 0) Then
         m_bShowBalloonTips = True
         itm.Text = "Don't show balloon tips"
      Else
         m_bShowBalloonTips = False
         itm.Text = "Show balloon tips"
      End If
      itm.ItemData = m_bShowBalloonTips
      ' Note that Explorer automatically returns
      ' to the page we accessed preferences from
      ' here.

   Case "USEAUTOCOMPLETE"
      '
      If (itm.ItemData = 0) Then
         m_bUseAutoComplete = True
         itm.Text = "Turn AutoComplete off"
      Else
         m_bUseAutoComplete = False
         itm.Text = "Turn AutoComplete on"
      End If
      itm.ItemData = m_bUseAutoComplete

   End Select

End Sub

Private Sub chkAlign_Click()
   If (chkAlign.Value = vbChecked) Then
      barSearch.Width = 3915
      barSearch.Align = vbAlignLeft
   Else
      barSearch.Align = vbAlignNone
      Form_Resize
   End If
End Sub

Private Sub chkVisible_Click()
   barSearch.Visible = (chkVisible.Value = vbChecked)
End Sub

Private Sub cmdAction_Click(Index As Integer)
   Select Case Index
   Case 0
      Select Case m_sCurrentPage
      Case "OTHERSEARCH", "PREFERENCES"
         ' Go back to prior page:
         Select Case m_sPreviousPage
         Case "ADVANCEDSEARCH"
            initAdvancedSearch
         Case "OTHERSEARCH"
            initOtherSearchOptions
         Case "CHOOSECHARACTER"
            initChooseCharacter
         Case "PREFERENCES"
            initAdvancedSearch
         End Select

      Case "ADVANCEDSEARCH"
         ' start searching:
         lblSearch.Caption = "Searching... Please wait..."

      End Select
   Case 1
   End Select
End Sub

Private Sub Form_Load()
   
   barSearch.Style = eSearchStyle
   barSearch.ImageList = ilsIcons.hIml
   
   initAdvancedSearch
   
End Sub

Private Sub Form_Resize()
   On Error Resume Next
   If (chkAlign.Value = vbUnchecked) Then
      barSearch.Move Me.ScaleHeight \ 6, Me.ScaleWidth \ 6, Me.ScaleWidth - (Me.ScaleWidth \ 3), Me.ScaleHeight - (Me.ScaleHeight \ 3)
   End If
End Sub

Private Sub Form_Terminate()
   If (Forms.Count = 0) Then
      UnloadApp
   End If
End Sub

Private Sub optModified_Click(Index As Integer)
Dim b As Boolean
   b = optModified(4).Value
   cboDateType.Enabled = b
   cboDateType.BackColor = IIf(b, vbWindowBackground, vbButtonFace)
   txtFrom.Enabled = b
   txtFrom.BackColor = IIf(b, vbWindowBackground, vbButtonFace)
   txtTo.Enabled = b
   txtTo.BackColor = IIf(b, vbWindowBackground, vbButtonFace)
End Sub

Private Sub optSize_Click(Index As Integer)
Dim b As Boolean
   b = optSize(4).Value
   cboSize.Enabled = b
   cboSize.BackColor = IIf(b, vbWindowBackground, vbButtonFace)
   txtSize.Enabled = b
   txtSize.BackColor = IIf(b, vbWindowBackground, vbButtonFace)
End Sub
