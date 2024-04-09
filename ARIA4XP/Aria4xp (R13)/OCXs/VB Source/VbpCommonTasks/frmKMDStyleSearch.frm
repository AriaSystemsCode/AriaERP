VERSION 5.00
Object = "{D3D6FDC7-C9A0-4D16-99C2-E7FA5234DE4A}#4.0#0"; "vbalExpBar.ocx"
Begin VB.Form frmMediaSearch 
   BackColor       =   &H80000005&
   Caption         =   "Media Search Sample"
   ClientHeight    =   6765
   ClientLeft      =   5760
   ClientTop       =   2775
   ClientWidth     =   6750
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmKMDStyleSearch.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   6765
   ScaleWidth      =   6750
   Begin VB.PictureBox picCommonOptions 
      BorderStyle     =   0  'None
      Height          =   315
      Left            =   3600
      ScaleHeight     =   315
      ScaleWidth      =   2895
      TabIndex        =   10
      Top             =   2940
      Visible         =   0   'False
      Width           =   2895
      Begin VB.OptionButton optCommon 
         Caption         =   "&All"
         Height          =   195
         Index           =   0
         Left            =   0
         TabIndex        =   11
         Top             =   0
         Width           =   495
      End
   End
   Begin VB.PictureBox picMoreOptions 
      BorderStyle     =   0  'None
      Height          =   2955
      Left            =   3600
      ScaleHeight     =   2955
      ScaleWidth      =   2895
      TabIndex        =   8
      Top             =   3300
      Visible         =   0   'False
      Width           =   2895
      Begin VB.TextBox txtOption 
         Height          =   315
         Index           =   0
         Left            =   240
         TabIndex        =   12
         Top             =   300
         Width           =   2535
      End
      Begin VB.CheckBox chkOption 
         Caption         =   "Title"
         Height          =   255
         Index           =   0
         Left            =   0
         TabIndex        =   9
         Top             =   0
         Width           =   2835
      End
   End
   Begin VB.PictureBox picSearch 
      BorderStyle     =   0  'None
      Height          =   435
      Left            =   3660
      ScaleHeight     =   435
      ScaleWidth      =   2835
      TabIndex        =   5
      Top             =   420
      Width           =   2835
      Begin VB.CommandButton cmdStop 
         Caption         =   "S&top"
         Enabled         =   0   'False
         Height          =   375
         Left            =   1500
         TabIndex        =   7
         Top             =   0
         Width           =   1215
      End
      Begin VB.CommandButton cmdSearch 
         Caption         =   "&Search"
         Height          =   375
         Left            =   60
         TabIndex        =   6
         Top             =   0
         Width           =   1215
      End
   End
   Begin VB.ComboBox cboSearchFor 
      Height          =   315
      ItemData        =   "frmKMDStyleSearch.frx":1272
      Left            =   3660
      List            =   "frmKMDStyleSearch.frx":129A
      TabIndex        =   4
      Top             =   60
      Width           =   2835
   End
   Begin VB.PictureBox picSearchType 
      BorderStyle     =   0  'None
      Height          =   1815
      Left            =   3660
      ScaleHeight     =   1815
      ScaleWidth      =   2835
      TabIndex        =   1
      Top             =   1080
      Width           =   2835
      Begin VB.PictureBox picTypeGroup 
         BorderStyle     =   0  'None
         Height          =   1515
         Left            =   240
         ScaleHeight     =   1515
         ScaleWidth      =   2535
         TabIndex        =   13
         Top             =   300
         Width           =   2535
         Begin VB.OptionButton optSearchType 
            Caption         =   "E&verything"
            Height          =   195
            Index           =   0
            Left            =   0
            TabIndex        =   19
            Top             =   0
            Value           =   -1  'True
            Width           =   1155
         End
         Begin VB.OptionButton optSearchType 
            Caption         =   "&Audio"
            Height          =   195
            Index           =   1
            Left            =   0
            TabIndex        =   18
            Top             =   240
            Width           =   1155
         End
         Begin VB.OptionButton optSearchType 
            Caption         =   "&Images"
            Height          =   195
            Index           =   2
            Left            =   0
            TabIndex        =   17
            Top             =   480
            Width           =   1155
         End
         Begin VB.OptionButton optSearchType 
            Caption         =   "&Documents"
            Height          =   195
            Index           =   3
            Left            =   0
            TabIndex        =   16
            Top             =   720
            Width           =   1155
         End
         Begin VB.OptionButton optSearchType 
            Caption         =   "Soft&ware"
            Height          =   195
            Index           =   4
            Left            =   0
            TabIndex        =   15
            Top             =   960
            Width           =   1155
         End
         Begin VB.OptionButton optSearchType 
            Caption         =   "Pla&ylists"
            Height          =   195
            Index           =   5
            Left            =   0
            TabIndex        =   14
            Top             =   1200
            Width           =   1155
         End
      End
      Begin VB.OptionButton optSearchDomain 
         Caption         =   "&Web Search"
         Height          =   195
         Index           =   1
         Left            =   1380
         TabIndex        =   3
         Top             =   60
         Width           =   1335
      End
      Begin VB.OptionButton optSearchDomain 
         Caption         =   "&P2P Search"
         Height          =   195
         Index           =   0
         Left            =   60
         TabIndex        =   2
         Top             =   60
         Width           =   1155
      End
   End
   Begin vbalExplorerBarLib.vbalExplorerBarCtl barSearch 
      Align           =   3  'Align Left
      Height          =   6765
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   3495
      _ExtentX        =   6165
      _ExtentY        =   11933
      BackColorEnd    =   0
      BackColorStart  =   0
   End
   Begin VB.Label lblSizeTest 
      AutoSize        =   -1  'True
      Caption         =   "Label1"
      Height          =   195
      Left            =   0
      TabIndex        =   20
      Top             =   6420
      Width           =   465
   End
End
Attribute VB_Name = "frmMediaSearch"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private m_sCurrentSearch As String

Private Sub setOptionColours()
   
   ' any control that you add that isn't transparent
   ' (or rectangular and completely occupies the space
   ' with a different colour) needs its back colour
   ' set.
   
   ' If you add a PictureBox, by default its backcolor
   ' *is* set, but any child controls do not have
   ' backcolor set.
   
Dim i As Long
   For i = optSearchDomain.LBound To optSearchDomain.UBound
      optSearchDomain(i).BackColor = barSearch.DefaultPanelColor(True)
   Next i
   
   For i = optSearchType.LBound To optSearchType.UBound
      optSearchType(i).BackColor = barSearch.DefaultPanelColor(True)
   Next i
   
   For i = optCommon.LBound To optCommon.UBound
      optCommon(i).BackColor = barSearch.DefaultPanelColor(True)
   Next i
   
   For i = chkOption.LBound To chkOption.UBound
      chkOption(i).BackColor = barSearch.DefaultPanelColor(False)
   Next i
   
   picTypeGroup.BackColor = picTypeGroup.Container.BackColor
   
End Sub

Private Sub initSpecificSearch(ByVal sSearchType As String)
Dim cBar As cExplorerBar
Dim cItem As cExplorerBarItem
   
   If Not (m_sCurrentSearch = sSearchType) Then
      
      ' in reality we'd probably have some
      ' sort of resource to configure the
      ' options you might have for a search
      ' like this here.  but this is just
      ' a demo.
   
      ' Set up some general stuff:
      With barSearch
         Select Case sSearchType
         Case "DEFAULT"
            .Bars("SEARCH").Title = "Search for all files"
            .Bars("TIPS").Items("TIP").Text = "Did you know that you can install a whole pile of useless crap on your computer?  Try our new Insta-Shite service and you too can have every single last tiny detail of your computer usage recorded and sent to unscrupulous sales people."
            .Bars("TIPS").Items("TIPLINK").Text = "Try Insta-Shite Now!"
         Case "AUDIO"
            .Bars("SEARCH").Title = "Search for audio files"
            setCommonOptions "All", "Title", "Artist"
            setDetailedOptions "Title", "Artist", "Album", "Genre"
            .Bars("TIPS").Items("TIP").Text = "Try searching for something that is really popular, like 'April Lasagne' or 'speedranch^jansky noise' in order to get a result.  If you don't find a result, don't give in, simply devote your entire life to endlessly clicking the 'Find More Sources' menu option until you finally do get something.  Or you expire."
            .Bars("TIPS").Items("TIPLINK").Text = "speedranch^jansky noise"
         Case "IMAGES"
            .Bars("SEARCH").Title = "Search for images"
            setCommonOptions "All", "Title"
            setDetailedOptions "Title", "Artist", "Tags"
            .Bars("TIPS").Items("TIP").Text = "If you're looking for an image of a popular female actress, model or performer, try spelling her name incorrectly in order to get more search results.  Don't forget that it may be the Search 'Filter' that is removing the donkey pix you're really after, so don't hesitate to turn it off."
            .Bars("TIPS").Items("TIPLINK").Text = "donkey legs in tights"
         Case "DOCUMENTS"
            .Bars("SEARCH").Title = "Search for secret documents"
            setCommonOptions "All", "Title", "Author"
            setDetailedOptions "Title", "Author", "Summary"
            .Bars("TIPS").Items("TIP").Text = "everything within takes place after jack died and before my mom and i drowned in a burning ferry in the cool tannin-tinted guaviare river, in east-central colombia, with forty-two locals we hadn't yet met.  it was a clear and eyeblue day, that day, as was the first day of this story, a few years ago in january, on chicago's north side, in the opulent shadow of wrigley and with the wind coming low and searching off the jagged half-frozen lake.  i was inside, very warm, but yet fractious and unenamoured."
            .Bars("TIPS").Items("TIPLINK").Text = "dave eggers"
         Case "SOFTWARE"
            .Bars("SEARCH").Title = "Search for warez"
            setCommonOptions "All", "Title"
            setDetailedOptions "Title", "Author", "Summary", "OS", "Language"
            .Bars("TIPS").Items("TIP").Text = "Have you tried BitTorrent or W.A.S.T.E. yet?  They're kinda cool, and they're open source, with no adverts.  If not, don't forget to download our sponsor's software offering this month, which will do something like searching for products you might be interested in whilst modifying your cursors and allowing you to accelerate your offline downloads. And it will send your usage data to an organisation run by someone who would be prepared to murder his own grandmother if it would secure a 0.002% profit derived from sending spam."
            .Bars("TIPS").Items("TIPLINK").Text = "W.A.S.T.E."
         Case "PLAYLISTS"
            .Bars("SEARCH").Title = "Search for playlists"
            setCommonOptions "All", "Title", "Artist"
            setDetailedOptions "Playlist Title", "Artist", "Song Title", "Album", "Genre", "Author"
            .Bars("TIPS").Items("TIP").Text = "So the only searches you've tried which find anything are either for 'Michael Jackson' or 'April Lasagne'.  Surely now you're appetite must be whetted enough that you _need_ to get a playlist of this sterling material organised the way only a fan could?"
            .Bars("TIPS").Items("TIPLINK").Text = "Worse Than Amazon"
         End Select
      End With
   
   
      
      ' Check the search to go to:
      If sSearchType = "DEFAULT" Then
         
         ' need to remove BACK if there:
         If barSearch.Bars("SEARCH").Items.Exists("BACK") Then
            barSearch.Bars("SEARCH").Items.Remove "BACK"
         End If
         
         ' need to set the common search options control off:
         Set barSearch.Bars("SEARCH").Items("SEARCHFORCOMMON").Control = Nothing
            
         ' search type should be on:
         Set barSearch.Bars("SEARCH").Items("SEARCHTYPE").Control = picSearchType
         
         ' need to ensure the "MOREOPTIONS" bar isn't there:
         If barSearch.Bars.Exists("MOREOPTIONS") Then
            barSearch.Bars.Remove "MOREOPTIONS"
         End If
         
      Else
         ' Add BACK option:
         If Not (barSearch.Bars("SEARCH").Items.Exists("BACK")) Then
            barSearch.Bars("SEARCH").Items.Add , "BACK", "<< Back", , eItemLink
         End If
         
         ' set the common search options:
         Set barSearch.Bars("SEARCH").Items("SEARCHFORCOMMON").Control = picCommonOptions
      
         ' search type should be off:
         Set barSearch.Bars("SEARCH").Items("SEARCHTYPE").Control = Nothing
      
         ' Add "MOREOPTIONS" bar if not there;
         If Not (barSearch.Bars.Exists("MOREOPTIONS")) Then
            Set cBar = barSearch.Bars.Add("TIPS", "MOREOPTIONS", "More Search Options")
            Set cItem = cBar.Items.Add(, "MOREOPTIONSCONTROL", , , eItemControlPlaceHolder)
            cItem.Control = picMoreOptions
         End If
      
      End If
         
      m_sCurrentSearch = sSearchType

   End If
   
   setOptionColours
   
End Sub

Private Sub setCommonOptions(ParamArray optionNames() As Variant)
Dim i As Long
   For i = 1 To optCommon.UBound
      Unload optCommon(i)
   Next i
   
   optCommon(0).Caption = optionNames(0)
   For i = 1 To UBound(optionNames)
      Load optCommon(i)
      optCommon(i).Caption = optionNames(i)
      optCommon(i).Visible = True
      lblSizeTest.Caption = optionNames(i)
      optCommon(i).Move optCommon(i - 1).Left + 60 + optCommon(i - 1).Width, optCommon(i - 1).Top, lblSizeTest.Width + 360
   Next i
   
End Sub
Private Sub setDetailedOptions(ParamArray optionNames() As Variant)
Dim i As Long
   
   For i = 1 To chkOption.UBound
      Unload chkOption(i)
   Next i
   For i = 1 To txtOption.UBound
      Unload txtOption(i)
   Next i
   chkOption(0).Value = vbUnchecked
   
   chkOption(0).Caption = optionNames(0)
   txtOption(0).Visible = False
   For i = 1 To UBound(optionNames)
      Load chkOption(i)
      chkOption(i).Caption = optionNames(i)
      Load txtOption(i)
      chkOption(i).Move chkOption(i - 1).Left, chkOption(i - 1).Top + chkOption(i - 1).Height + 60, chkOption(i - 1).Width, chkOption(i - 1).Height
      chkOption(i).Visible = True
   Next i
   picMoreOptions.Height = chkOption(UBound(optionNames)).Top + chkOption(UBound(optionNames)).Height + 60
   
End Sub


Private Sub initControl()
Dim cBar As cExplorerBar
Dim cItem As cExplorerBarItem

   With barSearch
      
      .BackColorStart = vbWindowBackground
      .BackColorEnd = vbWindowBackground
      .UseExplorerStyle = False
      
      Set cBar = .Bars.Add(, "SEARCH", "Search for all files")
      cBar.IsSpecial = True
      
      Set cItem = cBar.Items.Add(, "SEARCHFORCAPTION", "Search for:", , eItemText)
      cItem.SpacingAfter = 4
                        
      Set cItem = cBar.Items.Add(, "SEARCHFOR", , , eItemControlPlaceHolder)
      cItem.SpacingAfter = 4
      cItem.Control = cboSearchFor
            
      Set cItem = cBar.Items.Add(, "SEARCHFORCOMMON", , , eItemControlPlaceHolder)
      ' no control initially
            
      Set cItem = cBar.Items.Add(, "SEARCHCOMMAND", , , eItemControlPlaceHolder)
      cItem.Control = picSearch
      
      Set cItem = cBar.Items.Add(, "SEARCHTYPE", , , eItemControlPlaceHolder)
      cItem.Control = picSearchType
      
      Set cItem = cBar.Items.Add(, "SEARCHFILTER", "Search &Filter")
      
      Set cBar = .Bars.Add(, "TIPS", "Tip")
      Set cItem = cBar.Items.Add(, "TIP", "Did you know that you can install a whole pile of useless crap on your computer?  Try our new Insta-Shite service and you too can have every single last tiny detail of your computer usage recorded and sent to unscrupulous sales people.", , eItemText)
      Set cItem = cBar.Items.Add(, "TIPLINK", "Try Insta-Shite Now!", , eItemLink)
            
   End With

   m_sCurrentSearch = "DEFAULT"
   
   setOptionColours
   
End Sub

Private Sub barSearch_ItemClick(itm As vbalExplorerBarLib.cExplorerBarItem)
   '
   Select Case itm.Key
   Case "TIPLINK"
      MsgBox "Here we would ShellEx to go to the website offer, or load it into a WebBrowser control.", vbInformation
   Case "SEARCHFILTER"
      MsgBox "Show the search filter box here.", vbInformation
   Case "BACK"
      optSearchType(0).Value = True
   End Select
   '
End Sub

Private Sub barSearch_SettingChange()
   '
   setOptionColours
   '
End Sub

Private Sub chkOption_Click(Index As Integer)
Dim i As Long
   If (chkOption(Index).Value = vbChecked) Then
      ' Show the associated text box; move everything below down by the text box height:
      txtOption(Index).Visible = True
      txtOption(Index).Top = chkOption(Index).Top + chkOption(Index).Height
      For i = Index + 1 To chkOption.UBound
         chkOption(i).Top = chkOption(i).Top + txtOption(Index).Height
         txtOption(i).Top = chkOption(i).Top + chkOption(i).Height
      Next i
      ' ensure any common option for this is disabled:
      For i = optCommon.LBound To optCommon.UBound
         If (optCommon(i).Caption = chkOption(Index).Caption) Then
            optCommon(i).Enabled = False
         End If
      Next i
   Else
      ' Hide the associated text box; move everything below up by the text box height:
      txtOption(Index).Visible = False
      For i = Index + 1 To chkOption.UBound
         chkOption(i).Top = chkOption(i).Top - txtOption(Index).Height
         txtOption(i).Top = chkOption(i).Top + chkOption(i).Height
      Next i
      ' ensure any common option for this is re-enabled:
      For i = optCommon.LBound To optCommon.UBound
         If (optCommon(i).Caption = chkOption(Index).Caption) Then
            optCommon(i).Enabled = True
         End If
      Next i
   End If
   If (chkOption(chkOption.UBound).Value = vbChecked) Then
      picMoreOptions.Height = txtOption(txtOption.UBound).Top + txtOption(txtOption.UBound).Height + 60
   Else
      picMoreOptions.Height = chkOption(chkOption.UBound).Top + chkOption(chkOption.UBound).Height + 60
   End If
   ' tell the control the bar size has changed:
   If (barSearch.Bars.Exists("MOREOPTIONS")) Then
      barSearch.Bars("MOREOPTIONS").Items("MOREOPTIONSCONTROL").Control = picMoreOptions
   End If
End Sub

Private Sub Form_Load()

   ' Set the initial default control
   ' state:
   initControl
   
   ' In reality here you'd save the last chosen setting
   ' and restore it using initSpecificSearch if
   ' relevant
   
End Sub

Private Sub optMediaCommon_Click(Index As Integer)

End Sub

Private Sub Form_Terminate()
   If (Forms.Count = 0) Then
      UnloadApp
   End If
End Sub

Private Sub optSearchType_Click(Index As Integer)
   Select Case Index
   Case 0
      initSpecificSearch "DEFAULT"
   Case 1
      initSpecificSearch "AUDIO"
   Case 2
      initSpecificSearch "IMAGES"
   Case 3
      initSpecificSearch "DOCUMENTS"
   Case 4
      initSpecificSearch "SOFTWARE"
   Case 5
      initSpecificSearch "PLAYLISTS"
   End Select
End Sub

Private Sub picMoreOptions_Resize()
Dim i As Long
   For i = chkOption.LBound To chkOption.UBound
      chkOption(i).Width = picMoreOptions.ScaleWidth - chkOption(i).Left
      txtOption(i).Width = picMoreOptions.ScaleWidth - txtOption(i).Left
   Next i
End Sub

Private Sub picSearch_Resize()
   cmdSearch.Move 0, 0
   cmdStop.Move picSearch.Width - cmdStop.Width, 0
End Sub
