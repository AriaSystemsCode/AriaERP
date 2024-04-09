VERSION 5.00
Object = "{77EBD0B1-871A-4AD1-951A-26AEFE783111}#2.0#0"; "vbalExpBar6.ocx"
Object = "{396F7AC0-A0DD-11D3-93EC-00C0DFE7442A}#1.0#0"; "vbalIml6.ocx"
Begin VB.Form frmTestExplorerBar 
   Caption         =   "vbAccelerator - Explorer Bar Control Demonstration Application"
   ClientHeight    =   5565
   ClientLeft      =   4725
   ClientTop       =   2025
   ClientWidth     =   7530
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmTestExplorerBar.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   371
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   502
   Begin vbalIml6.vbalImageList ilsTitleIcons 
      Left            =   6840
      Top             =   1440
      _ExtentX        =   953
      _ExtentY        =   953
      IconSizeX       =   32
      IconSizeY       =   32
      ColourDepth     =   32
      Size            =   4412
      Images          =   "frmTestExplorerBar.frx":1272
      Version         =   131072
      KeyCount        =   1
      Keys            =   ""
   End
   Begin VB.PictureBox picRes 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      Height          =   1260
      Left            =   6120
      Picture         =   "frmTestExplorerBar.frx":23CE
      ScaleHeight     =   1260
      ScaleWidth      =   1200
      TabIndex        =   21
      Top             =   3420
      Visible         =   0   'False
      Width           =   1200
   End
   Begin VB.CommandButton cmdMedia 
      Caption         =   "&Media..."
      Height          =   435
      Left            =   5340
      TabIndex        =   9
      Top             =   3660
      Width           =   1515
   End
   Begin VB.CheckBox chkCustomColours 
      Caption         =   "Custom Colo&urs"
      Enabled         =   0   'False
      Height          =   255
      Left            =   3600
      TabIndex        =   8
      Top             =   3720
      Width           =   3195
   End
   Begin VB.CommandButton cmdSearch 
      Caption         =   "&Search..."
      Height          =   435
      Left            =   5340
      TabIndex        =   5
      Top             =   3180
      Width           =   1515
   End
   Begin VB.CheckBox chkUseExplorer 
      Caption         =   "Use E&xplorer Style"
      Height          =   255
      Left            =   3360
      TabIndex        =   4
      Top             =   3420
      Value           =   1  'Checked
      Width           =   3495
   End
   Begin VB.CheckBox chkRedraw 
      Caption         =   "Redra&w"
      Height          =   195
      Left            =   3360
      TabIndex        =   3
      Top             =   3180
      Value           =   1  'Checked
      Width           =   3495
   End
   Begin VB.Frame fraTestItems 
      Caption         =   "Test Ite&ms"
      Height          =   1455
      Left            =   3240
      TabIndex        =   2
      Top             =   1620
      Width           =   3735
      Begin VB.PictureBox picFrameFixer 
         BorderStyle     =   0  'None
         Height          =   1035
         Left            =   120
         ScaleHeight     =   1035
         ScaleWidth      =   3495
         TabIndex        =   10
         Top             =   240
         Width           =   3495
         Begin VB.CommandButton cmdClearItems 
            Caption         =   "Clear..."
            Height          =   315
            Left            =   1260
            TabIndex        =   14
            Top             =   360
            Width           =   1215
         End
         Begin VB.CommandButton cmdInsertItem 
            Caption         =   "Insert..."
            Height          =   315
            Left            =   1260
            TabIndex        =   13
            Top             =   0
            Width           =   1215
         End
         Begin VB.CommandButton cmdRemoveItem 
            Caption         =   "Remove..."
            Height          =   315
            Left            =   0
            TabIndex        =   12
            Top             =   360
            Width           =   1215
         End
         Begin VB.CommandButton cmdAddItem 
            Caption         =   "Add..."
            Height          =   315
            Left            =   0
            TabIndex        =   11
            Top             =   0
            Width           =   1215
         End
      End
   End
   Begin VB.Frame fraEvents 
      Caption         =   "Events"
      Height          =   1455
      Left            =   3240
      TabIndex        =   6
      Top             =   4020
      Width           =   3735
      Begin VB.ListBox lstEvents 
         Height          =   1035
         Left            =   120
         TabIndex        =   7
         TabStop         =   0   'False
         Top             =   240
         Width           =   3495
      End
   End
   Begin vbalIml6.vbalImageList ilsIcons 
      Left            =   6840
      Top             =   780
      _ExtentX        =   953
      _ExtentY        =   953
      ColourDepth     =   32
      Size            =   12628
      Images          =   "frmTestExplorerBar.frx":72D0
      Version         =   131072
      KeyCount        =   11
      Keys            =   "ÿÿÿÿÿÿÿÿÿÿ"
   End
   Begin VB.Frame fraTestBars 
      Caption         =   "&Test Bars"
      Height          =   1455
      Left            =   3240
      TabIndex        =   1
      Top             =   60
      Width           =   3735
      Begin VB.PictureBox picFraFixer 
         BorderStyle     =   0  'None
         Height          =   1095
         Left            =   120
         ScaleHeight     =   1095
         ScaleWidth      =   3555
         TabIndex        =   15
         Top             =   240
         Width           =   3555
         Begin VB.CommandButton cmdAdd 
            Caption         =   "A&dd..."
            Height          =   315
            Left            =   0
            TabIndex        =   20
            Top             =   0
            Width           =   1215
         End
         Begin VB.CommandButton cmdRemove 
            Caption         =   "&Remove..."
            Height          =   315
            Left            =   0
            TabIndex        =   19
            Top             =   360
            Width           =   1215
         End
         Begin VB.CommandButton cmdInsert 
            Caption         =   "I&nsert..."
            Height          =   315
            Left            =   1260
            TabIndex        =   18
            Top             =   0
            Width           =   1215
         End
         Begin VB.CommandButton cmdClear 
            Caption         =   "&Clear"
            Height          =   315
            Left            =   1260
            TabIndex        =   17
            Top             =   360
            Width           =   1215
         End
         Begin VB.CommandButton cmdVisible 
            Caption         =   "Make &Visible..."
            Height          =   315
            Left            =   0
            TabIndex        =   16
            Top             =   720
            Width           =   1215
         End
      End
   End
   Begin vbalExplorerBarLib6.vbalExplorerBarCtl vbalExplorerBarCtl1 
      Align           =   3  'Align Left
      Height          =   5565
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   3180
      _ExtentX        =   5609
      _ExtentY        =   9816
      BackColorEnd    =   -1
      BackColorStart  =   -1
   End
End
Attribute VB_Name = "frmTestExplorerBar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private m_lNewKey As Long

Private Sub logEvent(ByVal sEvent As String)
   lstEvents.AddItem sEvent
   lstEvents.ListIndex = lstEvents.ListCount - 1
End Sub

Private Property Get NewKey() As String
   m_lNewKey = m_lNewKey + 1
   NewKey = "NEWKEY" & m_lNewKey
End Property

Private Function getBarList() As String
Dim i As Long
Dim sKeys As String
   With vbalExplorerBarCtl1
      For i = 1 To .Bars.Count
         If (i > 1) Then sKeys = sKeys & ", "
         sKeys = sKeys & .Bars(i).Key
      Next i
   End With
   getBarList = sKeys
End Function
Private Function getItemList(ByVal sBarKey As String) As String
Dim i As Long
Dim sKeys As String
   With vbalExplorerBarCtl1.Bars(sBarKey)
      For i = 1 To .Items.Count
         If (i > 1) Then sKeys = sKeys & ", "
         sKeys = sKeys & .Items(i).Key
      Next i
   End With
   getItemList = sKeys
End Function

Private Sub chkCustomColours_Click()
Dim i As Long
Dim j As Long
   If (chkCustomColours.Value = vbChecked) Then
      With vbalExplorerBarCtl1
         .Redraw = False
         .BackColorStart = RGB(255, 239, 154)
         .BackColorEnd = RGB(137, 129, 93)
         For i = 1 To .Bars.Count
            With .Bars(i)
               If (.IsSpecial) Then
                  .TitleBackColorLight = RGB(137, 129, 93)
                  .TitleBackColorDark = RGB(89, 84, 61)
                  .TitleForeColor = RGB(255, 255, 230)
                  .TitleForeColorOver = RGB(255, 239, 154)
                  .BackColor = RGB(255, 253, 245)
               Else
                  .TitleBackColorLight = RGB(255, 255, 230)
                  .TitleBackColorDark = RGB(255, 239, 154)
                  .TitleForeColor = RGB(89, 84, 61)
                  .TitleForeColorOver = RGB(137, 129, 93)
                  .BackColor = RGB(255, 249, 225)
               End If
               For j = 1 To .Items.Count
                  With .Items(j)
                     .TextColor = RGB(89, 84, 61)
                     .TextColorOver = RGB(170, 163, 130)
                  End With
               Next j
            End With
         Next i
         .Redraw = True
      End With
   Else
      With vbalExplorerBarCtl1
         .Redraw = False
         .BackColorStart = -1
         .BackColorEnd = -1
         For i = 1 To .Bars.Count
            With .Bars(i)
               .TitleBackColorDark = -1
               .TitleBackColorLight = -1
               .TitleForeColor = -1
               .TitleForeColorOver = -1
               .BackColor = -1
               For j = 1 To .Items.Count
                  With .Items(j)
                     .TextColor = -1
                     .TextColorOver = -1
                  End With
               Next j
            End With
         Next i
         .Redraw = True
      End With
   End If
End Sub

Private Sub chkRedraw_Click()
   vbalExplorerBarCtl1.Redraw = (chkRedraw.Value = vbChecked)
End Sub

Private Sub chkUseExplorer_Click()
   vbalExplorerBarCtl1.UseExplorerStyle = (chkUseExplorer.Value = Checked)
   chkCustomColours.Enabled = (chkUseExplorer.Value = Unchecked)
   If Not (chkCustomColours.Enabled) Then
      chkCustomColours.Value = Unchecked
   End If
End Sub

Private Sub cmdAdd_Click()
Dim sTitle As String
   sTitle = InputBox("Enter new bar title")
   With vbalExplorerBarCtl1
      .Bars.Add , _
         NewKey, _
         sTitle
   End With
End Sub

Private Sub cmdAddItem_Click()
Dim sBar As String
Dim sTitle As String
   With vbalExplorerBarCtl1
      If (.Bars.Count > 0) Then
         sBar = InputBox("Which bar to add an item to? (One of: " & getBarList & ")", , .Bars(1).Key)
         If Len(sBar) > 0 Then
            sTitle = InputBox("Enter title for new item:")
            .Bars(sBar).Items.Add , NewKey, sTitle, Rnd * ilsIcons.ImageCount
         End If
      Else
         MsgBox "Add a bar to the control first.", vbInformation
      End If
   End With
End Sub

Private Sub cmdClear_Click()
   vbalExplorerBarCtl1.Bars.Clear
End Sub

Private Sub cmdClearItems_Click()
Dim sKeys As String
Dim sI As String
   With vbalExplorerBarCtl1
      If (.Bars.Count > 0) Then
         sI = InputBox("Which bar to clear items of? (One of: " & getBarList & ")", , .Bars(1).Key)
         If Len(sI) > 0 Then
            .Bars(sI).Items.Clear
         End If
      Else
         MsgBox "No bars in control.", vbInformation
      End If
   End With
End Sub

Private Sub cmdInsert_Click()
Dim sTitle As String
   sTitle = InputBox("Enter new bar title")
   With vbalExplorerBarCtl1
      If (.Bars.Count > 0) Then
         .Bars.Add _
            1, NewKey, _
            sTitle
      Else
         .Bars.Add , _
            NewKey, _
            sTitle
      End If
   End With
      
End Sub

Private Sub cmdInsertItem_Click()
Dim sBar As String
Dim sTitle As String
   
   With vbalExplorerBarCtl1
      If (.Bars.Count > 0) Then
         sBar = InputBox("Which bar to insert an item to? (One of: " & getBarList & ")", , .Bars(1).Key)
         If Len(sBar) > 0 Then
            sTitle = InputBox("Enter title for new item:")
            If (.Bars(sBar).Items.Count > 0) Then
               .Bars(sBar).Items.Add 1, NewKey, sTitle, Rnd * ilsIcons.ImageCount
            Else
               .Bars(sBar).Items.Add , NewKey, sTitle, Rnd * ilsIcons.ImageCount
            End If
         End If
      Else
         MsgBox "No bars in control.", vbInformation
      End If
   End With

End Sub

Private Sub cmdMedia_Click()
   Dim f As New frmMediaSearch
   f.Show
End Sub

Private Sub cmdRemove_Click()
Dim sBar As String
   With vbalExplorerBarCtl1
      If (.Bars.Count > 0) Then
         sBar = InputBox("Which bar to remove? (One of: " & getBarList & ")", , .Bars(1).Key)
         If Len(sBar) > 0 Then
            .Bars.Remove sBar
         End If
      Else
         MsgBox "No bars in control.", vbInformation
      End If
   End With
End Sub

Private Sub cmdRemoveItem_Click()
Dim sBar As String
Dim sItem As String
   With vbalExplorerBarCtl1
      If (.Bars.Count > 0) Then
         sBar = InputBox("Which bar to remove an item from? (One of: " & getBarList & ")", , .Bars(1).Key)
         If Len(sBar) > 0 Then
            If (.Bars(sBar).Items.Count > 0) Then
               sItem = InputBox("Which item to remove from the bar? (One of: " & getItemList(sBar) & ")", , .Bars(sBar).Items(1).Key)
               If (Len(sItem) > 0) Then
                  .Bars(sBar).Items.Remove sItem
               End If
            Else
               MsgBox "No items in this bar.", vbInformation
            End If
         End If
      Else
         MsgBox "No bars in control.", vbInformation
      End If
   End With
End Sub

Private Sub cmdSearch_Click()
   Dim f As New frmTestSearchBar
   f.Show
End Sub

Private Sub cmdVisible_Click()
Dim sBar As String
   With vbalExplorerBarCtl1
      If (.Bars.Count > 0) Then
         sBar = InputBox("Which bar to ensure visible? (One of: " & getBarList & ")", , .Bars(1).Key)
         If Len(sBar) > 0 Then
            .Bars(sBar).EnsureVisible
         End If
      Else
         MsgBox "No bars in control.", vbInformation
      End If
   End With
End Sub

Private Sub Form_Load()
Dim cBar As cExplorerBar
Dim cItem As cExplorerBarItem

   With vbalExplorerBarCtl1
      .Redraw = False
      '.UseExplorerStyle = False
      
      .ImageList = ilsIcons.hIml
      .BarTitleImageList = ilsTitleIcons.hIml
      
      Set cBar = .Bars.Add(, "SPECIAL", "Picture &Tasks")
      cBar.IsSpecial = True
      cBar.ToolTipText = "These tasks apply to picture files and folders you select."
      cBar.IconIndex = 0
      Set cItem = cBar.Items.Add(, "SLIDESHOW", "View as s&lide show", 0)
      cItem.ToolTipText = "Arranges all the pictures in this folder as a slideshow."
      Set cItem = cBar.Items.Add(, "ORDER", "&Order prints online", 1)
      cItem.ToolTipText = "Starts the Online Print Ordering Wizard, which helps you order prints of your digital pictures."
      Set cItem = cBar.Items.Add(, "PRINT", "&Print pictures", 2)
      cItem.ToolTipText = "Starts the Photo Printing Wizard, which helps you format and print your pictures."
      Set cItem = cBar.Items.Add(, "CDCOPY", "Cop&y all items to CD", 3)
      cItem.ToolTipText = "Copies the selected items to the CD-R folder so that you can burn them on a Compact Disc."
      cBar.WatermarkPicture = picRes.Picture
      
      
      Set cBar = .Bars.Add(, "FILEFOLDER", "File and Folder Tasks")
      cBar.ToolTipText = "These tasks apply to the files and folders you select."
      Set cItem = cBar.Items.Add(, "NEWFOLDER", "Make a new &folder", 4)
      Dim sFnt As New StdFont
      sFnt.Name = "Verdana"
      sFnt.Size = 11
      sFnt.Italic = True
      Set cItem.Font = sFnt
      Set cItem = cBar.Items.Add(, "PUBLISHFOLDER", "Publis&h this folder to the Web", 5)
      Set cItem = cBar.Items.Add(, "SHAREFOLDER", "Sh&are this folder", 6)
      
      Set cBar = .Bars.Add(, "PLACES", "Other Places")
      cBar.ToolTipText = "These links open other folders and take you quickly to useful places"
      Set cItem = cBar.Items.Add(, "PLACE:1", "My Documents", 7)
      Set cItem = cBar.Items.Add(, "PLACE:2", "Shared Pictures", 8)
      Set cItem = cBar.Items.Add(, "PLACE:3", "My Computer", 9)
      Set cItem = cBar.Items.Add(, "PLACE:4", "My Network Places", 10)
      Dim i As Long
      For i = 1 To 30
         Set cItem = cBar.Items.Add(, "PLACE:" & (i + 4), "Place " & i, 10)
      Next i
            
      Set cBar = .Bars.Add(, "DETAILS", "Details")
      cBar.ToolTipText = "This section displays the size, type and other information about the selected item"
      Set cItem = cBar.Items.Add(, "NAME", "Star Wars and Other Space Themes.jpg")
      cItem.ItemType = eItemText
      cItem.Bold = True
      Set cItem = cBar.Items.Add(, "TYPE", "JPEG Image")
      cItem.ItemType = eItemText
      cItem.SpacingAfter = 4
      Set cItem = cBar.Items.Add(, "DIMENSIONS", "Dimensions: 1447 x 1448")
      cItem.ItemType = eItemText
      cItem.SpacingAfter = 4
      Set cItem = cBar.Items.Add(, "SIZE", "Size: 782KB")
      cItem.ItemType = eItemText
      cItem.SpacingAfter = 4
      Set cItem = cBar.Items.Add(, "DATE", "Date Modified: 07 October 2002, 10:48")
      cItem.ItemType = eItemText
      
      .Redraw = True
   End With
End Sub

Private Sub Form_Terminate()
   If (Forms.Count = 0) Then
      UnloadApp
   End If
End Sub

Private Sub vbalExplorerBarCtl1_BarClick(bar As vbalExplorerBarLib6.cExplorerBar)
   logEvent "BarClick: " & bar.Key & " (" & bar.Title & ")"
End Sub

Private Sub vbalExplorerBarCtl1_BarRightClick(bar As vbalExplorerBarLib6.cExplorerBar)
   logEvent "BarRightClick: " & bar.Key & " (" & bar.Title & ")"
End Sub

Private Sub vbalExplorerBarCtl1_GotFocus()
   logEvent "GotFocus"
End Sub

Private Sub vbalExplorerBarCtl1_Highlight(bar As vbalExplorerBarLib6.cExplorerBar, itm As vbalExplorerBarLib6.cExplorerBarItem)
   If Not (bar Is Nothing) Then
      If Not (itm Is Nothing) Then
         logEvent "Highlight Item: " & itm.Key
      Else
         logEvent "Highlight Bar: " & bar.Key
      End If
   End If
End Sub

Private Sub vbalExplorerBarCtl1_ItemClick(itm As vbalExplorerBarLib6.cExplorerBarItem)
   logEvent "ItemClick: " & itm.Key & " (" & itm.Text & ")"
End Sub

Private Sub vbalExplorerBarCtl1_ItemRightClick(itm As vbalExplorerBarLib6.cExplorerBarItem)
   logEvent "ItemRightClick: " & itm.Key & " (" & itm.Text & ")"
End Sub

Private Sub vbalExplorerBarCtl1_LostFocus()
   logEvent "LostFocus"
End Sub


