VERSION 5.00
Object = "{396F7AC0-A0DD-11D3-93EC-00C0DFE7442A}#1.0#0"; "vbalIml6.ocx"
Object = "{E910F8E1-8996-4EE9-90F1-3E7C64FA9829}#1.1#0"; "vbaListView6.ocx"
Begin VB.Form frmTestListView 
   Caption         =   "vbAccelerator ListView Control Tester"
   ClientHeight    =   8055
   ClientLeft      =   4110
   ClientTop       =   3120
   ClientWidth     =   9210
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmTestListView.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   8055
   ScaleWidth      =   9210
   Begin vbalListViewLib6.vbalListViewCtl lvwMain 
      Height          =   4035
      Left            =   60
      TabIndex        =   32
      Top             =   420
      Width           =   5175
      _ExtentX        =   9128
      _ExtentY        =   7117
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      MultiSelect     =   -1  'True
      LabelEdit       =   0   'False
      AutoArrange     =   0   'False
      HeaderButtons   =   0   'False
      HeaderTrackSelect=   0   'False
      HideSelection   =   0   'False
      InfoTips        =   0   'False
   End
   Begin VB.PictureBox picStyle 
      Align           =   1  'Align Top
      BorderStyle     =   0  'None
      Height          =   375
      Left            =   0
      ScaleHeight     =   375
      ScaleWidth      =   9210
      TabIndex        =   23
      Top             =   0
      Width           =   9210
      Begin VB.CheckBox chkGroupView 
         Caption         =   "&Grouped View"
         Height          =   255
         Left            =   2940
         TabIndex        =   28
         Top             =   60
         UseMaskColor    =   -1  'True
         Width           =   2295
      End
      Begin VB.ComboBox cboView 
         Height          =   315
         ItemData        =   "frmTestListView.frx":1272
         Left            =   600
         List            =   "frmTestListView.frx":1274
         Style           =   2  'Dropdown List
         TabIndex        =   24
         Top             =   60
         Width           =   2235
      End
      Begin VB.Label lblInfo 
         Caption         =   "View:"
         Height          =   315
         Index           =   1
         Left            =   60
         TabIndex        =   25
         Top             =   120
         Width           =   435
      End
   End
   Begin VB.PictureBox picStatus 
      Align           =   2  'Align Bottom
      BorderStyle     =   0  'None
      Height          =   315
      Left            =   0
      ScaleHeight     =   315
      ScaleWidth      =   9210
      TabIndex        =   22
      Top             =   7740
      Width           =   9210
      Begin VB.Label lblStatus 
         Caption         =   "vbAccelerator ListView"
         Height          =   255
         Left            =   60
         TabIndex        =   26
         Top             =   0
         Width           =   3195
      End
   End
   Begin VB.PictureBox picOptions 
      Align           =   2  'Align Bottom
      BorderStyle     =   0  'None
      Height          =   2790
      Left            =   0
      ScaleHeight     =   2790
      ScaleWidth      =   9210
      TabIndex        =   5
      Top             =   4950
      Width           =   9210
      Begin VB.CheckBox chkCustomDraw 
         Caption         =   "Custom Draw"
         Height          =   195
         Left            =   2460
         TabIndex        =   33
         Top             =   2520
         Value           =   1  'Checked
         Width           =   2775
      End
      Begin VB.CheckBox chkBorderSelect 
         Caption         =   "&Border Select (Large Icons)"
         Height          =   255
         Left            =   60
         TabIndex        =   30
         Top             =   2040
         Width           =   2295
      End
      Begin VB.CheckBox chkAutoArrange 
         Caption         =   "Auto-Arran&ge"
         Height          =   255
         Left            =   60
         TabIndex        =   29
         Top             =   300
         Value           =   1  'Checked
         Width           =   2295
      End
      Begin VB.CheckBox chkHeaderDragDrop 
         Caption         =   "&Header Drag-Drop (Report)"
         Height          =   255
         Left            =   60
         TabIndex        =   27
         Top             =   1800
         UseMaskColor    =   -1  'True
         Width           =   2295
      End
      Begin VB.CheckBox chkEnabled 
         Caption         =   "&Enabled"
         Height          =   255
         Left            =   60
         TabIndex        =   19
         Top             =   60
         Value           =   1  'Checked
         Width           =   2295
      End
      Begin VB.ComboBox cboBorder 
         Height          =   315
         Left            =   3360
         Style           =   2  'Dropdown List
         TabIndex        =   18
         Top             =   60
         Width           =   2235
      End
      Begin VB.ComboBox cboAppearance 
         Height          =   315
         Left            =   3360
         Style           =   2  'Dropdown List
         TabIndex        =   17
         Top             =   420
         Width           =   2235
      End
      Begin VB.CheckBox chkHideSelection 
         Caption         =   "&Hide Selection"
         Height          =   255
         Left            =   60
         TabIndex        =   16
         Top             =   840
         Width           =   2295
      End
      Begin VB.CheckBox chkMultiSelect 
         Caption         =   "&Multi-Select"
         Height          =   255
         Left            =   60
         TabIndex        =   15
         Top             =   1080
         Value           =   1  'Checked
         Width           =   2295
      End
      Begin VB.CheckBox chkBackground 
         Caption         =   "&Background Picture"
         Height          =   195
         Left            =   2460
         TabIndex        =   14
         Top             =   840
         Width           =   2235
      End
      Begin VB.CheckBox chkInfoTips 
         Caption         =   "&Info Tips"
         Height          =   195
         Left            =   2460
         TabIndex        =   13
         Top             =   1080
         Value           =   1  'Checked
         Width           =   2235
      End
      Begin VB.CheckBox chkLabelEdit 
         Caption         =   "Label Edi&t"
         Height          =   255
         Left            =   60
         TabIndex        =   12
         Top             =   1320
         Width           =   2295
      End
      Begin VB.CheckBox chkGridLines 
         Caption         =   "&Gridlines (Report)"
         Height          =   195
         Left            =   2460
         TabIndex        =   11
         Top             =   1320
         Width           =   2235
      End
      Begin VB.CheckBox chkHeaderButtons 
         Caption         =   "&Header Buttons (Report)"
         Height          =   255
         Left            =   60
         TabIndex        =   10
         Top             =   1560
         Value           =   1  'Checked
         Width           =   2295
      End
      Begin VB.CheckBox chkSubItemImages 
         Caption         =   "&Sub-Item Images (Report)"
         Height          =   195
         Left            =   2460
         TabIndex        =   9
         Top             =   1560
         Width           =   2235
      End
      Begin VB.CheckBox chkCheckBoxes 
         Caption         =   "&Check Boxes"
         Height          =   195
         Left            =   2460
         TabIndex        =   8
         Top             =   1800
         Width           =   2235
      End
      Begin VB.CheckBox chkFlatScrollBars 
         Caption         =   "&Flat Scroll Bars"
         Height          =   195
         Left            =   2460
         TabIndex        =   7
         Top             =   2040
         Width           =   2235
      End
      Begin VB.CheckBox chkFullRowSelect 
         Caption         =   "F&ull Row Select (Report or Tile)"
         Height          =   195
         Left            =   2460
         TabIndex        =   6
         Top             =   2280
         Width           =   2775
      End
      Begin VB.Label lblInfo 
         Caption         =   "BorderStyle:"
         Height          =   315
         Index           =   0
         Left            =   2400
         TabIndex        =   21
         Top             =   120
         Width           =   915
      End
      Begin VB.Label lblInfo 
         Caption         =   "Appearance:"
         Height          =   315
         Index           =   2
         Left            =   2400
         TabIndex        =   20
         Top             =   480
         Width           =   915
      End
   End
   Begin VB.PictureBox picTest 
      Align           =   4  'Align Right
      BorderStyle     =   0  'None
      Height          =   4575
      Left            =   7995
      ScaleHeight     =   4575
      ScaleWidth      =   1215
      TabIndex        =   0
      Top             =   375
      Width           =   1215
      Begin VB.CommandButton cmdWorkAreas 
         Caption         =   "&Work Areas.."
         Height          =   375
         Left            =   0
         TabIndex        =   31
         Top             =   1680
         Width           =   1155
      End
      Begin VB.CommandButton cmdAdd 
         Caption         =   "&Add..."
         Height          =   375
         Left            =   0
         TabIndex        =   4
         Top             =   0
         Width           =   1155
      End
      Begin VB.CommandButton cmdRemove 
         Caption         =   "&Remove..."
         Height          =   375
         Left            =   0
         TabIndex        =   3
         Top             =   360
         Width           =   1155
      End
      Begin VB.CommandButton cmdInfo 
         Caption         =   "&Info..."
         Height          =   375
         Left            =   0
         TabIndex        =   2
         Top             =   720
         Width           =   1155
      End
      Begin VB.CommandButton cmdNew 
         Caption         =   "&New..."
         Height          =   375
         Left            =   0
         TabIndex        =   1
         Top             =   1260
         Width           =   1155
      End
      Begin vbalIml6.vbalImageList ilsIcons32 
         Left            =   300
         Top             =   2640
         _ExtentX        =   953
         _ExtentY        =   953
         IconSizeX       =   32
         IconSizeY       =   32
         ColourDepth     =   24
         Size            =   158832
         Images          =   "frmTestListView.frx":1276
         Version         =   131072
         KeyCount        =   36
         Keys            =   "ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ"
      End
      Begin vbalIml6.vbalImageList ilsIcons16 
         Left            =   300
         Top             =   2040
         _ExtentX        =   953
         _ExtentY        =   953
         ColourDepth     =   24
         Size            =   43624
         Images          =   "frmTestListView.frx":27F06
         Version         =   131072
         KeyCount        =   38
         Keys            =   "ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ"
      End
      Begin vbalIml6.vbalImageList ilsIcons48 
         Left            =   300
         Top             =   3240
         _ExtentX        =   953
         _ExtentY        =   953
         IconSizeX       =   48
         IconSizeY       =   48
         ColourDepth     =   24
         Size            =   502320
         Images          =   "frmTestListView.frx":3298E
         Version         =   131072
         KeyCount        =   52
         Keys            =   "ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ"
      End
   End
End
Attribute VB_Name = "frmTestListView"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cboAppearance_Click()
   lvwMain.Appearance = cboAppearance.ItemData(cboAppearance.ListIndex)
End Sub

Private Sub cboBorder_Click()
   lvwMain.BorderStyle = cboBorder.ItemData(cboBorder.ListIndex)
End Sub

Private Sub cboView_Click()
   If cboView.ListIndex > -1 Then
      lvwMain.View = cboView.ItemData(cboView.ListIndex)
   End If
End Sub

Private Sub chkAutoArrange_Click()
   lvwMain.AutoArrange = (chkAutoArrange.Value = vbChecked)
End Sub

Private Sub chkBackground_Click()
   If chkBackground.Value = Checked Then
      lvwMain.BackColor = -1
      lvwMain.BackgroundPicture = App.Path & "\back.jpg"
   Else
      lvwMain.BackColor = vbWindowBackground
      lvwMain.BackgroundPicture = ""
   End If
End Sub

Private Sub chkBorderSelect_Click()
   lvwMain.ItemBorderSelect = (chkBorderSelect.Value = Checked)
End Sub

Private Sub chkCheckBoxes_Click()
   lvwMain.CheckBoxes = (chkCheckBoxes.Value = Checked)
End Sub

Private Sub chkCustomDraw_Click()
   lvwMain.CustomDraw = (chkCustomDraw.Value = Checked)
End Sub

Private Sub chkEnabled_Click()
   lvwMain.Enabled = (chkEnabled.Value = Checked)
End Sub

Private Sub chkFlatScrollBars_Click()
   lvwMain.FlatScrollBar = (chkFlatScrollBars.Value = Checked)
End Sub

Private Sub chkFullRowSelect_Click()
   lvwMain.FullRowSelect = (chkFullRowSelect.Value = Checked)
End Sub

Private Sub chkGridLines_Click()
   lvwMain.GridLines = (chkGridLines.Value = Checked)
End Sub

Private Sub chkGroupView_Click()
   
   ' very slow unless we do this
   lvwMain.Visible = False
   If (chkGroupView.Value = vbChecked) Then
      Dim i As Long
      
      ' Create three groups and display them on screen:
      lvwMain.ItemGroups.Enabled = True
      
      If (lvwMain.ItemGroups.Count = 0) Then
         ' Create a group and add the first five items to it:
         Dim cG As cItemGroup
         Set cG = lvwMain.ItemGroups.Add(1, "GROUP1", "First Five Items")
         Debug.Print cG.Header
         For i = 1 To 5
            lvwMain.ListItems(i).Group = cG
         Next i
         
         ' Create a group and add the next ten items:
         Set cG = lvwMain.ItemGroups.Add(5, "GROUP2", "Next Ten Items")
         For i = 6 To 15
            lvwMain.ListItems(i).Group = cG
         Next i
         
         ' And the rest:
         Set cG = lvwMain.ItemGroups.Add(15, "GROUP3", "The Remainder")
         For i = 16 To lvwMain.ListItems.Count
            lvwMain.ListItems(i).Group = cG
         Next i
      End If
      
   Else
      ' Hide all the groups:
      lvwMain.ItemGroups.Enabled = False
      
   End If
   lvwMain.Visible = True
   
End Sub

Private Sub chkHeaderButtons_Click()
   lvwMain.HeaderButtons = (chkHeaderButtons.Value = Checked)
End Sub

Private Sub chkHeaderDragDrop_Click()
   lvwMain.HeaderDragDrop = (chkHeaderDragDrop.Value = Checked)
End Sub

Private Sub chkHideSelection_Click()
   lvwMain.HideSelection = (chkHideSelection.Value = Checked)
End Sub

Private Sub chkInfoTips_Click()
   lvwMain.InfoTips = (chkInfoTips.Value = Checked)
End Sub

Private Sub chkLabelEdit_Click()
   lvwMain.LabelEdit = (chkLabelEdit.Value = Checked)
End Sub

Private Sub chkMultiSelect_Click()
   lvwMain.MultiSelect = (chkMultiSelect.Value = Checked)
End Sub

Private Sub chkSubItemImages_Click()
Dim i As Long
   lvwMain.SubItemImages = (chkSubItemImages.Value = Checked)
   If chkSubItemImages.Value = Checked Then
      With lvwMain.ListItems
         For i = 1 To .Count
            With .Item(i).SubItems(1)
               .IconIndex = Rnd * ilsIcons16.ImageCount
               Debug.Print .IconIndex
            End With
         Next i
      End With
   End If
End Sub

Private Sub cmdAdd_Click()
Dim sText As String
Dim sKey As String
On Error GoTo ErrorHandler
   sText = InputBox$("Please enter the caption of the item to add", , "Test Item " & lvwMain.ListItems.Count + 1)
   If sText <> "" Then
      sKey = InputBox$("Please enter the key for the item:", , "C" & lvwMain.ListItems.Count + 1)
      If sKey <> "" Then
         lvwMain.ListItems.Add , sKey, sText
      End If
   End If
   Exit Sub
ErrorHandler:
   MsgBox "Error: " & Err.Description & " [" & Err.Number & "]", vbInformation
   Exit Sub
End Sub

Private Sub cmdInfo_Click()
On Error GoTo ErrorHandler
Dim sInfo As String
   If Not lvwMain.SelectedItem Is Nothing Then
      With lvwMain.SelectedItem
         sInfo = "Text = " & .Text & vbCrLf
         sInfo = sInfo & "BackColor = " & .BackColor & vbCrLf
         sInfo = sInfo & "ForeColor = " & .ForeColor & vbCrLf
         sInfo = sInfo & "Tag = " & .Tag & vbCrLf
         sInfo = sInfo & "ToolTipText = " & .ToolTipText & vbCrLf
         sInfo = sInfo & "Checked = " & .Checked & vbCrLf
         sInfo = sInfo & "Cut = " & .Cut & vbCrLf
         sInfo = sInfo & "Selected = " & .Selected & vbCrLf
         sInfo = sInfo & "Hot = " & .Hot & vbCrLf
         sInfo = sInfo & "Indent = " & .Indent & vbCrLf
         sInfo = sInfo & "ItemData = " & .ItemData & vbCrLf
         sInfo = sInfo & "Key = " & .Key & vbCrLf
         sInfo = sInfo & "Left =" & .Left & vbCrLf
         sInfo = sInfo & "Top = " & .Top & vbCrLf
         MsgBox sInfo, vbInformation
      End With
   Else
      MsgBox "No item is selected.", vbInformation
   End If
   Exit Sub
ErrorHandler:
   MsgBox "Error: " & Err.Description & " [" & Err.Number & "]", vbInformation
   Exit Sub
End Sub

Private Sub cmdNew_Click()
   Dim f As New frmTestListView
   f.Show
   f.Move Me.Left + 32 * Screen.TwipsPerPixelX, Me.Top + 32 * Screen.TwipsPerPixelY
End Sub

Private Sub cmdRemove_Click()
On Error GoTo ErrorHandler
   If Not lvwMain.SelectedItem Is Nothing Then
      lvwMain.ListItems.Remove lvwMain.SelectedItem.Key
   Else
      MsgBox "No item is selected.", vbInformation
   End If
   Exit Sub
ErrorHandler:
   MsgBox "Error: " & Err.Description & " [" & Err.Number & "]", vbInformation
   Exit Sub
End Sub


Private Sub cmdWorkAreas_Click()
   Dim fW As New frmTestWorkAreas
   fW.Show
End Sub

Private Sub Form_Resize()
   On Error Resume Next
   lvwMain.Move _
      lvwMain.Left, _
      lvwMain.Top, _
      Me.ScaleWidth - picTest.Width - Me.ScaleX(4, vbPixels, Me.ScaleMode), _
      Me.ScaleHeight - lvwMain.Top - picOptions.Height - picStatus.Height - Me.ScaleY(4, vbPixels, Me.ScaleMode)
End Sub

Private Sub lvwMain_AfterLabelEdit(Cancel As Boolean, NewString As String, Item As cListItem)
   Debug.Print "After Label Edit: ", NewString, Item.Text
End Sub

Private Sub lvwMain_BeforeLabelEdit(Cancel As Boolean, Item As cListItem)
   Debug.Print "Before Label Edit: ", Item.Text
End Sub

Private Sub lvwMain_Click()
   lblStatus.Caption = "Click"
End Sub

Private Sub lvwMain_ColumnClick(Column As cColumn)
   ' Sort according to the column type:
   Select Case Column.Key
   Case "NAME"
      Column.SortType = eLVSortString
      Column.SortOrder = NewSortOrder(Column.SortOrder)
   Case "DATE"
      Column.SortType = eLVSortDate
      Column.SortOrder = NewSortOrder(Column.SortOrder)
   Case "SIZE"
      Column.SortType = eLVSortNumeric
      Column.SortOrder = NewSortOrder(Column.SortOrder)
   End Select
   lvwMain.ListItems.SortItems
End Sub

Private Function NewSortOrder(ByVal SortOrder As ESortOrderConstants) As ESortTypeConstants
   Select Case SortOrder
   Case eSortOrderNone, eSortOrderDescending
      NewSortOrder = eSortOrderAscending
   Case eSortOrderAscending
      NewSortOrder = eSortOrderDescending
   End Select
End Function

Private Sub lvwMain_DblClick()
   lblStatus.Caption = "Double Click"
End Sub

Private Sub lvwMain_ItemClick(Item As cListItem)
   lblStatus.Caption = "Clicked Item " & Item.Text
End Sub

Private Sub lvwMain_ItemDblClick(Item As cListItem)
   lblStatus.Caption = "Double-Clicked Item " & Item.Text
End Sub

Private Sub lvwMain_KeyDown(KeyCode As Integer, Shift As Integer)
   lblStatus.Caption = "KeyDown " & KeyCode & ",Shift"
End Sub

Private Sub lvwMain_KeyPress(KeyAscii As Integer)
   lblStatus.Caption = "KeyPress " & KeyAscii
End Sub

Private Sub lvwMain_KeyUp(KeyCode As Integer, Shift As Integer)
   lblStatus.Caption = "KeyUp " & KeyCode & ",Shift"
End Sub

Private Sub lvwMain_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
   lblStatus.Caption = "MouseDown " & x & "," & y
End Sub

Private Sub lvwMain_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
   lblStatus.Caption = "MouseMove " & x & "," & y
End Sub

Private Sub lvwMain_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
   lblStatus.Caption = "MouseUp " & x & "," & y
End Sub

Private Sub Form_Load()
Dim i As Long
Dim j As Long
   
   Me.Show
   Me.Refresh
   
   With cboView
      .AddItem "Tiles"
      .ItemData(.NewIndex) = 4
      .AddItem "Large Icons"
      .ItemData(.NewIndex) = 0
      .AddItem "Small Icons"
      .ItemData(.NewIndex) = 2
      .AddItem "List"
      .ItemData(.NewIndex) = 3
      .AddItem "Details"
      .ItemData(.NewIndex) = 1
      
      .ListIndex = 0
   End With
   
   With cboBorder
      .AddItem "None"
      .ItemData(.NewIndex) = 0
      .AddItem "Fixed Single"
      .ItemData(.NewIndex) = 1
      .AddItem "Thin"
      .ItemData(.NewIndex) = 2
      .ListIndex = 1
   End With

   With cboAppearance
      .AddItem "Flat"
      .ItemData(.NewIndex) = 0
      .AddItem "3D"
      .ItemData(.NewIndex) = 1
      .ListIndex = 1
   End With
      
Dim colX As cColumn
Dim itmX As cListItem
         
   With lvwMain
      .Visible = False
      .CustomDraw = True
            
      .AutoArrange = True
      
      ' Set up image lists:
      .ImageList(eLVLargeIcon) = ilsIcons32
      .ImageList(eLVSmallIcon) = ilsIcons16
      .ImageList(eLVTileImages) = ilsIcons48
      .ImageList(eLVHeaderImages) = ilsIcons16
      
      ' Add column headers
      Set colX = .Columns.Add(, "NAME", "Name")
      colX.Tag = "Stores the name of the item"
      colX.IconIndex = 0
      Set colX = .Columns.Add(, "DATE", "Date")
      colX.Tag = "Stores the date of the item"
      colX.IconIndex = 1
      Set colX = .Columns.Add(, "SIZE", "Size")
      colX.Tag = "Stores the size of the item"
      colX.Alignment = eLVColumnAlignRight
            
      For i = 1 To 3
         .Columns(i).ItemData = i * 100
      Next i
      
      With .ListItems
         For i = 1 To 100
            Set itmX = .Add(, "I" & i, "Test Item " & i, Rnd * ilsIcons32.ImageCount, Rnd * ilsIcons16.ImageCount)
            If (i Mod 2) = 0 Then
               itmX.ToolTipText = "This is a test tool tip for item " & i
            End If
            With itmX.SubItems(1)
               .Caption = DateSerial(Year(Now), Rnd * Month(Now) + 1, Rnd * Day(Now) + 1)
               .ShowInTile = ((i Mod 2) = 0)
               '.IconIndex = itmX.IconIndex
            End With
            With itmX.SubItems(2)
               .Caption = CLng(Rnd * 1024 * 1024)
               .ShowInTile = True
            End With
            If (i = 1) Then
               ' test font/colours:
               itmX.BackColor = RGB(98, 176, 255)
               itmX.ForeColor = RGB(240, 248, 255)
               Dim sFnt As New StdFont
               sFnt.Name = "Tahoma"
               sFnt.Size = 10
               sFnt.Bold = True
               itmX.Font = sFnt
            End If
         Next i
      End With
      
      .TileViewItemLines = 3
               
      .Visible = True
   End With
   
   
End Sub

Private Sub lvwMain_OLEStartDrag(Data As DataObject, AllowedEffects As Long)
   AllowedEffects = vbDropEffectMove
End Sub

Private Sub lvwMain_Resize()
   '
   'lvwMain.Arrange eLVAlignLeft
   '
End Sub
