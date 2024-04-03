VERSION 5.00
Object = "{396F7AC0-A0DD-11D3-93EC-00C0DFE7442A}#1.0#0"; "vbalIml6.ocx"
Object = "{E910F8E1-8996-4EE9-90F1-3E7C64FA9829}#1.0#0"; "vbaListView6.ocx"
Begin VB.Form frmTestWorkAreas 
   Caption         =   "ListView Work Areas Tester"
   ClientHeight    =   6540
   ClientLeft      =   3120
   ClientTop       =   1995
   ClientWidth     =   7335
   LinkTopic       =   "Form1"
   ScaleHeight     =   6540
   ScaleWidth      =   7335
   Begin vbalListViewLib6.vbalListViewCtl lvwWorkAreas 
      Height          =   6315
      Left            =   60
      TabIndex        =   0
      Top             =   120
      Width           =   7155
      _ExtentX        =   12621
      _ExtentY        =   11139
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
   Begin vbalIml6.vbalImageList ilsIcons32 
      Left            =   6060
      Top             =   180
      _ExtentX        =   953
      _ExtentY        =   953
      IconSizeX       =   32
      IconSizeY       =   32
      ColourDepth     =   24
      Size            =   158832
      Images          =   "frmTestWorkAreas.frx":0000
      Version         =   131072
      KeyCount        =   36
      Keys            =   "ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ"
   End
End
Attribute VB_Name = "frmTestWorkAreas"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
Dim wx As cWorkArea
Dim itmX As cListItem
Dim i As Long

   With lvwWorkAreas
      .ImageList(eLVLargeIcon) = ilsIcons32
      
      Set wx = .WorkAreas.Add(, "W1", 0, 0, .ScaleWidth \ 2 - 20, .ScaleHeight \ 2 - 20)
      wx.Tag = "Work Area 1"
      Set wx = .WorkAreas.Add(, "W2", .ScaleWidth \ 2, 0, .ScaleWidth \ 2 - 20, .ScaleHeight \ 2 - 20)
      wx.Tag = "Work Area 2"
      Set wx = .WorkAreas.Add(, "W3", 0, .ScaleHeight \ 2, .ScaleWidth, 1280)
      wx.Tag = "Work Area 3"
      
      For i = 1 To .WorkAreas.Count
         With .WorkAreas(i)
            Debug.Print .Left, .Top, .Width, .Height, .Key, .Tag
         End With
      Next i
      
      For i = 1 To 20
         Set itmX = .ListItems.Add(, "I" & i, "Item" & i, ilsIcons32.ImageCount * Rnd)
         If (i > 6) Then
            ' place into bottom area
            itmX.Top = .ScaleHeight
         ElseIf (i > 3) Then
            ' place in right hand area
            itmX.Left = .ScaleWidth \ 2 + 20
            itmX.Top = 0
         End If
      Next i
      
      ' Set up some groups for the work areas;
      With .ItemGroups
       '  .Add 1, "GROUP1", "Top Left Group"
       '  .Add 4, "GROUP2", "Top Right Group"
         
      End With
   End With
End Sub

Private Sub Form_Resize()
On Error Resume Next
   lvwWorkAreas.Move lvwWorkAreas.Left, lvwWorkAreas.Top, Me.ScaleWidth - lvwWorkAreas.Left * 2, Me.ScaleHeight - lvwWorkAreas.Top * 2
End Sub

Private Sub lvwWorkAreas_OLEStartDrag(Data As DataObject, AllowedEffects As Long)
   AllowedEffects = vbDropEffectMove
End Sub
