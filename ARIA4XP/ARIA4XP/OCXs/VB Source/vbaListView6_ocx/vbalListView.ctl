VERSION 5.00
Begin VB.UserControl vbalListViewCtl 
   Alignable       =   -1  'True
   BackColor       =   &H80000005&
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   ControlContainer=   -1  'True
   ScaleHeight     =   3600
   ScaleWidth      =   4800
   ToolboxBitmap   =   "vbalListView.ctx":0000
   Begin VB.Label lblName 
      BackStyle       =   0  'Transparent
      Height          =   255
      Left            =   60
      TabIndex        =   0
      Top             =   60
      Width           =   4575
   End
End
Attribute VB_Name = "vbalListViewCtl"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

' =========================================================================
' vbalListViewCtl
'
' Implements a ListView using the API.
'
' Thanks in no particular order for getting this to work:
'
' - Dan Litwin for his TreeView demonstrating a
'   whole pile of nice tricks and techniques, particularly
'   a working and workable CustomDraw implementation
' - Mike Gainer for demonstrating the IOLEInPlaceActiveObject
'   code
' - Matt Currland/Bill Storage for writing the OLEGuids TypeLib
'   and publishing the info about it to the VB world
' - VBNet for the ListView tricks section
' - Jeffery. M Richter for Spy++
' - Charlie Kindel, Michael Nelson, Michael Antonio for OLEView
' - Brad Martinez for the fantastic IShellFolderEx_TLB TypeLib
' - Bruce McKinney for Hardcore Visual Basic, CopyMemory,
'   ObjectFromPtr, Subclassing and Timer Assistant (even if
'   it was broken...)
' - Brokenwood - Cricket Pitch Shiraz
' - City Rockers - Futurism 1 & 2 compilations
' - 2 Many DJs - As Heard on Radio Soulwax Pt II
' - KFC
' - Marlboro Lights
' - Lucy C xxx
'
' No thanks:
'
' - Windows Common Controls MSDN documentation - seriously blows goats.
' - MSCOMCTL.OCX - yes I know you had the source code, v. clever,
'   but did you really have to?
' - Western Digital.  I want a want an MTBF somewhere in the high
'   trillions of years from now on please.
'
'
' SteveMac, 2003, vbAccelerator.com, 3:01am.
'
' =========================================================================


Public Enum EAppearanceConstants
   eLVFlat
   eLV3D
End Enum

Public Enum EBorderStyleConstants
   eLVNone
   eLVFixedSingle
   eLVThin
End Enum

Public Enum EArrangeStyleConstants
   eLVArrangeDefault = LVA_DEFAULT
   eLVAlignLeft = LVA_ALIGNLEFT
   eLVAlignTOp = LVA_ALIGNTOP
   eLVSnapToGrid = LVA_SNAPTOGRID
End Enum

Public Enum EImageListTypeConstants
   eLVLargeIcon = LVSIL_NORMAL
   eLVSmallIcon = LVSIL_SMALL
   eLVStateImages = LVSIL_STATE
   eLVHeaderImages = &H8000&  ' Not part of ComCtl32.DLL
   eLVTileImages = &H8001&    ' Not part of ComCtl32.DLL
End Enum

Public Enum EViewStyleConstants
   eViewIcon = LVS_ICON
   eViewSmallIcon = LVS_SMALLICON
   eViewList = LVS_LIST
   eViewDetails = LVS_REPORT
   eViewTile = LV_VIEW_TILE
End Enum

Public Enum EColumnHeaderAlignConstants
   eLVColumnAlignLeft
   eLVColumnAlignCenter
   eLVColumnAlignRight
End Enum

Public Enum ESortOrderConstants
   eSortOrderNone
   eSortOrderAscending
   eSortOrderDescending
End Enum

Public Enum ESortTypeConstants
   eLVSortString
   eLVSortStringNoCase
   eLVSortNumeric
   eLVSortDate
   eLVSortIcon
   eLVSortChecked
   eLVSortIndent
   eLVSortSelected
   eLVSortTag
   eLVSortItemData
End Enum

Public Enum EItemGroupHeaderAlignConstants
   eLVGroupHeaderAlignLeft = LVGA_FOOTER_LEFT
   eLVGroupHeaderAlignCentre = LVGA_FOOTER_CENTER
   eLVGroupHeaderAlignRight = LVGA_FOOTER_RIGHT
End Enum

Public Enum EItemGroupStateConstants
   eLVGroupCollapsed = LVGS_COLLAPSED
   eLVGroupHidden = LVGS_HIDDEN
   eLVGroupNormal = LVGS_NORMAL
End Enum

Public Enum EItemGroupTypeConstants
   eLVGroupHeader = LVGF_HEADER
   eLVGroupFooter = LVGF_FOOTER
End Enum

Public Event AfterLabelEdit(Cancel As Boolean, NewString As String, Item As cListItem)
Attribute AfterLabelEdit.VB_Description = "Raised after a label edit action has been completed."
Public Event BeforeLabelEdit(Cancel As Boolean, Item As cListItem)
Attribute BeforeLabelEdit.VB_Description = "Raised before a label edit action is about to occur."
Public Event Click()
Attribute Click.VB_Description = "Raised when the control is clicked."
Public Event ColumnClick(Column As cColumn)
Attribute ColumnClick.VB_Description = "Raised when a column is clicked in Report mode."
Public Event DblClick()
Attribute DblClick.VB_Description = "Raised when the control is double clicked."
Public Event ItemClick(Item As cListItem)
Attribute ItemClick.VB_Description = "Raised when an item is clicked."
Public Event ItemDblClick(Item As cListItem)
Attribute ItemDblClick.VB_Description = "Raised when an item is double clicked."
Public Event KeyDown(KeyCode As Integer, Shift As Integer)
Attribute KeyDown.VB_Description = "KeyDown event"
Public Event KeyPress(KeyAscii As Integer)
Attribute KeyPress.VB_Description = "KeyPress event"
Public Event KeyUp(KeyCode As Integer, Shift As Integer)
Attribute KeyUp.VB_Description = "KeyUp event"
Public Event MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Attribute MouseDown.VB_Description = "MouseDown event"
Public Event MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
Attribute MouseMove.VB_Description = "MouseMove event"
Public Event MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
Attribute MouseUp.VB_Description = "MouseUp event"
Public Event OLECompleteDrag(Effect As Long)
Attribute OLECompleteDrag.VB_Description = "OLECompleteDrag event."
Public Event OLEDragDrop(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, X As Single, Y As Single)
Attribute OLEDragDrop.VB_Description = "OLEDragDrop event."
Public Event OLEDragOver(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, X As Single, Y As Single, state As Integer)
Attribute OLEDragOver.VB_Description = "OLEDragOver event."
Public Event OLEGiveFeedback(Effect As Long, DefaultCursors As Boolean)
Attribute OLEGiveFeedback.VB_Description = "OLEGiveFeedback event."
Public Event OLESetData(Data As DataObject, DataFormat As Integer)
Attribute OLESetData.VB_Description = "OLESetData event."
Public Event OLEStartDrag(Data As DataObject, AllowedEffects As Long)
Attribute OLEStartDrag.VB_Description = "Starts a drag drop operation."
Public Event Resize()
Attribute Resize.VB_Description = "Raised when the control is resized."

' The implementation:

Implements ISubclass

Private m_hWndParent As Long
Private m_hWnd As Long
Private m_hMod As Long
Private m_lMajor As Long
Private m_lMinor As Long

Private m_eAppearance As EAppearanceConstants
Private m_eBorder As EBorderStyleConstants
Private m_bCustomDraw As Boolean
Private m_bEnabled As Boolean

' Styles:
Private m_bEditLabels As Boolean
Private m_bSingleSelect As Boolean
Private m_bHideSelection As Boolean
Private m_bAutoArrange As Boolean
Private m_bNoScrollBar As Boolean
Private m_bNoColumnHeaders As Boolean
Private m_bHeaderButtons As Boolean
Private m_bHeaderTrackSelect As Boolean
Private m_bGridLines As Boolean
Private m_bDoubleBuffer As Boolean
Private m_bFlatScrollBar As Boolean
Private m_bSubItemImages As Boolean
Private m_bCheckBoxes As Boolean
Private m_bInfoTips As Boolean
Private m_bLabelTips As Boolean
Private m_bTrackSelect As Boolean
Private m_bHeaderDragDrop As Boolean
Private m_bFullRowSelect As Boolean
Private m_bOneClickActivate As Boolean
Private m_eStyle As EViewStyleConstants
Private m_bItemBorderSelect As Boolean

' Groups:
Private m_bGroupEnabled As Boolean
Private m_colGroups As Collection

' Colours/background/appearance
Private m_oBackColor As OLE_COLOR
Private m_oForeColor As OLE_COLOR
Private m_sPicture As String
Private m_bTilePicture As Boolean
Private m_lPictureXOffset As Long
Private m_lPictureYOffset As Long
Private m_lIconSpaceX As Long
Private m_lIconSpaceY As Long

' Over-riding VB UserControl's default IOLEInPlaceActivate:
Private m_IPAOHookStruct As IPAOHookStruct
Private m_bRunTime As Boolean

' Image Lists:
Private m_hIml(0 To 4) As Long
Private m_lIconSizeX(0 To 4) As Long
Private m_lIconSizeY(0 To 4) As Long

' LVItem for generic work
Private m_tLV As LVITEM
' Mousedown pos:
Private m_tMouseDownPos As POINTAPI
Private m_iBtnDown As Integer
' Drag item:
Private m_eOLEDragMode As OLEDragConstants
Private m_lDragItem As Long
Private m_hImlDrag As Long
Private m_cDrag As New pcImageListDrag
' Edit mode
Private m_bInEdit As Boolean

' Keys:
Private m_colKeys As Collection

' Header:
Private Type tColHeaderInfo
   lId As Long
   sKey As String
   sText As String
   sTag As String
   lItemData As Long
   vIcon As Variant
   eSortOrder As ESortOrderConstants
   ESortType As ESortTypeConstants
End Type
Private m_tColInfo() As tColHeaderInfo

' Work areas;
Private Type tWorkArea
   lId As Long
   lItemData As Long
   sKey As String
   sTag As String
   tR As RECT
End Type
Private m_tWorkArea() As tWorkArea
Private m_iWorkAreaCount As Long

Public Event ItemSelected(ByVal strKey As String)

Public Sub RaiseItemSelected(ByVal strKey As String)
  RaiseEvent ItemSelected(strKey)
End Sub

Public Property Get TileViewItemLines() As Long
Attribute TileViewItemLines.VB_Description = "Gets/sets the number of lines to display next to the item in Tile View mode."
Dim tLVI As LVTILEVIEWINFO
   tLVI.cbSize = Len(tLVI)
   tLVI.dwMask = LVTVIM_COLUMNS
   SendMessage m_hWnd, LVM_GETTILEVIEWINFO, 0, tLVI
   TileViewItemLines = tLVI.cLines
End Property
Public Property Let TileViewItemLines(ByVal lLines As Long)
Dim tLVI As LVTILEVIEWINFO
   tLVI.cbSize = Len(tLVI)
   tLVI.dwMask = LVTVIM_COLUMNS
   SendMessage m_hWnd, LVM_GETTILEVIEWINFO, 0, tLVI
   tLVI.cLines = lLines
   SendMessage m_hWnd, LVM_SETTILEVIEWINFO, 0, tLVI

End Property
Friend Property Get fWorkAreaIndexForId(ByVal lId As Long) As Long
Dim i As Long
   For i = 1 To m_iWorkAreaCount
      If (m_tWorkArea(i).lId = lId) Then
         fWorkAreaIndexForId = i
         Exit Property
      End If
   Next i
End Property
Friend Property Get fWorkAreaIdForIndex(ByVal lIndex As Long) As Long
   fWorkAreaIdForIndex = m_tWorkArea(lIndex).lId
End Property
Friend Property Get fWorkAreaCount() As Long
   fWorkAreaCount = m_iWorkAreaCount
End Property
Friend Sub fWorkAreasClear()
   If (m_iWorkAreaCount > 0) Then
      m_iWorkAreaCount = 0
      SendMessage m_hWnd, LVM_SETWORKAREAS, 0, ByVal 0&
   End If
End Sub
Friend Property Get fWorkAreaItemData(ByVal lIndex As Long) As Long
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      fWorkAreaItemData = m_tWorkArea(lIndex).lItemData
   End If
End Property
Friend Property Let fWorkAreaItemData(ByVal lIndex As Long, ByVal lItemData As Long)
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      m_tWorkArea(lIndex).lItemData = lItemData
   End If
End Property
Friend Property Get fWorkAreaTag(ByVal lIndex As Long) As String
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      fWorkAreaTag = m_tWorkArea(lIndex).sTag
   End If
End Property
Friend Property Let fWorkAreaTag(ByVal lIndex As Long, ByVal sTag As String)
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      m_tWorkArea(lIndex).sTag = sTag
   End If
End Property
Friend Property Get fWorkAreaKey(ByVal lIndex As Long) As String
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      fWorkAreaKey = m_tWorkArea(lIndex).sKey
   End If
End Property
Friend Property Let fWorkAreaKey(ByVal lIndex As Long, ByVal sKey As String)
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      m_tWorkArea(lIndex).sKey = sKey
   End If
End Property
Friend Property Get fWorkAreaLeft(ByVal lIndex As Long) As Long
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      fWorkAreaLeft = m_tWorkArea(lIndex).tR.left
   End If
End Property
Friend Property Let fWorkAreaLeft(ByVal lIndex As Long, ByVal lLeft As Long)
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      m_tWorkArea(lIndex).tR.left = lLeft
      pSetWorkAreas
   End If
End Property
Friend Property Get fWorkAreaTop(ByVal lIndex As Long) As Long
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      fWorkAreaTop = m_tWorkArea(lIndex).tR.top
   End If
End Property
Friend Property Let fWorkAreaTop(ByVal lIndex As Long, ByVal lTop As Long)
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      m_tWorkArea(lIndex).tR.top = lTop
      pSetWorkAreas
   End If
End Property
Friend Property Get fWorkAreaRight(ByVal lIndex As Long) As Long
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      fWorkAreaRight = m_tWorkArea(lIndex).tR.right
   End If
End Property
Friend Property Let fWorkAreaRight(ByVal lIndex As Long, ByVal lRight As Long)
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      m_tWorkArea(lIndex).tR.right = lRight
      pSetWorkAreas
   End If
End Property
Friend Property Get fWorkAreaBottom(ByVal lIndex As Long) As Long
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      fWorkAreaBottom = m_tWorkArea(lIndex).tR.bottom
   End If
End Property
Friend Property Let fWorkAreaBottom(ByVal lIndex As Long, ByVal lBottom As Long)
   If (lIndex > 0) And (lIndex <= m_iWorkAreaCount) Then
      m_tWorkArea(lIndex).tR.bottom = lBottom
      pSetWorkAreas
   End If
End Property
Friend Sub fRemoveWorkArea(ByVal lIndex As Long)
Dim i As Long
   If (m_iWorkAreaCount <= 1) Then
      m_iWorkAreaCount = 0
      Erase m_tWorkArea
      pSetWorkAreas
   Else
      For i = lIndex To m_iWorkAreaCount - 1
         LSet m_tWorkArea(i) = m_tWorkArea(i + 1)
      Next i
      m_iWorkAreaCount = m_iWorkAreaCount - 1
      pSetWorkAreas
   End If
End Sub
Friend Function fAddWorkArea( _
      ByVal lIndex As Long, _
      ByVal sKey As String, _
      Optional left As Variant, _
      Optional top As Variant, _
      Optional Width As Variant, _
      Optional Height As Variant _
   ) As Long
Dim lId As Long
Dim pos As Long
Dim inPos As Single
   lId = NextItemID()
   If Len(sKey) = 0 Then
      sKey = "W" & lId
   End If
   m_iWorkAreaCount = m_iWorkAreaCount + 1
   ReDim Preserve m_tWorkArea(1 To m_iWorkAreaCount) As tWorkArea
   If (lIndex > 0) Then
      Dim i As Long
      For i = m_iWorkAreaCount - 1 To lIndex Step -1
         LSet m_tWorkArea(i + 1) = m_tWorkArea(i)
      Next i
   Else
      lIndex = m_iWorkAreaCount
   End If
   With m_tWorkArea(lIndex)
      .lId = lId
      .sKey = sKey
      With .tR
         If Not IsMissing(left) Then
            inPos = left
            fUnScale inPos, 0, pos, 0
            .left = pos
         End If
         If Not IsMissing(Width) Then
            inPos = Width
            fUnScale inPos, 0, pos, 0
            .right = pos + .left
         End If
         If Not IsMissing(top) Then
            inPos = top
            fUnScale 0, inPos, 0, pos
            .top = pos
         End If
         If Not IsMissing(Height) Then
            inPos = Height
            fUnScale 0, inPos, 0, pos
            .bottom = pos + .top
         End If
      End With
   End With
   pSetWorkAreas

   fAddWorkArea = lId

End Function
Private Sub pSetWorkAreas()
   If (m_iWorkAreaCount > 0) Then
      ReDim tR(0 To m_iWorkAreaCount - 1) As RECT
      Dim i As Long
      For i = 1 To m_iWorkAreaCount
         LSet tR(i - 1) = m_tWorkArea(i).tR
      Next i
      SendMessage m_hWnd, LVM_SETWORKAREAS, m_iWorkAreaCount, tR(0)
   Else
      SendMessage m_hWnd, LVM_SETWORKAREAS, 0, ByVal 0&
   End If
End Sub
Friend Function fScale(xPixels As Long, yPixels As Long, X As Single, Y As Single)
   X = ScaleX(xPixels, vbPixels, UserControl.ScaleMode)
   Y = ScaleY(yPixels, vbPixels, UserControl.ScaleMode)
End Function
Friend Function fUnScale(X As Single, Y As Single, xPixels As Long, yPixels As Long)
   xPixels = ScaleX(X, UserControl.ScaleMode, vbPixels)
   yPixels = ScaleY(Y, UserControl.ScaleMode, vbPixels)
End Function
Friend Property Get fHeaderhWnd() As Long
   fHeaderhWnd = SendMessageLong(m_hWnd, LVM_GETHEADER, 0, 0)
End Property
Friend Function fIsDuplicateColumnKey(ByVal sKey As String) As Boolean
Dim i As Long
   For i = 1 To fColumnCount
      If m_tColInfo(i).sKey = sKey Then
         fIsDuplicateColumnKey = True
         Exit Function
      End If
   Next i
End Function
Friend Function fAddColumn( _
      ByVal lId As Long, _
      ByVal sKey As String, _
      ByVal iColBefore As Long, _
      ByVal sText As String, _
      ByVal vIcon As Variant, _
      ByVal iImage As Long, _
      ByVal lWidth As Single _
   ) As Long
Dim tLVH As LVCOLUMN
Dim xPixels As Long, yPixels As Long
Dim lOrigColCount As Long
Dim lIndex As Long
Dim i As Long
   
   lOrigColCount = fColumnCount()
   With tLVH
      '.fmt =
      .mask = LVCF_FMT Or LVCF_WIDTH
      If iImage > -1 Then
         .fmt = .fmt Or LVCFMT_IMAGE 'Or LVCFMT_BITMAP_ON_RIGHT
         .mask = .mask Or LVCF_IMAGE
      End If
      If Len(sText) > 0 Then
         .cchTextMax = Len(sText) + 1
         .pszText = sText
         .mask = .mask Or LVCF_TEXT
      End If
      fUnScale lWidth, 0, xPixels, yPixels
      .cx = xPixels
   End With
   SendMessage m_hWnd, LVM_INSERTCOLUMN, iColBefore, tLVH
   If fColumnCount > lOrigColCount Then
      ' We have succeeded; store the additional info
      ' associated with this column:
      ReDim Preserve m_tColInfo(1 To fColumnCount) As tColHeaderInfo
      lIndex = iColBefore + 1
      For i = fColumnCount - 1 To lIndex Step -1
         LSet m_tColInfo(i + 1) = m_tColInfo(i)
      Next i
      With m_tColInfo(lIndex)
         .sKey = sKey
         .vIcon = vIcon
         .sText = sText
         .lId = lId
      End With
      fAddColumn = lIndex
   End If
   
End Function
Friend Property Get fColumnCount() As Long
   fColumnCount = SendMessageLong(fHeaderhWnd, HDM_GETITEMCOUNT, 0, 0)
End Property
Friend Property Get fColumnText(ByVal lColumn As Long) As String
    fColumnText = m_tColInfo(lColumn).sText
End Property
Friend Property Let fColumnText(ByVal lColumn As Long, ByVal sText As String)
    If (lColumn > 0) And (lColumn <= fColumnCount) Then
        m_tColInfo(lColumn).sText = sText
    Else
        gErr 8, "vbalListViewCtl"
    End If
End Property
Friend Property Get fColumnTag(ByVal lColumn As Long) As String
    fColumnTag = m_tColInfo(lColumn).sTag
End Property
Friend Property Let fColumnTag(ByVal lColumn As Long, ByVal sTag As String)
    If (lColumn > 0) And (lColumn <= fColumnCount) Then
        m_tColInfo(lColumn).sTag = sTag
    Else
        gErr 8, "vbalListViewCtl"
    End If
End Property
Friend Property Get fColumnSortOrder(ByVal lColumn As Long) As ESortOrderConstants
   fColumnSortOrder = m_tColInfo(lColumn).eSortOrder
End Property
Friend Property Let fColumnSortOrder(ByVal lColumn As Long, ByVal eSortOrder As ESortOrderConstants)
Dim i As Long
   If (lColumn > 0) And (lColumn <= fColumnCount) Then
      m_tColInfo(lColumn).eSortOrder = eSortOrder
      If Not (eSortOrder = eSortOrderNone) Then
         For i = 1 To fColumnCount
            If Not (i = lColumn) Then
               m_tColInfo(i).eSortOrder = eSortOrderNone
            End If
         Next i
      End If
   Else
      gErr 8, "vbalListViewCtl"
   End If
End Property
Friend Property Get fColumnSortType(ByVal lColumn As Long) As ESortTypeConstants
    fColumnSortType = m_tColInfo(lColumn).ESortType
End Property
Friend Property Let fColumnSortType(ByVal lColumn As Long, ByVal eType As ESortTypeConstants)
    If (lColumn > 0) And (lColumn <= fColumnCount) Then
        m_tColInfo(lColumn).ESortType = eType
    Else
        gErr 8, "vbalListViewCtl"
    End If
End Property
Friend Property Get fColumnItemData(ByVal lColumn As Long) As Long
    fColumnItemData = m_tColInfo(lColumn).lItemData
End Property
Friend Property Let fColumnItemData(ByVal lColumn As Long, ByVal lItemData As Long)
    If (lColumn > 0) And (lColumn <= fColumnCount) Then
        m_tColInfo(lColumn).lItemData = lItemData
    Else
        gErr 8, "vbalListViewCtl"
    End If
End Property
Friend Property Get fColumnKey(ByVal lColumn As Long) As String
    fColumnKey = m_tColInfo(lColumn).sKey
End Property
Friend Property Let fColumnKey(ByVal lColumn As Long, ByVal sKey As String)
    If (lColumn > 0) And (lColumn <= fColumnCount) Then
        m_tColInfo(lColumn).sKey = sKey
    Else
        gErr 8, "vbalListViewCtl"
    End If
End Property
Friend Property Get fColumnImageOnRight(ByVal lColumn As Long) As Boolean
   If (lColumn > 0) And (lColumn <= fColumnCount) Then
      Dim tHI As LVCOLUMN
      tHI.mask = LVCF_FMT
      If (pbGetHeaderItemInfo(lColumn, tHI)) Then
         fColumnImageOnRight = ((tHI.fmt And LVCFMT_BITMAP_ON_RIGHT) = LVCFMT_BITMAP_ON_RIGHT)
      End If
   Else
      gErr 8, "vbalListViewCtl"
   End If
End Property
Friend Property Let fColumnImageOnRight(ByVal lColumn As Long, ByVal bImageOnRight As Boolean)
    If (lColumn > 0) And (lColumn <= fColumnCount) Then
      Dim tHI As LVCOLUMN
      tHI.mask = LVCF_FMT
      If (pbGetHeaderItemInfo(lColumn, tHI)) Then
         If bImageOnRight Then
            tHI.fmt = tHI.fmt Or LVCFMT_BITMAP_ON_RIGHT
         Else
            tHI.fmt = tHI.fmt And Not LVCFMT_BITMAP_ON_RIGHT
         End If
         pbSetHeaderItemInfo lColumn, tHI
      End If
    Else
        gErr 8, "vbalListViewCtl"
    End If
End Property

Friend Property Get fColumnIndex(ByVal lColumn As Long) As Long
Dim tHI As LVCOLUMN
   tHI.mask = LVCF_ORDER
   If (pbGetHeaderItemInfo(lColumn, tHI)) Then
      fColumnIndex = tHI.iOrder
   End If
End Property
Friend Property Let fColumnIndex(ByVal lColumn As Long, ByVal lOrder As Long)
Dim tHI As LVCOLUMN
   If Not (fColumnIndex(lColumn) = lOrder) Then
      tHI.mask = LVCF_ORDER
      tHI.iOrder = lOrder
      If (pbSetHeaderItemInfo(lColumn, tHI)) Then
         ' ok
      Else
         ' error
         Debug.Print "Set column order error"
      End If
   End If
End Property
Friend Property Get fColumnTextAlign(ByVal lColumn As Long) As EColumnHeaderAlignConstants
Dim tHI As LVCOLUMN
Dim eAlign As EColumnHeaderAlignConstants
   tHI.mask = LVCF_FMT
   If (pbGetHeaderItemInfo(lColumn, tHI)) Then
      eAlign = tHI.fmt And LVCFMT_JUSTIFYMASK
      ' Parse:
      If (eAlign = eLVColumnAlignCenter) Then
         eAlign = eLVColumnAlignRight
      ElseIf (eAlign = eLVColumnAlignRight) Then
         eAlign = eLVColumnAlignCenter
      End If
      fColumnTextAlign = eAlign
   End If
End Property
Friend Property Let fColumnTextAlign(ByVal lColumn As Long, ByVal eAlign As EColumnHeaderAlignConstants)
Dim tHI As LVCOLUMN
Dim eParseAlign As EColumnHeaderAlignConstants
   tHI.mask = LVCF_FMT
   If (pbGetHeaderItemInfo(lColumn, tHI)) Then
      tHI.fmt = tHI.fmt And Not LVCFMT_JUSTIFYMASK
      If (eAlign = eLVColumnAlignCenter) Then
         eParseAlign = eLVColumnAlignRight
      ElseIf (eAlign = eLVColumnAlignRight) Then
         eParseAlign = eLVColumnAlignCenter
      Else
         eParseAlign = eAlign
      End If
      tHI.fmt = tHI.fmt Or eParseAlign
      If (pbSetHeaderItemInfo(lColumn, tHI)) Then
      Else
         ' failed.
      End If
   End If
End Property
Friend Property Get fColumnWidth(ByVal lColumn As Long) As Long
Dim tHI As LVCOLUMN
   tHI.mask = LVCF_WIDTH
   If (pbGetHeaderItemInfo(lColumn, tHI)) Then
       fColumnWidth = tHI.cx
   Else
      ' Error
      Debug.Print "Get column width error"
   End If
End Property

Friend Property Let fColumnWidth(ByVal lColumn As Long, ByVal lWidthPixels As Long)
Dim tHI As LVCOLUMN
    If Not (fColumnWidth(lColumn) = lWidthPixels) Then
        tHI.mask = LVCF_WIDTH
        tHI.cx = lWidthPixels
        If (pbSetHeaderItemInfo(lColumn, tHI)) Then
            'RaiseEvent ColumnWidthChanged(lColumn, lWidthPixels)
        Else
            ' Error
            Debug.Print "Set column width error"
        End If
    End If
End Property
Friend Property Get fColumnImage(ByVal lColumn As Long) As Long
Dim tHI As LVCOLUMN
   tHI.mask = LVCF_FMT
   If (pbGetHeaderItemInfo(lColumn, tHI)) Then
      If (tHI.fmt And LVCFMT_IMAGE) = LVCFMT_IMAGE Then
         tHI.mask = LVCF_IMAGE
         If (pbGetHeaderItemInfo(lColumn, tHI)) Then
             fColumnImage = tHI.iImage
         Else
             ' Error
             Debug.Print "Get column image error"
         End If
      Else
         fColumnImage = -1
      End If
   End If
End Property

Friend Property Let fColumnImage(ByVal lColumn As Long, ByVal lImage As Long)
Dim tHI As LVCOLUMN
   If Not (fColumnImage(lColumn) = lImage) Then
      tHI.mask = LVCF_FMT
      If pbGetHeaderItemInfo(lColumn, tHI) Then
         If (pbValidImage(lImage) < 0) Then
            tHI.fmt = tHI.fmt Or LVCFMT_IMAGE
            tHI.mask = tHI.mask Or LVCF_IMAGE
            tHI.iImage = lImage
         Else
            tHI.fmt = tHI.fmt And Not LVCFMT_IMAGE
         End If
         If (pbSetHeaderItemInfo(lColumn, tHI)) Then
             ' ok
         Else
             ' Error
             Debug.Print "Set column image error"
         End If
      End If
   End If
End Property
Private Function pbValidImage(ByVal lImgIndex As Long) As Boolean
Dim iCount As Long
   If Not (m_hIml(3) = 0) Then
      iCount = ImageList_GetImageCount(m_hIml(3))
      If (lImgIndex > -1) And (lImgIndex < iCount) Then
         pbValidImage = True
      End If
   End If
End Function
Private Function pbGetHeaderItemInfo(ByVal lCol As Long, tHI As LVCOLUMN) As Boolean
   lCol = lCol - 1
   tHI.iSubItem = lCol
   If Not (SendMessage(m_hWnd, LVM_GETCOLUMN, lCol, tHI) = 0) Then
       pbGetHeaderItemInfo = True
   End If
End Function
Private Function pbSetHeaderItemInfo(ByVal lCol As Long, tHI As LVCOLUMN) As Boolean
   lCol = lCol - 1
   tHI.iSubItem = lCol
   If Not (SendMessage(m_hWnd, LVM_SETCOLUMN, lCol, tHI) = 0) Then
       pbSetHeaderItemInfo = True
   End If
End Function

Friend Sub fClearItems()
   SendMessageLong m_hWnd, LVM_DELETEALLITEMS, 0, 0
End Sub
Public Property Get OLEDragMode() As OLEDragConstants
Attribute OLEDragMode.VB_Description = "Gets/sets the OLEDragMode of the control."
   OLEDragMode = m_eOLEDragMode
End Property
Public Property Let OLEDragMode(ByVal eMode As OLEDragConstants)
   m_eOLEDragMode = eMode
   PropertyChanged "OLEDragMode"
End Property
Public Property Get OLEDropMode() As OLEDropConstants
Attribute OLEDropMode.VB_Description = "Gets/sets the OLEDropMode of the control."
   OLEDropMode = UserControl.OLEDropMode
End Property
Public Property Let OLEDropMode(ByVal eMode As OLEDropConstants)
   UserControl.OLEDropMode = eMode
   PropertyChanged "OLEDropMode"
End Property
Public Property Get ScaleMode() As ScaleModeConstants
Attribute ScaleMode.VB_Description = "Gets/sets the ScaleMode of the control."
   ScaleMode = UserControl.ScaleMode
End Property
Public Property Let ScaleMode(ByVal eMode As ScaleModeConstants)
   UserControl.ScaleMode = eMode
   PropertyChanged "ScaleMode"
End Property
Public Property Get ScaleWidth() As Single
Attribute ScaleWidth.VB_Description = "Gets the scaled width of the control using the ScaleMode."
   ScaleWidth = UserControl.ScaleWidth
End Property
Public Property Get ScaleHeight() As Single
Attribute ScaleHeight.VB_Description = "Gets the scaled height of the control using the ScaleMode."
   ScaleHeight = UserControl.ScaleHeight
End Property
Public Property Get Appearance() As EAppearanceConstants
Attribute Appearance.VB_Description = "Gets/sets the appearance of the control's borders."
Attribute Appearance.VB_ProcData.VB_Invoke_Property = ";Appearance"
Attribute Appearance.VB_UserMemId = -520
   Appearance = m_eAppearance
End Property
Public Property Let Appearance(ByVal eStyle As EAppearanceConstants)
   m_eAppearance = eStyle
   pSetBorder
   PropertyChanged "Appearance"
End Property
Public Property Get BorderStyle() As EBorderStyleConstants
Attribute BorderStyle.VB_Description = "Gets/sets the border style of the ListView control."
Attribute BorderStyle.VB_ProcData.VB_Invoke_Property = ";Appearance"
Attribute BorderStyle.VB_UserMemId = -504
   BorderStyle = m_eBorder
End Property
Public Property Let BorderStyle(ByVal eStyle As EBorderStyleConstants)
   m_eBorder = eStyle
   pSetBorder
   PropertyChanged "BorderStyle"
End Property
Private Sub pSetBorder()
Dim lS As Long
   If m_eAppearance = eLVFlat Then
      ' Flat border
      UserControl.BorderStyle() = 0
      pSetWinExStyle 0, WS_EX_STATICEDGE Or WS_EX_CLIENTEDGE
      If m_eBorder > eLVNone Then
         pSetStyle WS_BORDER, 0
      Else
         pSetStyle 0, WS_BORDER
      End If
   Else
      ' 3d border
      pSetStyle 0, WS_BORDER
      If m_eBorder = eLVFixedSingle Then
         UserControl.BorderStyle() = 1
         pSetWinExStyle 0, WS_EX_CLIENTEDGE Or WS_EX_STATICEDGE
      Else
         UserControl.BorderStyle() = 0
         If m_eBorder = eLVThin Then
            pSetWinExStyle WS_EX_STATICEDGE, WS_EX_CLIENTEDGE
         Else
            pSetWinExStyle 0, WS_EX_STATICEDGE Or WS_EX_CLIENTEDGE
         End If
      End If
   End If
End Sub

Public Property Get Enabled() As Boolean
Attribute Enabled.VB_Description = "Gets/sets whether the control is enabled or not."
Attribute Enabled.VB_ProcData.VB_Invoke_Property = ";Behavior"
Attribute Enabled.VB_UserMemId = -514
   Enabled = m_bEnabled
End Property
Public Property Let Enabled(ByVal bState As Boolean)
   m_bEnabled = bState
   UserControl.Enabled = bState
   EnableWindow m_hWnd, Abs(bState)
   PropertyChanged "Enabled"
End Property

Friend Sub fStartEdit(ByVal lIndex As Long)
   SendMessageLong m_hWnd, LVM_EDITLABEL, -1, 0
   SendMessageLong m_hWnd, LVM_EDITLABEL, lIndex - 1, 0
End Sub
Friend Sub fEnsureVisible(ByVal lIndex As Long)
   SendMessageLong m_hWnd, LVM_ENSUREVISIBLE, lIndex - 1, 0
End Sub

Private Function ComCtlVersion( _
        ByRef lMajor As Long, _
        ByRef lMinor As Long, _
        Optional ByRef lBuild As Long _
    ) As Boolean
Dim hMod As Long
Dim lR As Long
Dim lptrDLLVersion As Long
Dim tDVI As DLLVERSIONINFO

    lMajor = 0: lMinor = 0: lBuild = 0

    hMod = LoadLibrary("comctl32.dll")
    If Not (hMod = 0) Then
        lR = S_OK
        '/*
        ' You must get this function explicitly because earlier versions of the DLL
        ' don't implement this function. That makes the lack of implementation of the
        ' function a version marker in itself. */
        lptrDLLVersion = GetProcAddress(hMod, "DllGetVersion")
        If Not (lptrDLLVersion = 0) Then
            tDVI.cbSize = Len(tDVI)
            lR = DllGetVersion(tDVI)
            If (lR = S_OK) Then
                lMajor = tDVI.dwMajor
                lMinor = tDVI.dwMinor
                lBuild = tDVI.dwBuildNumber
            End If
        Else
            'If GetProcAddress failed, then the DLL is a version previous to the one
            'shipped with IE 3.x.
            lMajor = 4
        End If
        FreeLibrary hMod
        ComCtlVersion = True
    End If

End Function
Public Function HitTest(ByVal X As Single, ByVal Y As Single) As cListItem
Attribute HitTest.VB_Description = "Returns the list item at the specified point in the control, if there is one."
Dim tP As POINTAPI
Dim lIdx As Long
Dim lPtr As Long
Dim pc As pcListItem
Dim cI As cListItem
   fUnScale X, Y, tP.X, tP.Y
   lIdx = fHitTest(tP.X, tP.Y)
   If lIdx > 0 Then
      lPtr = fItemData(lIdx)
      If Not lPtr = 0 Then
         Set pc = ObjectFromPtr(lPtr)
         Set cI = New cListItem
         cI.fInit m_hWnd, pc.ID, lPtr
         Set HitTest = cI
      End If
   End If
End Function
Friend Function fHitTest(ByVal X As Long, ByVal Y As Long) As Long
Dim tLVHI As LVHITTESTINFO
   
   tLVHI.pt.X = X
   tLVHI.pt.Y = Y
   fHitTest = SendMessage(m_hWnd, LVM_HITTEST, 0, tLVHI) + 1
   
End Function
Friend Function fVerifylParam(ByVal lParam As Long) As Long
Dim tVFI As LVFINDINFO
Dim lIdx As Long

   tVFI.flags = LVFI_PARAM
   tVFI.lParam = lParam
   lIdx = SendMessage(m_hWnd, LVM_FINDITEM, -1, tVFI)
   fVerifylParam = lIdx + 1
   
End Function
Friend Function fVerifyColumnID(ByVal lId As Long) As Long
Dim i As Long
   For i = 1 To fColumnCount
      If m_tColInfo(i).lId = lId Then
         fVerifyColumnID = i
         Exit Function
      End If
   Next i
End Function
Friend Function fVerifyGroupID(ByVal lId As Long) As Boolean
Dim lR As Long
   lR = SendMessageLong(m_hWnd, LVM_HASGROUP, lId, 0)
   fVerifyGroupID = Not (lR = 0)
End Function
Friend Sub fItemRedraw(ByVal iIndex As Long)
   SendMessageLong m_hWnd, LVM_REDRAWITEMS, iIndex - 1, iIndex - 1
End Sub
Friend Property Get fItemPositionX(ByVal iIndex As Long) As Long
Dim tP As POINTAPI
   SendMessage m_hWnd, LVM_GETITEMPOSITION, iIndex - 1, tP
   fItemPositionX = tP.X
End Property
Friend Property Get fItemPositionY(ByVal iIndex As Long) As Long
Dim tP As POINTAPI
   SendMessage m_hWnd, LVM_GETITEMPOSITION, iIndex - 1, tP
   fItemPositionY = tP.Y
End Property
Friend Sub fGetItemPosition(ByVal iIndex As Long, ByRef X As Long, ByRef Y As Long)
Dim tP As POINTAPI
   SendMessage m_hWnd, LVM_GETITEMPOSITION, iIndex - 1, tP
   X = tP.X
   Y = tP.Y
End Sub
Friend Sub fSetItemPosition(ByVal iIndex As Long, ByVal X As Long, ByVal Y As Long)
Dim tP As POINTAPI
Dim lR As Long
   tP.X = X
   tP.Y = Y
   lR = SendMessage(m_hWnd, LVM_SETITEMPOSITION32, iIndex - 1, tP)
End Sub
Friend Sub fGetItemRect(ByVal iIndex As Long, ByRef tR As RECT)
   SendMessage m_hWnd, LVM_GETITEMRECT, iIndex - 1, tR
End Sub

Public Sub Arrange(ByVal eArrange As EArrangeStyleConstants)
Attribute Arrange.VB_Description = "Arranges the icons in a Tile, Icon or Small Icons view."
   SendMessageLong m_hWnd, LVM_ARRANGE, eArrange, 0
End Sub

Public Sub Scroll(ByVal dx As Long, ByVal dy As Long)
Attribute Scroll.VB_Description = "Scrolls the control by the specified offset in pixels."
   SendMessageLong m_hWnd, LVM_SCROLL, dx, dy
End Sub

Friend Sub fClear()
   SendMessageLong m_hWnd, LVM_DELETEALLITEMS, 0, 0
End Sub
Friend Sub fClearColumns()
   ' TODO
End Sub
Friend Sub fRemoveItem(ByVal lIndex As Long)
   SendMessageLong m_hWnd, LVM_DELETEITEM, lIndex - 1, 0
   ' NOTE: the bulk of the work in actually removing
   ' the item will be done in the LVN_DELETEITEM
   ' notification.
End Sub
Friend Sub fRemoveColumn(ByVal lIndex As Long)
Dim lOrigColCount As Long
Dim lNowCount As Long
Dim i As Long
   lOrigColCount = fColumnCount
   SendMessageLong m_hWnd, LVM_DELETECOLUMN, lIndex - 1, 0
   lNowCount = fColumnCount
   If lNowCount < lOrigColCount Then
      If lNowCount > 0 Then
         For i = lIndex To lOrigColCount - 1
            LSet m_tColInfo(i) = m_tColInfo(i + 1)
         Next i
      Else
         Erase m_tColInfo
      End If
   End If
End Sub
Friend Sub fGetItemGroup(ByVal lIndex As Long, lGroupId As Long, lGroupPtr As Long)
Dim lPtr As Long
   lGroupId = 0
   lGroupPtr = 0
   pGetStyle lIndex - 1, LVIF_GROUPID
   lPtr = m_tLV.iGroupId
   If Not (lPtr = 0) Then
      If fVerifyGroupID(lPtr) Then
         Dim pc As pcItemGroup
         Set pc = ObjectFromPtr(lPtr)
         lGroupPtr = lPtr
         lGroupId = pc.ID
      End If
   End If
End Sub
Friend Sub fSetItemGroup(ByVal lIndex As Long, ByVal sGroupKey As String)
   pGetStyle lIndex - 1, LVIF_GROUPID
   m_tLV.iGroupId = m_colGroups.Item(sGroupKey)
   pSetIStyle lIndex - 1, LVIF_GROUPID
End Sub
Friend Property Get fItemCaption(ByVal lIndex As Long) As String
   pGetStyle lIndex - 1, LVIF_TEXT
   fItemCaption = m_tLV.pszText
End Property
Friend Property Let fItemCaption(ByVal lIndex As Long, ByVal sCaption As String)
   pGetStyle lIndex - 1, LVIF_TEXT
   pSetIStyle lIndex - 1, LVIF_TEXT, sCaption
End Property
Friend Property Get fSubItemCaption(ByVal lIndex As Long, ByVal lSubItemIndex As Long) As String
   pGetStyle lIndex - 1, LVIF_TEXT, lSubItemIndex
   fSubItemCaption = m_tLV.pszText
End Property
Friend Property Let fSubItemCaption(ByVal lIndex As Long, ByVal lSubItemIndex As Long, ByVal sCaption As String)
   pGetStyle lIndex - 1, LVIF_TEXT, lSubItemIndex
   pSetIStyle lIndex - 1, LVIF_TEXT, sCaption, lSubItemIndex
End Property

Friend Property Get fSubItemIconIndex(ByVal lIndex As Long, ByVal lSubItemIndex As Long) As Long
   pGetStyle lIndex - 1, LVIF_IMAGE, lSubItemIndex
   fSubItemIconIndex = m_tLV.iImage
End Property
Friend Property Let fSubItemIconIndex(ByVal lIndex As Long, ByVal lSubItemIndex As Long, ByVal lImage As Long)
   pGetStyle lIndex - 1, LVIF_IMAGE, lSubItemIndex
   m_tLV.iImage = lImage
   pSetIStyle lIndex - 1, LVIF_IMAGE, , lSubItemIndex
End Property

Friend Property Get fSubItemShowInTile(ByVal lIndex As Long, ByVal lSubItemIndex As Long) As Boolean
   '
   If (m_lMajor >= 6) Then
      ReDim lOrigCols(0 To fColumnCount - 1) As Long
      m_tLV.puColumns = VarPtr(lOrigCols(0))
      pGetStyle lIndex - 1, LVIF_COLUMNS
      If (m_tLV.cColumns > 0) Then
         Dim i As Long
         For i = 0 To m_tLV.cColumns - 1
            If (lOrigCols(i) = lSubItemIndex) Then
               fSubItemShowInTile = True
            End If
         Next i
      End If
   End If
   '
End Property
Friend Property Let fSubItemShowInTile(ByVal lIndex As Long, ByVal lSubItemIndex As Long, ByVal bState As Boolean)
   '
   If (m_lMajor >= 6) Then
      ' Check what we already have:
      ReDim lOrigCols(0 To fColumnCount - 1) As Long
      Dim lOrigColCount As Long
      
      m_tLV.puColumns = VarPtr(lOrigCols(0))
      pGetStyle lIndex - 1, LVIF_COLUMNS
      lOrigColCount = m_tLV.cColumns
            
      Dim lFoundIndex As Long
      Dim i As Long
      Dim lNewCols() As Long
      Dim tSANew As SAFEARRAY1D
      
      lFoundIndex = -1
      For i = 0 To lOrigColCount - 1
         If (lOrigCols(i) = lSubItemIndex) Then
            lFoundIndex = i
            Exit For
         End If
      Next i
      
      If (bState) Then
         ' adding
         If (lFoundIndex = -1) Then
            ReDim lNewCols(0 To lOrigColCount) As Long
            For i = 0 To lOrigColCount - 1
               lNewCols(i) = lOrigCols(i)
            Next i
            lNewCols(lOrigColCount) = lSubItemIndex
            m_tLV.cColumns = lOrigColCount + 1
            m_tLV.puColumns = VarPtr(lNewCols(0))
            pSetIStyle lIndex - 1, LVIF_COLUMNS
         Else
            ' nothing to do
         End If
      Else
         ' removing
         If (lFoundIndex = -1) Then
            ' nothing to do
         Else
            If (lOrigColCount = 1) Then
               ' clearing:
               m_tLV.cColumns = 0
               m_tLV.puColumns = 0
               pSetIStyle lIndex - 1, LVIF_COLUMNS
            Else
               ' modifying
               ReDim lNewCols(0 To lOrigColCount - 2) As Long
               For i = lFoundIndex To lOrigColCount - 2
                  lNewCols(i) = lOrigCols(i + 1)
               Next i
               m_tLV.cColumns = lOrigColCount - 1
               m_tLV.puColumns = VarPtr(lNewCols(0))
               pSetIStyle lIndex - 1, LVIF_COLUMNS
            End If
         End If
         
      End If
         
   End If
   
End Property


Friend Property Get fItemIconIndex(ByVal lIndex As Long) As Long
   pGetStyle lIndex - 1, LVIF_IMAGE
   fItemIconIndex = m_tLV.iImage
End Property
Friend Property Let fItemIconIndex(ByVal lIndex As Long, ByVal lIconIndex As Long)
   pGetStyle lIndex - 1, LVIF_IMAGE
   m_tLV.iImage = lIconIndex
   pSetIStyle lIndex - 1, LVIF_IMAGE
End Property
Friend Property Get fItemStateIconIndex(ByVal lIndex As Long) As Long
   Debug.Assert "TODO" = ""
   pGetStyle lIndex - 1, LVIF_IMAGE
   fItemStateIconIndex = m_tLV.iImage
End Property
Friend Property Let fItemStateIconIndex(ByVal lIndex As Long, ByVal lIconIndex As Long)
   Debug.Assert "TODO" = ""
   pGetStyle lIndex - 1, LVIF_IMAGE
   m_tLV.iImage = lIconIndex
   pSetIStyle lIndex - 1, LVIF_IMAGE
End Property

Friend Property Get fItemIndent(ByVal lIndex As Long) As Long
   pGetStyle lIndex - 1, LVIF_INDENT
   fItemIndent = m_tLV.iIndent
End Property
Friend Property Let fItemIndent(ByVal lIndex As Long, ByVal lIndent As Long)
   pGetStyle lIndex - 1, LVIF_INDENT
   m_tLV.iIndent = lIndent
   pSetIStyle lIndex - 1, LVIF_INDENT
End Property
Friend Property Get fItemData(ByVal lIndex As Long) As Long
   pGetStyle lIndex - 1, LVIF_PARAM
   fItemData = m_tLV.lParam
End Property
Friend Property Let fItemData(ByVal lIndex As Long, ByVal lItemData As Long)
   pGetStyle lIndex - 1, LVIF_PARAM
   m_tLV.lParam = lItemData
   pSetIStyle lIndex - 1, LVIF_PARAM
End Property
Friend Property Get fItemCut(ByVal lIndex As Long) As Boolean
    fItemCut = pIsState(lIndex - 1, LVIS_CUT, True)
End Property
Friend Property Let fItemCut(ByVal lIndex As Long, ByVal bState As Boolean)
    pSetState lIndex - 1, LVIS_CUT, bState
End Property
Friend Property Get fItemHot(ByVal lIndex As Long) As Boolean
   If lIndex > 0 Then
      fItemHot = ((lIndex - 1) = SendMessageLong(m_hWnd, LVM_GETHOTITEM, 0, 0))
   End If
End Property
Friend Property Let fItemHot(ByVal lIndex As Long, ByVal bState As Boolean)
   If bState Then
      SendMessageLong m_hWnd, LVM_SETHOTITEM, lIndex - 1, 0
   End If
End Property
Friend Property Get fItemSelected(ByVal lIndex As Long) As Boolean
    fItemSelected = pIsState(lIndex - 1, LVIS_SELECTED, True)
End Property
Friend Property Let fItemSelected(ByVal lIndex As Long, ByVal bState As Boolean)
    pSetState lIndex - 1, LVIS_SELECTED, bState
End Property
Friend Property Get fItemChecked(ByVal lIndex As Long) As Boolean
Dim lR As Long
   
   lR = SendMessage(m_hWnd, LVM_GETITEMSTATE, lIndex - 1, LVIS_STATEIMAGEMASK)
   fItemChecked = ((lR And &H2000&) = &H2000&)
   
End Property
Friend Property Let fItemChecked(ByVal lIndex As Long, ByVal bState As Boolean)
Dim lR As Long
Dim tLV As LVITEM
   
   tLV.iItem = lIndex - 1
   tLV.mask = LVIF_STATE
   tLV.stateMask = &H3000&
   If (SendMessage(m_hWnd, LVM_GETITEMSTATE, lIndex - 1, LVIS_STATEIMAGEMASK) And &H2000&) = &H2000& Then
     ' uncheck
     tLV.state = &H1000&
   Else
     ' check
     tLV.state = &H2000&
   End If
   lR = SendMessage(m_hWnd, LVM_SETITEMSTATE, lIndex - 1, tLV)
   
End Property

Friend Function fIsDuplicateGroupKey(ByVal sKey As String) As Boolean
Dim lPtr As Long
   On Error Resume Next
   lPtr = m_colGroups.Item(sKey)
   ' no error means we found it already!
   fIsDuplicateGroupKey = (Err.Number = 0)
End Function
Friend Sub fAddGroupKey(ByVal sKey As String, ByVal lPtr As Long)
   m_colGroups.Add lPtr, sKey
End Sub
Friend Sub fChangeGroupKey(ByVal sKey As String, ByVal sNewKey As String)
Dim lPtr As Long
   lPtr = m_colGroups.Item(sKey)
   m_colGroups.Remove sKey
   m_colGroups.Add lPtr, sNewKey
End Sub
Friend Function fIsDuplicateItemKey(ByVal sKey As String) As Boolean
Dim lPtr As Long
   On Error Resume Next
   lPtr = m_colKeys.Item(sKey)
   ' no error means we found it already!
   fIsDuplicateItemKey = (Err.Number = 0)
End Function
Friend Sub fAddItemKey(ByVal sKey As String, ByVal lPtr As Long)
   m_colKeys.Add lPtr, sKey
End Sub
Friend Sub fChangeKey(ByVal sKey As String, ByVal sNewKey As String)
Dim lPtr As Long
   lPtr = m_colKeys.Item(sKey)
   m_colKeys.Remove sKey
   m_colKeys.Add lPtr, sNewKey
End Sub
Friend Function fIDForColumnIndex(ByVal lIndex As Long) As Long
   fIDForColumnIndex = m_tColInfo(lIndex).lId
End Function
Friend Function fIndexForColumnKey(ByVal sKey As String) As Long
Dim lIdx As Long
   
   For lIdx = 1 To fColumnCount
      If m_tColInfo(lIdx).sKey = sKey Then
         fIndexForColumnKey = lIdx
         Exit Function
      End If
   Next lIdx
   gErr 8, "vbalListViewCtl"
   
End Function
Friend Function fItemIndexForKey(ByVal sKey As String) As Long
Dim lPtr As Long
Dim lIdx As Long
   
   On Error Resume Next
   lPtr = m_colKeys.Item(sKey)
   If Err.Number = 0 Then
      lIdx = fVerifylParam(lPtr)
      On Error GoTo 0
      If lIdx > 0 Then
         fItemIndexForKey = lIdx
      Else
         ' Problem!
         Debug.Assert "Key in collection not in LV" = ""
         gErr 6, "vbalListViewCtl"
      End If
   Else
      On Error GoTo 0
      ' The key does not exist!
      gErr 6, "vbalListViewCtl"
   End If
End Function
Friend Function fGroupIndexForKey(ByVal sKey As String) As Long
Dim lPtr As Long
Dim lIdx As Long
   
   On Error Resume Next
   lPtr = m_colGroups.Item(sKey)
   If Err.Number = 0 Then
      lIdx = fVerifyGroupID(lPtr)
      On Error GoTo 0
      If lIdx > 0 Then
         fGroupIndexForKey = lIdx
      Else
         ' Problem!
         Debug.Assert "Key in collection not in LV" = ""
         gErr 6, "vbalListViewCtl"
      End If
   Else
      On Error GoTo 0
      ' The key does not exist!
      gErr 6, "vbalListViewCtl"
   End If
End Function

' Retrieves the item info into ItemStyle module variable.
Private Sub pGetStyle(ByVal lIndex As Long, ByVal lMask As Long, Optional ByVal lSubItem As Long = 0)
Dim sBuf As String
Dim lPos As Long
Dim lR As Long
Dim iMsg As Long
    
   iMsg = LVM_GETITEM
   m_tLV.mask = lMask
   If (lMask And LVIF_TEXT) = LVIF_TEXT Then
      sBuf = String(261, 0)
      m_tLV.pszText = sBuf
      m_tLV.cchTextMax = 260
   End If
   m_tLV.iItem = lIndex
   m_tLV.iSubItem = lSubItem
   
   m_tLV.iGroupId = 0
   m_tLV.iImage = 0
   m_tLV.iIndent = 0
   m_tLV.lParam = 0
   m_tLV.state = 0
   m_tLV.cColumns = 0
   If Not ((lMask And LVIF_COLUMNS) = LVIF_COLUMNS) Then
      m_tLV.puColumns = 0
   End If
   
   If (lSubItem > 0) Then
      lR = SendMessage(m_hWnd, iMsg, lIndex, m_tLV)
   Else
      lR = SendMessage(m_hWnd, iMsg, 0, m_tLV)
   End If
   If (lR = 0) Then
      Debug.Print "Operation Failed:", Hex(m_tLV.mask)
   End If
   lPos = InStr(m_tLV.pszText, Chr$(0))
   If lPos > 0 Then
      m_tLV.pszText = left$(m_tLV.pszText, lPos - 1)
   End If
   m_tLV.cchTextMax = Len(m_tLV.pszText)
    
End Sub

' SetIStyle, not to be confused with SetStyle.
' Sets the item info from ItemStyle module variable.
Private Sub pSetIStyle(ByVal lIndex As Long, ByVal lMask As Long, Optional sText As String, Optional lSubItem As Long = 0)
Dim sBuf As String
Dim lPos As Long
Dim lR As Long
Dim iMsg As Long
   
   iMsg = LVM_SETITEM
   If ((lMask And LVIF_TEXT) = LVIF_TEXT) Then
      sBuf = String$(261, 0)
      m_tLV.pszText = sBuf
      m_tLV.cchTextMax = 260
      If Len(sText) > 0 Then
         LSet m_tLV.pszText = sText & Chr$(0)
         m_tLV.cchTextMax = Len(sText) + 1
      End If
   End If
   m_tLV.iItem = lIndex
   m_tLV.iSubItem = lSubItem
   m_tLV.mask = lMask
   If (lSubItem > 0) Then
      lR = SendMessage(m_hWnd, iMsg, lIndex, m_tLV)
   Else
      lR = SendMessage(m_hWnd, iMsg, 0, m_tLV)
   End If
   If (lR = 0) Then
      Debug.Print "Operation Failed:", Hex(lMask)
   End If
   
   UpdateWindow m_hWnd
   
End Sub

Private Function pIsState(ByVal lIndex As Long, ByVal lValue As Long, Optional bUseAsMask As Boolean = False) As Long
   If bUseAsMask Then
      m_tLV.stateMask = lValue
   End If
   m_tLV.iItem = lIndex
   pGetStyle lIndex, LVIF_STATE
   pIsState = CBool(m_tLV.state And lValue)
End Function

Private Sub pSetState(ByVal lIndex As Long, ByVal lValue As Long, ByVal bState As Boolean, Optional bUseAsMask As Boolean = True)
   If bUseAsMask Then
      m_tLV.stateMask = lValue
   End If
   pGetStyle lIndex, LVIF_STATE
   If bState Then
      m_tLV.state = m_tLV.state Or lValue
   Else
      m_tLV.state = m_tLV.state And (Not lValue)
   End If
   pSetIStyle lIndex, LVIF_STATE
End Sub


Public Property Get IconSpaceX() As Long
Attribute IconSpaceX.VB_Description = "Gets/sets the horizontal icon spacing in Report and Tile views."
Attribute IconSpaceX.VB_ProcData.VB_Invoke_Property = ";Appearance"
   IconSpaceX = m_lIconSpaceX
End Property
Public Property Let IconSpaceX(ByVal X As Long)
   m_lIconSpaceX = X
   If Not m_hWnd = 0 Then
      pSetIconSpacing
   End If
   PropertyChanged "IconSpaceX"
End Property
Public Property Get IconSpaceY() As Long
Attribute IconSpaceY.VB_Description = "Gets/sets the the vertical icon spacing in Report and Tile views."
Attribute IconSpaceY.VB_ProcData.VB_Invoke_Property = ";Appearance"
   IconSpaceY = m_lIconSpaceY
End Property
Public Property Let IconSpaceY(ByVal Y As Long)
   m_lIconSpaceY = Y
   If Not m_hWnd = 0 Then
      pSetIconSpacing
   End If
   PropertyChanged "IconSpaceY"
End Property
Private Sub pSetIconSpacing()
Dim lXY As Long
Dim lXYD As Long
Dim lXYC As Long
Dim cx As Long, cy As Long
   
   ' Set cx=-1, cy=-1 to reset to default and return current settings:
   lXYD = &HFFFFFFFF
   lXYC = SendMessageLong(m_hWnd, LVM_SETICONSPACING, 0, lXYD)
   ' cX is loword:
   cx = (lXYC And &HFFFF&)
   ' cY is hiword:
   cy = (lXYC \ &H10000)
   If m_lIconSpaceX > 0 Then cx = m_lIconSpaceX
   If m_lIconSpaceY > 0 Then cy = m_lIconSpaceY
   
   lXY = cx And &H7FFF
   lXY = lXY Or ((cy And &H7FFF) * &H10000)
   SendMessageLong m_hWnd, LVM_SETICONSPACING, 0, lXY
   
End Sub
Public Property Let ImageList(Optional ByVal eSize As EImageListTypeConstants = eLVLargeIcon, ilsThis As Object)
Attribute ImageList.VB_Description = "Sets one of the five image lists that can be used in the control: tile, large , small, column headers and state icons."
Attribute ImageList.VB_ProcData.VB_Invoke_PropertyPut = ";Data"
   pImageList eSize, ilsThis
End Property
Public Property Set ImageList(Optional ByVal eSize As EImageListTypeConstants = eLVLargeIcon, ilsThis As Object)
   pImageList eSize, ilsThis
End Property
Private Sub pImageList(ByVal eType As EImageListTypeConstants, ilsThis As Variant)
Dim hIml As Long
   
   If (VarType(ilsThis) = vbLong) Then
      ' Assume a handle to an image list:
      hIml = ilsThis
   ElseIf (VarType(ilsThis) = vbObject) Then
      ' Assume a VB image list:
      On Error Resume Next
      ' Get the image list initialised..
      ilsThis.ListImages(1).Draw 0, 0, 0, 1
      hIml = ilsThis.hImagelist
      If (Err.Number = 0) Then
          ' OK
      Else
          gErr 4, "vbalListViewCtl"
      End If
      On Error GoTo 0
   End If
   
   If Not (hIml = 0) Then
      Dim iIndex As Long
      
      iIndex = 0
      Select Case eType
      Case eLVTileImages
         iIndex = 4
      Case eLVStateImages
         iIndex = LVSIL_STATE
      Case eLVSmallIcon
         iIndex = LVSIL_SMALL
      Case eLVLargeIcon
         iIndex = LVSIL_NORMAL
      Case eLVHeaderImages
         iIndex = 3
      End Select
              
      ' If we have a valid image list, then associate it with the control:
      m_hIml(iIndex) = hIml
      ImageList_GetIconSize m_hIml(iIndex), m_lIconSizeX(iIndex), m_lIconSizeY(iIndex)
              
      ' Set image list as required:
      Select Case eType
      Case eLVTileImages
         If (View = eViewTile) Then
            SendMessageLong m_hWnd, LVM_SETIMAGELIST, LVSIL_NORMAL, hIml
         End If
      Case eLVHeaderImages
         Dim lhWnd As Long
         lhWnd = SendMessageLong(m_hWnd, LVM_GETHEADER, 0, 0)
         If Not (lhWnd = 0) Then
            SendMessageLong lhWnd, HDM_SETIMAGELIST, 0, hIml
         End If
      Case eLVLargeIcon
         If (View = eViewTile) Then
            If (m_hIml(4) = 0) Then ' no tile icons provided
               SendMessageLong m_hWnd, LVM_SETIMAGELIST, LVSIL_NORMAL, hIml
            End If
         Else
            SendMessageLong m_hWnd, LVM_SETIMAGELIST, LVSIL_NORMAL, hIml
         End If
      Case eLVSmallIcon
         SendMessageLong m_hWnd, LVM_SETIMAGELIST, LVSIL_SMALL, hIml
      Case eLVStateImages
         SendMessageLong m_hWnd, LVM_SETIMAGELIST, LVSIL_STATE, hIml
      End Select
   
   End If
   
End Sub

Friend Property Get fGroupsEnabled() As Boolean
   fGroupsEnabled = m_bGroupEnabled
End Property
Friend Property Let fGroupsEnabled(ByVal bState As Boolean)
Dim lR As Long
   lR = SendMessageLong(m_hWnd, LVM_ENABLEGROUPVIEW, Abs(bState), 0)
   If (lR = -1) Then
      gErr 9, "vbalListViewCtl"
   Else
      m_bGroupEnabled = bState
   End If
End Property

Friend Sub fGroupClear()
   ' Clear all the data associated with existing groups:
   SendMessageLong m_hWnd, LVM_REMOVEALLGROUPS, 0, 0
   
   Dim vlPtr As Variant
   Dim iUnk As IShellFolderEx_TLB.IUnknown
   For Each vlPtr In m_colGroups
      Set iUnk = ObjectFromPtr(vlPtr)
      iUnk.Release
   Next
   Set m_colGroups = New Collection
   
End Sub

Friend Function fRemoveGroup(ByVal lIndex As Long) As Boolean
Dim lR As Long
   If (m_colGroups.Count > 1) Then
      
   '   Dim lPtr As Long
   '   Dim tG As tGroupInfo
   '   LSet tG = m_tGroup(lIndex)
   '   Dim i As Long
   '   For i = lIndex To m_iGroupCount - 1
   '      LSet m_tGroup(i) = m_tGroup(i + 1)
   '   Next i
   '   Dim iUnk As IShellFolderEx_TLB.IUnknown
   '   Set iUnk = ObjectFromPtr(tG.lPtrGroup)
   '   iUnk.Release
   ''   m_iGroupCount = m_iGroupCount - 1
   '  SendMessageLong m_hWnd, LVM_REMOVEGROUP, tG.lId, 0
      Debug.Assert Not (lR = -1)
      fRemoveGroup = Not (lR = -1)
   Else
      fGroupClear
   End If
End Function

Friend Property Get fGroupHeader(ByVal lPtrGroup As Long) As String
Dim tLVG As LVGROUP
Dim lR As Long
Dim hMem As Long
Dim lPtrMem As Long
Dim b() As Byte
Dim s As String
Dim iPos As Long
   
   tLVG.cbSize = Len(tLVG)
   tLVG.mask = LVGF_HEADER
   tLVG.cchHeader = 260
   hMem = LocalAlloc(GPTR, tLVG.cchHeader * 2)
   lPtrMem = LocalLock(hMem)
   tLVG.pszHeader = lPtrMem
   lR = SendMessage(m_hWnd, LVM_GETGROUPINFO, lPtrGroup, tLVG)
   ReDim b(0 To tLVG.cchHeader * 2 + 2) As Byte
   CopyMemory b(0), ByVal tLVG.pszHeader, tLVG.cchHeader * 2
   s = b
   iPos = InStr(s, vbNullChar)
   If (iPos > 1) Then
      fGroupHeader = left(s, iPos - 1)
   Else
      fGroupHeader = s
   End If
   LocalUnlock hMem
   LocalFree hMem
End Property
Friend Property Let fGroupHeader(ByVal lPtrGroup As Long, ByVal sHeader As String)
Dim tLVG As LVGROUP
Dim lR As Long
Dim hMem As Long
Dim lPtrMem As Long
Dim b() As Byte

   tLVG.cbSize = Len(tLVG)
   tLVG.mask = LVGF_HEADER
   tLVG.cchHeader = Len(sHeader)
   hMem = LocalAlloc(GPTR, tLVG.cchHeader * 2 + 2)
   lPtrMem = LocalLock(hMem)
   b = sHeader
   CopyMemory ByVal lPtrMem, b(0), tLVG.cchHeader * 2
   tLVG.pszHeader = lPtrMem
   lR = SendMessage(m_hWnd, LVM_SETGROUPINFO, lPtrGroup, tLVG)
   LocalUnlock hMem
   LocalFree hMem
   
End Property
Friend Property Get fGroupAlign(ByVal lPtrGroup As Long) As EItemGroupHeaderAlignConstants
Dim tLVG As LVGROUP
Dim lR As Long
   tLVG.cbSize = Len(tLVG)
   tLVG.mask = LVGF_ALIGN
   lR = SendMessage(m_hWnd, LVM_GETGROUPINFO, lPtrGroup, tLVG)
   fGroupAlign = tLVG.uAlign
End Property
Friend Property Let fGroupAlign(ByVal lPtrGroup As Long, ByVal eAlign As EItemGroupHeaderAlignConstants)
Dim tLVG As LVGROUP
Dim lR As Long
   tLVG.cbSize = Len(tLVG)
   tLVG.mask = LVGF_ALIGN
   tLVG.uAlign = eAlign
   lR = SendMessage(m_hWnd, LVM_GETGROUPINFO, lPtrGroup, tLVG)
End Property
Friend Property Get fGroupState(ByVal lPtrGroup As Long) As EItemGroupStateConstants
Dim tLVG As LVGROUP
Dim lR As Long
   tLVG.cbSize = Len(tLVG)
   tLVG.mask = LVGF_STATE
   lR = SendMessage(m_hWnd, LVM_GETGROUPINFO, lPtrGroup, tLVG)
   fGroupState = tLVG.state
End Property
Friend Property Let fGroupState(ByVal lPtrGroup As Long, ByVal eState As EItemGroupStateConstants)
Dim tLVG As LVGROUP
Dim lR As Long
   tLVG.cbSize = Len(tLVG)
   tLVG.mask = LVGF_STATE
   tLVG.state = eState
   lR = SendMessage(m_hWnd, LVM_GETGROUPINFO, lPtrGroup, tLVG)
End Property
Friend Property Get fGroupType(ByVal lPtrGroup As Long) As EItemGroupTypeConstants

End Property
Friend Property Let fGroupType(ByVal lPtrGroup As Long, ByVal eType As EItemGroupTypeConstants)

End Property

Friend Property Get fGroupCount() As Long
   fGroupCount = m_colGroups.Count
End Property

Friend Property Get fItemCountPerPage() As Long
   fItemCountPerPage = SendMessageLong(m_hWnd, LVM_GETCOUNTPERPAGE, 0, 0)
End Property

Friend Property Get fCount() As Long
   fCount = SendMessageLong(m_hWnd, LVM_GETITEMCOUNT, 0, 0)
End Property

Friend Function fAddGroup( _
      ByVal sText As String, _
      Optional ByVal lIndex As Long = -1, _
      Optional ByVal lItemData As Long = 0 _
   )
Dim tLVG As LVGROUP
Dim lOrigCount As Long
Dim lR As Long
Dim hMem As Long
Dim lPtrMem As Long
Dim b() As Byte

   fGroupsEnabled = True
   lOrigCount = fGroupCount()
   tLVG.cbSize = Len(tLVG)
   tLVG.iGroupId = lItemData
   tLVG.mask = LVGF_ALIGN Or LVGF_STATE Or LVGF_HEADER Or LVGF_GROUPID
   tLVG.state = LVGS_NORMAL
   tLVG.uAlign = LVGA_HEADER_LEFT
   tLVG.cchHeader = 260
   hMem = LocalAlloc(GPTR, (tLVG.cchHeader + 1) * 2)
   lPtrMem = LocalLock(hMem)
   b = sText
   CopyMemory ByVal lPtrMem, b(0), LenB(sText)
   tLVG.pszHeader = lPtrMem
   lR = SendMessage(m_hWnd, LVM_INSERTGROUP, lIndex, tLVG)
   LocalUnlock hMem
   LocalFree hMem
   
   fAddGroup = Not (lR = -1)
   
End Function

Friend Function fAddItem( _
      ByVal sText As String, _
      Optional ByVal lIndex As Long = 1, _
      Optional ByVal iIcon As Long = -1, _
      Optional ByVal iIndent As Long = 0, _
      Optional ByVal lItemData As Long = 0 _
   ) As Boolean
Dim tLV As LVITEM
Dim lR As Long
Dim lOrigCount As Long
   lOrigCount = fCount()
   tLV.pszText = sText & vbNullChar
   tLV.cchTextMax = Len(sText) + 1
   tLV.iImage = iIcon
   tLV.iIndent = iIndent
   tLV.lParam = lItemData
   tLV.iItem = lIndex - 1
   tLV.mask = LVIF_TEXT Or LVIF_IMAGE Or LVIF_PARAM Or LVIF_INDENT
   lR = SendMessage(m_hWnd, LVM_INSERTITEM, 0, tLV)
   fAddItem = Not (fCount() = lOrigCount)
End Function

Public Property Get TopItem() As cListItem
Attribute TopItem.VB_Description = "Gets the top-most visible item in the control."
Attribute TopItem.VB_ProcData.VB_Invoke_Property = ";Data"
Dim cI As cListItem
Dim pc As pcListItem
Dim lTop As Long
Dim lParam As Long
   lTop = SendMessageLong(m_hWnd, LVM_GETTOPINDEX, 0, 0) + 1
   If lTop > -1 Then
      lParam = fItemData(lTop + 1)
      If Not (lParam = 0) Then
         Set pc = ObjectFromPtr(lParam)
         Set cI = New cListItem
         cI.fInit m_hWnd, pc.ID, lParam
         Set TopItem = cI
      End If
   End If
End Property

Public Property Get SelectedItem() As cListItem
Attribute SelectedItem.VB_Description = "Gets the selected item (if any) in the control."
Attribute SelectedItem.VB_ProcData.VB_Invoke_Property = ";Data"
Dim cI As cListItem
Dim pc As pcListItem
Dim lFlags As Long
Dim lItem As Long
Dim lParam As Long

   lFlags = LVNI_SELECTED
   If GetFocus() = m_hWnd Then
      lFlags = lFlags Or LVNI_FOCUSED
   End If
   lItem = SendMessageLong(m_hWnd, LVM_GETNEXTITEM, -1, lFlags)
      If lItem > -1 Then
      lParam = fItemData(lItem + 1)
      If Not (lParam = 0) Then
         Set pc = ObjectFromPtr(lParam)
         Set cI = New cListItem
         cI.fInit m_hWnd, pc.ID, lParam
         Set SelectedItem = cI
      End If
   End If
   
End Property
Public Property Get Columns() As cColumns
Attribute Columns.VB_Description = "Gets the collection of Columns associated with the control."
   Dim cH As New cColumns
   cH.fInit m_hWnd
   Set Columns = cH
End Property
Public Property Get ItemGroups() As cItemGroups
Attribute ItemGroups.VB_Description = "Gets the collection of groups in the ListView control.  Requires ComCtl32.DLL v6.0."
   If (m_lMajor >= 6) Then
      Dim cG As New cItemGroups
      cG.fInit m_hWnd
      Set ItemGroups = cG
   End If
End Property
Public Property Get ListItems() As cListItems
Attribute ListItems.VB_Description = "Gets the collection of ListItems in the control."
Attribute ListItems.VB_ProcData.VB_Invoke_Property = ";Data"
   Dim cI As New cListItems
   cI.fInit m_hWnd
   Set ListItems = cI
End Property
Public Property Get WorkAreas() As cWorkAreas
Attribute WorkAreas.VB_Description = "Gets the collection of WorkAreas in the ListView."
   Dim cW As New cWorkAreas
   cW.fInit m_hWnd
   Set WorkAreas = cW
End Property

Public Property Get ListItemsPerPage() As Long
Attribute ListItemsPerPage.VB_Description = "Gets the number of ListItems which fit on a page."
Attribute ListItemsPerPage.VB_ProcData.VB_Invoke_Property = ";Appearance"
   ListItemsPerPage = SendMessageLong(m_hWnd, LVM_GETCOUNTPERPAGE, 0, 0)
End Property
Public Property Get OriginX() As Long
Attribute OriginX.VB_Description = "Gets/sets the x origin point for Tile, Large Icons and Small Icon views."
Attribute OriginX.VB_ProcData.VB_Invoke_Property = ";Appearance"
Dim tP As POINTAPI
   SendMessage m_hWnd, LVM_GETORIGIN, 0, tP
   OriginX = tP.X
End Property
Public Property Get OriginY() As Long
Attribute OriginY.VB_Description = "Gets/sets the y origin point for Tile, Large Icons and Small Icon views."
Attribute OriginY.VB_ProcData.VB_Invoke_Property = ";Appearance"
Dim tP As POINTAPI
   SendMessage m_hWnd, LVM_GETORIGIN, 0, tP
   OriginY = tP.Y
End Property
Public Sub Refresh()
Attribute Refresh.VB_Description = "Refreshes the control."
   SendMessageLong m_hWnd, LVM_UPDATE, 0, 0
End Sub
Friend Sub fSortItems()
Dim i As Long
Dim iSortCol As Long
   
   ' Find which column to sort on:
   For i = 1 To fColumnCount
      If Not (m_tColInfo(i).eSortOrder = eSortOrderNone) Then
         iSortCol = i
         Exit For
      End If
   Next i
   
   ' If we get one...
   If iSortCol > 0 Then
      ' Initiate the sort:
      SortInit m_tColInfo(iSortCol).eSortOrder, m_tColInfo(iSortCol).ESortType, iSortCol - 1
      SendMessageLong m_hWnd, LVM_SORTITEMS, m_hWnd, AddressOf LVWSortCompare
   End If
   
End Sub

Public Property Get hwnd() As Long
Attribute hwnd.VB_Description = "Gets the Window Handle of this control (vbalListViewCtl)."
Attribute hwnd.VB_ProcData.VB_Invoke_Property = ";Misc"
Attribute hwnd.VB_UserMemId = -515
   hwnd = m_hWndParent
End Property
Public Property Get hWndListView() As Long
Attribute hWndListView.VB_Description = "Gets the window handle of the ListView itself."
Attribute hWndListView.VB_ProcData.VB_Invoke_Property = ";Misc"
   hWndListView = m_hWnd
End Property
Public Property Get hWndEdit() As Long
Attribute hWndEdit.VB_Description = "Gets the window handle of the label edit text box."
Attribute hWndEdit.VB_ProcData.VB_Invoke_Property = ";Misc"
   hWndEdit = SendMessageLong(m_hWnd, LVM_GETEDITCONTROL, 0, 0)
End Property
Public Property Get BackgroundPicture() As String
Attribute BackgroundPicture.VB_Description = "Gets/sets the filename of an image to display in the background of the ListView."
Attribute BackgroundPicture.VB_ProcData.VB_Invoke_Property = "StandardPicture;Appearance"
   BackgroundPicture = m_sPicture
End Property
Public Property Let BackgroundPicture(ByVal sURL As String)
Dim tLBI As LVBKIMAGE
   m_sPicture = sURL
   If Not (m_hWnd = 0) Then
      tLBI.pszImage = sURL & Chr$(0)
      tLBI.cchImageMax = Len(sURL) + 1
      tLBI.ulFlags = LVBKIF_SOURCE_URL
      If (m_bTilePicture) Then
         tLBI.ulFlags = tLBI.ulFlags Or LVBKIF_STYLE_TILE
      Else
         tLBI.ulFlags = tLBI.ulFlags Or LVBKIF_STYLE_NORMAL
         tLBI.xOffsetPercent = m_lPictureXOffset
         tLBI.yOffsetPercent = m_lPictureYOffset
      End If
      SendMessage m_hWnd, LVM_SETBKIMAGE, 0, tLBI
      ' Set the background colour of the ListView to &HFFFFFFFF (-1)
      ' so it will be transparent!
      SendMessageLong m_hWnd, LVM_SETTEXTBKCOLOR, 0, CLR_NONE
   End If
   PropertyChanged "BackgroundPicture"
End Property
Public Property Let TileBackgroundPicture(ByVal bTile As Boolean)
Attribute TileBackgroundPicture.VB_Description = "Gets/sets whether the background picture will be tiled or not. Defaults to True."
   If Not (m_bTilePicture = bTile) Then
      m_bTilePicture = bTile
      If (Len(m_sPicture) > 0) Then
         BackgroundPicture = m_sPicture
      End If
      PropertyChanged "TileBackgroundPicture"
   End If
End Property
Public Property Get TileBackgroundPicture() As Boolean
   TileBackgroundPicture = m_bTilePicture
End Property
Public Property Let BackgroundPictureXOffset(ByVal lOffset As Long)
Attribute BackgroundPictureXOffset.VB_Description = "Gets/sets the horizontal offset for the background picture from the origin if it is not tiled, as a percentage of the width of the control.  0 = Left, 50 = Centre, 100 = Right."
   If Not (m_lPictureXOffset = lOffset) Then
      m_lPictureXOffset = lOffset
      If (Len(m_sPicture) > 0) Then
         BackgroundPicture = m_sPicture
      End If
      PropertyChanged "BackgroundPictureXOffset"
   End If
End Property
Public Property Get BackgroundPictureXOffset() As Long
   BackgroundPictureXOffset = m_lPictureXOffset
End Property
Public Property Let BackgroundPictureYOffset(ByVal lOffset As Long)
Attribute BackgroundPictureYOffset.VB_Description = "Gets/sets the veritcal offset for the background picture from the origin if it is not tiled, as a percentage of the height of the control.  0 = Top, 50 = Centre, 100 = Bottom."
   If Not (m_lPictureYOffset = lOffset) Then
      m_lPictureYOffset = lOffset
      If (Len(m_sPicture) > 0) Then
         BackgroundPicture = m_sPicture
      End If
      PropertyChanged "BackgroundPictureYOffset"
   End If
End Property
Public Property Get BackgroundPictureYOffset() As Long
   BackgroundPictureYOffset = m_lPictureYOffset
End Property
Public Property Get BackColor() As OLE_COLOR
Attribute BackColor.VB_Description = "Gets/sets the background colour of the control."
Attribute BackColor.VB_ProcData.VB_Invoke_Property = ";Appearance"
Attribute BackColor.VB_UserMemId = -501
   BackColor = m_oBackColor
End Property
Public Property Let BackColor(ByVal oColor As OLE_COLOR)
   m_oBackColor = oColor
   If Not m_hWnd = 0 Then
      If (oColor = -1) Then
         SendMessageLong m_hWnd, LVM_SETBKCOLOR, 0, -1
         SendMessageLong m_hWnd, LVM_SETTEXTBKCOLOR, 0, -1
      Else
         SendMessageLong m_hWnd, LVM_SETBKCOLOR, 0, TranslateColor(oColor)
         SendMessageLong m_hWnd, LVM_SETTEXTBKCOLOR, 0, TranslateColor(oColor)
      End If
   End If
   PropertyChanged "BackColor"
End Property
Public Property Get ForeColor() As OLE_COLOR
Attribute ForeColor.VB_Description = "Gets/sets the default item foreground colour of the control."
Attribute ForeColor.VB_ProcData.VB_Invoke_Property = ";Appearance"
Attribute ForeColor.VB_UserMemId = -513
   ForeColor = m_oForeColor
End Property
Public Property Let ForeColor(ByVal oColor As OLE_COLOR)
   m_oForeColor = oColor
   If Not m_hWnd = 0 Then
      SendMessageLong m_hWnd, LVM_SETTEXTCOLOR, 0, TranslateColor(oColor)
   End If
   PropertyChanged "ForeColor"
End Property
Public Property Get CustomDraw() As Boolean
Attribute CustomDraw.VB_Description = "Gets/sets whether CustomDraw processing is applied. Must be set true if you want to customise individual items' fonts or colours."
Attribute CustomDraw.VB_ProcData.VB_Invoke_Property = ";Behavior"
   CustomDraw = m_bCustomDraw
End Property
Public Property Let CustomDraw(ByVal bState As Boolean)
   m_bCustomDraw = bState
   PropertyChanged "CustomDraw"
End Property
Public Property Get HeaderButtons() As Boolean
Attribute HeaderButtons.VB_Description = "Gets/sets whether the column headers in report mode are clickable buttons or not."
Attribute HeaderButtons.VB_ProcData.VB_Invoke_Property = ";Behavior"
   HeaderButtons = m_bHeaderButtons
End Property
Public Property Let HeaderButtons(ByVal bState As Boolean)
Dim lS As Long
Dim lhWnd As Long
   m_bHeaderButtons = bState
   If Not (m_hWnd = 0) Then
      ' Set the Buttons mode of the ListView's header control:
      lhWnd = SendMessageLong(m_hWnd, LVM_GETHEADER, 0, 0)
      If Not (lhWnd = 0) Then
         lS = GetWindowLong(lhWnd, GWL_STYLE)
         If bState Then
            lS = lS Or HDS_BUTTONS
         Else
            lS = lS And Not HDS_BUTTONS
         End If
         SetWindowLong lhWnd, GWL_STYLE, lS
      End If
   End If
   PropertyChanged "HeaderButtons"
End Property
Public Property Get HeaderTrackSelect() As Boolean
Attribute HeaderTrackSelect.VB_Description = "Gets/sets whether the header items highlight when the mouse tracks over them.  This always occurs under ComCtl6.0/XP."
Attribute HeaderTrackSelect.VB_ProcData.VB_Invoke_Property = ";Behavior"
   HeaderTrackSelect = m_bHeaderTrackSelect
End Property
Public Property Let HeaderTrackSelect(ByVal bState As Boolean)
Dim lS As Long
Dim lhWnd As Long

   m_bHeaderTrackSelect = bState

   ' Set the track select mode of the ListView's header control:
   If Not (m_hWnd = 0) Then
      lhWnd = SendMessageLong(m_hWnd, LVM_GETHEADER, 0, 0)
      If Not (lhWnd = 0) Then
         lS = GetWindowLong(lhWnd, GWL_STYLE)
         If bState Then
            lS = lS And Not HDS_BUTTONS
         Else
            lS = lS Or HDS_BUTTONS
         End If
         SetWindowLong lhWnd, GWL_STYLE, lS
      End If
   End If
   PropertyChanged "HeaderTracKSelect)"

End Property

Public Property Get View() As EViewStyleConstants
Attribute View.VB_Description = "Gets/sets the control View.  Tile View is only supported under ComCtl32.DLL v6.0."
Attribute View.VB_ProcData.VB_Invoke_Property = ";Appearance"
   View = m_eStyle
End Property
Public Property Let View(ByVal eView As EViewStyleConstants)
   m_eStyle = eView
   If (m_lMajor >= 6) Then
      SendMessageLong m_hWnd, LVM_SETVIEW, eView, 0
      If (m_hIml(4) = 0) Then
         SendMessageLong m_hWnd, LVM_SETIMAGELIST, LVSIL_NORMAL, m_hIml(0)
      Else
         If (View = eViewTile) Then
            SendMessageLong m_hWnd, LVM_SETIMAGELIST, LVSIL_NORMAL, m_hIml(4)
         Else
            SendMessageLong m_hWnd, LVM_SETIMAGELIST, LVSIL_NORMAL, m_hIml(0)
         End If
      End If
   Else
      pSetStyle eView, (LVS_ICON Or LVS_SMALLICON Or LVS_REPORT Or LVS_LIST)
   End If
   PropertyChanged "View"
End Property
Public Property Get MultiSelect() As Boolean
Attribute MultiSelect.VB_Description = "Gets/sets whether multiple items can be selected or not."
Attribute MultiSelect.VB_ProcData.VB_Invoke_Property = ";Behavior"
   MultiSelect = Not (m_bSingleSelect)
End Property
Public Property Let MultiSelect(ByVal bState As Boolean)
   m_bSingleSelect = Not (bState)
   If m_bSingleSelect Then
      pSetStyle LVS_SINGLESEL, 0
   Else
      pSetStyle 0, LVS_SINGLESEL
   End If
   PropertyChanged "MultiSelect"
End Property
Public Property Get HideSelection() As Boolean
Attribute HideSelection.VB_Description = "Gets/sets whether the selected item is hidden when the control is out of focus."
Attribute HideSelection.VB_ProcData.VB_Invoke_Property = ";Appearance"
   HideSelection = m_bHideSelection
End Property
Public Property Let HideSelection(ByVal bHide As Boolean)
   m_bHideSelection = bHide
   If bHide Then
      pSetStyle 0, LVS_SHOWSELALWAYS
   Else
      pSetStyle LVS_SHOWSELALWAYS, 0
   End If
End Property

Public Property Get DoubleBuffer() As Boolean
Attribute DoubleBuffer.VB_Description = "Gets/sets whether the control's rendering is double-buffered for a smoother display or not.  Has no effect on pre-XP systems."
   DoubleBuffer = m_bDoubleBuffer
End Property
Public Property Let DoubleBuffer(ByVal bState As Boolean)
   m_bDoubleBuffer = bState
   If bState Then
      pSetExStyle LVS_EX_DOUBLEBUFFER, 0
   Else
      pSetExStyle 0, LVS_EX_DOUBLEBUFFER
   End If
   PropertyChanged "DoubleBuffer"
End Property

Public Property Get SubItemImages() As Boolean
Attribute SubItemImages.VB_Description = "Gets/sets whether sub items display images or not in Report view."
Attribute SubItemImages.VB_ProcData.VB_Invoke_Property = ";Appearance"
   SubItemImages = m_bSubItemImages
End Property
Public Property Let SubItemImages(ByVal bState As Boolean)
   m_bSubItemImages = bState
   If bState Then
      pSetExStyle LVS_EX_SUBITEMIMAGES, 0
   Else
      pSetExStyle 0, LVS_EX_SUBITEMIMAGES
   End If
   PropertyChanged "SubItemImages"
End Property
Public Property Get FlatScrollBar() As Boolean
Attribute FlatScrollBar.VB_Description = "Gets/sets whether a FlatScrollBar is displayed.  Not valid when using XP Visual Styles."
Attribute FlatScrollBar.VB_ProcData.VB_Invoke_Property = ";Appearance"
   FlatScrollBar = m_bFlatScrollBar
End Property
Public Property Let FlatScrollBar(ByVal bState As Boolean)
   m_bFlatScrollBar = bState
   If bState Then
      pSetExStyle LVS_EX_FLATSB, 0
   Else
      pSetExStyle 0, LVS_EX_FLATSB
   End If
   PropertyChanged "FlatScrollBar"
End Property
Public Property Get GridLines() As Boolean
Attribute GridLines.VB_Description = "Gets/sets whether grid lines are displayed in report mode."
Attribute GridLines.VB_ProcData.VB_Invoke_Property = ";Appearance"
   GridLines = m_bGridLines
End Property
Public Property Let GridLines(ByVal bState As Boolean)
   m_bGridLines = bState
   If bState Then
      pSetExStyle LVS_EX_GRIDLINES, 0
   Else
      pSetExStyle 0, LVS_EX_GRIDLINES
   End If
   PropertyChanged "GridLines"
End Property
Public Property Get OneClickActivate() As Boolean
Attribute OneClickActivate.VB_Description = "Gets/sets whether the control HotTracks items; the idea is that a single click would then be the way to start the item, rather than a double click as normal."
Attribute OneClickActivate.VB_ProcData.VB_Invoke_Property = ";Behavior"
   OneClickActivate = m_bOneClickActivate
End Property
Public Property Let OneClickActivate(ByVal bState As Boolean)
   m_bOneClickActivate = bState
   If bState Then
      pSetExStyle LVS_EX_ONECLICKACTIVATE, 0
   Else
      pSetExStyle 0, LVS_EX_ONECLICKACTIVATE
   End If
   PropertyChanged "OneClickActivate"
End Property
Public Property Get ItemBorderSelect() As Boolean
Attribute ItemBorderSelect.VB_Description = "Gets/sets whether an item is selected by drawing a border around the icon rather than selecting it in Large Icons view."
   ItemBorderSelect = m_bItemBorderSelect
End Property
Public Property Let ItemBorderSelect(ByVal bState As Boolean)
   m_bItemBorderSelect = bState
   If bState Then
      pSetExStyle LVS_EX_BORDERSELECT, 0
   Else
      pSetExStyle 0, LVS_EX_BORDERSELECT
   End If
   PropertyChanged "ItemBorderSelect"
End Property
Public Property Get InfoTips() As Boolean
Attribute InfoTips.VB_Description = "Gets/sets whether the control displays tooltips."
Attribute InfoTips.VB_ProcData.VB_Invoke_Property = ";Behavior"
   InfoTips = m_bInfoTips
End Property
Public Property Let InfoTips(ByVal bState As Boolean)
   m_bInfoTips = bState
   If bState Then
      pSetExStyle LVS_EX_INFOTIP, 0
   Else
      pSetExStyle 0, LVS_EX_INFOTIP
   End If
   PropertyChanged "InfoTips"
End Property
Public Property Get LabelTips() As Boolean
Attribute LabelTips.VB_Description = "Gets/sets whether an ToolTip will be shown for items that are partially hidden."
   LabelTips = m_bLabelTips
End Property
Public Property Let LabelTips(ByVal bState As Boolean)
   m_bLabelTips = bState
   If bState Then
      pSetExStyle LVS_EX_LABELTIP, 0
   Else
      pSetExStyle 0, LVS_EX_LABELTIP
   End If
   PropertyChanged "LabelTips"
End Property
Public Property Get CheckBoxes() As Boolean
Attribute CheckBoxes.VB_Description = "Gets/sets whether check boxes appear for each item in the ListVIew."
Attribute CheckBoxes.VB_ProcData.VB_Invoke_Property = ";Appearance"
   CheckBoxes = m_bCheckBoxes
End Property
Public Property Let CheckBoxes(ByVal bState As Boolean)
   m_bCheckBoxes = bState
   If bState Then
      pSetExStyle LVS_EX_CHECKBOXES, 0
   Else
      pSetExStyle 0, LVS_EX_CHECKBOXES
   End If
   PropertyChanged "CheckBoxes"
End Property
Public Property Get TrackSelect() As Boolean
Attribute TrackSelect.VB_Description = "Gets/sets whether the items in the ListView highlight when the mouse is over them."
Attribute TrackSelect.VB_ProcData.VB_Invoke_Property = ";Behavior"
   TrackSelect = m_bTrackSelect
End Property
Public Property Let TrackSelect(ByVal bState As Boolean)
   m_bTrackSelect = bState
   If bState Then
      pSetExStyle LVS_EX_TRACKSELECT, 0
   Else
      pSetExStyle 0, LVS_EX_TRACKSELECT
   End If
   PropertyChanged "TrackSelect"
End Property
Public Property Get HeaderDragDrop() As Boolean
Attribute HeaderDragDrop.VB_Description = "Gets/sets whether the user can reorder columns using drag-drop in Report mode."
Attribute HeaderDragDrop.VB_ProcData.VB_Invoke_Property = ";Behavior"
   HeaderDragDrop = m_bHeaderDragDrop
End Property
Public Property Let HeaderDragDrop(ByVal bState As Boolean)
   m_bHeaderDragDrop = bState
   If bState Then
      pSetExStyle LVS_EX_HEADERDRAGDROP, 0
   Else
      pSetExStyle 0, LVS_EX_HEADERDRAGDROP
   End If
   PropertyChanged "HeaderDragDrop"
End Property
Public Property Get FullRowSelect() As Boolean
Attribute FullRowSelect.VB_Description = "Gets/sets whether the full width of an item and its subitems are selected in Report or Tile views."
Attribute FullRowSelect.VB_ProcData.VB_Invoke_Property = ";Behavior"
   FullRowSelect = m_bFullRowSelect
End Property
Public Property Let FullRowSelect(ByVal bState As Boolean)
   m_bFullRowSelect = bState
   If bState Then
      pSetExStyle LVS_EX_FULLROWSELECT, 0
   Else
      pSetExStyle 0, LVS_EX_FULLROWSELECT
   End If
   PropertyChanged "FullRowSelect"
End Property
Public Property Get NoScrollBar() As Boolean
Attribute NoScrollBar.VB_Description = "Gets/sets whether the control has no scroll bar."
Attribute NoScrollBar.VB_ProcData.VB_Invoke_Property = ";Behavior"
   NoScrollBar = m_bNoScrollBar
End Property
Public Property Let NoScrollBar(ByVal bState As Boolean)
   m_bNoScrollBar = bState
   If bState Then
      pSetStyle LVS_NOSCROLL, 0
   Else
      pSetStyle 0, LVS_NOSCROLL
   End If
   PropertyChanged "NoScrollBar"
End Property
Public Property Get NoColumnHeaders() As Boolean
Attribute NoColumnHeaders.VB_Description = "Gets/sets whether no column headers will be shown in Report view."
   NoColumnHeaders = m_bNoColumnHeaders
End Property
Public Property Let NoColumnHeaders(ByVal bState As Boolean)
   m_bNoColumnHeaders = bState
   If bState Then
      pSetStyle LVS_NOCOLUMNHEADER, 0
   Else
      pSetStyle 0, LVS_NOCOLUMNHEADER
   End If
   PropertyChanged "NoColumnHeaders"
End Property
Public Property Get AutoArrange() As Boolean
Attribute AutoArrange.VB_Description = "Gets/sets whether items in the ListView are automatically aligned to a grid in Tile, Report or Small Icon views."
Attribute AutoArrange.VB_ProcData.VB_Invoke_Property = ";Behavior"
   AutoArrange = m_bAutoArrange
End Property
Public Property Let AutoArrange(ByVal bState As Boolean)
   m_bAutoArrange = bState
   If bState Then
      pSetStyle LVS_AUTOARRANGE, 0
   Else
      pSetStyle 0, LVS_AUTOARRANGE
   End If
   PropertyChanged "AutoArrange"
End Property
Public Property Get LabelEdit() As Boolean
Attribute LabelEdit.VB_Description = "Gets/sets whether the item labels can be edited or not."
Attribute LabelEdit.VB_ProcData.VB_Invoke_Property = ";Behavior"
   LabelEdit = m_bEditLabels
End Property
Public Property Let LabelEdit(ByVal bState As Boolean)
   m_bEditLabels = bState
   If bState Then
      pSetStyle LVS_EDITLABELS, 0
   Else
      pSetStyle 0, LVS_EDITLABELS
   End If
   PropertyChanged "LabelEdit"
End Property

Private Sub pSetStyle(ByVal lStyle As Long, ByVal lStyleNot As Long)
Dim lS As Long
   If Not m_hWnd = 0 Then
      lS = GetWindowLong(m_hWnd, GWL_STYLE)
      lS = lS And Not lStyleNot
      lS = lS Or lStyle
      SetWindowLong m_hWnd, GWL_STYLE, lS
      SetWindowPos m_hWnd, 0, 0, 0, 0, 0, SWP_NOMOVE Or SWP_NOSIZE Or SWP_NOOWNERZORDER Or SWP_NOZORDER Or SWP_FRAMECHANGED
   End If
End Sub
Private Sub pSetWinExStyle(ByVal lStyle As Long, ByVal lStyleNot As Long)
Dim lS As Long
   If Not m_hWnd = 0 Then
      lS = GetWindowLong(m_hWnd, GWL_EXSTYLE)
      lS = lS And Not lStyleNot
      lS = lS Or lStyle
      SetWindowLong m_hWnd, GWL_EXSTYLE, lS
      SetWindowPos m_hWnd, 0, 0, 0, 0, 0, SWP_NOMOVE Or SWP_NOSIZE Or SWP_NOOWNERZORDER Or SWP_NOZORDER Or SWP_FRAMECHANGED
   End If
End Sub
Private Sub pSetExStyle(ByVal lStyle As Long, ByVal lStyleNot As Long)
Dim lS As Long
   If Not m_hWnd = 0 Then
      lS = SendMessageLong(m_hWnd, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0)
      lS = lS And Not lStyleNot
      lS = lS Or lStyle
      SendMessageLong m_hWnd, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, lS
   End If
End Sub
Private Function pIFF(ByVal bSwitch As Boolean, ByVal lTrue As Long, ByVal lFalse As Long) As Long
   If bSwitch Then
      pIFF = lTrue
   Else
      pIFF = lFalse
   End If
End Function

Private Function pbCreate() As Boolean
Dim lStyle As Long
Dim lExStyle As Long
Dim tR As RECT
Dim lXY As Long
Dim lXYD As Long
Dim lXYC As Long
Dim i As Long

   pDestroy
   
   InitCommonControls
   
   lStyle = WS_TABSTOP Or WS_VISIBLE Or WS_CHILD
   lStyle = lStyle Or _
         (View() And Not LV_VIEW_TILE) Or _
         pIFF(MultiSelect, 0, LVS_SINGLESEL) Or _
         LVS_SHAREIMAGELISTS Or _
         pIFF(LabelEdit, LVS_EDITLABELS, 0) Or _
         pIFF(HideSelection, 0, LVS_SHOWSELALWAYS) Or _
         pIFF(AutoArrange, LVS_AUTOARRANGE, 0) Or _
         pIFF(NoScrollBar, LVS_NOSCROLL, 0) Or _
         pIFF(MultiSelect, 0, LVS_SINGLESEL) Or _
         pIFF(NoColumnHeaders, LVS_NOCOLUMNHEADER, 0)
         
   lExStyle = GetWindowLong(m_hWndParent, GWL_EXSTYLE)
   lExStyle = lExStyle And Not WS_EX_CLIENTEDGE
   GetClientRect m_hWndParent, tR
   m_hWnd = CreateWindowEx( _
         lExStyle, WC_LISTVIEW, "", _
         lStyle, 0, 0, tR.right - tR.left, tR.bottom - tR.top, _
         m_hWndParent, 0, App.hInstance, 0)
   If Not (m_hWnd = 0) Then
      AttachMessage Me, m_hWndParent, WM_NOTIFY
      AttachMessage Me, m_hWndParent, WM_SETFOCUS
      AttachMessage Me, m_hWnd, WM_SETFOCUS
      AttachMessage Me, m_hWnd, WM_MOUSEACTIVATE
      
      AttachMessage Me, m_hWnd, WM_MOUSEMOVE
      AttachMessage Me, m_hWnd, WM_LBUTTONDOWN
      AttachMessage Me, m_hWnd, WM_RBUTTONDOWN
      AttachMessage Me, m_hWnd, WM_MBUTTONDOWN
      
      AttachMessage Me, m_hWnd, WM_KEYDOWN
      AttachMessage Me, m_hWnd, WM_CHAR
      AttachMessage Me, m_hWnd, WM_KEYUP
      
      lXYD = &HFFFFFFFF
      lXYC = SendMessageLong(m_hWnd, LVM_SETICONSPACING, 0, lXYD)
      m_lIconSpaceX = (lXYC And &HFFFF&)
      m_lIconSpaceY = (lXYC \ &H10000)
            
      Set m_colKeys = New Collection
      Set m_colGroups = New Collection
      
      If (m_lMajor > 5) Or ((m_lMajor = 5) And (m_lMinor >= 8)) Then
         ' ...you will get better results if you send a CCM_SETVERSION message with the wParam value
         ' set to 5 before adding any items to the control
         SendMessageLong m_hWnd, CCM_SETVERSION, m_lMajor, 0
      End If
      
      pbCreate = True
   End If
End Function
Private Sub pDestroy()
Dim i As Long
   If Not m_hWnd = 0 Then
   
      ' Remove all the items:
      fClear
      ' Remove all the groups:
      fGroupClear
   
      ' Clear drag image list if still one around
      If Not (m_hImlDrag = 0) Then
         ImageList_Destroy m_hImlDrag
      End If
      
      ' Unsubclass
      DetachMessage Me, m_hWndParent, WM_NOTIFY
      DetachMessage Me, m_hWndParent, WM_SETFOCUS
      DetachMessage Me, m_hWnd, WM_SETFOCUS
      DetachMessage Me, m_hWnd, WM_MOUSEACTIVATE
      
      DetachMessage Me, m_hWnd, WM_MOUSEMOVE
      DetachMessage Me, m_hWnd, WM_LBUTTONDOWN
      DetachMessage Me, m_hWnd, WM_RBUTTONDOWN
      DetachMessage Me, m_hWnd, WM_MBUTTONDOWN
      
      DetachMessage Me, m_hWnd, WM_KEYDOWN
      DetachMessage Me, m_hWnd, WM_CHAR
      DetachMessage Me, m_hWnd, WM_KEYUP
      
      ' Stop tooltips:
      'RemoveFromToolTip m_hWnd
      
      ' Remove our property:
      RemoveProp m_hWnd, gcObjectProp
      
      ' Destroy ListView window:
      ShowWindow m_hWnd, SW_HIDE
      SetParent m_hWnd, 0
      DestroyWindow m_hWnd
      m_hWnd = 0
      
   End If
   Set m_colKeys = Nothing
   
End Sub


Private Function plCustomDraw(ByVal lParam As Long) As Long
Dim NMLVCD As NMLVCUSTOMDRAW
Dim lLen As Long
Dim rc As RECT, trc As RECT
Dim lIndex As Long
Dim hBr As Long
Dim lHDC As Long
Dim i As Long, iP As Long
Dim lR As Long
Dim lFlags As Long
Dim pc As pcListItem
   
   If Not m_bCustomDraw Then
      plCustomDraw = CDRF_DODEFAULT
   Else
   
      ' Get the CustomDraw data.
      lLen = Len(NMLVCD)
      ' Check if COMCTL< 4.71, if so, drop 4 bytes off the len
      If m_lMajor < 4 Or (m_lMajor = 4 And m_lMinor < 71) Then
         lLen = lLen - 4
      End If
      CopyMemory NMLVCD, ByVal lParam, lLen
      
      Select Case NMLVCD.nmcd.dwDrawStage
      Case CDDS_PREPAINT
         ' Tell it we want to be told when an
         ' item is drawn.
         plCustomDraw = CDRF_NOTIFYITEMDRAW
   
      Case CDDS_ITEMPREPAINT
            
         ' An item is about to be drawn:
         plCustomDraw = CDRF_NOTIFYITEMDRAW
      
         If Not (NMLVCD.nmcd.lItemlParam = 0) Then
            'CustomDraw = CDRF_NEWFONT

            Set pc = ObjectFromPtr(NMLVCD.nmcd.lItemlParam)
      
            ' Set colours/fonts:
            If pc.ForeColor = CLR_NONE Then
               NMLVCD.clrText = TranslateColor(ForeColor)
            Else
               'Debug.Print "Trying to set colour: " & pc.ForeColor
               NMLVCD.clrText = TranslateColor(pc.ForeColor)
            End If
         
            If Len(m_sPicture) > 0 Then
               NMLVCD.clrTextBk = CLR_NONE
            Else
               If m_bEnabled Then
                  If pc.BackColor = CLR_NONE Then
                     NMLVCD.clrTextBk = TranslateColor(BackColor)
                  Else
                     NMLVCD.clrTextBk = TranslateColor(pc.BackColor)
                  End If
               Else
                  NMLVCD.clrTextBk = TranslateColor(vbButtonFace)
               End If
            End If
         
            If Not (pc.Font Is Nothing) Then
               SelectObject NMLVCD.nmcd.hdc, pc.Font.hFont
            End If
         
            CopyMemory ByVal lParam, NMLVCD, lLen
         
         End If
      '
      
      Case CDDS_ITEMPOSTPAINT
         plCustomDraw = CDRF_NEWFONT
      
      End Select
   
   End If
   

End Function

Friend Function TranslateAccelerator(lpMsg As VBOleGuids.Msg) As Long
Dim lhWnd As Long
      
   TranslateAccelerator = S_FALSE
   If Not (m_hWnd = 0) Then
      ' Here you can modify the response to the key down
      ' accelerator command using the values in lpMsg.  This
      ' can be used to capture Tabs, Returns, Arrows etc.
      ' Just process the message as required and return S_OK.
      If lpMsg.message = WM_KEYDOWN Or lpMsg.message = WM_KEYUP Then
         Select Case lpMsg.wParam And &HFFFF&
         Case vbKeyUp, vbKeyDown, vbKeyLeft, vbKeyRight, vbKeyPageDown, vbKeyPageUp, vbKeyHome, vbKeyEnd, vbKeyReturn
            'Debug.Print lpMsg.wParam
            If m_bInEdit Then
               lhWnd = SendMessageLong(m_hWnd, LVM_GETEDITCONTROL, 0, 0)
               SendMessageLong lhWnd, lpMsg.message, lpMsg.wParam, lpMsg.lParam
            Else
               SendMessageLong m_hWnd, lpMsg.message, lpMsg.wParam, lpMsg.lParam
            End If
            TranslateAccelerator = S_OK
         End Select
      End If
   End If
   
End Function

Private Function pButton(ByVal iMsg As Long) As Integer
   Select Case iMsg
   Case WM_LBUTTONDOWN
      pButton = vbLeftButton
   Case WM_RBUTTONDOWN
      pButton = vbRightButton
   Case WM_MBUTTONDOWN
      pButton = vbMiddleButton
   Case WM_MOUSEMOVE
      Select Case True
      Case GetAsyncKeyState(vbKeyLButton)
         pButton = vbLeftButton
      Case GetAsyncKeyState(vbKeyRButton)
         pButton = vbRightButton
      Case GetAsyncKeyState(vbKeyMButton)
         pButton = vbMiddleButton
      End Select
   End Select
End Function
Private Function pShiftState() As Integer
Dim lS As Integer
   If GetAsyncKeyState(vbKeyShift) Then
      lS = lS Or vbShiftMask
   End If
   If GetAsyncKeyState(vbKeyMenu) Then
      lS = lS Or vbAltMask
   End If
   If GetAsyncKeyState(vbKeyControl) Then
      lS = lS Or vbCtrlMask
   End If
   pShiftState = lS
End Function


Private Property Let ISubclass_MsgResponse(ByVal RHS As EMsgResponse)
   '
End Property

Private Property Get ISubclass_MsgResponse() As EMsgResponse
   If (CurrentMessage = WM_NOTIFY) Then
      ISubclass_MsgResponse = emrPreprocess
   ElseIf (CurrentMessage = WM_LBUTTONDOWN) Or (CurrentMessage = WM_RBUTTONDOWN) Or (CurrentMessage = WM_MBUTTONDOWN) Then
      ISubclass_MsgResponse = emrPostProcess
   Else
      ISubclass_MsgResponse = emrConsume
   End If
End Property

Private Function ISubClass_WindowProc(ByVal hwnd As Long, ByVal Msg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Dim tNMH As NMHDR
Dim bCancel As Boolean
Dim cI As cListItem
Dim sText As String
Dim lLen As Long
Dim b() As Byte
Dim tNMLVDI As NMLVDISPINFO
Dim tNMLVOD As NMLVODSTATECHANGE
Dim iPos As Long
Dim tMsg As Msg
Dim tNMLV As NMLISTVIEW
Dim iUnk As IShellFolderEx_TLB.IUnknown
Dim pc As pcListItem
Dim tP As POINTAPI
Dim lIdx As Long
Dim lPtr As Long
Dim X As Single, Y As Single
Dim iBtn As Integer, iShift As Integer
Dim bIsClick As Boolean
Dim nKey As Integer
   
   Select Case Msg
   Case WM_KEYDOWN
      nKey = (wParam And &H7FFF&)
      iShift = pShiftState()
      RaiseEvent KeyDown(nKey, iShift)
      wParam = nKey
      ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
   
   Case WM_KEYUP
      nKey = (wParam And &H7FFF&)
      iShift = pShiftState()
      RaiseEvent KeyUp(nKey, iShift)
      wParam = nKey
      ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
   
   Case WM_CHAR
      nKey = (wParam And &H7FFF&)
      RaiseEvent KeyPress(nKey)
      wParam = nKey
      ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
      
   Case WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN
      'ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
      iBtn = pButton(Msg)
      iShift = pShiftState()
      GetCursorPos tP
      ScreenToClient m_hWnd, tP
      LSet m_tMouseDownPos = tP
      fScale tP.X, tP.Y, X, Y
      RaiseEvent MouseDown(iBtn, iShift, X, Y)
      m_iBtnDown = iBtn
         
   Case WM_MOUSEMOVE
      ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
      iBtn = pButton(Msg)
      iShift = pShiftState()
      GetCursorPos tP
      ScreenToClient m_hWnd, tP
      fScale tP.X, tP.Y, X, Y
      RaiseEvent MouseMove(iBtn, iShift, X, Y)
   
   Case WM_NOTIFY
      ' The LV is telling us something.  Shall we listen?
      CopyMemory tNMH, ByVal lParam, Len(tNMH)
            
      Select Case tNMH.code
      Case NM_CUSTOMDRAW
         'Debug.Print "Custom Draw"
         ISubClass_WindowProc = plCustomDraw(lParam)

      Case NM_RETURN
         ' Enter button pressed.
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         
      Case NM_CLICK, NM_RCLICK
         ' Click: fund
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         GetCursorPos tP
         ScreenToClient m_hWnd, tP
         lIdx = fHitTest(tP.X, tP.Y)
         If lIdx > 0 Then
            lPtr = fItemData(lIdx)
            If Not lPtr = 0 Then
               Set pc = ObjectFromPtr(lPtr)
               Set cI = New cListItem
               cI.fInit m_hWnd, pc.ID, lPtr
               RaiseEvent ItemClick(cI)
               bIsClick = True
            End If
         End If
         If Not bIsClick Then
            RaiseEvent Click
         End If
         GetCursorPos tP
         ScreenToClient m_hWnd, tP
         LSet m_tMouseDownPos = tP
         fScale tP.X, tP.Y, X, Y
         RaiseEvent MouseUp(m_iBtnDown, pShiftState(), X, Y)
         
      Case NM_DBLCLK
         ' Double click
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         
         ' Note: we're using screen position here but for COMCTL32.DLL
         ' version 4.71 or higher you can actually get the information
         ' directly from lParam, which points to an NMLISTVIEW.
         GetCursorPos tP
         ScreenToClient m_hWnd, tP
         lIdx = fHitTest(tP.X, tP.Y)
         If lIdx > 0 Then
            lPtr = fItemData(lIdx)
            If Not lPtr = 0 Then
               Set pc = ObjectFromPtr(lPtr)
               Set cI = New cListItem
               cI.fInit m_hWnd, pc.ID, lPtr
               RaiseEvent ItemDblClick(cI)
               bIsClick = True
            End If
         End If
      
         If Not bIsClick Then
            RaiseEvent DblClick
         End If
      
      Case LVN_ITEMCHANGING
         ' Editing the state of an item
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         'CopyMemory tNMLVOD, ByVal lParam, Len(tNMLVOD)
         
      Case LVN_ITEMCHANGED
         ' Edited the state of an item
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         'CopyMemory tNMLVOD, ByVal lParam, Len(tNMLVOD)
      
      Case LVN_INSERTITEM
         ' Notification
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
      
      Case LVN_DELETEITEM
         ' Delete one item.  You would use this to delete
         ' any associated data
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         CopyMemory tNMLV, ByVal lParam, Len(tNMLV)
         If Not tNMLV.lParam = 0 Then
            Set pc = ObjectFromPtr(tNMLV.lParam)
            m_colKeys.Remove pc.Key
            Set iUnk = pc
            iUnk.Release
            tNMLV.lParam = 0
            CopyMemory tNMLV, ByVal lParam, Len(tNMLV)
         End If
      
      Case LVN_DELETEALLITEMS
         ' Delete all items.  You would use this as a shortcut
         ' to delete any associated data for all items
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
      
      Case LVN_BEGINLABELEDIT
         
         'Debug.Print "Begin Label Edit"
         lLen = Len(tNMLVDI)
         If m_lMajor = 4 And m_lMinor <= 70 Then
            lLen = lLen - 4
         End If
         CopyMemory tNMLVDI, ByVal lParam, Len(tNMLVDI)
         Set cI = New cListItem
         Set pc = ObjectFromPtr(tNMLVDI.Item.lParam)
         cI.fInit m_hWnd, pc.ID, tNMLVDI.Item.lParam
         RaiseEvent BeforeLabelEdit(bCancel, cI)
         If bCancel Then
            ISubClass_WindowProc = 1
         Else
            m_bInEdit = True
         End If
         
      Case LVN_ENDLABELEDIT
         'Debug.Print "End Label Edit"
         lLen = Len(tNMLVDI)
         If m_lMajor = 4 And m_lMinor <= 70 Then
            lLen = lLen - 4
         End If
         CopyMemory tNMLVDI, ByVal lParam, lLen
         lLen = lstrlen(tNMLVDI.Item.pszText)
         If lLen > 1 Then ' Includes Null Character; GPF otherwise..
            ReDim b(0 To tNMLVDI.Item.cchTextMax) As Byte
            CopyMemory b(0), ByVal tNMLVDI.Item.pszText, tNMLVDI.Item.cchTextMax - 1
            sText = StrConv(b, vbUnicode)
            iPos = InStr(sText, vbNullChar)
            If iPos > 1 Then
               sText = left$(sText, iPos - 1)
            ElseIf iPos = 1 Then
               sText = ""
            End If
         End If
         Set cI = New cListItem
         Set pc = ObjectFromPtr(tNMLVDI.Item.lParam)
         cI.fInit m_hWnd, pc.ID, tNMLVDI.Item.lParam
         RaiseEvent AfterLabelEdit(bCancel, sText, cI)
         If Not bCancel Then
            ISubClass_WindowProc = 1
         Else
            ISubClass_WindowProc = 0
         End If
         
         m_bInEdit = False
         
      Case LVN_COLUMNCLICK
         '
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         CopyMemory tNMLV, ByVal lParam, Len(tNMLV)
         Dim cH As cColumn
         Set cH = New cColumn
         cH.fInit m_hWnd, fIDForColumnIndex(tNMLV.iSubItem + 1)
         RaiseEvent ColumnClick(cH)
      
      Case LVN_BEGINDRAG
         ' Begin drag
         CopyMemory tNMLV, ByVal lParam, Len(tNMLV)
         m_lDragItem = tNMLV.iItem
         UserControl.OLEDrag
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         
      Case LVN_BEGINRDRAG
         ' Begin Right click drag
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
      
      Case LVN_ITEMACTIVATE
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         Dim tLVIA As NMITEMACTIVATE
         CopyMemory tLVIA, ByVal lParam, Len(tLVIA)
         
      Case LVN_HOTTRACK
         ' Hot tracking
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         
      Case LVN_GETDISPINFO
         '
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
      
      Case LVN_SETDISPINFO
         '
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
      
      Case LVN_KEYDOWN
         Dim tLV As NMLVKEYDOWN
         CopyMemory tLV, ByVal lParam, Len(tLV)
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
      
      Case LVN_MARQUEEBEGIN
         ' "Marquee" is defined as when you draw a bounding box
         ' around the items in the ListView:
         
         ' Return non-zero to stop
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
      
      Case LVN_GETINFOTIP
         ' Info tips:
         Dim tLVIT As NMLVGETINFOTIP_NOSTRING
         Dim sTip As String
         
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         CopyMemory tLVIT, ByVal lParam, Len(tLVIT)
         lPtr = fItemData(tLVIT.iItem + 1)
         If Not lPtr = 0 Then
            Set pc = ObjectFromPtr(lPtr)
            sTip = pc.ToolTipText
            If Len(sTip) > 0 Then
               sTip = sTip & vbNullChar
               tLVIT.cchTextMax = Len(sTip)
               gsInfoTipBuffer = StrConv(sTip, vbFromUnicode)
               tLVIT.pszText = StrPtr(gsInfoTipBuffer)
               CopyMemory ByVal lParam, tLVIT, Len(tLVIT)
            End If
         End If
      Case Else
      
         ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         
      End Select
            
   ' ------------------------------------------------------------------------------
   ' Implement focus.  Many many thanks to Mike Gainer for showing me this
   ' code.
   Case WM_SETFOCUS
      ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
      If (m_hWnd = hwnd) Or (m_bInEdit And (GetFocus() = hWndEdit())) Then
         ' The ListView control:
         Dim pOleObject                  As IOleObject
         Dim pOleInPlaceSite             As IOleInPlaceSite
         Dim pOleInPlaceFrame            As IOleInPlaceFrame
         Dim pOleInPlaceUIWindow         As IOleInPlaceUIWindow
         Dim pOleInPlaceActiveObject     As IOleInPlaceActiveObject
         Dim PosRect                     As RECT
         Dim ClipRect                    As RECT
         Dim FrameInfo                   As OLEINPLACEFRAMEINFO
         Dim grfModifiers                As Long
         Dim AcceleratorMsg              As Msg
         
         'Get in-place frame and make sure it is set to our in-between
         'implementation of IOleInPlaceActiveObject in order to catch
         'TranslateAccelerator calls
         Set pOleObject = Me
         Set pOleInPlaceSite = pOleObject.GetClientSite
         If Not pOleInPlaceSite Is Nothing Then
            pOleInPlaceSite.GetWindowContext pOleInPlaceFrame, pOleInPlaceUIWindow, VarPtr(PosRect), VarPtr(ClipRect), VarPtr(FrameInfo)
            If Not (m_IPAOHookStruct.ThisPointer = 0) Then
               CopyMemory pOleInPlaceActiveObject, m_IPAOHookStruct.ThisPointer, 4
               If Not pOleInPlaceActiveObject Is Nothing Then
                  If Not pOleInPlaceFrame Is Nothing Then
                     pOleInPlaceFrame.SetActiveObject pOleInPlaceActiveObject, vbNullString
                     If Not pOleInPlaceUIWindow Is Nothing Then
                        pOleInPlaceUIWindow.SetActiveObject pOleInPlaceActiveObject, vbNullString
                     End If
                  End If
               End If
               CopyMemory pOleInPlaceActiveObject, 0&, 4
            End If
         End If
      Else
         ' THe user control:
         SetFocusAPI m_hWnd
      End If
      
   Case WM_MOUSEACTIVATE
      If m_bInEdit Then
         If (GetFocus() = m_hWnd Or GetFocus() = hWndEdit()) Then
            ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         Else
            SetFocusAPI m_hWndParent
            ISubClass_WindowProc = MA_NOACTIVATE
         End If
      Else
         If Not (GetFocus() = m_hWnd) Then
            SetFocusAPI m_hWndParent
            ISubClass_WindowProc = MA_NOACTIVATE
         Else
            ISubClass_WindowProc = CallOldWindowProc(hwnd, Msg, wParam, lParam)
         End If
      End If
   ' End Implement focus.
   ' ------------------------------------------------------------------------------
   
   
   End Select
   
   Static strKey As String
   
   If Me.SelectedItem Is Nothing Then
     If strKey <> "" Then
       strKey = ""
       RaiseEvent ItemSelected(strKey)
     End If
   Else
     If strKey <> Me.SelectedItem.Key Then
       strKey = Me.SelectedItem.Key
       RaiseEvent ItemSelected(strKey)
     End If
   End If
End Function

Private Function pbInitialise() As Boolean
   m_bRunTime = UserControl.Ambient.UserMode
   If m_bRunTime Then
      m_hWndParent = UserControl.hwnd
      If IsWindow(m_hWndParent) = 0 Then
         Debug.Print "Invalid m_hWndParent:= " & m_hWndParent
      End If
      If pbCreate() Then
         'Debug.Print "RunTime: Created Control"
         ' Cool!
         SetProp m_hWnd, gcObjectProp, ObjPtr(Me)
         pbInitialise = True
      Else
         Debug.Print "Failed to create object."
      End If
   End If
End Function
Private Sub pTerminate()
   '
   pDestroy
   '
End Sub
Public Property Get ItemFromDragData(Data As DataObject) As cListItem
Attribute ItemFromDragData.VB_Description = "Gets the item that's being dragged from the drag DataObject."
Dim lPtr As Long
   lPtr = plPointerFromDragData(Data)
   If Not (lPtr = 0) Then
      Dim pc As pcListItem
      Set pc = ObjectFromPtr(lPtr)
      Dim c As New cListItem
      c.fInit m_hWnd, pc.ID, lPtr
      Set ItemFromDragData = c
   End If
End Property
Private Function plPointerFromDragData(Data As DataObject) As Long
Dim s As String
Dim b() As Byte
   On Error Resume Next
   b = Data.GetData(&HFFFFBF10)
   s = b
   On Error GoTo 0
   Dim iPos As Long
   iPos = InStr(s, "I:")
   If (iPos > 0) Then
      Dim lPtr As Long
      lPtr = CLng(Trim(Mid(s, iPos + 2)))
      plPointerFromDragData = lPtr
   End If
End Function
Private Function plFindNearestItem(tP As POINTAPI) As Long
Dim i As Long
Dim tR As RECT
Dim lx As Long
Dim ly As Long
Dim lDistSq As Long
Dim lMinDistSq As Long
Dim lMinItem As Long

   lMinItem = -1
   lMinDistSq = &H7FFFFFFF
   For i = 1 To fCount
      tR.left = LVIR_BOUNDS
      SendMessage m_hWnd, LVM_GETITEMRECT, i - 1, tR
      lx = tP.X - (tR.left + (tR.right - tR.left) \ 2)
      ly = tP.Y - (tR.top + (tR.bottom - tR.top) \ 2)
      lDistSq = lx * lx + ly * ly
      If (lDistSq < lMinDistSq) Then
         lMinDistSq = lDistSq
         lMinItem = i
      End If
   Next i
   plFindNearestItem = lMinItem
End Function

Private Sub UserControl_InitProperties()
   '
   pbInitialise
   '
End Sub

Private Sub UserControl_OLECompleteDrag(Effect As Long)
   '
   m_cDrag.CompleteDrag
   If Not (m_hImlDrag = 0) Then
      ImageList_Destroy m_hImlDrag
   End If
   
   RaiseEvent OLECompleteDrag(Effect)
   '
End Sub

Private Sub UserControl_OLEDragDrop(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, X As Single, Y As Single)
   '
   m_cDrag.CompleteDrag
   If Not (m_hImlDrag = 0) Then
      ImageList_Destroy m_hImlDrag
   End If
   
   RaiseEvent OLEDragDrop(Data, Effect, Button, Shift, X, Y)
   
   If ((Effect And vbDropEffectMove) = vbDropEffectMove) Then
      Dim lPtr As Long
      lPtr = plPointerFromDragData(Data)
      If Not (lPtr = 0) Then
         ' We have an item:
         Dim tP As POINTAPI
         GetCursorPos tP
         ScreenToClient m_hWnd, tP
         
         If (fGroupsEnabled) Then
            ' Which item are we most close to being over?
            Dim lIndex As Long
            Dim tLVHT As LVHITTESTINFO
            
            lIndex = -1
            LSet tLVHT.pt = tP
            SendMessage m_hWnd, LVM_HITTEST, 0, tLVHT
            If (tLVHT.iItem <= 0) Then
               If (tLVHT.flags And LVHT_NOWHERE) = LVHT_NOWHERE Then
                  lIndex = plFindNearestItem(tP)
               End If
            Else
               lIndex = tLVHT.iItem
            End If
            
            If (lIndex > -1) Then
               If Not (lIndex = m_lDragItem) Then
                  ' we want to swap these items.  First check they're in
                  ' the same groups:
                  Dim lGroupPtrDragItem As Long
                  Dim lGroupPtrNewItem As Long
                                    
                  pGetStyle lIndex - 1, LVIF_GROUPID
                  lGroupPtrNewItem = m_tLV.iGroupId
                  
                  pGetStyle m_lDragItem, LVIF_GROUPID
                  lGroupPtrDragItem = m_tLV.iGroupId

                  
                  If Not (lGroupPtrDragItem = lGroupPtrNewItem) Then
                     ' First move the drag item to the new group:
                     m_tLV.iGroupId = lGroupPtrNewItem
                     pSetIStyle m_lDragItem, LVIF_GROUPID
                  End If
                  
                  ' Now we need to set the position of the item within the group:
                  
               End If
            End If
            
         Else
            ' We just reposition the item:
            SendMessage m_hWnd, LVM_SETITEMPOSITION32, m_lDragItem, tP
         End If
                  
      End If
   End If
   '
End Sub

Private Sub UserControl_OLEDragOver(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, X As Single, Y As Single, state As Integer)
   '
   RaiseEvent OLEDragOver(Data, Effect, Button, Shift, X, Y, state)

   Dim lPtr As Long
   lPtr = plPointerFromDragData(Data)
   If Not (lPtr = 0) Then
      ' We have an internal item:
      Dim tP As POINTAPI
      GetCursorPos tP
      Dim tR As RECT
      GetWindowRect m_hWnd, tR
      Dim lStyle As Long
      lStyle = GetWindowLong(m_hWnd, GWL_STYLE)
      If ((lStyle And WS_VSCROLL) = WS_VSCROLL) Then
         If Abs(tP.Y - tR.top) < 24 Then
            ' scroll up?
            m_cDrag.HideDragImage True
            SendMessageLong m_hWnd, WM_VSCROLL, SB_LINEUP, 0
            m_cDrag.HideDragImage False
         ElseIf Abs(tP.Y - tR.bottom) < 24 Then
            ' scroll down?
            m_cDrag.HideDragImage True
            SendMessageLong m_hWnd, WM_VSCROLL, SB_LINEDOWN, 0
            m_cDrag.HideDragImage False
         End If
      ElseIf ((lStyle And WS_HSCROLL) = WS_HSCROLL) Then
         If Abs(tP.X - tR.left) < 24 Then
            ' scroll left?
            m_cDrag.HideDragImage True
            SendMessageLong m_hWnd, WM_HSCROLL, SB_LINELEFT, 0
            m_cDrag.HideDragImage False
         ElseIf Abs(tP.X - tR.right) < 24 Then
            ' scroll right?
            m_cDrag.HideDragImage True
            SendMessageLong m_hWnd, WM_HSCROLL, SB_LINERIGHT, 0
            m_cDrag.HideDragImage False
         End If
      End If
      If (AutoArrange) Then
         ' We should be able to draw insertion marks, depending
         ' on the view:
         
      End If
   End If
   '
End Sub

Private Sub UserControl_OLEGiveFeedback(Effect As Long, DefaultCursors As Boolean)
   '
   RaiseEvent OLEGiveFeedback(Effect, DefaultCursors)
   m_cDrag.DragDrop
   '
End Sub

Private Sub UserControl_OLESetData(Data As DataObject, DataFormat As Integer)
   '
   RaiseEvent OLESetData(Data, DataFormat)
   '
End Sub

Private Sub UserControl_OLEStartDrag(Data As DataObject, AllowedEffects As Long)
   '
   If Not (m_hImlDrag = 0) Then
      ImageList_Destroy m_hImlDrag
   End If
   
   Dim lPtr As Long
   lPtr = fItemData(m_lDragItem + 1)
   If Not (lPtr = 0) Then
      Dim b() As Byte
      Dim s As String
      s = "I:" & lPtr
      b = s
      Data.SetData b, &HFFFFBF10
      If (m_eOLEDragMode = vbOLEDropAutomatic) Then
         AllowedEffects = vbDropEffectMove Or vbDropEffectCopy
      End If
      RaiseEvent OLEStartDrag(Data, AllowedEffects)
      If Not (AllowedEffects = 0) Then
         Dim tP As POINTAPI
         m_hImlDrag = SendMessage(m_hWnd, LVM_CREATEDRAGIMAGE, m_lDragItem, tP)
         m_cDrag.hImagelist = m_hImlDrag
         m_cDrag.StartDrag 0, -8, -8
      End If
   End If
   '
End Sub

Private Sub UserControl_Paint()
   If m_hWnd = 0 Then
      If Not (lblName.Caption = UserControl.Extender.Name) Then
         lblName.Caption = UserControl.Extender.Name
      End If
   End If
End Sub

Private Sub UserControl_ReadProperties(PropBag As PropertyBag)
   
   pbInitialise
   
   Dim sFnt As New StdFont
   Set UserControl.Font = PropBag.ReadProperty("Font", sFnt)
   ForeColor = PropBag.ReadProperty("ForeColor", vbWindowText)
   BackColor = PropBag.ReadProperty("BackColor", vbWindowBackground)
   View = PropBag.ReadProperty("View", eViewIcon)
   MultiSelect = PropBag.ReadProperty("MultiSelect", False)
   LabelEdit = PropBag.ReadProperty("LabelEdit", True)
   GridLines = PropBag.ReadProperty("GridLines", False)
   FullRowSelect = PropBag.ReadProperty("FullRowSelect", False)
   AutoArrange = PropBag.ReadProperty("AutoArrange", True)
   BackgroundPicture = PropBag.ReadProperty("BackgroundPicture", "")
   Appearance = PropBag.ReadProperty("Appearance", eLV3D)
   BorderStyle = PropBag.ReadProperty("BorderStyle", eLVFixedSingle)
   CheckBoxes = PropBag.ReadProperty("CheckBoxes", False)
   TrackSelect = PropBag.ReadProperty("TrackSelect", False)
   CustomDraw = PropBag.ReadProperty("CustomDraw", True)
   Enabled = PropBag.ReadProperty("Enabled", True)
   FlatScrollBar = PropBag.ReadProperty("FlatScrollBar", False)
   HeaderButtons = PropBag.ReadProperty("HeaderButtons", True)
   HeaderTrackSelect = PropBag.ReadProperty("HeaderTrackSelect", True)
   HideSelection = PropBag.ReadProperty("HideSelection", True)
   InfoTips = PropBag.ReadProperty("InfoTips", True)
   LabelTips = PropBag.ReadProperty("LabelTips", False)
   NoScrollBar = PropBag.ReadProperty("NoScrollBar", False)
   OneClickActivate = PropBag.ReadProperty("OneClickActivate", False)
   OLEDropMode = PropBag.ReadProperty("OLEDropMode", vbOLEDropNone)
   OLEDragMode = PropBag.ReadProperty("OLEDragMode", vbOLEDragManual)
   ItemBorderSelect = PropBag.ReadProperty("ItemBorderSelect", False)
   ScaleMode = PropBag.ReadProperty("ScaleMode", vbTwips)
   DoubleBuffer = PropBag.ReadProperty("DoubleBuffer", False)
   NoColumnHeaders = PropBag.ReadProperty("NoColumnHeaders", False)
   TileBackgroundPicture = PropBag.ReadProperty("TileBackgroundPicture", True)
   BackgroundPictureXOffset = PropBag.ReadProperty("BackgroundPictureXOffset", 0)
   BackgroundPictureYOffset = PropBag.ReadProperty("BackgroundPictureYOffset", 0)
   '
End Sub


Private Sub UserControl_Resize()
Dim tTR As RECT
   If Not m_hWnd = 0 Then
      GetClientRect m_hWndParent, tTR
      SetWindowPos m_hWnd, 0, tTR.left, tTR.top, tTR.right - tTR.left, tTR.bottom - tTR.top, SWP_NOZORDER Or SWP_NOOWNERZORDER
   Else
      lblName.Width = UserControl.ScaleWidth
      If Not (lblName.Caption = UserControl.Extender.Name) Then
         lblName.Caption = UserControl.Extender.Name
      End If
   End If
   RaiseEvent Resize
End Sub

Private Sub UserControl_Initialize()
   
   Set aaa = Me
   
   ' For XP Visual Styles:
   m_hMod = LoadLibrary("shell32.dll")
   InitCommonControls

   ' Attach custom IOleInPlaceActiveObject interface
   Dim IPAO As IOleInPlaceActiveObject
   With m_IPAOHookStruct
      Set IPAO = Me
      CopyMemory .IPAOReal, IPAO, 4
      CopyMemory .TBEx, Me, 4
      .lpVTable = IPAOVTable
      .ThisPointer = VarPtr(m_IPAOHookStruct)
   End With
   
   ' Default properties:
   m_bEnabled = True
   m_bCustomDraw = True
   m_eBorder = eLVFixedSingle
   m_eAppearance = eLV3D
   m_oBackColor = vbWindowBackground
   m_oForeColor = vbWindowText
   m_bTilePicture = True
   
   ComCtlVersion m_lMajor, m_lMinor
   
End Sub

Private Sub UserControl_Terminate()
   ' Detach the custom IOleInPlaceActiveObject interface
   ' pointers.
   With m_IPAOHookStruct
      CopyMemory .IPAOReal, 0&, 4
      CopyMemory .TBEx, 0&, 4
   End With
   pTerminate
   If Not (m_hMod = 0) Then
      FreeLibrary m_hMod
   End If
   
   Set aaa = Nothing
End Sub

Private Sub UserControl_WriteProperties(PropBag As PropertyBag)
   '
   Dim sFnt As New StdFont
   PropBag.WriteProperty "Font", UserControl.Font, sFnt
   PropBag.WriteProperty "ForeColor", ForeColor, vbWindowText
   PropBag.WriteProperty "BackColor", BackColor, vbWindowBackground
   PropBag.WriteProperty "View", View, eViewIcon
   PropBag.WriteProperty "MultiSelect", MultiSelect, False
   PropBag.WriteProperty "LabelEdit", LabelEdit, True
   PropBag.WriteProperty "GridLines", GridLines, False
   PropBag.WriteProperty "FullRowSelect", FullRowSelect, False
   PropBag.WriteProperty "AutoArrange", AutoArrange, True
   PropBag.WriteProperty "BackgroundPicture", BackgroundPicture, ""
   PropBag.WriteProperty "Appearance", Appearance, eLV3D
   PropBag.WriteProperty "BorderStyle", BorderStyle, eLVFixedSingle
   PropBag.WriteProperty "CheckBoxes", CheckBoxes, False
   PropBag.WriteProperty "TrackSelect", TrackSelect, False
   PropBag.WriteProperty "CustomDraw", CustomDraw, True
   PropBag.WriteProperty "Enabled", Enabled, True
   PropBag.WriteProperty "FlatScrollBar", FlatScrollBar, False
   PropBag.WriteProperty "HeaderButtons", HeaderButtons, True
   PropBag.WriteProperty "HeaderTrackSelect", HeaderTrackSelect, True
   PropBag.WriteProperty "HideSelection", HideSelection, True
   PropBag.WriteProperty "InfoTips", InfoTips, True
   PropBag.WriteProperty "LabelTips", LabelTips, False
   PropBag.WriteProperty "NoScrollBar", NoScrollBar, False
   PropBag.WriteProperty "OneClickActivate", OneClickActivate, False
   PropBag.WriteProperty "OLEDropMode", OLEDropMode, vbOLEDropNone
   PropBag.WriteProperty "OLEDragMode", OLEDragMode, vbOLEDragManual
   PropBag.WriteProperty "ItemBorderSelect", ItemBorderSelect, False
   PropBag.WriteProperty "ScaleMode", ScaleMode, vbTwips
   PropBag.WriteProperty "DoubleBuffer", DoubleBuffer, False
   PropBag.WriteProperty "NoColumnHeaders", NoColumnHeaders, False
   PropBag.WriteProperty "TileBackgroundPicture", TileBackgroundPicture, True
   PropBag.WriteProperty "BackgroundPictureXOffset", BackgroundPictureXOffset, 0
   PropBag.WriteProperty "BackgroundPictureYOffset", BackgroundPictureYOffset, 0
   '
End Sub

Public Sub Clear()
  Set aaa = Nothing
End Sub
