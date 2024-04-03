VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cCommandBarInt"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

' cComamndBarInt is the real cCommandBar object.
' It holds a collection of object pointers to cCommandBarItemInt
' objects as well as a collection of each of the controls which
' own this command bar.

Private m_colhWndUser As Collection
Private m_colhWndParent As Collection

Private m_sKey As String
Private m_sTitle As String
Private m_colItems As Collection

Friend Function AcceleratorMatches( _
      ByVal hWndActiveForm As Long, _
      ByVal vKey As Integer, _
      ByVal shiftMask As Long, _
      ctlNotify As vbalCommandBar _
   ) As cButtonInt
Dim vlPtr As Variant
Dim btn As cButtonInt
Dim cMatch As cButtonInt
   For Each vlPtr In m_colItems
      Set btn = ObjectFromPtr(vlPtr)
      If Not (btn Is Nothing) Then
         Set cMatch = btn.AcceleratorMatches(hWndActiveForm, vKey, shiftMask, True, ctlNotify)
         If Not (cMatch Is Nothing) Then
            Set AcceleratorMatches = cMatch
            Exit Function
         End If
      End If
   Next
End Function

Friend Function AddRefhWnd(ByVal hWnd As Long, ByVal hWndParent As Long)
   If Not (CollectionContains(m_colhWndUser, "H" & hWnd)) Then
      m_colhWndUser.Add hWnd, "H" & hWnd
      m_colhWndParent.Add hWndParent, "H" & hWnd
      ' Ensure that the buttons get a reference to the parent
      ' form:
      Dim vlPtr As Variant
      Dim btn As cButtonInt
      For Each vlPtr In m_colItems
         Set btn = ObjectFromPtr(vlPtr)
         If Not (btn Is Nothing) Then
            btn.AddParenthWnd hWnd, hWndParent
         End If
      Next vlPtr
   End If
End Function
Friend Function ReleaseRef(ByVal hWnd As Long)
Dim hWndParent As Long
   hWndParent = m_colhWndParent("H" & hWnd)
   m_colhWndUser.Remove "H" & hWnd
   m_colhWndParent.Remove "H" & hWnd
   ' Remove the parent form for the buttons:
   Dim vlPtr As Variant
   For Each vlPtr In m_colItems
      Dim btn As cButtonInt
      Set btn = ObjectFromPtr(vlPtr)
      If Not (btn Is Nothing) Then
         btn.RemoveParenthWnd hWndParent
      End If
   Next vlPtr
End Function
Friend Sub NotifyUsers(ByVal eventType As Long, Optional Item As cButtonInt = Nothing)
Dim vHwnd As Variant
Dim ctlCmdBar As vbalCommandBar
Static noRecurse As Boolean


   If Not (Item Is Nothing) Then
      If (eventType = CHANGENOTIFICATIONBUTTONCHECKCHANGE) Then
         
         If Not noRecurse Then
         
            Dim bThisIsChecked As Boolean
            Dim bFoundCheckedItem As Boolean
            Dim vlPtr As Variant
            Dim btnInt As cButtonInt
            Dim iIndex As Long
            Dim iFirstIndex As Long
            Dim iLastIndex As Long
            Dim iBtnIndex As Long
            Dim iCheckCount As Long
            Dim bFoundButton As Boolean
            
            ' Confirm which other items to
            ' change check state:
            If (Item.Style = eRadio) Or (Item.Style = eRadioNullable) Then
                           
               bThisIsChecked = Item.Checked
               bFoundCheckedItem = bThisIsChecked
               
               noRecurse = True
               
               For Each vlPtr In m_colItems
                  iIndex = iIndex + 1
                  Set btnInt = ObjectFromPtr(vlPtr)
                  If (btnInt.Key = Item.Key) Then
                     bFoundButton = True
                     iBtnIndex = iIndex
                     iLastIndex = iIndex
                  Else
                     If (bFoundButton) Then
                        If (btnInt.Style = eSeparator) Then
                           iLastIndex = iIndex - 1
                           Exit For
                        Else
                           iLastIndex = iIndex
                        End If
                     Else
                        If (iFirstIndex = 0) Then
                           iFirstIndex = iIndex
                        ElseIf (btnInt.Style = eSeparator) Then
                           iFirstIndex = iIndex + 1
                        End If
                     End If
                  End If
               Next
               
               If (iFirstIndex = 0) Then
                  iFirstIndex = 1
               End If
               
               If (Item.Style = eRadioNullable) Then
                  ' any other items in the same group
                  ' must be unchecked
                  If (Item.Checked) Then
                     For iIndex = iFirstIndex To iLastIndex
                        If (iIndex <> iBtnIndex) Then
                           Set btnInt = ObjectFromPtr(m_colItems(iIndex))
                           btnInt.Checked = False
                        End If
                     Next iIndex
                  End If
                  
               ElseIf (Item.Style = eRadio) Then
                  ' any other items in the same group must
                  ' be unchecked; if no items are checked
                  ' then this item must be rechecked.
                  If (Item.Checked) Then
                     For iIndex = iFirstIndex To iLastIndex
                        If Not (iIndex = iBtnIndex) Then
                           Set btnInt = ObjectFromPtr(m_colItems(iIndex))
                           btnInt.Checked = False
                        End If
                     Next iIndex
                  Else
                     For iIndex = iFirstIndex To iLastIndex
                        Set btnInt = ObjectFromPtr(m_colItems(iIndex))
                        iCheckCount = iCheckCount + Abs(btnInt.Checked)
                     Next iIndex
                     If (iCheckCount = 0) Then
                        Item.Checked = True
                     End If
                  End If
                  
               End If
               
               noRecurse = False
            End If
                     
         End If
      End If
   End If

   For Each vHwnd In m_colhWndUser
      If ControlFromhWnd(vHwnd, ctlCmdBar) Then
         If (ctlCmdBar.fInUse) Then
            ctlCmdBar.ChangeNotification Me, eventType, Item
         End If
      End If
   Next
   
End Sub
Friend Sub fInit(ByVal sKey As String)
   m_sKey = sKey
End Sub
Friend Property Get Key() As String
   Key = m_sKey
End Property
Friend Property Get IndexOf(ByVal sBtnKey As String) As Long
Dim iItem As Long
Dim vlPtr As Variant
Dim btnInt As cButtonInt
   For Each vlPtr In m_colItems
      iItem = iItem + 1
      Set btnInt = ObjectFromPtr(vlPtr)
      If (btnInt.Key = sBtnKey) Then
         IndexOf = iItem
         Exit For
      End If
   Next
End Property
Friend Property Get Title() As String
   Title = m_sTitle
End Property
Friend Property Let Title(ByVal sTitle As String)
   m_sTitle = sTitle
   NotifyUsers 3
End Property
Friend Property Get Count() As Long
   Count = m_colItems.Count
End Property
Friend Property Get Item(ByVal index As Variant) As cButtonInt
Dim lPtr As Long
   lPtr = m_colItems(index)
   If Not (lPtr = 0) Then
      Set Item = ObjectFromPtr(lPtr)
   End If
End Property
Friend Sub Add(button As cButtonInt)
Dim sKey As String
   sKey = button.Key
   If CollectionContains(m_colItems, sKey) Then
      gErr 5
   Else
      m_colItems.Add ObjPtr(button), sKey
      button.AddedToBar Me
      NotifyUsers 1
   End If
End Sub
Friend Sub InsertBefore(button As cButtonInt, buttonBefore As cButtonInt)
   If CollectionContains(m_colItems, button.Key) Then
      gErr 5
   Else
      If Not (CollectionContains(m_colItems, buttonBefore.Key)) Then
         gErr 3
      Else
         m_colItems.Add ObjPtr(button), button.Key, buttonBefore.Key
         Dim i As Long
         For i = 1 To m_colhWndUser.Count
            button.AddParenthWnd m_colhWndUser(i), m_colhWndParent(i)
         Next i
         button.AddedToBar Me
         NotifyUsers 1
      End If
   End If
End Sub
Friend Sub InsertAfter(button As cButtonInt, buttonAfter As cButtonInt)
   If CollectionContains(m_colItems, button.Key) Then
      gErr 5
   Else
      If Not (CollectionContains(m_colItems, buttonAfter.Key)) Then
         gErr 3
      Else
         m_colItems.Add ObjPtr(button), button.Key, , buttonAfter.Key
         Dim i As Long
         For i = 1 To m_colhWndUser.Count
            button.AddParenthWnd m_colhWndUser(i), m_colhWndParent(i)
         Next i
         button.AddedToBar Me
         NotifyUsers 1
      End If
   End If
End Sub
Friend Sub Remove(button As cButtonInt)
   If CollectionContains(m_colItems, button.Key) Then
      Dim vlhWnd As Variant
      For Each vlhWnd In m_colhWndParent
         button.RemoveParenthWnd vlhWnd
      Next vlhWnd
      m_colItems.Remove button.Key
      button.RemovedFromBar Me
      NotifyUsers 1
   Else
      gErr 3
   End If
End Sub
Friend Sub Clear()
Dim vlPtr As Variant
Dim itm As cButtonInt
   For Each vlPtr In m_colItems
      Set itm = ObjectFromPtr(vlPtr)
      Dim vlhWnd As Variant
      For Each vlhWnd In m_colhWndParent
         itm.RemoveParenthWnd vlhWnd
      Next vlhWnd
      If Not (itm Is Nothing) Then
         itm.RemovedFromBar Me
      End If
   Next
   Set m_colItems = New Collection
   NotifyUsers 1
End Sub
'Friend Sub CalculateMenuWithVisibleCheckSize( _
'      cMP As cMeasureButtonParams, _
'      ByRef menuWidth As Long, _
'      ByRef menuHeight As Long, _
'      ByRef Item() As cDisplayButtonInfo _
'   )
'
'   ' basically same as calculate menu size,
'   ' but we add an extra check width and ignore
'   ' if items are marked as invisible.
'   CalculateMenuSize cMP, menuWidth, menuHeight, Item(), True
'
'   menuWidth = menuWidth + cMP.IconHeight + 8
'   Dim iItem As Long
'   For iItem = 1 To m_colItems.Count
'      Item(iItem).Right = menuWidth
'   Next iItem
'
'End Sub
Friend Sub CalculateMenuSize( _
      cMP As cMeasureButtonParams, _
      ByRef menuWidth As Long, _
      ByRef menuHeight As Long, _
      ByRef Item() As cDisplayButtonInfo, _
      Optional ByVal bIncludeInvisible As Boolean = False _
   )
   
   '
   menuWidth = 0
   menuHeight = 0
   
   If (m_colItems.Count > 0) Then
      
      ' ensure we calculate the required height:
      cMP.Height = 0
      
      Dim vlPtr As Variant
      Dim btnInt As cButtonInt
      Dim maxWidth As Long
      Dim btnWidth As Long
      Dim btnHeight As Long
      Dim iItem As Long
      Dim bLastHadNoCaption As Boolean
      Dim iConsecutiveNoCaptionItems As Long
      Dim iMaxConsecutiveNoCaptionItems As Long
      Dim iNoCaptionWidth As Long
      Dim lHeight As Long
      Dim x As Long
      Dim y As Long
      
      
      For Each vlPtr In m_colItems
         
         iItem = iItem + 1
         btnWidth = 0
         btnHeight = 0
      
         Set btnInt = ObjectFromPtr(vlPtr)
         If Not (btnInt Is Nothing) Then
            If (btnInt.Visible Or bIncludeInvisible) Then
               If Not (btnInt.InfrequentlyUsed) Or mCommandBars.ShowingInfrequentlyUsed Or bIncludeInvisible Then
                  
                  cMP.Height = 0
                  btnWidth = btnInt.Size(cMP, Item(iItem).Hidden)
                  btnHeight = cMP.Height
                  If (btnWidth > maxWidth) Then
                     maxWidth = btnWidth
                  End If
                  
                  If Len(btnInt.Caption) = 0 And Not (btnInt.Style = eSeparator) Then
                     bLastHadNoCaption = True
                     iConsecutiveNoCaptionItems = iConsecutiveNoCaptionItems + 1
                     iNoCaptionWidth = btnWidth
                  Else
                     bLastHadNoCaption = False
                     If (iConsecutiveNoCaptionItems > iMaxConsecutiveNoCaptionItems) Then
                        iMaxConsecutiveNoCaptionItems = iConsecutiveNoCaptionItems
                     End If
                     iConsecutiveNoCaptionItems = 0
                  End If
                  
               End If
            End If
         End If
            
         Item(iItem).left = 0
         If (menuHeight = 0) Then
            menuHeight = 1
         End If
         Item(iItem).top = menuHeight
         menuHeight = menuHeight + btnHeight
         Item(iItem).bottom = menuHeight
         Item(iItem).right = btnWidth
      
      Next vlPtr
            
      If (iMaxConsecutiveNoCaptionItems > 2) And Not (bIncludeInvisible) Then
         ' Recalc to place the items:
         ' How many items can we fit into the suggested menu width?
         If (maxWidth < iNoCaptionWidth * 8 + 28) Then
            maxWidth = iNoCaptionWidth * 8 + 28 ' TODO fix magic number
         End If
         menuWidth = maxWidth
      
         iItem = 0
         bLastHadNoCaption = False
         iConsecutiveNoCaptionItems = 0
         y = 1
         
         For Each vlPtr In m_colItems
            
            iItem = iItem + 1
         
            Set btnInt = ObjectFromPtr(vlPtr)
            If Not (btnInt Is Nothing) Then
               If (btnInt.Visible) Then
                  If Not (btnInt.InfrequentlyUsed) Or mCommandBars.ShowingInfrequentlyUsed Then
            
                     If Len(btnInt.Caption) = 0 And Not (btnInt.Style = eSeparator) Then
                        If Not (bLastHadNoCaption) Then
                           bLastHadNoCaption = True
                        End If
                        If (iConsecutiveNoCaptionItems Mod 8) = 0 Then
                           If (iItem > 1) Then
                              y = y + Item(iItem - 1).bottom - Item(iItem - 1).top
                           End If
                           x = 26 ' TODO fix magic number
                        Else
                           x = x + iNoCaptionWidth
                        End If
                        Item(iItem).left = x
                        Item(iItem).right = x + iNoCaptionWidth
                        lHeight = Item(iItem).bottom - Item(iItem).top
                        Item(iItem).top = y
                        Item(iItem).bottom = y + lHeight
                        iConsecutiveNoCaptionItems = iConsecutiveNoCaptionItems + 1
                     Else
                        bLastHadNoCaption = False
                        iConsecutiveNoCaptionItems = 0
                        Item(iItem).right = menuWidth
                        lHeight = Item(iItem).bottom - Item(iItem).top
                        If (iItem > 1) Then
                           y = y + Item(iItem - 1).bottom - Item(iItem - 1).top
                        End If
                        Item(iItem).top = y
                        Item(iItem).bottom = y + lHeight
                     End If
                  End If
               End If
            End If
            
         Next vlPtr
         
         menuHeight = y + Item(m_colItems.Count).bottom - Item(m_colItems.Count).top
         
      Else
      
         menuWidth = maxWidth
         ' Set the width for the items:
         For iItem = 1 To m_colItems.Count
            Item(iItem).right = menuWidth
         Next
         
      End If
      
   End If
   '
End Sub
Friend Sub Draw( _
      cDP As cDrawButtonParams, _
      Item() As cDisplayButtonInfo _
   )
   If (m_colItems.Count > 0) Then
      
      Dim vlPtr As Variant
      Dim btnInt As cButtonInt
      Dim iItem As Long
      Dim bVisible As Boolean
      
      For Each vlPtr In m_colItems
         iItem = iItem + 1
         Set btnInt = ObjectFromPtr(vlPtr)
         If Not (btnInt Is Nothing) Then
            bVisible = btnInt.Visible And Not (Item(iItem).Hidden)
            If Not (bVisible) Then
               If (cDP.SizeStyle = COMMANDBARSIZESTYLEMENUVISIBLECHECK) Then
                  bVisible = Not (btnInt.VisibleCheck = vbGrayed)
               End If
            End If
            
            If (bVisible And (Item(iItem).bottom - Item(iItem).top) > 0) Then
               cDP.left = Item(iItem).left
               cDP.top = Item(iItem).top
               cDP.Size = Item(iItem).right - Item(iItem).left
               cDP.Height = Item(iItem).bottom - Item(iItem).top
               cDP.MouseDownButton = Item(iItem).mouseDown
               cDP.MouseOverButton = Item(iItem).mouseOver
               cDP.MouseDownSplit = Item(iItem).MouseDownSplit
               cDP.MouseOverSplit = Item(iItem).MouseOverSplit
               cDP.ShowingMenu = Item(iItem).ShowingMenu
               btnInt.Draw cDP
            End If
         End If
      Next
   End If
End Sub
Friend Sub DrawOneButton( _
      cDP As cDrawButtonParams, _
      itemIndex As Long _
   )
On Error Resume Next
Dim vlPtr As Variant
Dim btnInt As cButtonInt
   vlPtr = m_colItems(itemIndex)
   Set btnInt = ObjectFromPtr(vlPtr)
   If Not (btnInt Is Nothing) Then
      btnInt.Draw cDP
   End If
End Sub
Friend Sub ClickButton( _
      index As Long _
   )
   If (Item(index).VisibleCheck = vbGrayed) Then
      If (Item(index).Style = eCheck) Or (Item(index).Style = eRadio) Or (Item(index).Style = eRadioNullable) Then
         ' this will cause notify users to be called.
          Item(index).Checked = Not (Item(index).Checked)
      Else
         '
      End If
   Else
      Item(index).VisibleCheck = IIf(Item(index).VisibleCheck = vbChecked, vbUnchecked, vbChecked)
      Item(index).Checked = Not (Item(index).Checked)
   End If
   
End Sub
Friend Sub CalculateToolbarSize( _
      cMP As cMeasureButtonParams, _
      ByRef toolbarWidth As Long, _
      ByRef toolbarHeight As Long, _
      ByRef Item() As cDisplayButtonInfo _
   )
   
   ' The toolbar width is constrained.  We calculate
   ' the desired height for a single button and also
   ' hide buttons with lower priorities if there isn't
   ' enough room.  We also return the actual width
   ' needed for the bar in the toolbarWidth param
   '
   ' The first loop calculates the total width we end up with
   '
   
   Dim availableWidth As Long
   Dim availableHeight As Long
   Dim availableSize As Long
   
   availableWidth = toolbarWidth
   availableHeight = toolbarHeight
   If (cMP.Orientation = eLeft) Or (cMP.Orientation = eRight) Then
      availableSize = availableHeight
   Else
      availableSize = availableWidth
   End If
   toolbarWidth = 0
   toolbarHeight = 0
   
   If (m_colItems.Count > 0) Then
      
      'cMP.Height = 0
      
      Dim vlPtr As Variant
      Dim btnInt As cButtonInt
      Dim btnWidth As Long
      Dim btnHeight As Long
      Dim iItem As Long
      Dim iReHide As Long
      Dim lastRight As Long
      Dim offset As Long
      Dim eLastStyle As EButtonStyle
      Dim lastBtnWidth As Long
      Dim bResizeAfterHiding As Boolean
      Dim removedSize As Long
      Dim removedSizeTarget As Long
      Dim hitTarget As Boolean
      Dim additionalSize As Long
      Dim additionalSizeTarget As Long
      Dim possibleAdditionalSize As Long
      Dim possibleUnHidden() As Long
      Dim possibleUnHiddenCount As Long
      Dim hasHidden As Boolean
      Dim hasVisibleSeparators As Boolean
      Dim toolbarRowHeight() As Long
      Dim toolbarRow As Long
      Dim iRow As Long
            
      Do
         If (cMP.RightToLeft) Then
            lastRight = availableSize
         Else
            lastRight = 0
         End If
      
         eLastStyle = 0
         toolbarWidth = 0
         toolbarHeight = 0
         lastBtnWidth = 0
         iItem = 0
         
         toolbarRow = 1
         ReDim toolbarRowHeight(1 To 1) As Long
         toolbarRowHeight(1) = 0

         For Each vlPtr In m_colItems
            iItem = iItem + 1
            
            Set btnInt = ObjectFromPtr(vlPtr)
            If Not (btnInt Is Nothing) Then
               If (Item(iItem).Hidden) Then
                  hasHidden = True
               End If
               If (btnInt.Visible And Not Item(iItem).Hidden) Then
                  If ((eLastStyle = eSeparator) And (btnInt.Style = eSeparator)) Then
                     btnWidth = 0
                     btnHeight = 0
                  Else
                     If (btnInt.Style = eSeparator) Then
                        hasVisibleSeparators = True
                     End If
                     btnWidth = btnInt.Size(cMP, Item(iItem).Hidden)
                     btnHeight = cMP.Height
                     If (btnHeight > toolbarRowHeight(toolbarRow)) Then
                        toolbarRowHeight(toolbarRow) = btnHeight
                     End If
                  End If
               Else
                  btnWidth = 0
                  btnHeight = 0
               End If
            End If
            lastBtnWidth = btnWidth
            If Not (lastBtnWidth = 0) Then
               eLastStyle = btnInt.Style
            End If
   
            If (cMP.Orientation = eLeft) Or (cMP.Orientation = eRight) Then
               Item(iItem).top = lastRight
               lastRight = lastRight + btnWidth
               Item(iItem).bottom = lastRight
               Item(iItem).left = 0
               Item(iItem).right = btnHeight
            Else
               If (cMP.RightToLeft) Then
                  Item(iItem).right = lastRight
                  lastRight = Item(iItem).right - btnWidth
                  If (lastRight < 0) And (cMP.SizeStyle = COMMANDBARSIZESTYLETOOLBARMENU) Then
                     toolbarRow = toolbarRow + 1
                     ReDim Preserve toolbarRowHeight(1 To toolbarRow) As Long
                     toolbarRowHeight(toolbarRow) = btnHeight
                     Item(iItem).left = availableSize
                     lastRight = availableSize - btnWidth
                  End If
                  Item(iItem).left = lastRight
               Else
                  Item(iItem).left = lastRight
                  lastRight = Item(iItem).left + btnWidth
                  If (lastRight >= availableSize) And (cMP.SizeStyle = COMMANDBARSIZESTYLETOOLBARMENU) Then
                     toolbarRow = toolbarRow + 1
                     ReDim Preserve toolbarRowHeight(1 To toolbarRow) As Long
                     toolbarRowHeight(toolbarRow) = btnHeight
                     Item(iItem).left = 0
                     lastRight = btnWidth
                  End If
                  Item(iItem).right = lastRight
               End If
               Item(iItem).top = 0
               For iRow = 1 To toolbarRow - 1
                  Item(iItem).top = Item(iItem).top + toolbarRowHeight(iRow)
               Next iRow
               Item(iItem).bottom = btnHeight + Item(iItem).top
            End If
         Next
         
         If (toolbarWidth < lastRight) Then
            toolbarWidth = lastRight
         End If
               
         ' TODO: button priority
         If Not (bResizeAfterHiding) And Not (cMP.SizeStyle = COMMANDBARSIZESTYLETOOLBARMENU) Then
            If (toolbarWidth > availableSize) Then
               ' start making items invisible working
               ' backwards from the end downwards:
               removedSizeTarget = toolbarWidth - availableSize
               For iItem = m_colItems.Count To 1 Step -1
                  vlPtr = m_colItems(iItem)
                  Set btnInt = ObjectFromPtr(vlPtr)
                  If Not (btnInt Is Nothing) Then
                     If (btnInt.Visible And Not Item(iItem).Hidden) Then
                        If Not (hitTarget) Then
                           Item(iItem).Hidden = True
                           If Not (btnInt.PanelControl Is Nothing) Then
                              btnInt.PanelControl.Visible = False
                           End If
                           removedSize = removedSize + Item(iItem).right - Item(iItem).left
                           If (removedSize >= removedSizeTarget) Then
                              hitTarget = True
                              If Not (hasVisibleSeparators) Then
                                 Exit For
                              End If
                           End If
                        Else
                           If (btnInt.Style = eSeparator) Then
                              Item(iItem).Hidden = True
                              If Not (btnInt.PanelControl Is Nothing) Then
                                 btnInt.PanelControl.Visible = False
                              End If
                              Exit For
                           Else
                              Item(iItem).Hidden = True
                              If Not (btnInt.PanelControl Is Nothing) Then
                                 btnInt.PanelControl.Visible = False
                              End If
                           End If
                        End If
                     End If
                  End If
               Next iItem
               bResizeAfterHiding = True
            
            ElseIf (hasHidden) Then
               ' Start making items visible working
               ' forwards from the start:
               additionalSizeTarget = availableSize - toolbarWidth
               For iItem = 1 To m_colItems.Count
                  vlPtr = m_colItems(iItem)
                  Set btnInt = ObjectFromPtr(vlPtr)
                  If Not (btnInt Is Nothing) Then
                     If (btnInt.Visible) Then
                        If (Item(iItem).Hidden) Then
                           If (btnInt.Style = eSeparator And possibleUnHiddenCount > 0) Then
                              ' its all good
                              Exit For
                           Else
                              Item(iItem).Hidden = False
                              possibleAdditionalSize = btnInt.Size(cMP, Item(iItem).Hidden)
                              If (additionalSize + possibleAdditionalSize <= additionalSizeTarget) Then
                                 possibleUnHiddenCount = possibleUnHiddenCount + 1
                                 ReDim Preserve possibleUnHidden(1 To possibleUnHiddenCount) As Long
                                 possibleUnHidden(possibleUnHiddenCount) = iItem
                                 additionalSize = additionalSize + possibleAdditionalSize
                              Else
                                 Item(iItem).Hidden = True
                                 If Not (btnInt.PanelControl Is Nothing) Then
                                    btnInt.PanelControl.Visible = False
                                 End If
                                 If (hasVisibleSeparators) Then
                                    For iReHide = 1 To possibleUnHiddenCount
                                       Set btnInt = ObjectFromPtr(m_colItems(possibleUnHidden(iReHide)))
                                       If Not (btnInt Is Nothing) Then
                                          Item(iItem).Hidden = True
                                          If Not (btnInt.PanelControl Is Nothing) Then
                                             btnInt.PanelControl.Visible = False
                                          End If
                                       End If
                                    Next iReHide
                                 End If
                                 Exit For
                              End If
                           End If
                        End If
                     End If
                  End If
               Next iItem
               bResizeAfterHiding = True
            End If
         Else
            bResizeAfterHiding = False
         End If
      Loop While bResizeAfterHiding
      
      toolbarHeight = 0
      For iRow = 1 To toolbarRow
         toolbarHeight = toolbarHeight + toolbarRowHeight(iRow)
      Next iRow
      
      ' If we're a vertical toolbar then there can be a difference
      ' between an individual button's width and the width of the
      ' toolbar as a whole:
      If (cMP.Orientation = eLeft Or cMP.Orientation = eRight) Then
         iItem = 0
         For Each vlPtr In m_colItems
            iItem = iItem + 1
            Set btnInt = ObjectFromPtr(vlPtr)
            If Not (btnInt Is Nothing) Then
               If btnInt.Style = eSeparator Then
                  Item(iItem).right = toolbarHeight
               Else
                  If (Item(iItem).right - Item(iItem).left < toolbarHeight) Then
                     ' centre:
                     offset = (toolbarHeight - (Item(iItem).right - Item(iItem).left)) / 2
                     Item(iItem).left = Item(iItem).left + offset
                     Item(iItem).right = Item(iItem).right + offset
                  End If
               End If
            End If
         Next
      End If
   End If
   
   
   '
   
End Sub
Friend Sub CalculateWrappableToolbarSize( _
      cMP As cMeasureButtonParams, _
      ByRef toolbarWidth As Long, _
      ByRef toolbarHeight As Long, _
      ByRef Item() As cDisplayButtonInfo _
   )
   
   ' The width is kind of constrained but it will be
   ' adjusted to the nearest snap position if we need
   ' to wrap
   
   
      
End Sub

Friend Sub Dispose()
   Set m_colItems = New Collection
   Set m_colhWndUser = New Collection
End Sub

Private Sub Class_Initialize()
   Set m_colhWndUser = New Collection
   Set m_colhWndParent = New Collection
   Set m_colItems = New Collection
End Sub

Private Sub Class_Terminate()
   Clear
End Sub
