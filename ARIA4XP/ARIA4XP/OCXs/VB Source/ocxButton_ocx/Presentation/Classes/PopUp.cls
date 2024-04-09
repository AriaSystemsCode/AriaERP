VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "PopUp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"Bar"
Attribute VB_Ext_KEY = "Member0" ,"Bar"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

Public Event Click(ByRef objBar As Bar)
Public Event MouseOver(ByRef objBar As Bar)
Public Event MenuBarsInitialized()
Public Event MenuBarsRemoved()

Private mColMain            As Collection
Private WithEvents mobjMenu As vbpAriaPMenu.cPopupMenu
Attribute mobjMenu.VB_VarHelpID = -1
Private mlngParenthWnd      As Long
Private mobjImageList       As ImageList

Friend Property Get ParenthWnd() As Long
  ParenthWnd = mlngParenthWnd
End Property

Friend Property Let ParenthWnd(ByVal hWnd As Long)
  mlngParenthWnd = hWnd
End Property

Friend Property Get ImageList() As Long
  ImageList = mobjImageList
End Property

Friend Property Set ImageList(ByRef objImageList As ImageList)
  Set mobjImageList = objImageList
  Call InitImageList
End Property

Private Sub InitImageList()
  With mobjImageList
    .ListImages.Clear
    .ImageHeight = 16
    .ImageWidth = 16
    .ListImages.Add , "TICK", LoadResPicture("TICK", vbResIcon)
  End With
End Sub

Private Function CreateMenu() As Boolean
  Dim bolRetValue As Boolean
  If mobjMenu Is Nothing Then
    If mlngParenthWnd > -1 Then
      Set mobjMenu = New vbpAriaPMenu.cPopupMenu
      
      With mobjMenu
        .Clear
        .hWndOwner = mlngParenthWnd
        .ImageList = mobjImageList
        .TickIconIndex = 0
      End With
      
      bolRetValue = True
    End If
  Else
    bolRetValue = True
  End If
  CreateMenu = bolRetValue
End Function

Private Function AbbMenuBar(ByRef objBar As Bar, _
                            ByVal strParentKey As String, _
                            ByVal strKeyBefore As String) As Boolean
  Dim bolRetValue As Boolean
  
  If CreateMenu() Then
    Dim lngParentIndex As Long
    If Len(Trim(strParentKey)) > 0 Then
      lngParentIndex = mobjMenu.IndexForKey(strParentKey)
    End If
    
    If Len(Trim(strKeyBefore)) = 0 Then
      objBar.Index = mobjMenu.AddItem(objBar.CaptionWithShortCut, , , _
                                      lngParentIndex, _
                                      objBar.IconIndex, _
                                      objBar.Checked, _
                                      objBar.Enabled, _
                                      objBar.Key)
      bolRetValue = True
    Else
      If Not Item(strKeyBefore) Is Nothing Then
        objBar.Index = mobjMenu.InsertItem(objBar.CaptionWithShortCut, strKeyBefore, , , _
                                           objBar.IconIndex, _
                                           objBar.Checked, _
                                           objBar.Enabled, _
                                           objBar.Key)
        bolRetValue = True
      End If
    End If
  End If
  
  AbbMenuBar = bolRetValue
End Function

Public Function Show(ByVal lngLeft As Long, ByVal lngTop As Long) As String
  Show = ""
  If Not mobjMenu Is Nothing Then
    Dim lngIndex As Long
    lngIndex = mobjMenu.ShowPopupAbsolute(lngLeft, lngTop)
    If lngIndex > 0 Then
      Show = mobjMenu.ItemKey(lngIndex)
    End If
  End If
End Function

Public Function Add(ByVal strKey As String, _
                    Optional ByVal strCaption As String = "", _
                    Optional ByVal bolChecked As Boolean = False, _
                    Optional ByVal bolEnabled As Boolean = True, _
                    Optional ByVal strParentKey As String = "", _
                    Optional ByVal picIcon As StdPicture = Nothing, _
                    Optional ByVal strKeyBefore As String = "") As Bar
  
' commented the shortcut because of the GPF
'Optional ByVal strShortCut As String = "", _

  Dim objNewMember As Bar
  If Len(Trim(strKey)) > 0 And Item(strKey) Is Nothing Then
    
    If CreateMenu() Then
      ' Create a new object
      Set objNewMember = New Bar
      
      ' Set the properties passed into the method
      With objNewMember
        .Key = strKey
        Set .ImageList = mobjImageList
        Set .Menu = mobjMenu
        strCaption = Trim(Replace(strCaption, vbTab, Space(0), 1, -1, vbTextCompare))
        .Caption = strCaption
        
        ' commented the shortcut because of the GPF
        '.ShortCut = strShortCut
        
        If Mid(strCaption, 1, 1) <> "-" Then
          .Checked = bolChecked
          .Enabled = bolEnabled
          Set .Icon = picIcon
        End If
      
        If AbbMenuBar(objNewMember, strParentKey, strKeyBefore) Then
          Call mColMain.Add(objNewMember, strKey)
          If mColMain.Count = 1 Then
            RaiseEvent MenuBarsInitialized
          End If
        Else
          Set objNewMember = Nothing
        End If
      End With
    End If
    
    If mColMain.Count = 0 Then
      Set mobjImageList = Nothing
      Set mobjMenu = Nothing
    End If
  End If
  
  ' Return the object created
  Set Add = objNewMember
  Set objNewMember = Nothing
End Function

Public Property Get Item(ByVal strKey As String) As Bar
Attribute Item.VB_UserMemId = 0
  On Error Resume Next
  Set Item = mColMain(strKey)
  On Error GoTo 0
End Property

Public Property Get Count() As Long
  Count = mColMain.Count
End Property

Public Sub Remove(ByVal strKey As String)
  If Not Item(strKey) Is Nothing Then
    If mobjMenu.Count = 1 Then
      Call InitImageList
      Call mobjMenu.Clear
    Else
      Call mobjMenu.RemoveItem(strKey)
      If Item(strKey).IconIndex > -1 Then
        mobjImageList.ListImages.Remove (strKey)
      End If
    End If
    
    Call mColMain.Remove(strKey)
    
    If mColMain.Count = 0 Then
      RaiseEvent MenuBarsRemoved
    End If
  End If
End Sub

Public Sub Clear()
  Dim bolRaise As Boolean
  
  If mColMain.Count > 0 Then
    bolRaise = True
    Call InitImageList
    Call mobjMenu.Clear
    Do While mColMain.Count > 0
      mColMain.Remove mColMain.Count
    Loop
  End If
  
  If bolRaise Then RaiseEvent MenuBarsRemoved
End Sub

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
  Set NewEnum = mColMain.[_NewEnum]
End Property

Private Sub Class_Initialize()
  Set mColMain = New Collection
  mlngParenthWnd = -1
  Set mobjImageList = Nothing
End Sub

Private Sub Class_Terminate()
  Set mColMain = Nothing
  
  Set mobjImageList = Nothing
  If Not mobjMenu Is Nothing Then
    mobjMenu.Clear
  End If
  Set mobjMenu = Nothing
End Sub

Private Sub mobjMenu_Click(ItemNumber As Long)
  RaiseEvent Click(mColMain(mobjMenu.ItemKey(ItemNumber)))
End Sub

Private Sub mobjMenu_ItemHighlight(ItemNumber As Long, bEnabled As Boolean, bSeparator As Boolean)
  If Not bSeparator Then
    RaiseEvent MouseOver(mColMain(mobjMenu.ItemKey(ItemNumber)))
  End If
End Sub

Public Property Get FirstBar() As Bar
  If Not mobjMenu Is Nothing Then
    If mobjMenu.Count > 0 Then
      Set FirstBar = Item(mobjMenu.ItemKey(1))
    End If
  End If
End Property
