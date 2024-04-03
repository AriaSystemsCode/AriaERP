VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Bar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

Private mobjMenu         As vbpAriaPMenu.cPopupMenu
Private mobjImageList    As ImageList
Private mlngIndex        As Long
Private mstrKey          As String
Private mstrCaption      As String
Private mstrShorCut      As String
Private mbolChecked      As Boolean
Private mbolEnabled      As Boolean
Private mlngIconIndex    As Long
Private mlngParentIndex  As Long

Private Sub Class_Initialize()
  mlngIconIndex = -1
  mlngIndex = -1
End Sub

Private Sub Class_Terminate()
  Set mobjImageList = Nothing
  Set mobjMenu = Nothing
End Sub

Friend Property Let Key(ByVal strData As String)
  mstrKey = strData
End Property

Public Property Get Key() As String
  Key = mstrKey
End Property

Public Property Let Caption(ByVal strData As String)
  If Not mobjMenu Is Nothing Then
    mstrCaption = strData
    If mlngIndex > -1 Then
      mobjMenu.Caption(mlngIndex) = CaptionWithShortCut
    End If
  End If
End Property

Public Property Get Caption() As String
  Dim lngPos As Long
  lngPos = InStr(1, mstrCaption, vbTab, vbTextCompare)
  
  Dim strRetValue As String
  strRetValue = mstrCaption
  If lngPos > 0 Then
    strRetValue = Mid$(mstrCaption, 1, lngPos - 1)
  End If
  
  Caption = strRetValue
End Property

'Public Property Let ShortCut(ByVal strData As String)
'  If Not mobjMenu Is Nothing Then
'    mstrShorCut = strData
'    If mlngIndex > -1 Then
'      mobjMenu.Caption(mlngIndex) = CaptionWithShortCut
'    End If
'  End If
'End Property
'
'Public Property Get ShortCut() As String
'  ShortCut = mstrShorCut
'End Property

Friend Property Get CaptionWithShortCut() As String
  CaptionWithShortCut = mstrCaption
  If Len(Trim(mstrShorCut)) > 0 Then
    CaptionWithShortCut = mstrCaption & vbTab & mstrShorCut
  End If
End Property

Public Property Let Checked(ByVal bolData As Boolean)
  If Not mobjMenu Is Nothing Then
    mbolChecked = bolData
    If mlngIndex > -1 Then
      mobjMenu.Checked(mlngIndex) = bolData
    End If
  End If
End Property

Public Property Get Checked() As Boolean
  Checked = mbolChecked
End Property

Public Property Let Enabled(ByVal bolData As Boolean)
  If Not mobjMenu Is Nothing Then
    mbolEnabled = bolData
    If mlngIndex > -1 Then
      mobjMenu.Enabled(mlngIndex) = bolData
    End If
  End If
End Property

Public Property Get Enabled() As Boolean
  Enabled = mbolEnabled
End Property

Public Property Set Icon(ByVal picIcon As StdPicture)
  If Not mobjImageList Is Nothing And Not mobjMenu Is Nothing Then
    If mlngIconIndex = -1 Then
      If Not picIcon Is Nothing Then
        mlngIconIndex = mobjImageList.ListImages.Count
        Call mobjImageList.ListImages.Add(mlngIconIndex + 1, mstrKey, picIcon)
        mobjMenu.ImageList = mobjImageList
      End If
    Else
      If picIcon Is Nothing Then
        Call mobjImageList.ListImages.Remove(mlngIconIndex + 1)
        mlngIconIndex = -1
      Else
        Set mobjImageList.ListImages(mlngIconIndex + 1).Picture = picIcon
      End If
      mobjMenu.ImageList = mobjImageList
    End If
  End If
End Property

Public Property Get Icon() As StdPicture
  If mlngIconIndex > -1 And Not mobjImageList Is Nothing Then
    Set Icon = mobjImageList.ListImages(mlngIconIndex + 1).Picture
  End If
End Property

Friend Property Let IconIndex(ByVal lngData As Long)
  mlngIconIndex = lngData
End Property

Friend Property Get IconIndex() As Long
  IconIndex = mlngIconIndex
End Property

Friend Property Let ParentIndex(ByVal lngData As Long)
  mlngParentIndex = lngData
End Property

Friend Property Get ParentIndex() As Long
  ParentIndex = mlngParentIndex
End Property

Friend Property Set ImageList(ByRef objList As ImageList)
  Set mobjImageList = objList
End Property

Friend Property Set Menu(ByRef objMenu As vbpAriaPMenu.cPopupMenu)
  Set mobjMenu = objMenu
End Property

Friend Property Let Index(ByRef lngIndex As Long)
  mlngIndex = lngIndex
End Property

