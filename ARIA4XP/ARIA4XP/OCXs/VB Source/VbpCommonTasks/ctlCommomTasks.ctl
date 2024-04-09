VERSION 5.00
Begin VB.UserControl ctlCommomTasks 
   AutoRedraw      =   -1  'True
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   ScaleHeight     =   3600
   ScaleWidth      =   4800
   Begin vbpCommonTasks.vbalExplorerBarCtl vbalExplorerBarCtl 
      Height          =   3225
      Left            =   60
      TabIndex        =   0
      Top             =   90
      Width           =   2955
      _extentx        =   5212
      _extenty        =   5689
      backcolorend    =   0
      backcolorstart  =   0
   End
End
Attribute VB_Name = "ctlCommomTasks"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Event ItemClick(objItem As Object)

Private Sub UserControl_Resize()
  Call vbalExplorerBarCtl.Move(ScaleLeft, _
                               ScaleTop, _
                               ScaleWidth, _
                               ScaleHeight)
End Sub

Public Function GetObject() As Object
  Set GetObject = vbalExplorerBarCtl
End Function

Public Function AddNewControl(ByVal strName As String, _
                              ByVal strControlType As String, _
                              ByVal BolCheckForExist As Boolean) As Object
If BolCheckForExist Then
  Dim objCont
  For Each objCont In UserControl.Controls
    If objCont.Name = strName Then
      Set AddNewControl = objCont
    End If
  Next objCont
End If

If AddNewControl Is Nothing Then
  Set AddNewControl = UserControl.Controls.Add(strControlType, strName)
End If
End Function

Public Sub SetPicture(ByVal vntPicture)
  UserControl.Picture = vntPicture
End Sub

Private Sub vbalExplorerBarCtl_ItemClick(ByRef objItem As cExplorerBarItem)
  RaiseEvent ItemClick(objItem)
End Sub
