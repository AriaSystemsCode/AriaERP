VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Fields"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mcolFields As New Collection
Public Sub add(ByVal strField)
mcolFields.add strField
End Sub

Public Sub clear()
While mcolFields.count > 0
  mcolFields.Remove 1
Wend
End Sub

Property Get count() As Integer
count = mcolFields.count
End Property

Property Get item(ByVal fieldsIndex As Integer) As String
item = mcolFields.item(fieldsIndex)
End Property
